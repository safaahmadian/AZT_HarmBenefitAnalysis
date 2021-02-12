#------define states as class ----------------

model_state <- function(name, exac_rate, cost, qaly, trans_list, has_adv_evnt) {
  value <- list(name = name, exac_rate = exac_rate, cost = cost, qaly = qaly, trans_list = trans_list, has_adv_evnt = has_adv_evnt)
  attr(value, 'class') <- 'model_state' #  test it with: print(attr(states_list[[1]],'class'))
  return(value)
}

print.model_state<- function(obj){
  cat('Name = ', obj$name, '\n')
  cat('Exac_rate = ', obj$exac_rate, '\n')
  cat('Cost = ', obj$cost, '\n')
  cat('QALY = ', obj$qaly, '\n')
  cat('Transition list = ', obj$trans_list, '\n')
  cat('get treatment? ', obj$has_adv_evnt, '\n')
}

define_states <- function(is_treatment_group){
  states = list()
  
  for (i in c(1:(total_states_tx_group-1))){
    cost = costs[i]
    qaly = qalys[i]
    
    if(i <= disease_states){ #states with adverse events
      has_adv_evnt = TRUE
      name = paste("State", i,"with_adv_evnt", sep = "_")
      trans_list = fill_state_trans(i) 
      exac_rate = exac_rates_notx[i]
    }
    else{ # normal states
      has_adv_evnt = FALSE
      name = paste("State", i,"tx", sep = "_")
      trans_list = fill_state_trans(i) 
      if(is_treatment_group){
        exac_rate = change_by_odds(exac_rates_notx[i],treatment_effect)
        # ------------QUIT_RATE-----???----------
      }
      else{
        exac_rate = exac_rates_notx[i]
      }
    }
    curr_state <- model_state(name, exac_rate, cost, qaly, trans_list, has_adv_evnt) 
    states[[length(states)+1]] <- curr_state
  }
  curr_state = model_state("Death", 0, 0, 0, c(rep(0,(length(states))),1), FALSE)  
  states[[length(states)+1]] <- curr_state
  
  return(states)
}

#----------------------

markov_cal <- function(input, trans_mat, is_treatment_group){
  
  total_years <- input$total_years
  states_num <- input$states_num
  gold_stages <- input$gold_stages
  discount_rate <- input$discount_rate
  
  distributions_mat = matrix(0, nrow = (total_years+1), ncol =(states_num), byrow = TRUE) 
  results_mat = matrix(0 , nrow = (total_years), ncol = 24, byrow = TRUE) 
  colnames(results_mat) = c("1.Total exacerbations:  ","2.Severe exacerbations:  ", "3.Have total hearing-loss by this time:  ",
                            "4.New hearing-loss:  ","5.Rate of gast_events:  ","6.mortality rates: ",
                            "7.QALY-loss due to exacerbations:  ","8.QALY-loss due to hearing-loss:  ","9.QALY-loss due to GIs: ",
                            "10.Final QALY:  ", "11.alive: ", "12.base QALY: ", "13.discounted exacerbations", "14.discounted severe exacs:","15.discounted QALY:",
                            "16.discounted hearing incidents:", "17.discounted GI symptoms:", "18.mortality due to CVD:","19.discounted mortality:(DELETED)",
                            "20. death due to exacerbation", "21. death except exacerbation","22. discounted death due to exacerbation", "23.discounted death except exacerbation", 
                            "24.discounted hearing prevalence:")
  rownames(results_mat) = lapply(c(1:total_years), function(x) paste0("year ",x))
  results_mat_discounted = results_mat
  
  distributions_mat[1,] = input$initial_dists
  
  for (i in c(1:(total_years))){
    
    for (j in c(1:(states_num))){
      distributions_mat[i+1,j] = sum(distributions_mat[i,]*trans_mat[[i]][,j]) # So, each row of dist_mat[i,] shows what was the dists in the beginning of year i => dist_mat[2,7] shows how many died during the first year or how many poeple are not alive in the beginning of year 2
    }
    
    if(is_treatment_group){
      #First update the exac_rates values for each year
      exac_rates <- c((input$exac_rates_notx[1:gold_stages]*current_effect(i,input$treatment_effect,input$resist_param)),input$exac_rates_notx[(gold_stages+1):states_num])
      severe_exac_rates <- c((input$severe_exac_rates_notx[1:gold_stages]*current_effect(i,input$treatment_effect,input$resist_param)),input$severe_exac_rates_notx[(gold_stages+1):states_num])
      tx_GI_rate <- input$rate_gast_events*input$gast_evnt_RR
      results_mat[i,18] = (1-distributions_mat[i,states_num])*input$background_mortality_probs[i]*((360+5*input$CVD_risk)/365-1) # those alive in last year*back*excess risk of CVD = (RR-1)
    }
    else{
      exac_rates <- input$exac_rates_notx
      severe_exac_rates <- input$severe_exac_rates_notx
      tx_GI_rate <- input$rate_gast_events
      results_mat[i,18] = 0
    }
    
    # distrubution i+1 is in fact the proportion of alive poeple in the previous year, so technically we are calculating the rate of exacs for the previous year
    results_mat[i,1] = sum(distributions_mat[i+1,]*exac_rates)# Here we are assuming that those who experienced adv_events up to this year, have an exac_rate similar to those who don't get tx (so we assume they didn't recieve tx in the last year)
    results_mat[i,2] = sum(distributions_mat[i+1,]*severe_exac_rates)
    
    # prevalence of hearing loss:
    results_mat[i,3] = sum(distributions_mat[i+1,(gold_stages+1):(states_num-1)]) 
    
    # new incidents of hearing-loss
    for (j in c((gold_stages+1):(states_num-1))){
      results_mat[i,4] = results_mat[i,4] + (distributions_mat[i+1,j] - distributions_mat[i,j]*trans_mat[[i]][j,j])
    }
    
    #GI rates
    results_mat[i,5] = tx_GI_rate*sum(distributions_mat[i+1,1:gold_stages]) + input$rate_gast_events*sum(distributions_mat[i+1,(gold_stages+1):(states_num-1)])
    
    #Exac QALY loss:
    results_mat[i,7] = sum(distributions_mat[i+1,]*((exac_rates-severe_exac_rates)*input$qaly_loss_exac + severe_exac_rates*input$qaly_loss_exac_severe)) 
    
    # calculate death due to exac:
    results_mat[i,20] =  sum(distributions_mat[i,]*trans_mat[[i]][,(states_num+1)])
    
    # total  death in this year
    results_mat[i,6] = distributions_mat[i+1,states_num]-distributions_mat[i,states_num] 
    # death except for exacs
    results_mat[i,21] = results_mat[i,6] - results_mat[i,20]
    # alive
    results_mat[i,11] = 1-distributions_mat[i+1,states_num]
    
    #hearing impairment QALY loss:
    results_mat[i,8] = results_mat[i,4]*input$qaly_loss_hearing + (results_mat[i,3]-results_mat[i,4])*(input$qaly_loss_hearing+input$hearing_improvement)
    #GI QALY loss:
    results_mat[i,9] = results_mat[i,5]*input$qaly_loss_GI
    
    results_mat[i,10] = sum(distributions_mat[i+1,]*input$qaly_baseline)-results_mat[i,7]-results_mat[i,8]-results_mat[i,9]
    results_mat[i,12] = sum(distributions_mat[i+1,]*input$qaly_baseline)
    
    results_mat[i,13] = results_mat[i,1]/(1+discount_rate)^(i-1)
    results_mat[i,14] = results_mat[i,2]/(1+discount_rate)^(i-1)
    results_mat[i,15] = results_mat[i,10]/(1+discount_rate)^(i-1)
    results_mat[i,16] = results_mat[i,4]/(1+discount_rate)^(i-1)
    results_mat[i,24] = results_mat[i,3]/(1+discount_rate)^(i-1)
    results_mat[i,17] = results_mat[i,5]/(1+discount_rate)^(i-1)
    results_mat[i,22] = results_mat[i,20]/(1+discount_rate)^(i-1)
    results_mat[i,23] = results_mat[i,21]/(1+discount_rate)^(i-1)
    
  }
  
  # print("distributions matrix:")
  # for (i in c(1:dim(distributions_mat)[1])){
  #   cat(paste0(i,":  "))
  #   for (j in c(1:dim(distributions_mat)[2])){
  #     cat(distributions_mat[i,j],"    ")
  #   }
  #   cat("\n")
  # }
  
  # print(distributions_mat)
  
  
  
  # print("Sanity check 2 (sum of rows in dist_mat:")
  # for (i in c(1:dim(distributions_mat)[1])){
  #   print(sum(distributions_mat[i,]))
  # }
  
  
  return(results_mat)
}



# update_exac_history <- function(input, mod_exac, sev_exac){ #ref : ECLIPSE
#   mod_exac_rates <- exp(-0.9060150 + 0.4077639*mod_exac + 0.2357867*sev_exac)
#   
#   input$severe_exac_rates_notx <- exp(-2.1514253 + 0.2388958*mod_exac + 0.7738942*sev_exac)
#   input$exac_rates_notx <- exp(-0.6575906 + 0.3732514*mod_exac + 0.4268865*sev_exac)
# 
#   cat ("plus: " , (mod_exac_rates + input$severe_exac_rates_notx ), '\n' )
#   cat ("total: ", input$exac_rates_notx, '\n')
#   
#   return(input)
# }


#---------------------------------------

# run_model <- function(is_treatment_group){ # is_treatment_group = TRUE => running the model for the treatment group
# 
#   transition_matrix = list()
#   input <- model_input$values
# 
#   for (i in c(1:input$total_years)){
#     if(is_treatment_group){
#       input$exac_rates_tx <<- c((input$exac_rates_notx[1:input$gold_stages]*current_effect(i,input$treatment_effect,input$resist_param)),input$exac_rates_notx[(input$gold_stages+1):input$states_num])
#       input$severe_exac_rates_tx <<- c((input$severe_exac_rates_notx[1:input$gold_stages]*current_effect(i,input$treatment_effect,input$resist_param)),input$severe_exac_rates_notx[(input$gold_stages+1):input$states_num])
#     }
#     transition_matrix[[i]] = fill_trans_mat(input,i,is_treatment_group)
#   }
#   results = markov_cal(input,transition_matrix,is_treatment_group)
#   print(results)
#   print("Rate of CVD:")
#   print(sum(results[,18]))
#   print("Discounted rate of CVD:")
#   print(sum(results[,19]))
# 
#   print("total QALY loss due to exacerbations:")
#   print(sum(results[,7]))
# 
#   print("total QALY loss due to hearing loss:")
#   print(sum(results[,8]))
# 
#   print("total QALY loss due to GIs:")
#   print(sum(results[,9]))
# 
#   print("Total QALY:")
#   print(sum(results[,10]))
#   print("Total discounted QALY:")
#   print(sum(results[,15]))
# 
#   print("Total person-year:")
#   print(sum(results[,11]))
# 
#   print("Total QALY per person-year:")
#   print(sum(results[,10])/sum(results[,11]))
# 
#   print("Total discounted QALY per person-year:")
#   print(sum(results[,15])/sum(results[,11]))
# }

#----------------
