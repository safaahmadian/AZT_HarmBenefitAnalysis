
library(ggplot2)
library(dplyr)
library(ggthemes)
library(hrbrthemes)

library(reshape2)
library(patchwork) # To display 2 charts together
library(plotly)


estBetaParams <- function(mu, var) {

  
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)

  return(params = list(alpha = alpha, beta = beta))
}

set_probabilistic_params <- function(input){ #Gamma : œ: shape  ß:scale ( if ß=1 : standard Gamma distribution. ß =   œ = rate)

  stages <- input$gold_stages
  
  treatment_effect_rnd <- rlnorm(1,mean = log(input$treatment_effect), sd = (log(input$treatment_effect_UCI)-log(input$treatment_effect))/1.96)
  input$treatment_effect <- treatment_effect_rnd
  
  hearing_loss_RR_rnd <- rlnorm(1,mean = log(input$hearing_loss_RR), sd = (log(input$hearing_loss_RR_UCI)-log(input$hearing_loss_RR))/1.96)
  input$hearing_loss_RR <- hearing_loss_RR_rnd
  # if(is.na(log(input$hearing_loss_RR))){
  #   print("log(input$hearing_loss_RR) is Nan")
  # }
  # if(is.na((log(input$hearing_loss_RR_UCI)-log(input$hearing_loss_RR))/1.96)){
  #   print("(log(input$hearing_loss_RR_UCI)-log(input$hearing_loss_RR))/1.96 is Nan")
  # }

  hearing_rate_change_rnd <- rnorm(1,mean = input$hearing_rate_change, sd =(input$hearing_rate_change_UCI-input$hearing_rate_change)/1.96)
  input$hearing_rate_change <- hearing_rate_change_rnd

  gast_evnt_RR_rnd <- rlnorm(1,mean = log(input$gast_evnt_RR), sd = (log(input$gast_evnt_RR_UCI)-log(input$gast_evnt_RR))/1.96)
  input$gast_evnt_RR <- gast_evnt_RR_rnd

  CVD_risk_rnd <- rlnorm(1,mean = log(input$CVD_risk), sd = (log(input$CVD_risk_UCI)-log(input$CVD_risk))/1.96)
  input$CVD_risk <- CVD_risk_rnd
  
  trans_probs_rnd = integer(stages)
  exac_rates_notx_rnd = integer(stages)
  severe_exac_rates_notx_rnd = integer(stages)
  qaly_baseline_rnd = integer(stages)
  qaly_loss_exac_rnd = integer(stages)
  qaly_loss_exac_severe_rnd = integer(stages)
  
  for (i in c(1:stages)){
    if(i<stages){
      trans_probs_rnd[i] <- rbeta(1, shape1 = input$trans_probs_alpha[i], shape2 = input$trans_probs_beta[i])
    }
    
    #--------------------------------------Exacerbation rates-------------------------------
    exac_rates_notx_rnd[i] <- rlnorm(1, mean = log(input$exac_rates_notx[i]), sd = (log(input$exac_rates_notx_UCI[i])-log(input$exac_rates_notx[i]))/1.96)
    severe_exac_rates_notx_rnd[i] <- rlnorm(1, mean = log(input$severe_exac_rates_notx[i]), sd = (log(input$severe_exac_rates_notx_UCI[i])-log(input$severe_exac_rates_notx[i]))/1.96)

    # if(is.na(log(input$severe_exac_rates_notx[i]))){
    #   print("log(input$severe_exac_rates_notx[i]) is Nan")
    # }
    # if(is.na((log(input$severe_exac_rates_notx_UCI[i])-log(input$severe_exac_rates_notx[i]))/1.96)){
    #   print("(log(input$severe_exac_rates_notx_UCI[i])-log(input$severe_exac_rates_notx[i]))/1.96 is Nan")
    # }
        #--------------------------------------UTILITY-------------------------------
    qaly_base_params = estBetaParams(input$qaly_baseline[i],((input$qaly_baseline_UCI[i]-input$qaly_baseline[i])/1.96)^2)
    qaly_baseline_rnd[i]<- rbeta(n=1, shape1 = qaly_base_params$alpha, shape2 = qaly_base_params$beta)

    qaly_exac_params = estBetaParams(input$qaly_loss_exac[i],input$qaly_loss_exac_SE[i]^2)
    qaly_loss_exac_rnd[i]<- rbeta(n=1, shape1 = qaly_exac_params$alpha, shape2 = qaly_exac_params$beta)

    qaly_sev_exac_params = estBetaParams(input$qaly_loss_exac_severe[i],input$qaly_loss_exac_severe_SE[i]^2)
    qaly_loss_exac_severe_rnd[i] <- rbeta(n=1, shape1 = qaly_sev_exac_params$alpha, shape2 = qaly_sev_exac_params$beta)

  }
  
  input$trans_probs <- c(trans_probs_rnd,trans_probs_rnd,0)
  input$exac_rates_notx <- c(exac_rates_notx_rnd,exac_rates_notx_rnd,0)
  input$severe_exac_rates_notx <- c(severe_exac_rates_notx_rnd,severe_exac_rates_notx_rnd,0)
  input$qaly_baseline <- c(qaly_baseline_rnd,qaly_baseline_rnd,0)
  input$qaly_loss_exac <- c(qaly_loss_exac_rnd,qaly_loss_exac_rnd,0)
  input$qaly_loss_exac_severe <- c(qaly_loss_exac_severe_rnd,qaly_loss_exac_severe_rnd,0)

  exac_mortality_params = estBetaParams(input$exac_mortality_prob,((input$exac_mortality_prob_UCI-input$exac_mortality_prob)/1.96)^2)
  exac_mortality_rnd <- rbeta(n=1, shape1 = exac_mortality_params$alpha, shape2 =exac_mortality_params$beta)
  input$exac_mortality_prob <- exac_mortality_rnd

  qaly_hearing_params = estBetaParams(input$qaly_loss_hearing,input$qaly_loss_hearing_SE^2)
  qaly_loss_hearing_rnd <- rbeta(n=1, shape1 = qaly_hearing_params$alpha, shape2 = qaly_hearing_params$beta)
  input$qaly_loss_hearing <-qaly_loss_hearing_rnd

  hearing_improvement_rnd <- rnorm(n = 1, mean = input$hearing_improvement, sd = ((input$hearing_improvement_UCI-input$hearing_improvement)/1.96))
  input$hearing_improvement <- hearing_improvement_rnd

  qaly_GI_params = estBetaParams(input$qaly_loss_GI,input$qaly_loss_GI_SE^2)
  qaly_loss_GI_rnd <- rbeta(n=1, shape1 = qaly_GI_params$alpha, shape2 = qaly_GI_params$beta)
  input$qaly_loss_GI <- qaly_loss_GI_rnd
  
  RR_exac_0hist_rnd <- rnorm(n = 1, mean = input$RR_exac_0hist[1], sd =input$RR_exac_0hist[2] )
  input$RR_exac_0hist <- RR_exac_0hist_rnd
  
  RR_exac_2hist_rnd <- rnorm(n = 1, mean = input$RR_exac_2hist[1], sd =input$RR_exac_2hist[2] )
  input$RR_exac_2hist <- RR_exac_2hist_rnd
  
  RR_exac_3hist_rnd <- rnorm(n = 1, mean = input$RR_exac_3hist[1], sd =input$RR_exac_3hist[2] )
  input$RR_exac_3hist <- RR_exac_3hist_rnd

  # cat('treatment_effect = ', input$treatment_effect, '\n')
  # cat('hearing_loss_RR = ', input$hearing_loss_RR, '\n')
  # cat('hearing_rate_change = ', input$hearing_rate_change, '\n')
  # cat('gast_evnt_RR = ', input$gast_evnt_RR, '\n')
  # cat('CVD_risk = ', input$CVD_risk, '\n')
  # cat('trans_probs ', input$trans_probs, '\n')
  # 
  # cat('exac_mortality_prob = ', input$exac_mortality_prob, '\n')
  # # 
  # cat('exac_rates_notx  = ', input$exac_rates_notx , '\n')
  # cat('severe_exac_rates_notx  = ',  input$severe_exac_rates_notx , '\n')
  # 
  # cat('qaly_baseline = ', input$qaly_baseline, '\n')
  # cat('qaly_loss_exac = ', input$qaly_loss_exac, '\n')
  # cat('qaly_loss_exac_severe = ', input$qaly_loss_exac_severe, '\n')
  # cat('qaly_loss_hearing = ', input$qaly_loss_hearing, '\n')
  # cat('qaly_loss_GI = ', input$qaly_loss_GI, '\n')
  # 
  # cat('hearing_improvement= ', input$hearing_improvement, '\n')
  # print("-------------------------------------")
  
  return(input)
}

calculate_mortality <- function(input,year, state, is_tx_group){ 
  # age = input$avg_age + year
  mortality <- rep(0, 2)
  prob_exac_mortality <- input$exac_mortality_prob
  # odds_exac_death = exp(-13 + 0.049*(age)+7.7) #source: EPIC

  if(is_tx_group){
    prob_exac_death = (1-exp(-input$severe_exac_rates_tx[state]))*(prob_exac_mortality)
    # prob_exac_death = (1-exp(-input$severe_exac_rates_tx[state]))*(odds_exac_death/(odds_exac_death+1))
    
    # Added risk due to CVD
    mortality[1] = (input$background_mortality_probs[year]*(360+5*input$CVD_risk)/365)
    #Mortality due to exacerbation
    mortality[2] =  prob_exac_death

  }
  else{
    prob_exac_death = (1-exp(-input$severe_exac_rates_notx[state]))*(prob_exac_mortality)
    # prob_exac_death = (1-exp(-input$severe_exac_rates_notx[state]))*(odds_exac_death/(odds_exac_death+1))
    mortality[1] = input$background_mortality_probs[year] 
    mortality[2] = prob_exac_death
  }
  return(mortality)
}

current_effect <- function(year,treatment_effect,resist_param){
  if (year>1){
    # effect = treatment_effect^(2/year)    # 0.7808472 - 0.9635736

    effect = treatment_effect^exp(-resist_param*(year-1))
    # effect = 1

  }
  else{
    effect = treatment_effect
  }
  return(effect)
}

#update transition matrix for each year
# Tansition Matrix:
#     S'1	        S'2	        S'3	        S1	        S2	        S3	       D
# S'1	P(S'1->S'1)	P(S'1->S'2)	P(S'1->S'3)	P(S'1->S1)	P(S'1->S2)	P(S'1->S3)	P(S'1->D)
# S'2	P(S'1->S'1)	P(S'1->S'2)	P(S'1->S'3)	P(S'1->S1)	P(S'1->S2)	P(S'1->S3)	P(S'1->D)
# S'3	P(S'1->S'1)	P(S'1->S'2)	P(S'1->S'3)	P(S'1->S1)	P(S'1->S2)	P(S'1->S3)	P(S'1->D)
# S3	P(S'1->S'1)	P(S'1->S'2)	P(S'1->S'3)	P(S'1->S1)	P(S'1->S2)	P(S'1->S3)	P(S'1->D)
# S3	P(S'1->S'1)	P(S'1->S'2)	P(S'1->S'3)	P(S'1->S1)	P(S'1->S2)	P(S'1->S3)	P(S'1->D)
# S3	P(S'1->S'1)	P(S'1->S'2)	P(S'1->S'3)	P(S'1->S1)	P(S'1->S2)	P(S'1->S3)	P(S'1->D)
fill_trans_mat <- function(input, year, is_treatment_group){
  
  trans_mat = matrix(0, nrow = input$states_num, ncol = input$states_num+1, byrow = TRUE) # the extra column is to keep the mortality due to exac for each state
  for (i in c(1:(input$states_num-1))){
    trans_mat[i,i+1] = input$trans_probs[i]
    if (i <= input$gold_stages){ 
      trans_mat[i,i+input$gold_stages] = input$hearing_rate_change
      if (is_treatment_group){
        trans_mat[i,i+input$gold_stages] = trans_mat[i,i+input$gold_stages]*input$hearing_loss_RR
      }
    }
    mortality = calculate_mortality(input,year,i,is_treatment_group)
    trans_mat[i,input$states_num] = mortality[1]+mortality[2]  # mortality + mortality due to exac
    trans_mat[i,input$states_num+1] = mortality[2]  #mortality due to exac
    
    trans_mat[i,i] = 1-(Reduce("+",trans_mat[i,c(i:input$states_num)]))
  }
  trans_mat[input$states_num,input$states_num] = 1

  return(trans_mat)
}
  
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
      results_mat[i,18] = (1-distributions_mat[i,states_num])*input$background_mortality_probs[i]*((360+5*input$CVD_risk)/365-1) # those who will not be alive next year due to CVD:  alive in this year*back*excess risk of CVD = (RR-1)
    }
    else{
      exac_rates <- input$exac_rates_notx
      severe_exac_rates <- input$severe_exac_rates_notx
      tx_GI_rate <- input$rate_gast_events
      results_mat[i,18] = 0
    }
    
    results_mat[i,1] = sum(distributions_mat[i,]*exac_rates)
    results_mat[i,2] = sum(distributions_mat[i,]*severe_exac_rates)
    
    # prevalence of hearing loss:
    results_mat[i,3] = sum(distributions_mat[i,(gold_stages+1):(states_num-1)]) 
    
    
    #GI rates
    results_mat[i,5] = tx_GI_rate*sum(distributions_mat[i,1:gold_stages]) + input$rate_gast_events*sum(distributions_mat[i,(gold_stages+1):(states_num-1)])
    
    #Exac QALY loss:
    results_mat[i,7] = sum(distributions_mat[i,]*((exac_rates-severe_exac_rates)*input$qaly_loss_exac + severe_exac_rates*input$qaly_loss_exac_severe)) 
    
    if(i > 1){
      # new incidents of hearing-loss
      for (j in c((gold_stages+1):(states_num-1))){
        results_mat[i,4] = results_mat[i,4] + (distributions_mat[i,j] - distributions_mat[i-1,j]*trans_mat[[i-1]][j,j])
      }
      
      # total death in this year
      results_mat[i,6] = distributions_mat[i,states_num]-distributions_mat[i-1,states_num] 
      # death due to exac:
      results_mat[i,20] =  sum(distributions_mat[i-1,]*trans_mat[[i-1]][,(states_num+1)])
      # other death
      results_mat[i,21] = results_mat[i,6] - results_mat[i,20]
    }
    
    # alive
    results_mat[i,11] = 1-distributions_mat[i,states_num]
    
    #hearing impairment QALY loss:
    results_mat[i,8] = results_mat[i,4]*input$qaly_loss_hearing + (results_mat[i,3]-results_mat[i,4])*(input$qaly_loss_hearing+input$hearing_improvement)
    #GI QALY loss:
    results_mat[i,9] = results_mat[i,5]*input$qaly_loss_GI
    
    results_mat[i,10] = sum(distributions_mat[i,]*input$qaly_baseline)-results_mat[i,7]-results_mat[i,8]-results_mat[i,9]
    results_mat[i,12] = sum(distributions_mat[i,]*input$qaly_baseline)
    
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
  return(results_mat)
}

calliberate_backMortality <- function (){
  
  MC_cycles = 40
  
  new_back_mortality <- model_input$values$background_mortality_probs
  
  for ( cycle in c(1:MC_cycles) ) {
    input <- model_input$values # deterministic
    input$background_mortality_probs <- new_back_mortality
    
    #--------PLACEBO---------
    print(cycle)
    is_treatment_group = FALSE
    
    transition_matrix = list()
    for (i in c(1:input$total_years)){
      transition_matrix[[i]] = fill_trans_mat(input,i,is_treatment_group)
    }
    
    results = markov_cal(input, transition_matrix,is_treatment_group)
    
    final_results = list(total_exacs = sum(results[,13]),total_sev_exacs = sum(results[,14]),
                         hearing_prev = sum(results[,3]),hearing_incidents = sum(results[,16]),GI_events = sum(results[,17]),
                         mortality = sum(results[,6]), mortality_exac = sum(results[,20]), mortality_not_exac = sum(results[,21]),
                         base_qaly = sum(results[,12]),qaly_loss_exac = sum(results[,7]), qaly_loss_hearing = sum(results[,8]), qaly_loss_GI = sum(results[,9]),
                         total_qaly = sum(results[,15]), life_expec = sum(results[,11]))
    
    print("Mortality")
    print(results[,6])
    new_back_mortality <- input$background_mortality_probs-0.03*(results[,20]/results[,11])
    # print(new_back_mortality)





  }
  
}
run_model_probabilistically <- function(){ # is_treatment_group = TRUE => running the model for the treatment group

  MC_cycles = 10
  MC_results_notx= matrix(0, nrow = MC_cycles, ncol = 14, byrow = TRUE) # length = length of final_results_val?
  MC_results_tx= matrix(0, nrow = MC_cycles, ncol = 14, byrow = TRUE) # length = length of final_results_val?
  
  colnames(MC_results_notx) <- (c("total_exacs" ,"total_sev_exacs" , "hearing_prevalence" ,"hearing_incidents", "GI_events" , "mortality" ,"mortality_exac" , "mortality_not_exac" , "base_qaly" ,
                                  "qaly_loss_exac" , "qaly_loss_hearing" , "qaly_loss_GI" , "total_qaly" , "life_expec" ))
  colnames(MC_results_tx) <- (c("total_exacs" ,"total_sev_exacs" , "hearing_prevalence" ,"hearing_incidents", "GI_events" , "mortality" ,"mortality_exac" , "mortality_not_exac" , "base_qaly" ,
                                  "qaly_loss_exac" , "qaly_loss_hearing" , "qaly_loss_GI" , "total_qaly" , "life_expec" ))
  
  # qaly_py= matrix(0, nrow = MC_cycles, ncol = 20, byrow = TRUE) # to calculate net QALY per year 
  
  new_back_mortality <- model_input$values$background_mortality_probs

  for ( cycle in c(1:MC_cycles) ) {
    input <- model_input$values
    # input <- set_probabilistic_params(input)
    input$background_mortality_probs <- new_back_mortality

#     input$severe_exac_rates_notx  <- model_input$values$severe_exac_rates_notx * input$RR_exac_2hist
#     input$exac_rates_notx  <- model_input$values$exac_rates_notx * input$RR_exac_2hist

    #--------PLACEBO---------
    print(cycle)
    is_treatment_group = FALSE

    transition_matrix = list()
    for (i in c(1:input$total_years)){
      transition_matrix[[i]] = fill_trans_mat(input,i,is_treatment_group)
    }

    results = markov_cal(input, transition_matrix,is_treatment_group)

    final_results = list(total_exacs = sum(results[,13]),total_sev_exacs = sum(results[,14]),
                         hearing_prev = sum(results[,3]),hearing_incidents = sum(results[,16]),GI_events = sum(results[,17]),
                         mortality = sum(results[,6]), mortality_exac = sum(results[,20]), mortality_not_exac = sum(results[,21]),
                         base_qaly = sum(results[,12]),qaly_loss_exac = sum(results[,7]), qaly_loss_hearing = sum(results[,8]), qaly_loss_GI = sum(results[,9]),
                         total_qaly = sum(results[,15]), life_expec = sum(results[,11]))


    final_results_vals = as.vector(unlist(final_results))
    MC_results_notx[cycle,] = final_results_vals
    
    new_back_mortality <- input$background_mortality_probs - results[,20]
    print("Mortality after reduction")
    print(results[,21])
    # placebo_qalys = results[,15] # to calculate net QALY per year 
  
    #--------TREATMENT---------
    is_treatment_group = TRUE

    transition_matrix = list()
    for (i in c(1:input$total_years)){

      input$exac_rates_tx <- c((input$exac_rates_notx[1:input$gold_stages]*current_effect(i,input$treatment_effect,input$resist_param)),input$exac_rates_notx[(input$gold_stages+1):input$states_num])
      input$severe_exac_rates_tx <- c((input$severe_exac_rates_notx[1:input$gold_stages]*current_effect(i,input$treatment_effect,input$resist_param)),input$severe_exac_rates_notx[(input$gold_stages+1):input$states_num])

      transition_matrix[[i]] = fill_trans_mat(input,i,is_treatment_group)
    }

    results = markov_cal(input, transition_matrix,is_treatment_group)

    final_results = list(total_exacs = sum(results[,13]),total_sev_exacs = sum(results[,14]),
                                               hearing_prev = sum(results[,3]),hearing_incidents = sum(results[,16]),GI_events = sum(results[,17]), mortality = sum(results[,6]),
                                               mortality_exac = sum(results[,20]), mortality_not_exac = sum(results[,21]),base_qaly = sum(results[,12]),
                                               qaly_loss_exac = sum(results[,7]), qaly_loss_hearing = sum(results[,8]), qaly_loss_GI = sum(results[,9]),
                                               total_qaly = sum(results[,15]), life_expec = sum(results[,11]))

    final_results_vals = as.vector(unlist(final_results))

    MC_results_tx[cycle,] = final_results_vals

    # qaly_py[cycle,] <-  (results[,15] - placebo_qalys) # to calculate net QALY per year
  }
  
  # print(colMeans(qaly_py)) # To calculate net QALT per year
  
  print("--------PLACEBO---------")
  results_avg = colMeans(MC_results_notx)
  print(results_avg)

  # print("+++++++++++ result per person/year +++++++++++")
  # results_py <- (MC_results_notx/MC_results_notx[,"life_expec"])
  # results_avg_py = colMeans(results_py)
  # print(results_avg_py)

  print("+++++++++++ CIs +++++++++++")
  for (i in c(1:ncol(MC_results_notx))){
    print(colnames(MC_results_notx)[i])
    print(quantile(MC_results_notx[,i],c(0.025, 0.975) ))

  }
  print("--------TREATMENT---------")
  results_avg = colMeans(MC_results_tx)
  print(results_avg)
  
  
  print("+++++++++++ CIs +++++++++++")
  for (i in c(1:ncol(MC_results_tx))){
    print(colnames(MC_results_tx)[i])
    print(quantile(MC_results_tx[,i],c(0.025, 0.975) ))
  }
  
  print("--------Net QALY---------")
  
  delta <- (MC_results_tx[,"total_qaly"]- MC_results_notx[,"total_qaly"])

  print("probability of positive net QALY:")
  print(pnorm(0, mean = mean(delta), sd = sd(delta), lower.tail=FALSE))
  print(quantile(delta,c(0.025, 0.975) ))
  print(mean(delta))
  
  print("+++++++++++ result per person/year +++++++++++")
  netQaly_py <- (MC_results_tx[,"total_qaly"]/MC_results_tx[,"life_expec"]) - (MC_results_notx[,"total_qaly"]/MC_results_notx[,"life_expec"])
  print(pnorm(0, mean = mean(netQaly_py), sd = sd(netQaly_py), lower.tail=FALSE))
  print(quantile(netQaly_py,c(0.025, 0.975) ))
  print(mean(netQaly_py))
  
  net_outcome <- (MC_results_tx) - (MC_results_notx)
  for (i in c(1:ncol(MC_results_notx))){
    print(colnames(MC_results_notx)[i])
    
    print(quantile(net_outcome[,i],c(0.025, 0.975) ))
    print(mean(net_outcome[,i]))
    
  }
  plot_netqaly (delta)
  
}

plot_netqaly <- function(net_qaly){
  df <- data.frame(net_qaly)

  p <- ggplot(df, aes(net_qaly)) + 
    ylab ("Density") + xlab ("\n Incremental Net QALY") + theme_minimal()+
    stat_function(fun = dnorm, args = list(mean = mean(df$net_qaly), sd = sd(df$net_qaly)), color="cyan4")+
    geom_area(stat = "function", args = list(mean = mean(df$net_qaly), sd = sd(df$net_qaly)), fun = dnorm, fill = "#E8F8F5", xlim = c(0, 0.6)) +
    # geom_segment(aes(x = mean(net_qaly), xend = mean(net_qaly), yend = 0, y=10),color = "tomato" )+
    geom_histogram(aes(y =..density..),
                   breaks = seq(-0.2, 0.6, by = 0.025), 
                   colour = "grey", fill = "white", alpha = .2) +
    theme(plot.subtitle = element_text(vjust = 1),plot.title = element_text(vjust = 0)) +
    xlim(-0.2,0.6)+
    geom_segment(aes(x = 0, xend = 0, yend = 0, y=0.75),color = "tomato",  )
    
  print(p)
  ggsave(plot = p, filename = "netQALY_1hist.pdf", height=15, units = "cm", dpi=300)
  
}


get_net_qaly <- function( res_param, exac_mort_param, MC_cycles = 1000){
  delta = c()
  
  for(cycle in c(1:MC_cycles)){   # Monte Carlo
    # print(cycle)
    input <- model_input$values
    input <- set_probabilistic_params(input)
    
    input$exac_mortality_prob = exac_mort_param
    input$resist_param  <- res_param
    
    # --------PLACEBO------------------------------------------
    is_treatment_group = FALSE
    
    transition_matrix = list()
    for (i in c(1:input$total_years)){
      transition_matrix[[i]] = fill_trans_mat(input,i,is_treatment_group)
    }
    
    results = markov_cal(input, transition_matrix,is_treatment_group)
    
    placebo_qaly = sum(results[,15])
    
    # --------TREATMENT-----------------------------------------
    is_treatment_group = TRUE
    
    transition_matrix = list()
    for (i in c(1:input$total_years)){
      
      input$exac_rates_tx <- c((input$exac_rates_notx[1:input$gold_stages]*current_effect(i,input$treatment_effect,input$resist_param)),input$exac_rates_notx[(input$gold_stages+1):input$states_num])
      input$severe_exac_rates_tx <- c((input$severe_exac_rates_notx[1:input$gold_stages]*current_effect(i,input$treatment_effect,input$resist_param)),input$severe_exac_rates_notx[(input$gold_stages+1):input$states_num])
      
      transition_matrix[[i]] = fill_trans_mat(input,i,is_treatment_group)
    }
    
    results2 = markov_cal(input, transition_matrix,is_treatment_group)
    
    treatment_qaly = sum(results2[,15])
    
    # --------DELTA----------------------------------------------
    delta = c(delta, (treatment_qaly - placebo_qaly)) 
    
  }
  
  return(mean(delta))
  
}

two_way_sensitivity <- function(){
  # sensitivityRange <- as_tibble(expand.grid(resist = seq(0,1,0.5), exac_mort = seq(0, 0.06, 0.03)))
  # sensitivityRange %>% mutate(net_qaly = get_net_qaly(10,resist,exac_mort))
  
  resist_range <- seq(0,1.1,0.1) 
  exac_mort_range <- seq(0, 0.15, 0.03)
  
  heatmap_input =  as.data.frame(matrix(0 , nrow = length(exac_mort_range)*length(resist_range), ncol = 3, byrow = TRUE) )
  colnames(heatmap_input) <- c("resistance", "exac_mort", "net_QALY")
  net_qaly = c()
  
  index = 0
  for (z in exac_mort_range){
    for ( k in resist_range) { # parameter range
      index = index + 1
      heatmap_input[index,1:2] = c(k, z)
      delta <- get_net_qaly (res_param = k,exac_mort_param = z )
      heatmap_input [index,3] <- delta
      
    }
  }
  saveRDS(heatmap_input, file = "heatmap_resistance.Rds")
  return(heatmap_input)
}

heatmap_plot <- function(data_input){
  # breaks <- c(-0.2,0.3,0.4)
  
  plot <- ggplot(data_input) + 
        geom_tile(aes(x = resistance, y = exac_mort, fill = net_QALY < 0))+
        ylab("mortality due to exacerbation")  + xlab("\n  resistance parameter ") 
        # scale_fill_distiller(palette = "Spectral", direction=-1, limits = c(-1,1)*max(abs(data_input$net_QALY))) +
        #scale_colour_gradient2(low = "red", mid = "green", high = "blue", midpoint = 0, breaks = breaks) +
    
        # theme_minimal()
  ggsave(plot = plot, filename = "Two_way_sensitivity_analysis.pdf", height=15, units = "cm", dpi=300)
  print(plot)
  
}

sensitivity_analysis_probabilistic <- function(){ 
  
  positive_net_qaly = c()
  net_qaly = c()  
  MC_cycles = 1000
  
  # var_range <- seq(0.1,1,0.05)
  # model = model_input$values$qaly_loss_hearing
  
  # var_range <- seq(0,0.3,0.05)
  # model = model_input$values$qaly_loss_GI
  
  # var_range <- seq(0,3,0.05)
  # model = 0.0377
  
  # var_range <- seq(0,1,0.1)
  # model = model_input$values$resist_param
  
  # var_range <- seq(0,1.1,0.1)
  # model = 1.626
  # AVG_exac_disqaly = 0.0377
  
  var_range <- seq(0,1,0.1)
    
    for ( k in var_range) { # parameter range
      delta = c()
      proportion = c()
      for(cycle in c(1:MC_cycles)){   # Monte Carlo
        print(cycle)
        input <- model_input$values
        input <- set_probabilistic_params(input)
        
        # input$resist_param  <-  k
        # input$exac_mortality_prob <- exac_mort_range[z]
        
        # input$exac_mortality_prob = 0.067
        # input$exac_mortality_prob_LCI = 0.057
        # input$exac_mortality_prob_UCI = 0.077
        
        # input$treatment_effect = 0.65
        # input$treatment_effect_LCI = 0.55
        # input$treatment_effect_UCI = 0.77
        # input$exac_mortality_prob = 0.156 *0.2
        # input$exac_mortality_prob_LCI = 0.109*0.2
        # input$exac_mortality_prob_UCI = 0.203*0.2
        # 
        # input$trans_probs = c(0.037,0.031,0,0.037,0.031,0)
        # input$trans_probs_alpha = c(0.037,0.031,0,0.037,0.031,0)*c(0.55,0.15,0,0.55,0.15,0)*305000 # number of transitions
        # input$trans_probs_beta =  c(0.55,0.15,0,0.55,0.15,0)*305000 - input$trans_probs_alpha # N - number of transitions. N from Hoogendoorn where Modereates are 55% of 305000, Severes are 15%
        # 
        
        # input$initial_dists <-c(0,0,1,0,0,0,0)
        # input$qaly_loss_exac  <- model_input$values$qaly_loss_exac * k
        # input$qaly_loss_exac_severe  <- model_input$values$qaly_loss_exac_severe * k
        # input$qaly_loss_GI  <- k
        # input$qaly_loss_hearing <- k
        
        
        
        # input$gast_evnt_RR <  k
        # input$hearing_loss_RR <- k
        
        # input$qaly_baseline <- input$qaly_baseline* 0
        
        input$treatment_effect  <- k
        # Parameter of interest
        # input$CVD_risk <- 1
        # input$severe_exac_rates_notx  <- model_input$values$severe_exac_rates_notx * k
        # input$exac_rates_notx  <- model_input$values$exac_rates_notx * k
        # input$total_years <- k
        
        # --------PLACEBO------------------------------------------
        is_treatment_group = FALSE
        
        transition_matrix = list()
        for (i in c(1:input$total_years)){
          transition_matrix[[i]] = fill_trans_mat(input,i,is_treatment_group)
        }
        
        results = markov_cal(input, transition_matrix,is_treatment_group)
        
        placebo_qaly = sum(results[,15])
        
        # --------TREATMENT-----------------------------------------
        is_treatment_group = TRUE
        
        transition_matrix = list()
        for (i in c(1:input$total_years)){
          
          input$exac_rates_tx <- c((input$exac_rates_notx[1:input$gold_stages]*current_effect(i,input$treatment_effect,input$resist_param)),input$exac_rates_notx[(input$gold_stages+1):input$states_num])
          input$severe_exac_rates_tx <- c((input$severe_exac_rates_notx[1:input$gold_stages]*current_effect(i,input$treatment_effect,input$resist_param)),input$severe_exac_rates_notx[(input$gold_stages+1):input$states_num])
          
          transition_matrix[[i]] = fill_trans_mat(input,i,is_treatment_group)
        }
        
        results2 = markov_cal(input, transition_matrix,is_treatment_group)
        
        treatment_qaly = sum(results2[,15])
        
        # --------DELTA----------------------------------------------
        delta = c(delta, (treatment_qaly - placebo_qaly)) 
        
      }
      # print(quantile(delta,c(0.025, 0.975) ))
      
      positive_net_qaly = c(positive_net_qaly, length(which(delta>0))/length(delta) )
      net_qaly = c(net_qaly, mean(delta) )
    }
  
  print(net_qaly)
  threshold <- var_range[min(which(net_qaly < 0))]#**
  print(positive_net_qaly)
  #**
  plot_qaly( 0.69, threshold, var_range, positive_net_qaly, net_qaly)
  
}

plot_qaly <- function(model,threshold, var_range, positive_net_qaly, net_qaly){
  
  rate <- var_range
  qaly <- net_qaly
  prop <- positive_net_qaly
  
  
  df <- data.frame(rate, qaly, prop)
  
  p_rate <- ggplot(data = df, aes(x=rate, y=qaly)) + geom_point() + geom_line() +
    #geom_smooth() + 
    ylab("Net QALY") + xlab("\n  The relative risk of exacerbations in the treatment group ")+
    # ylab("Net QALY") + xlab("\n  Average disutility of mild/moderate exacerbations across the GOLD stages")+
    
    geom_vline(xintercept = threshold, color = "tomato") +
    annotate("text", x = threshold, y = -0.1, label = "threshold", color = "tomato") +
    geom_vline(xintercept = model, color = "#17A589") +
    annotate("text", x = model, y = 0.2, label = "Expected value", color = "#17A589") +
    # theme_tufte() +
    # theme_ipsum()+
    theme_minimal()+
    theme(axis.line = element_line(color = 'black'))
    
    # theme(axis.line.y = element_line(color = 'black'), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  # p_prop <- ggplot(data = df, aes(x=rate, y=prop*100)) + geom_point() + geom_line() +
  #   #geom_smooth() + 
  #   ylab("Net QALY gain %")  + xlab("\n  Disutility due to hearing loss ") + 
  #   geom_vline(xintercept = threshold, color = "tomato") +
  #   geom_vline(xintercept = model, color = "#17A589") +
  #   # scale_x_continuous(breaks = c(0, 0.25, 0.50, 0.69, 0.75, 1)) +
  #   # theme_tufte() + 
  #   # theme_ipsum()+
  #   theme_minimal()+
  #   theme(axis.line = element_line(color = 'black'))
  # p_rate/p_prop
  # p_rate
  ggsave(plot = p_rate, filename = "One_way_sensitivity_exacRR.pdf", height=15, units = "cm", dpi=300)
  print(p_rate)
  
}

