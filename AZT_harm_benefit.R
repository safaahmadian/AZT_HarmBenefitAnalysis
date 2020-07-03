
# ============= Prameters derived from studies ================ #

total_states = 4 # S1, S2, S3, D
total_states_tx_group = 7 # S1_tx, S1_notx, etc., D
disease_states = 3
total_years= 20

#exacerbation_rates for each state => GAMMA
exac_params = c(900,0.000666667,
                760,0.001,
                875,0.002)
exac_params_mat = matrix(data = exac_params, nrow = disease_states, ncol = 2, byrow = TRUE)
exac_rates_notx = integer(disease_states)

#costs for each state => GAMMA
cost_params = c(16,86.4375,
                16,118.5,
                16,609.5)
cost_params_mat = matrix(data = cost_params, nrow = disease_states, ncol = 2, byrow = TRUE)
costs = integer(disease_states)

#utilities for each state => NORMAL
qaly_params = c(0.8,0.05,
                0.72,0.03,
                0.72,0.03)
qaly_params_mat = matrix(data = qaly_params, nrow = disease_states, ncol = 2, byrow = TRUE)
qalys = integer(disease_states)

#trans_rates to next state, for each state => GAMMA (Add to this if more transitions are available. Also, change define_states)
transitions_params = c(1929,0.00001,
                       1440,0.00001)
trans_params_mat = matrix(data = transitions_params, nrow = disease_states-1, ncol = 2, byrow = TRUE)
trans_to_nexts = integer(disease_states-1)

#death_rates for each state => LOGNORMAL
death_params = c(0.277631737,0.14966725,
                 0.565313809,0.199080427,
                 1.088561953,0.250212629)
death_params_mat = matrix(data = death_params, nrow = disease_states, ncol = 2, byrow = TRUE)
death_rates = integer(disease_states)

#S1,S2,S3
initial_dists <-c(0.3,0.4,0.3)

# in 20 years
background_mortality_rates <- c(0.01593,0.01752,0.0193,0.02124,0.02329,0.02555, 0.0281,0.03104,
                                0.03429,0.03779,0.04165,0.04599, 0.05091,0.05631, 0.0621,0.06846,
                                0.07555, 0.08353,0.09214,0.10129)
discount_rate = 0.015
treatment_cost = 250
exac_cost_params =  c(16,114.609375)
exac_cost = 0

exac_qaly_params =  c(-0.06,0.02)
exac_qaly = 0

#OR of exac reduction in tx vs no-tx
tx_effect_params =   c(-0.287682072, 0.098658874 )# retrieved from Albert's study
treatment_effect = 0.73

hearing_loss_params =  c(0.155292884, 0.064346722) # retrieved from Li's study
hearing_loss_RR = 1.168 # in 20 years
background_hearing_rates <- rep(0.0001, 20)
hearing_loss_cost = 0
hearing_loss_qaly = 0
  
gast_evnt_params = c(0.171429116, 0.226130529) # retrieved from Li's study
gast_evnt_RR = 1.187 
# in 20 years
background_gast_rates <- rep(0.0001, 20)
gast_evnt_cost = 0
gast_evnt_qaly = 0

adv_evnt_cost = 0
adv_evnt_qaly = 0
# ================================================================== #

set_params <- function(){
  for (i in c(1:(disease_states))){
    exac_rates_notx[i] <<- rgamma(1, shape = exac_params_mat[i,1], scale = exac_params_mat[i,2]) # '<<-' resets the global value with the new value
    costs[i] <<- rgamma(1,shape = cost_params_mat[i,1], scale = cost_params_mat[i,2])
    qalys[i] <<- rnorm(1,mean = qaly_params_mat[i,1], sd = qaly_params_mat[i,2])
    if (i != disease_states){
      trans_to_nexts[i] <<- rgamma(1,shape = trans_params_mat[i,1], scale = trans_params_mat[i,2])
    }
    death_rates[i] <<- exp(rnorm(1,mean = death_params_mat[i,1], sd = death_params_mat[i,2]))
  }
  #duplicate for adverse-event states
  exac_rates_notx <<- c(exac_rates_notx,exac_rates_notx)
  costs <<- c(costs,costs)
  qalys <<- c(qalys,qalys)
  trans_to_nexts <<- c(trans_to_nexts,0,trans_to_nexts,0)
  print(trans_to_nexts)
  death_rates<<- c(death_rates,death_rates)

  exac_cost <<- rgamma(1,shape = exac_cost_params[1], scale = exac_cost_params[2])
  exac_qaly <<- rnorm(1,mean = exac_qaly_params[1], sd= exac_qaly_params[2])
  
  treatment_effect <<- exp(rnorm(1,mean = tx_effect_params[1], sd = tx_effect_params[2]))
  hearing_loss_RR <<- exp(rnorm(1,mean = hearing_loss_params[1], sd = hearing_loss_params[2]))
  gast_evnt_RR <<- exp(rnorm(1,mean = gast_evnt_params[1], sd = gast_evnt_params[2]))
  
  # gast_evnt_cost <<- qgamma()
  # gast_evnt_qaly <<- qgamma()
  # 
  # hearing_lossc-cost <<- qgamma()
  # hearing_loss_qaly <<- qgamma()
  
}

resistance_effect <- function(year, odds_ratio){ # will change according to the resistance effect but if it affect the rate, it would be much better
  return(odds_ratio)
}

change_by_odds <- function(rate, odds_ratio){ # change rate -> prob -> odds and then the reverse. 
  prob = 1-exp(-rate)
  odds = prob/(1-prob)
  new_odds = odds_ratio*odds
  new_prob = new_odds/(1+new_odds)
  return(new_prob)
}


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

fill_state_trans <- function(i){ 
  trans_list = integer(total_states_tx_group)
  trans_list[i+1] = trans_to_nexts[i]
  if (disease_states  < i && i < total_states_tx_group ){ #!!!!!!!! HEARINH _ RR and GAS_RR are not considered
    trans_list[i-disease_states] = 1 # The occurance of adverse effects is not associated with the disease-state so this probability should be 0, but wIe set it to 1 so later I can multiply it by the background-rates
  }

  trans_list[total_states_tx_group] = death_rates[i]
  
  return(trans_list)
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

#update transition matrix for each year
get_trans_mat <- function(states_list, year, is_treatment_group){
  
  trans_mat = matrix(0, nrow = total_states_tx_group-1, ncol = total_states_tx_group, byrow = TRUE) # we don't need transitions from death_state to others as it's always = 0
  for (i in c(1:(total_states_tx_group-1))){ # OR "for s in states" for more adjustability  
    trans_mat[i,] = states_list[[i]]$trans_list
    trans_mat[i,total_states_tx_group] = trans_mat[i,total_states_tx_group]*background_mortality_rates[year]
    if(!states_list[[i]]$has_adv_evnt){ 
      trans_mat[i,i-disease_states] = trans_mat[i,i-disease_states]*(background_hearing_rates[year]+background_gast_rates[year])
    }
    trans_mat[i,i] = 1-(Reduce("+",trans_mat[i,c(i:total_states_tx_group)]))
  }
  return(trans_mat)
  
  # Tansition Matrix:
  #     S'1	        S'2	        S'3	        S1	        S2	        S3	       D
  # S'1	P(S'1->S'1)	P(S'1->S'2)	P(S'1->S'3)	P(S'1->S1)	P(S'1->S2)	P(S'1->S3)	P(S'1->D)
  # S'2	P(S'1->S'1)	P(S'1->S'2)	P(S'1->S'3)	P(S'1->S1)	P(S'1->S2)	P(S'1->S3)	P(S'1->D)
  # S'3	P(S'1->S'1)	P(S'1->S'2)	P(S'1->S'3)	P(S'1->S1)	P(S'1->S2)	P(S'1->S3)	P(S'1->D)
  # S3	P(S'1->S'1)	P(S'1->S'2)	P(S'1->S'3)	P(S'1->S1)	P(S'1->S2)	P(S'1->S3)	P(S'1->D)
  # S3	P(S'1->S'1)	P(S'1->S'2)	P(S'1->S'3)	P(S'1->S1)	P(S'1->S2)	P(S'1->S3)	P(S'1->D)
  # S3	P(S'1->S'1)	P(S'1->S'2)	P(S'1->S'3)	P(S'1->S1)	P(S'1->S2)	P(S'1->S3)	P(S'1->D)
}

markov_cal <- function(trans_mat, states_list, is_treatment_group){


  distributions_mat = matrix(0, nrow = (total_years+1), ncol =(total_states_tx_group), byrow = TRUE) 
  results_mat = matrix(0 , nrow = (total_years+1), ncol = 3, byrow = TRUE) #for exac, cost, QALY
  distributions_mat[1,] = c(rep(0,disease_states),initial_dists,0)
  for (i in c(2:(total_years+1))){
    for (j in c(1:(total_states_tx_group))){
      distributions_mat[i,j] = sum( distributions_mat[(i-1),]*trans_mat[[i-1]][,j])
    }
    results_mat[i,1] = sum(distributions_mat[i,]*unlist(lapply(states_list,function(x) x$exac_rate)))# Here we are assuming that those who experienced adv_events up to this year, have an exac_rate similar to those who don't get tx (so we assume they didn't recieve tx in the last year)
    results_mat[i,2] = sum(distributions_mat[i,]*unlist(lapply(states_list,function(x) x$cost)))+results_mat[i,1]*exac_cost+ sum( adv_evnt_cost*distributions_mat[i,1:disease_states])
    results_mat[i,3] = sum(distributions_mat[i,]*unlist(lapply(states_list,function(x) x$qaly)))+results_mat[i,1]*exac_qaly + sum(adv_evnt_qaly*distributions_mat[i,1:disease_states])
    # results_mat[i,2] = sum(distributions_mat[i,]*unlist(lapply(states_list,function(x) x$cost)))+results_mat[i,1]*exac_cost + sum( adv_evnt_cost*distributions_mat[i,1:disease_states])
    # results_mat[i,3] = sum(distributions_mat[i,]*unlist(lapply(states_list,function(x) x$qaly)))+results_mat[i,1]*exac_qaly + sum(adv_evnt_qaly*distributions_mat[i,1:disease_states])
  }

  print("distributions matrix:")
  print(distributions_mat)
  print("results (Exacerbations - Costs - QALYs):")
  print(results_mat)
}

run_model<- function(is_treatment_group){ # is_treatment_group = TRUE => running the model for the treatment group
  
  set_params()
  states = define_states(is_treatment_group)
  transition_matrix = list()
  for (i in c(1:total_years)){
    transition_matrix[[i]] = get_trans_mat(states,i,is_treatment_group)
    # print("sanity check:")
    # for(j in c(1:(total_states-1))){ #length(transition_matrix[[i]][1,])
    #   print(sum(transition_matrix[[i]][j,]))
    # }
  }
  # print("transition matrix:")
  # print(transition_matrix)
  markov_cal(transition_matrix,states,is_treatment_group)

}



