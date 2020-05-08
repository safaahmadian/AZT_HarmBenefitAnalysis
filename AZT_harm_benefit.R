
# =============Prameters derived from studies # =================

num_states = 4 # S1, S2, S3, D
total_years= 20

#death_rates for each state => LOGNORMAL
death_params = c(0.277631737,0.14966725,
                 0.565313809,0.199080427,
                 1.088561953,0.250212629)
death_params_mat = matrix(data = death_params, nrow = num_states-1, ncol = 2, byrow = TRUE)
#exacerbation_rates for each state => GAMMA
exac_params = c(900,0.000666667,
                760,0.001,
                875,0.002)
exac_params_mat = matrix(data = exac_params, nrow = num_states-1, ncol = 2, byrow = TRUE)
#costs for each state => GAMMA
cost_params = c(16,86.4375,
                16,118.5,
                16,609.5)
cost_params_mat = matrix(data = cost_params, nrow = num_states-1, ncol = 2, byrow = TRUE)
#utilities for each state => NORMAL
qaly_params = c(0.8,0.05,
                0.72,0.03,
                0.72,0.03)
qaly_params_mat = matrix(data = qaly_params, nrow = num_states-1, ncol = 2, byrow = TRUE)
#trans_rates to next state, for each state => GAMMA (Add to this if more transitions are available. Also, change define_states)
transitions_params = c(1929,0.00001,
                       1440,0.00001)
trans_params_mat = matrix(data = transitions_params, nrow = num_states-2, ncol = 2, byrow = TRUE)

#S1,S2,S3
initial_dists <-c(0.3,0.4,0.3)

# in 20 years
background_mortality_rates <- c(0.01593,0.01752,0.0193,0.02124,0.02329,0.02555, 0.0281,0.03104,
                                0.03429,0.03779,0.04165,0.04599, 0.05091,0.05631, 0.0621,0.06846,
                                0.07555, 0.08353,0.09214,0.10129)
discount_rate = 0.015
treatment_cost = 250
exac_cost_params =  c(16,114.609375)
#====================================

# set_params <- function(){
# 
# }

model_state <- function(id, name, exac_rate, cost, qaly, trans_list) {
  value <- list(id = id, name = name, exac_rate = exac_rate, cost = cost, qaly = qaly, trans_list = trans_list)
  attr(value, 'class') <- 'model_state' #  call like: print(attr(states_list[[1]],'class'))
  
  return(value)
}

print.model_state<- function(obj){
  cat('ID = ', obj$id, '\n')
  cat('Name = ', obj$name, '\n')
  cat('Exac_rate = ', obj$exac_rate, '\n')
  cat('Cost = ', obj$cost, '\n')
  cat('QALY = ', obj$qaly, '\n')
  cat('Transition list = ', obj$trans_list, '\n')
}

fill_state_trans <- function(i){
  trans_list = integer(num_states)
  if (i != num_states-1){
    trans_list[i+1] = qgamma(runif(1),shape = trans_params_mat[i,1], scale = trans_params_mat[i,2])
  }
  death_HR = qnorm(runif(1),mean = death_params_mat[i,1], sd = death_params_mat[i,2])
  trans_list[num_states] = exp(death_HR)
  return(trans_list)
}


define_states <- function(){
  states = list()
  
  for (i in c(1:(num_states-1))){
    name = paste("State", i, sep = "_")
    trans_list = fill_state_trans(i)
    exac_rate = qgamma(runif(1),shape = exac_params_mat[i,1], scale = exac_params_mat[i,2])
    cost = qgamma(runif(1),shape = cost_params_mat[i,1], scale = cost_params_mat[i,2])
    qaly = qnorm(runif(1),mean = qaly_params_mat[i,1], sd= qaly_params_mat[i,2])
    curr_state<- model_state(i, name, exac_rate, cost, qaly, trans_list) 
    states[[length(states)+1]] <- curr_state
  }
  curr_state = model_state(num_states, "Death", 0, 0, 0, c(rep(0,(num_states-1)),1))  
  states[[length(states)+1]] <- curr_state
  return(states)
}
#update transition matrix for each year
update_trans_mat <- function(states_list, year){
  trans_mat = matrix(0, nrow = num_states-1, ncol = num_states, byrow = TRUE)
  for (i in c(1:(num_states-1))){
    trans_mat[i,] = states_list[[i]]$trans_list
    trans_mat[i,num_states] = trans_mat[i,num_states]*background_mortality_rates[year]
    trans_mat[i,i] = 1-(Reduce("+",trans_mat[i,c(i:num_states)]))
  }
  return(trans_mat)
}

markov_cal <- function(trans_mat, states_list){
  exac_cost = qgamma(runif(1),shape = exac_cost_params[1], scale = exac_cost_params[2])
  
  distributions_mat = matrix(data = integer((total_years+1)*(num_states-1)), nrow = (total_years+1), ncol =(num_states-1), byrow = TRUE)
  results_mat = matrix(data = integer((total_years+1)*3) , nrow = (total_years+1), ncol = 3, byrow = TRUE) #for exac, cost and QALY
  distributions_mat[1,] = initial_dists
  for (i in c(2:(total_years+1))){
    for (j in c(1:(num_states-1))){
      distributions_mat[i,j] = sum( distributions_mat[(i-1),]*trans_mat[[i-1]][,j])
    }
    results_mat[i,1] = sum(distributions_mat[i,]*unlist(lapply(head(states_list,-1),function(x) x$exac_rate)))
    results_mat[i,2] = sum(distributions_mat[i,]*unlist(lapply(head(states_list,-1),function(x) x$cost)))
    results_mat[i,3] = sum(distributions_mat[i,]*unlist(lapply(head(states_list,-1),function(x) x$qaly)))
  }
  print("distributions matrix:")
  print(distributions_mat)
  print("results (Exacerbations - Costs - QALYs):")
  print(results_mat)
  
}

run_model <- function(){
  states = define_states()
  transition_matrix = list()
  for (i in c(1:total_years)){
    transition_matrix[[i]] = update_trans_mat(states,i)
  }
  markov_cal(transition_matrix,states)
}



