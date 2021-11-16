
# ============= Prameters derived from studies ================ #

init_input <- function() {
  input <- list()
  input_help <- list()
  input_ref <- list()
  
  # Input example:
  # input_help$agent$p_female <- "Proportion of females in the population"
  # input$agent$p_female <- 0.5
  # input_ref$agent$p_female <- "Model assumption"
  
  input$states_num = 7 # S1_tx,S2_tx,S3_tx, S1_notx, S2_notx,S3_notx,D
  input$gold_stages = 3
  input$total_years= 20
  
  input$avg_age = 65
  
  input$discount_rate = 0.03
  
  input$initial_dists <-c(0.26,0.4,0.34,0,0,0,0)

  input$trans_probs = c(0.034,0.03,0,0.034,0.03,0)
  input$trans_probs_alpha = c(0.034,0.03,0,0.034,0.03,0)*c(0.55,0.15,0,0.55,0.15,0)*305000 # number of transitions
  input$trans_probs_beta =  c(0.55,0.15,0,0.55,0.15,0)*305000 - input$trans_probs_alpha # N - number of transitions. N from Hoogendoorn et al. where Moderates are 55% of 305000, Severs are 15%

  input$trans_probs_smoking = c(0.037,0.031,0,0.037,0.031,0)
  input$trans_probs_alpha_smoking = c(0.037,0.031,0,0.037,0.031,0)*c(0.55,0.15,0,0.55,0.15,0)*305000 # number of transitions
  input$trans_probs_beta_smoking =  c(0.55,0.15,0,0.55,0.15,0)*305000 - input$trans_probs_alpha # N - number of transitions. N from Hoogendoorn where Modereates are 55% of 305000, Severes are 15%
  
  
  # in 20 years from 65 - 85
  input$background_mortality_probs <- c(0.012710,0.013621,0.014620,0.015770,0.017100,0.018428, 0.020317,0.022102,
                                  0.024194,0.026342,0.029042,0.032001, 0.035443,0.039257, 0.043393,0.048163,
                                  0.053216, 0.059240,0.066564,0.074045,
                                  
                                  0.08422825,0.09438508, 0.10555131,0.11777638, 0.13109840, 0.14554106,
                                  0.1611136, 0.17780478, 0.19558155,0.21438729, 0.23413857, 0.25472827,
                                  0.27602455, 0.29787291, 0.32010340)
                                   # SUM = 0.665567
  
  input$exac_mortality_prob = 0.156 
  input$exac_mortality_prob_LCI = 0.109
  input$exac_mortality_prob_UCI = 0.203
  
  #OR of exac reduction in tx vs no-tx
  # input$treatment_effect = 0.69
  # input$treatment_effect_LCI = 0.54
  # input$treatment_effect_UCI = 0.89
  input$treatment_effect = 0.73
  input$treatment_effect_LCI = 0.63
  input$treatment_effect_UCI = 0.84
  
  input$resist_param = 0.22  # 0.44 ^ exp(-2k) = 0.59 => log 0.59 (0.44) = exp(-2k) =>-1/2*ln(log 0.59 (0.44) = k = 0.22
  
  input$CVD_risk = 2.88
  input$CVD_risk_LCI = 1.79
  input$CVD_risk_UCI = 4.63
  
  input$CVD_risk_length = 5
  
  input$hearing_loss_RR = 1.168 
  input$hearing_loss_RR_LCI = 1.030
  input$hearing_loss_RR_UCI = 1.325
  
  # reference_hearing_loss = -1.169+0.02312*65   #ß0+ß1.Age
  input$hearing_rate_change = 0.023 #ß1  => p(S1->S'1)[year=i] = p(S1->S'1)[year=i-1] + hearing_rate_change
  input$hearing_rate_change_LCI = 0.011
  input$hearing_rate_change_UCI = 0.035
  
  input$gast_evnt_RR = 1.187 
  input$gast_evnt_RR_LCI =0.761
  input$gast_evnt_RR_UCI = 1.849
  
  input$rate_gast_events = 0.33   
  input$rate_gast_events_alpha = 0.33*71812*(0.39+0.61*1.363)   
  input$rate_gast_events_beta = 0.67*71812*(0.39+0.61*1.363) 
  
  input$severe_exac_rates_notx = c(0.16,0.22,0.28,0.16,0.22,0.28,0) #source: Hoogendoorn
  input$severe_exac_rates_notx_LCI = c(0.07,0.20,0.14)
  input$severe_exac_rates_notx_UCI = c(0.33,0.23,0.63)
  
  input$exac_rates_notx = c(1.17,1.61,2.10,1.17,1.61,2.10,0)
  input$exac_rates_notx_LCI = c(0.93,1.51,1.51)
  input$exac_rates_notx_UCI = c(1.5,1.74,2.94)
  
  input$severe_exac_rates_tx =  input$severe_exac_rates_notx
  input$exac_rates_tx  = input$exac_rates_notx
  
  input$RR_exac_0hist = 0.16
  input$RR_exac_0hist_SD = 0.007 # estimate,SD = CI_length/3.9
  input$RR_exac_2hist = 1.3
  input$RR_exac_2hist_SD = 0.05
  input$RR_exac_3hist = 1.25
  input$RR_exac_3hist_SD = 0.04
  
  input$RR_severe_exac_0hist = 0.16
  input$RR_severe_exac_0hist_SD = 0.02
  input$RR_severe_exac_2hist = 1.26
  input$RR_severe_exac_2hist_SD =  0.1
  input$RR_severe_exac_3hist = 1.36
  input$RR_severe_exac_3hist_SD = 0.1
  
  
  input$qaly_baseline = c(0.787,0.750,0.647,0.787,0.750,0.647,0)
  input$qaly_baseline_LCI  = c(0.771,0.731,0.598,0.771,0.731,0.598,0)
  input$qaly_baseline_UCI  = c(0.802 ,0.768,0.695,0.802,0.768,0.695,0)
  
  # Used Spencer et al. and Sadatsafavi et al. : 
  #  = (Baseline(x1)/4 - QALY during exacerbation 3-month period (x2)*/4) => 
  # mean = x1/4 - x2/4 , variance1 = (n.SE1/4)^2 => variance (X1/4-X2/4) = n.(SE1/4)^2 + n.(SE2/4)^2 
  #*QALY during 3-month exacerbation period is equal to average QALY of a year with four exacerbations)
  input$qaly_loss_Spencer_baseline_SE = c(0.0075,0.0125,0.0125,0.0075,0.0125,0.0125,0)
  
  input$qaly_loss_exac_mod = c(0.0155,0.0488,0.0488,0.0155,0.0488 ,0.0488,0) 
  input$qaly_loss_exac_mod_var = (input$qaly_loss_Spencer_baseline_SE)^2 + c(0.0075,0.0125,0.0125,0.0075,0.0125,0.0125,0)^2

  # SE : 0.01060660 0.01767767 0.01767767 0.01060660 0.01767767 0.01767767 0.00000000

  input$qaly_loss_exac_severe  = c(0.0683,0.0655,0.0655,0.0683,0.0655,0.0655,0)
  input$qaly_loss_exac_severe_var = (input$qaly_loss_Spencer_baseline_SE)^2 + c(0.0175,0.0125,0.0125,0.0175,0.0125,0.0125,0)^2

  # SE: 0.01903943 0.01767767 0.01767767 0.01903943 0.01767767 0.01767767 0.00000000

  #derived from NICE using SD of Barton 2004 (0.02) 
  input$qaly_loss_hearing = 0.187
  input$qaly_loss_hearing_var = 0.0004
  
  #derived from Sullivan et al.
  input$qaly_loss_hearing_EQ5D = 0.006
  input$qaly_loss_hearing_SE = 0.0001

  #derived from Barton et al. 
  input$hearing_improvement = 0.06
  input$hearing_improvement_LCI = 0.044
  input$hearing_improvement_UCI = 0.073
  
  #derived from Sullivan et al. (The largest SE among the three conditions)
  input$qaly_loss_GI = 0.0261
  input$qaly_loss_GI_SE = 0.0002
  
  
  # ================================================================== #

  
  model_input <- list ("values" = input, "help" = input_help, "ref" = input_ref)
  return (model_input)
}

model_input <- init_input()




