
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
  
  input$discount_rate = 0.05
  
  input$initial_dists <-c(0.26,0.4,0.34,0,0,0,0)

  input$trans_probs = c(0.034,0.03,0,0.034,0.03,0)
  input$trans_probs_alpha = c(0.034,0.03,0,0.034,0.03,0)*c(0.55,0.15,0,0.55,0.15,0)*305000 # number of transitions
  input$trans_probs_beta =  c(0.55,0.15,0,0.55,0.15,0)*305000 - input$trans_probs_alpha # N - number of transitions. N from Hoogendoorn where Modereates are 55% of 305000, Severes are 15%


  
  # in 20 years from 65 - 85
  input$background_mortality_probs <- c(0.012710,0.013621,0.014620,0.015770,0.017100,0.018428, 0.020317,0.022102,
                                  0.024194,0.026342,0.029042,0.032001, 0.035443,0.039257, 0.043393,0.048163,
                                  0.053216, 0.059240,0.066564,0.074045) # SUM = 0.665567
  
  input$exac_mortality_prob = 0.156 
  input$exac_mortality_prob_LCI = 0.109
  input$exac_mortality_prob_UCI = 0.203
  
  #OR of exac reduction in tx vs no-tx
  input$treatment_effect = 0.69
  input$treatment_effect_LCI = 0.54
  input$treatment_effect_UCI = 0.89
  
  
  # resist_param = 0.34
  input$resist_param = 0.22
  
  input$CVD_risk = 2.88
  input$CVD_risk_LCI = 1.79
  input$CVD_risk_UCI = 4.63
  
  input$hearing_loss_RR = 1.168 # in 20 years
  input$hearing_loss_RR_LCI = 1.030
  input$hearing_loss_RR_UCI = 1.325
  
  # reference_hearing_loss = -1.169+0.02312*65   #ß0+ß1.Age
  input$hearing_rate_change = 0.023 #ß1  => p(S1->S'1)[year=i] = p(S1->S'1)[year=i-1] + hearing_rate_change
  input$hearing_rate_change_LCI = 0.011
  input$hearing_rate_change_UCI = 0.0325
  
  input$gast_evnt_RR = 1.187 
  input$gast_evnt_RR_LCI =0.761
  input$gast_evnt_RR_UCI = 1.849
  
  input$rate_gast_events = 0.33   # does not depend on age
  
  input$severe_exac_rates_notx = c(0.16,0.22,0.28,0.16,0.22,0.28,0) #source: Hoogendoorn
  input$severe_exac_rates_notx_LCI = c(0.07,0.20,0.14)
  input$severe_exac_rates_notx_UCI = c(0.33,0.23,0.63)
  
  input$exac_rates_notx = c(1.17,1.61,2.10,1.17,1.61,2.10,0)
  input$exac_rates_notx_LCI = c(0.93,1.51,1.51)
  input$exac_rates_notx_UCI = c(1.5,1.74,2.94)
  
  input$severe_exac_rates_tx =  input$severe_exac_rates_notx
  input$exac_rates_tx  = input$exac_rates_notx
  
  input$RR_exac_0hist <- c(0.16, 0.01) # estimate,SE
  input$RR_exac_1hist <- 1
  input$RR_exac_2hist <- c(1.3, 0.05)
  input$RR_exac_3hist <- c(1.25, 0.05) #obsExac_moderate.y1 >=2 | obsExac_severe.y1 >=1
  
  input$qaly_baseline = c(0.787,0.750,0.647,0.787,0.750,0.647,0)
  input$qaly_baseline_LCI  = c(0.771,0.731,0.598,0.771,0.731,0.598,0)
  input$qaly_baseline_UCI  = c(0.802 ,0.768,0.695,0.802,0.768,0.695,0)
  
  input$qaly_loss_exac = c(0.0155,0.0488,0.0488,0.0155,0.0488 ,0.0488,0)
  input$qaly_loss_exac_SE = c(0.0075,0.0125,0.0125,0.0075,0.0125,0.0125,0)
  
  input$qaly_loss_exac_severe  = c(0.0683,0.0655,0.0655,0.0683,0.0655,0.0655,0)
  input$qaly_loss_exac_severe_SE = c(0.0075,0.0125,0.0125,0.0075,0.0125,0.0125,0)
  
  input$qaly_loss_hearing = 0.187
  input$qaly_loss_hearing_SE = 0.02
  
  input$hearing_improvement = 0.06
  input$hearing_improvement_LCI = 0.044
  input$hearing_improvement_UCI = 0.073
  
  
  input$qaly_loss_GI = 0.0261
  input$qaly_loss_GI_SE = 0.0001
  
  
  # ================================================================== #

  
  model_input <- list ("values" = input, "help" = input_help, "ref" = input_ref)
  return (model_input)
}

model_input <- init_input()




