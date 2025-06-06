get_probabilities <- function(x1, x2, x3, y_prev = NULL, betas, scenario) {
  
  # Validation -----------------------------------------------------------------
  if (!scenario %in% 1:4) {
    stop("Scenario must be 1, 2, 3, or 4")
  }
  
  if(scenario %in% c(2, 3) && is.null(y_prev)) {
    stop("`y_prev` must be provided for scenarios 2 and 3.")
  }
  
  # Helper call ----------------------------------------------------------------
  if (!is.null(y_prev)) {
    prev_state_2 <- as.numeric(y_prev == 2)
    prev_state_3 <- as.numeric(y_prev == 3)
  } else {
    prev_state_2 <- 0
    prev_state_3 <- 0
  }
  
  # Scenario 1: Basic Model (No Markov Dependence) ----------------------------
  if(scenario == 1){
    # Linear predictor for state 2 (vs reference state 1)
    eta2 <- betas$alpha[1] + 
      betas$beta_1[1]*x1 + 
      betas$beta_2[1]*x2 + 
      betas$beta_3[1]*x3
    
    # Linear predictor for state 3 (vs reference state 1)
    eta3 <- betas$alpha[2] + 
      betas$beta_1[2]*x1 + 
      betas$beta_2[2]*x2 + 
      betas$beta_3[2]*x3
  } 
  
  # Scenario 2: Additive Markov Model -----------------------------------------
  else if (scenario == 2) {
    
    # Linear predictor for state 2 (vs reference state 1)
    eta2 <- betas$alpha[1] + 
      betas$beta_1[1]*x1 + 
      betas$beta_2[1]*x2 + 
      betas$beta_3[1]*x3 +
      betas$beta_4[1]*prev_state_2 + 
      betas$beta_5[1]*prev_state_3
    
    # Linear predictor for state 3 (vs reference state 1)
    eta3 <- betas$alpha[2] + 
      betas$beta_1[2]*x1 + 
      betas$beta_2[2]*x2 + 
      betas$beta_3[2]*x3 +
      betas$beta_4[2]*prev_state_2 + 
      betas$beta_5[2]*prev_state_3
    
  } 
  # Scenario 3: Multiplicative Markov Model -----------------------------------
  else if (scenario == 3) {
  
    # Linear predictor for state 2 with interaction terms
    eta2 <- betas$alpha[1] + 
      betas$beta_1[1]*x1 +
      betas$beta_2[1]*x2 +
      betas$beta_3[1]*x3 + 
      betas$beta_4[1]*prev_state_2 +
      betas$beta_5[1]*prev_state_3 +
      betas$beta_6[1]*x1*prev_state_2 +
      betas$beta_7[1]*x1*prev_state_3 +
      betas$beta_8[1]*x2*prev_state_2 +
      betas$beta_9[1]*x2*prev_state_3 +
      betas$beta_10[1]*x3*prev_state_2 +
      betas$beta_11[1]*x3*prev_state_3
    
    # Linear predictor for state 3 with interaction terms
    eta3 <- betas$alpha[2] + 
      betas$beta_1[2]*x1 + 
      betas$beta_2[2]*x2 +
      betas$beta_3[2]*x3 + 
      betas$beta_4[2]*prev_state_2 + 
      betas$beta_5[2]*prev_state_3 +
      betas$beta_6[2]*x1*prev_state_2 +
      betas$beta_7[2]*x1*prev_state_3 +
      betas$beta_8[2]*x2*prev_state_2 +
      betas$beta_9[2]*x2*prev_state_3 +
      betas$beta_10[2]*x3*prev_state_2 +
      betas$beta_11[2]*x3*prev_state_3
  } 
  # Scenario 4 - User supplied beta values
  # For this, we simply compute the same “full” linear predictor as in scenario 3,
  # but allow our betas to be zero if the fitted model did not include them.
  else if(scenario == 4) {
    
    # Linear predictor for state 2 with interaction terms
    eta2 <- betas$alpha[1] + 
      betas$beta_1[1]*x1 +
      betas$beta_2[1]*x2 +
      betas$beta_3[1]*x3 + 
      betas$beta_4[1]*prev_state_2 +
      betas$beta_5[1]*prev_state_3 +
      betas$beta_6[1]*x1*prev_state_2 +
      betas$beta_7[1]*x1*prev_state_3 +
      betas$beta_8[1]*x2*prev_state_2 +
      betas$beta_9[1]*x2*prev_state_3 +
      betas$beta_10[1]*x3*prev_state_2 +
      betas$beta_11[1]*x3*prev_state_3
    
    # Linear predictor for state 3 with interaction terms
    eta3 <- betas$alpha[2] + 
      betas$beta_1[2]*x1 + 
      betas$beta_2[2]*x2 +
      betas$beta_3[2]*x3 + 
      betas$beta_4[2]*prev_state_2 + 
      betas$beta_5[2]*prev_state_3 +
      betas$beta_6[2]*x1*prev_state_2 +
      betas$beta_7[2]*x1*prev_state_3 +
      betas$beta_8[2]*x2*prev_state_2 +
      betas$beta_9[2]*x2*prev_state_3 +
      betas$beta_10[2]*x3*prev_state_2 +
      betas$beta_11[2]*x3*prev_state_3
  }
  
  # Probability Calculation ---------------------------------------------------
  # Denominator for softmax function
  denom <- 1 + exp(eta2) + exp(eta3)
  
  # Convert etas into pi values
  pi1 <- 1 / denom
  pi2 <- exp(eta2) / denom
  pi3 <- exp(eta3) / denom
  
  # Return probabilities for states 1 (reference), 2, and 3
  return(c(pi1, pi2, pi3))
}