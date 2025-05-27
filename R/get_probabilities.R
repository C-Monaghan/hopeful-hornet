get_probabilities <- function(x1, x2, x3, y_prev = NULL, betas, scenario) {
  # Validation -----------------------------------------------------------------
  if (!scenario %in% 1:3) {
    stop("Scenario must be 1, 2, or 3")
  }
  
  if (scenario == 1 && !is.null(y_prev)) {
    warning("y_prev provided but not used in scenario 1")
  }
  
  if (scenario != 1 && is.null(y_prev)) {
    stop("y_prev must be provided for scenarios 2 and 3")
  }
  
  # Scenario 1: Basic Model (No Markov Dependence) ----------------------------
  if(scenario == 1){
    # Linear predictor for state 2 (vs reference state 1)
    lp1 <- betas$alpha[1] + 
      betas$beta_1[1]*x1 + 
      betas$beta_2[1]*x2 + 
      betas$beta_3[1]*x3
    
    # Linear predictor for state 3 (vs reference state 1)
    lp2 <- betas$alpha[2] + 
      betas$beta_1[2]*x1 + 
      betas$beta_2[2]*x2 + 
      betas$beta_3[2]*x3
    
  } 
  
  # Scenario 2: Additive Markov Model -----------------------------------------
  else if (scenario == 2) {
    # Convert previous state to numeric indicators
    prev_state_2 <- as.numeric(y_prev == 2)
    prev_state_3 <- as.numeric(y_prev == 3)
    
    # Linear predictor for state 2 (vs reference state 1)
    lp1 <- betas$alpha[1] + 
      betas$beta_1[1]*x1 + 
      betas$beta_2[1]*x2 + 
      betas$beta_3[1]*x3 +
      betas$beta_4[1]*prev_state_2 + 
      betas$beta_5[1]*prev_state_3
    
    # Linear predictor for state 3 (vs reference state 1)
    lp2 <- betas$alpha[2] + 
      betas$beta_1[2]*x1 + 
      betas$beta_2[2]*x2 + 
      betas$beta_3[2]*x3 +
      betas$beta_4[2]*prev_state_2 + 
      betas$beta_5[2]*prev_state_3
    
  } 
  # Scenario 3: Multiplicative Markov Model -----------------------------------
  else if (scenario == 3) {
    
    # Convert previous state to numeric indicators
    prev_state_2 <- as.numeric(y_prev == 2)
    prev_state_3 <- as.numeric(y_prev == 3)
    
    # Linear predictor for state 2 with interaction terms
    lp1 <- betas$alpha[1] + 
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
    lp2 <- betas$alpha[2] + 
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
  denom <- 1 + exp(lp1) + exp(lp2)
  
  # Return probabilities for states 1 (reference), 2, and 3
  return(c(
    1/denom, 
    exp(lp1)/denom, 
    exp(lp2)/denom))
}