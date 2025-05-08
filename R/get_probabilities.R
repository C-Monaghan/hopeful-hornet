get_probabilities <- function(x1, x2, x3, y_prev = NULL, betas, scenario) {
  if(scenario == 1){
    # Scenario 1: No previous state inclusion
    lp1 <- betas$alpha[1] + 
      betas$beta_1[1]*x1 + 
      betas$beta_2[1]*x2 + 
      betas$beta_3[1]*x3
    
    lp2 <- betas$alpha[2] + 
      betas$beta_1[2]*x1 + 
      betas$beta_2[2]*x2 + 
      betas$beta_3[2]*x3
    
  } else if (scenario == 2) {
    # Scenario 2: Additive model
    lp1 <- betas$alpha[1] + 
      betas$beta_1[1]*x1 + 
      betas$beta_2[1]*x2 + 
      betas$beta_3[1]*x3 +
      betas$beta_4[1]*(y_prev == 2) + 
      betas$beta_5[1]*(y_prev == 3)
    
    lp2 <- betas$alpha[2] + 
      betas$beta_1[2]*x1 + 
      betas$beta_2[2]*x2 + 
      betas$beta_3[2]*x3 +
      betas$beta_4[2]*(y_prev == 2) + 
      betas$beta_5[2]*(y_prev == 3)
    
  } else if (scenario == 3) {
    
    y_prev <- as.numeric(y_prev)
    
    # Scenario 3: Multiplicative model
    lp1 <- betas$alpha[1] + 
      betas$beta_1[1]*x1 + 
      betas$beta_2[1]*x2 +
      betas$beta_3[1]*x3 + 
      betas$beta_4[1]*(y_prev == 2) +
      betas$beta_5[1]*(y_prev == 3) +
      betas$beta_6[1]*x1*(y_prev == 2) +
      betas$beta_7[1]*x1*(y_prev == 3) +
      betas$beta_8[1]*x2*(y_prev == 2) +
      betas$beta_9[1]*x2*(y_prev == 3) +
      betas$beta_10[1]*x3*(y_prev == 2) +
      betas$beta_11[1]*x3*(y_prev == 3)
    
    lp2 <- betas$alpha[2] + 
      betas$beta_1[2]*x1 + 
      betas$beta_2[2]*x2 +
      betas$beta_3[2]*x3 + 
      betas$beta_4[2]*(y_prev == 2) + 
      betas$beta_5[2]*(y_prev == 3) +
      betas$beta_6[2]*x1*(y_prev == 2) +
      betas$beta_7[2]*x1*(y_prev == 3) +
      betas$beta_8[2]*x2*(y_prev == 2) +
      betas$beta_9[2]*x2*(y_prev == 3) +
      betas$beta_10[2]*x3*(y_prev == 2) +
      betas$beta_11[2]*x3*(y_prev == 3)
  }
  
  denom <- 1 + exp(lp1) + exp(lp2)
  
  # Return pi_1, pi_2, and pi_3
  return(c(1/denom, exp(lp1)/denom, exp(lp2)/denom))
}