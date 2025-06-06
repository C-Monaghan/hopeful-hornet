extract_betas <- function(fit) {
  
  coefs  <- stats::coef(fit) 
  
  # Start with an empty list of zeros:
  zero2 <- c(0, 0)
  
  out <- list(
    alpha  = zero2, 
    beta_1 = zero2, 
    beta_2 = zero2, 
    beta_3 = zero2, 
    beta_4 = zero2,   #   y_prev = 2 main effect
    beta_5 = zero2,   #   y_prev = 3 main effect
    beta_6 = zero2,   #   x1 * y_prev = 2
    beta_7 = zero2,   #   x1 * y_prev = 3
    beta_8 = zero2,   #   x2 * y_prev = 2
    beta_9 = zero2,   #   x2 * y_prev = 3
    beta_10 = zero2,  #   x3 * y_prev = 2
    beta_11 = zero2   #   x3 * y_prev = 3
  )
  
  # 1) Intercepts → “alpha”
  if ("(Intercept)" %in% colnames(coefs)) {
    out$alpha <- coefs[, "(Intercept)"]
  }
  
  # 2) x1: in your code, x1 is coded as 0/1 but appears in the multinom as “x11” if you let R convert factor automatically.
  if ("x11" %in% colnames(coefs)) {
    out$beta_1 <- coefs[, "x11"]
  }
  
  # 3) x2, x3, x4, x5
  for (i in 2:5) {
    nm <- paste0("x", i)
    if (nm %in% colnames(coefs)) {
      out[[paste0("beta_", i)]] <- coefs[, nm]
    }
  }
  
  # 4) y_prev main‐effects.  Assuming you created ‘y_prev’ as a factor, the dummy names are “y_prev2” and “y_prev3”:
  if ("y_prev2" %in% colnames(coefs)) {
    out$beta_4 <- coefs[, "y_prev2"]
  }
  if ("y_prev3" %in% colnames(coefs)) {
    out$beta_5 <- coefs[, "y_prev3"]
  }
  
  # 5) Interactions: x1:y_prev, x2:y_prev, x3:y_prev
  #   (R will typically name them “x11:y_prev2”, “x11:y_prev3”, etc. if x1 is a factor.)
  if ("x11:y_prev2" %in% colnames(coefs)) {
    out$beta_6 <- coefs[, "x11:y_prev2"]
  }
  if ("x11:y_prev3" %in% colnames(coefs)) {
    out$beta_7 <- coefs[, "x11:y_prev3"]
  }
  if ("x2:y_prev2" %in% colnames(coefs)) {
    out$beta_8 <- coefs[, "x2:y_prev2"]
  }
  if ("x2:y_prev3" %in% colnames(coefs)) {
    out$beta_9 <- coefs[, "x2:y_prev3"]
  }
  if ("x3:y_prev2" %in% colnames(coefs)) {
    out$beta_10 <- coefs[, "x3:y_prev2"]
  }
  if ("x3:y_prev3" %in% colnames(coefs)) {
    out$beta_11 <- coefs[, "x3:y_prev3"]
  }
  
  return(out)
}
