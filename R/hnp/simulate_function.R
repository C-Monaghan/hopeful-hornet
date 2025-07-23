simulate_function <- function(n, obj) {
  tryCatch({
    # Get predicted probabilities
    pred_probs <- predict(obj, type = "probs")
    
    # Handle binary case
    if (is.null(dim(pred_probs))) {
      pred_probs <- cbind(1 - pred_probs, pred_probs)
    }
    
    # Ensure we have proper category names
    categories <- c(1:3)
    if (ncol(pred_probs) != length(categories)) {
      stop("Probability matrix doesn't match category count")
    }
    
    # Simulate new responses
    y_sim <- apply(pred_probs, 1, function(p) {
      sample(categories, size = 1, prob = p)
    })
    
    factor(y_sim, levels = categories)
  }, error = function(e) {
    message("Simulation failed: ", conditionMessage(e))
    rep(NA, n)  # Return vector of correct length
  })
}