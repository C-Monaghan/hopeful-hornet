simulate_function <- function(n, obj) {
  tryCatch({
    
    pred_probs <- predict(obj, type = "probs")
    categories <- seq_len(ncol(pred_probs))
    
    y_sim <- apply(pred_probs, 1, function(p) {
      sample(categories, size = 1, prob = p)
    })
    
    factor(y_sim, levels = categories)
  }, error = function(e) {
    message("Simulation failed: ", conditionMessage(e))
    rep(NA, n)  # fallback vector
  })
}
