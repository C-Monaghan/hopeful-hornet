process_dataset <- function(data) {
  tryCatch({
    # Verify input data structure
    if (!is.data.frame(data) || !all(c("y", "x1", "x2", "x3") %in% names(data))) {
      stop("Invalid dataset structure.")
    }
    
    message("→ Fitting model ...")
    
    # Fit model
    model_fit <- nnet::multinom(y ~ x1 + x2 + x3, data = data, trace = FALSE)
    
    # Store original fit and data globally for refit_function
    # assign("fit_1", fit, envir = .GlobalEnv)
    # assign("data", data, envir = .GlobalEnv)
    
    message("→ Running HNP ...")
    
    # Run HNP
    hnp_obj <- hnp(
      model_fit,
      newclass = TRUE,
      diagfun = function(obj) extract_distance(obj, data, obs_mat),
      fitfun = function(resp) refit_function(resp, original_model = model_fit, original_data = data),
      simfun = simulate_function,
      how.many.out = TRUE,
      plot = FALSE,
      print.on = FALSE
    )
    
    # Return percentage outside envelope
    pct_out <- hnp_obj$out / hnp_obj$total * 100
    
    message("→ HNP complete, % outside: ", pct_out)
    
    pct_out
  }, error = function(e) {
    message("Processing failed: ", conditionMessage(e))
    NA_real_
  })
}