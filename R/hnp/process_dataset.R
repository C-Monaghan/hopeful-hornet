process_dataset <- function(data) {
  tryCatch({
    # Verify input data structure
    if (!is.data.frame(data)) stop("Input must be a data.frame")
    if (!all(c("y", "x1", "x2", "x3") %in% names(data))) {
      stop("Missing required columns")
    }
    
    # Fit model
    fit <- nnet::multinom(y ~ x1 + x2 + x3, data = data, trace = FALSE)
    
    # Store original fit globally for refit_function
    assign("fit_1", fit, envir = .GlobalEnv)
    
    # Run HNP
    hnp_obj <- hnp(
      fit,
      newclass = TRUE,
      diagfun = extract_distance,
      fitfun = refit_function,
      simfun = simulate_function,
      how.many.out = TRUE,
      plot = FALSE,
      print.on = FALSE
    )
    
    # Return percentage outside envelope
    hnp_obj$out / hnp_obj$total * 100
  }, error = function(e) {
    message("Processing failed: ", conditionMessage(e))
    NA_real_
  })
}