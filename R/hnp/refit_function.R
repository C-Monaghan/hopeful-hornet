refit_function <- function(resp_vector) {
  tryCatch({
    # Get the original model frame
    original_data <- model.frame(fit_1)
    
    # Ensure we have a valid response vector
    if (length(resp_vector) != nrow(original_data)) {
      stop("Response vector length doesn't match original data")
    }
    
    # Create new data frame with original structure
    new_data <- original_data
    new_data$y <- factor(resp_vector, levels = levels(original_data$y))
    
    # Refit the model
    updated_fit <- update(fit_1, data = new_data)
    
    # Verify the refit worked
    if (!inherits(updated_fit, "multinom")) stop("Refit failed")
    updated_fit
  }, error = function(e) {
    message("Refit failed: ", conditionMessage(e))
    NULL
  })
}