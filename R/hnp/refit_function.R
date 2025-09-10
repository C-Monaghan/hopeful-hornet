refit_function <- function(resp_vector, original_model, original_data) {
  tryCatch({
    if (length(resp_vector) != nrow(original_data)) {
      stop("Simulated response vector length mismatch.")
    }
    
    new_data <- original_data
    new_data$y <- factor(resp_vector, levels = levels(original_data$y))
    
    updated_model <- update(original_model, data = new_data)
    if (!inherits(updated_model, "multinom")) stop("Refit failed")
    updated_model
  }, error = function(e) {
    message("Refit failed: ", conditionMessage(e))
    NULL
  })
}
