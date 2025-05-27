estimate_transition_matrices <- function(model_results, test_data = NULL) {
  
  # If you don't specify the test data use test data from the train / test split
  if(is.null(test_data)) {
    test_data <- model_results$test_data
  }
  
  message("Generating predictions... please wait")
  
  # Getting predictions --------------------------------------------------------
  ## Null model
  model_results$estimated_transitions_null <- pbapply::pblapply(
    model_results$null_models, function(fits) {
    pbapply::pblapply(fits, create_estimated_transition_matrix, data = test_data)
  })
  
  # Reduced 1 model
  model_results$estimated_transitions_red_1 <- pbapply::pblapply(
    model_results$red_1_models, function(fits) {
    pbapply::pblapply(fits, create_estimated_transition_matrix, data = test_data)
  })
  
  # Reduced 2 model
  model_results$estimated_transitions_red_2 <- pbapply::pblapply(
    model_results$red_2_models, function(fits) {
      pbapply::pblapply(fits, create_estimated_transition_matrix, data = test_data)
    })
  
  # True model
  model_results$estimated_transitions_true <- pbapply::pblapply(
    model_results$true_models, function(fits) {
      pbapply::pblapply(fits, create_estimated_transition_matrix, data = test_data)
    })
  
  # Over fit model
  model_results$estimated_transitions_overfit <- pbapply::pblapply(
    model_results$of_models, function(fits) {
      pbapply::pblapply(fits, create_estimated_transition_matrix, data = test_data)
    })
  
  
  return(model_results)
}
