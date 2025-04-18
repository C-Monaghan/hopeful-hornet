estimate_transition_matrices <- function(model_results, test_data = NULL) {
  
  # If you don't specify the test data use test data from the train / test split
  if(is.null(test_data)) {
    test_data <- model_results$test_data
  }
  
  # Initialize storage for estimated matrices
  model_results$estimated_transitions_good <- vector("list", length(model_results$good_fits))
  
  model_results$estimated_transitions_bad  <- vector("list", length(model_results$bad_fits))
  
  names(model_results$estimated_transitions_good) <- names(model_results$good_fits)
  names(model_results$estimated_transitions_bad)  <- names(model_results$bad_fits)
  
  # Get all possible states
  states <- levels(test_data$y)
  
  # Loop through each sample size
  for(sample in names(model_results$good_fits)) {
    good_estimates <- list()
    bad_estimates <- list()
    
    for(rep in seq_along(model_results$good_fits[[sample]])) {
      
      # Get current models
      good_model <- model_results$good_fits[[sample]][[rep]]
      bad_model <- model_results$bad_fits[[sample]][[rep]]
      
      # Create estimated transition matrices
      # Using a helper function
      good_estimates[[rep]] <- create_estimated_transition_matrix(good_model, test_data)
      bad_estimates[[rep]] <- create_estimated_transition_matrix(bad_model, test_data)
    } # End of for loop over rep
    
    model_results$estimated_transitions_good[[sample]] <- good_estimates
    model_results$estimated_transitions_bad[[sample]] <- bad_estimates
  } # End of for loop over sample
  
  return(model_results)
}
