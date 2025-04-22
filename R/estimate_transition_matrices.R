#' Create Estimated Transition Matrix from Model Predictions
#'
#' Helper function that generates a transition matrix by predicting from a multinomial
#' model and calculating mean transition probabilities for each previous state.
#'
#' @param model A fitted nnet::multinom model object
#' @param data New data to use for predictions (must contain status_prev column)
#' @return A transition matrix with dimensions states x states
#'
#' @examples
#' \dontrun{
#' # After fitting a model with fit_markov_models()
#' test_data <- results$test_data
#' model <- results$good_fits$n_100[[1]]
#' trans_mat <- create_est_trans_matrix(model, test_data)
#' }
#'
#' @export

estimate_transition_matrices <- function(model_results, test_data = NULL) {
  
  # If you don't specify the test data use test data 
  # from the train / test split
  if(is.null(test_data)) {
    test_data <- model_results$test_data
  }
  
  # Initialize storage
  model_results$estimated_transitions_good <- lapply(model_results$good_fits, function(x) {
    lapply(x, create_estimated_transition_matrix, data = test_data)
  })
  
  model_results$estimated_transitions_bad <- lapply(model_results$bad_fits, function(x) {
    lapply(x, create_estimated_transition_matrix, data = test_data)
  })
  
  return(model_results)
}
