create_estimated_transition_matrix <- function(model, data) {
  
  # Validations ----------------------------------------------------------------
  if (!inherits(model, "multinom")) {
    stop("Model must be a nnet::multinom object")
  }
  if (!"y_prev" %in% colnames(data)) {
    stop("Data must contain a previous state column")
  }
  
  # Get all possible states from model
  states <- levels(data$y)
  
  # Get predicted probabilities
  pred_probs <- stats::predict(model, newdata = data, type = "probs")
  
  # If binary outcome, reformat to matrix
  if (length(states) == 2 && !is.matrix(pred_probs)) {
    pred_probs <- cbind(1 - pred_probs, pred_probs)
    colnames(pred_probs) <- states
  }
  
  # Initialize empty transition matrix
  trans_mat <- matrix(0, nrow = length(states),
                      ncol = length(states),
                      dimnames = list(From = states, To = states))
  
  # Calculate mean transition probabilities for each previous state
  for (prev_state in states) {
    mask <- data$y_prev == prev_state
    
    if (sum(mask) > 0) {
      trans_mat[prev_state, ] <- colMeans(pred_probs[mask, , drop = FALSE])
    }
  }
  
  trans_mat <- round(trans_mat, digits = 2)
  
  return(trans_mat)
}