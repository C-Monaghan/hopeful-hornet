extract_distance <- function(obj) {
  # Get predicted probabilities
  pred_probs <- predict(obj, type = "probs")
  
  # Get observed categories as matrix
  obs <- model.response(model.frame(obj))
  obs_matrix <- model.matrix(~ obs - 1)
  
  # Calculate Pearson residuals: (
  # - Observed - Predicted) / sqrt(Predicted * (1 - Predicted))
  residuals <- (obs_matrix - pred_probs) / sqrt(pred_probs * (1 - pred_probs))
  
  # Manhattan distance = sum of absolute residuals per observation
  manhattan_dist <- rowSums(abs(residuals))
  
  return(manhattan_dist)
}