tidy_metrics <- function(metrics) {
  
  # Ensures our object is stored as a data.table object ------------------------
  if(!is.data.table(metrics)) {
    metrics <- as.data.table(metrics)
  }
  
  # Renaming cells -------------------------------------------------------------
  metrics[, parent_block := str_replace(parent_block, "_", " ")]
  metrics[, parent_block := str_to_title(parent_block)]
  
  metrics[, sub_model := fcase(
    sub_model == "null_models", "Null Model",
    sub_model == "red_1_models", "Reduced Model 1",
    sub_model == "red_2_models", "Reduced Model 2",
    sub_model == "true_models", "True Model",
    sub_model == "of_models", "Overfit Model",
    default = sub_model
  )]
  
  metrics[, size_label := str_replace(size_label, "n_", "n = ")]
  metrics[, wave := str_replace(wave, "w_", "Wave ")]
  metrics[, wave := str_replace(wave, "_", " to ")]
  
  metrics[, metric := fcase(
    metric == "Frobenius", "Frobenius Distance",
    metric == "Manhattan", "Manhattan Distance",
    metric == "Max", "Max Difference",
    metric == "MeanAbs", "Mean Absolute Difference",
    metric == "RMSE", "Root Mean Square Error",
    metric == "Correlation", "Correlation Distance",
    metric == "KL", "Kullback-Leibler Divergence",
    default = metric
  )]
  
  # Factorise columns with explicit levels
  metrics[, parent_block := factor(
    parent_block,
    levels = c("Base Models", "Additive Models", "Multiplicative Models")
  )]
  
  metrics[, sub_model := factor(
    sub_model,
    levels = c("Null Model", "Reduced Model 1", "Reduced Model 2", 
               "True Model", "Overfit Model")
  )]
  
  metrics[, size_label := factor(
    size_label,
    levels = c("n = 100", "n = 250", "n = 1000")
  )]
  
  metrics[, wave := factor(
    wave,
    levels = c("Wave 1 to 2", "Wave 2 to 3")
  )]
  
  metrics[, metric := factor(
    metric,
    levels = c("Frobenius Distance", "Manhattan Distance",
               "Max Difference", "Mean Absolute Difference",
               "Root Mean Square Error", "Correlation Distance",
               "Kullback-Leibler Divergence")
  )]
  
  return(metrics)
}

