tidy_metrics <- function(metrics) {
  
  # Renaming cells 
  metrics <- metrics |>
    mutate(
      parent_block = stringr::str_replace(parent_block, "_", " "),
      parent_block = stringr::str_to_title(parent_block),
      sub_model = case_when(
        sub_model == "null_models" ~ "Null Model",
        sub_model == "red_1_models" ~ "Reduced Model 1",
        sub_model == "red_2_models" ~ "Reduced Model 2",
        sub_model == "true_models" ~ "True Model",
        sub_model == "of_models" ~ "Overfit Model",
        TRUE ~ sub_model
      ),
      size_label = stringr::str_replace(size_label, "n_", "n = "),
      wave = stringr::str_replace(wave, "w_", "Wave "),
      wave = stringr::str_replace(wave, "_", " to "),
      metric = case_when(
        metric == "Frobenius" ~ "Frobenius Distance",
        metric == "Manhattan" ~ "Manhattan Distance",
        metric == "Max" ~ "Max Difference",
        metric == "MeanAbs" ~ "Mean Absolute Difference",
        metric == "RMSE" ~ "Root Mean Square Error",
        metric == "Correlation" ~ "Correlation Distance",
        metric == "KL" ~ "Kullback-Leibler Divergence"
      )
    ) |>
    # Factorising
    mutate(
      parent_block = factor(
        parent_block,
        levels = c("Base Models", "Additive Models", "Multiplicative Models")),
      sub_model = factor(
        sub_model,
        levels = c("Null Model", "Reduced Model 1", "Reduced Model 2", 
                   "True Model", "Overfit Model")),
      size_label = factor(
        size_label,
        levels = c("n = 100", "n = 250", "n = 1000")),
      wave = factor(
        wave,
        levels = c("Wave 1 to 2", "Wave 2 to 3")),
      metric = factor(
        metric,
        levels = c("Frobenius Distance", "Manhattan Distance",
                   "Max Difference", "Mean Absolute Difference",
                   "Root Mean Square Error", "Correlation Distance",
                   "Kullback-Leibler Divergence"))
      )
  
  return(metrics)
}

