# Simulation Scenario 1 
# Assuming no previous response effect
# ------------------------------------------------------------------------------

rm(list = ls()) # To annoy Rafael

library(dplyr)
library(ggplot2)

# Loading simulation functions -------------------------------------------------
functions <- list.files(path = here::here("R/"), full.names = TRUE)

sapply(functions, source)

# Simulating data with default parameters --------------------------------------
simulation <- simulate_data(n_subjects = 5000, scenario = 1, seed = 123)

data <- simulation$data |>
  add_previous_status()

# Simulate good and bad markov models ------------------------------------------
models <- fit_markov_model(
  data = data, 
  sample_sizes = c(100, 250, 1000), 
  n_reps = 200,
  method = "Base",
  parallel = TRUE,
  seed = 125)

# Visualizations ---------------------------------------------------------------
# Observed transition matrices
plot_transitions(models$obs_trans, sample_size = "n_1000", rep = 1)
plot_multiple_transitions(models$obs_trans, sample_size = "n_1000", reps = 1:6)

# Estimated transition matrices
estimate_matrices <- estimate_transition_matrices(models, models$test_data)

# Splitting into each respective model
null_predictions  <- estimate_matrices$null_models
red_1_predictions <- estimate_matrices$red_1_models
red_2_predictions <- estimate_matrices$red_2_models
true_predictions  <- estimate_matrices$true_models
over_predictions  <- estimate_matrices$of_models

# Plotting
plot_transitions(null_predictions, sample_size = "n_1000",rep = 1, obs = FALSE)




plot_transitions(bad_predictions, sample_size = "n_1000", rep = 1, obs = FALSE)

plot_multiple_transitions(good_predictions, sample_size = "n_100", obs = FALSE)
plot_multiple_transitions(bad_predictions, sample_size = "n_100", obs = FALSE)

# Compare matrices
compare_transition_matrix(
  models = models, 
  results = good_predictions, 
  sample_size = "n_1000",
  rep = 10, 
  type = "bad")

# Calculating distance based metrics
distances <- calculate_matrix_distances(
  results = estimate_matrices, 
  sample_size = c("n_100", "n_250", "n_500", "n_1000"),
  rep = 1:100)

# Plotting distances -----------------------------------------------------------
distances |>
  group_by(sample_size, model_type, metric) |>
  summarise(mean_distance = mean(value)) |> 
  mutate(
    mean_distance = round(mean_distance, digits = 2),
    sample_size = stringr::str_replace(sample_size, "_", " = "),
    sample_size = factor(sample_size, levels = c("n = 100", "n = 250", "n = 500", "n = 1000"))) |>
  ggplot(aes(x = metric, y = mean_distance, fill = metric)) +
  geom_col(colour = "black") +
  geom_text(aes(label = mean_distance, vjust = -0.5)) +
  ggokabeito::scale_fill_okabe_ito() +
  scale_y_continuous(expand = expansion(mult = c(0.075, 0.075))) +
  labs(
    title = "Distance based metrics",
    subtitle = "Across sample sizes",
    x = "Distance based metric",
    y = "Mean value") +
  facet_grid(model_type ~ sample_size) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )