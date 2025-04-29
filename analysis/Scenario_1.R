# Simple simulation keeping all default parameters
# ------------------------------------------------------------------------------

rm(list = ls()) # To annoy Rafael

library(dplyr)
library(ggplot2)

# Loading simulation functions -------------------------------------------------
functions <- list.files(path = here::here("R/"), full.names = TRUE)

sapply(functions, source)

# Simulating data with default parameters --------------------------------------
simulation <- simulate_data(n_subjects = 2500, seed = 123)

data <- simulation$data |>
  add_previous_status()

# Simulate good and bad markov models ------------------------------------------
models <- fit_markov_model(
  data = data, 
  sample_sizes = c(100, 250, 500, 1000), 
  n_reps = 1000,
  parallel = TRUE,
  seed = 125)

# Visualizations ---------------------------------------------------------------
# Observed transition matrices
plot_transitions(models$obs_trans, sample_size = "n_1000", rep = 1)
plot_multiple_transitions(models$obs_trans, sample_size = "n_1000", reps = 1:6)

# Estimated transition matrices
estimate_matrices <- estimate_transition_matrices(models, models$test_data)

# Splitting based on good and bad
good_predictions <- estimate_matrices$estimated_transitions_good
bad_predictions  <- estimate_matrices$estimated_transitions_bad

# Plotting
plot_transitions(good_predictions, sample_size = "n_1000",rep = 1, obs = FALSE)
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