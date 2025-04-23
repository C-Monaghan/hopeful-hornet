rm(list = ls()) # To annoy Rafael

# Loading simulation functions -------------------------------------------------
functions <- list.files(path = here::here("R/"), full.names = TRUE)

sapply(functions, source)

# Simulating data with default parameters --------------------------------------
data <- simulate_data(n_subjects = 2500, seed = 123)$data |>
  add_previous_status()

# Simulate good and bad markov models ------------------------------------------
models <- fit_markov_model(
  data = data, 
  sample_sizes = c(100, 250, 500, 1000), 
  n_reps = 1000, 
  seed = 125)

# Visualizations ---------------------------------------------------------------
# Observed transition matrices
plot_transitions(models$obs_trans, sample_size = "n_1000")
plot_multiple_transitions(models$obs_trans, sample_size = "n_1000")

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
  sample_size = c("n_100", "n_250"),
  rep = 1:10)

distances |>
  ggplot(aes(x = sample_size, y = value, fill = metric)) +
  geom_col(colour = "black") +
  # geom_text(aes(label = mean_distance), vjust = -0.5) +
  ggokabeito::scale_fill_okabe_ito() +
  facet_grid(metric ~ model_type) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


distances |>
  group_by(sample_size, model_type, metric) |>
  dplyr::summarise(mean_distance = mean(value)) |> 
  dplyr::mutate(mean_distance = round(mean_distance, digits = 2)) |>
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

distances |>
  dplyr::filter(repitition == 1) |>
  ggplot(aes(x = metric, y = value, fill = metric)) +
  geom_col(colour = "black") +
  facet_grid(model_type ~ sample_size) +
  ggeasy::easy_remove_legend()

distances
  
  

# ALTERNATIVE ------------------------------------------------------------------
# More complex set up with custom parameters for number of: 
# - States
# - Waves
# - State specific means (x4 = lambda values; x5 = mu values)
# - State specific covariate effects (effects on transition probabilities)

data <- simulate_data(
  n_subjects = 2500, 
  y = 1:5, 
  n_waves = 10, 
  transition_matrix = NULL,
  initial_probs = rep(1 / 5, 5),
  state_means = list(
    x4 = c(2, 4, 3, 5, 1),
    x5 = c(25, 30, 40, 30, 50)),
  covariate_effects = list(
    x2 = c(0, 0, 0.4, 0.5, 0.2),
    x3 = c(0, 0.2, 0.3, 0, 0.4),
    x4 = c(0, 0.4, 0.6, 0.2, 0.1),
    x5 = c(0, 0.2, 0.2, 0.4, 0.7)),
  seed = 123)$data |>
  add_previous_status()

models <- fit_markov_model(
  data = data, 
  sample_sizes = c(100, 250, 500, 1000), 
  n_reps = 10, 
  seed = 125)



