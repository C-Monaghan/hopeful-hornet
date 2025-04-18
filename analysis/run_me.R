rm(list = ls())

# Loading simulation functions -------------------------------------------------
functions <- list.files(path = here::here("R/"), full.names = TRUE)

sapply(functions, source)

# Simulating data with default parameters --------------------------------------
data <- simulate_data(n_subjects = 2500, seed = 123)$data |>
  add_previous_status()

# Simulate good and bad markov models
models <- fit_markov_model(
  data = data, 
  sample_sizes = c(100, 250, 500, 1000), 
  n_reps = 4, 
  seed = 125)

# Let's look at observed transition matrices
plot_transitions(transition_list = models$obs_trans, sample_size = "n_100")
plot_multiple_transitions(transition_list = models$obs_trans, sample_size = "n_1000")

# Let's look at estimated transition matrices
estimate_transition_matrices(models, test_data = models$test_data)


# More complex set up with custom parameters for number of: 
# - States
# - Waves
# - State specific means (x4 = lambda values; x5 = mu values)
# - State specific covariate effects (effects on transition probabilities)

data <- simulate_data(
  n_subjects = 100, 
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
