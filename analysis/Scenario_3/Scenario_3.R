# Simulation Scenario 3 
# Assuming a multiplicative previous response effect
# ------------------------------------------------------------------------------

rm(list = ls()) # To annoy Rafael

library(dplyr)
library(ggplot2)

# Loading simulation functions -------------------------------------------------
functions <- list.files(path = here::here("R/"), full.names = TRUE)

sapply(functions, source)

# Simulating data with default parameters --------------------------------------
simulation <- simulate_data(n_subjects = 10000, scenario = 3, seed = 123)

data <- simulation$data |>
  add_previous_status()

# Simulate good and bad markov models ------------------------------------------
models <- fit_markov_model(
  data = data, 
  sample_sizes = c(100, 250, 1000), 
  n_reps = 200,
  method = "Multiplicative",
  parallel = TRUE,
  seed = 125)


# Estimated transition matrices
estimate_matrices <- estimate_transition_matrices(models, models$test_data)

# Splitting into each respective model
matrices <- list(
  "Observed"  = models$obs_trans,
  "Null"      = estimate_matrices$estimated_transitions_null,
  "Reduced 1" = estimate_matrices$estimated_transitions_red_1,
  "Reduced 2" = estimate_matrices$estimated_transitions_red_2,
  "True"      = estimate_matrices$estimated_transitions_true,
  "Overfit"   = estimate_matrices$estimated_transitions_overfit
)

# Plotting ---------------------------------------------------------------------
compare_transitions(
  transition_list = matrices, sample_size = "n_1000", rep = 1,
  obs = FALSE, model_names = NULL)

# Calculating distance based metrics
distances <- calculate_matrix_distances(
  results = estimate_matrices, 
  sample_size = c("n_100", "n_250", "n_1000"),
  rep = 1:200)

# Plotting distances -----------------------------------------------------------
## Using boxplots
distance_box <- distances |>
  mutate(
    sample_size = stringr::str_replace(sample_size, "_", " = "),
    sample_size = factor(sample_size, levels = c("n = 100", "n = 250", "n = 1000"))
  ) |>
  ggplot(aes(x = metric, y = value, fill = metric)) +
  geom_boxplot() +
  ggokabeito::scale_fill_okabe_ito() +
  labs(
    title = "Distance based metrics",
    subtitle = "Across sample sizes",
    x = "Distance based metric",
    y = "Distance Value") +
  facet_grid(model_type ~ sample_size) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

## Using bar charts
distance_bar <- distances |>
  group_by(sample_size, model_type, metric) |>
  summarise(mean_distance = mean(value)) |> 
  mutate(
    mean_distance = round(mean_distance, digits = 2),
    sample_size = stringr::str_replace(sample_size, "_", " = "),
    sample_size = factor(sample_size, levels = c("n = 100", "n = 250", "n = 1000"))) |>
  ggplot(aes(x = metric, y = mean_distance, fill = metric)) +
  geom_col(colour = "black") +
  geom_text(aes(label = mean_distance, vjust = -0.5)) +
  ggokabeito::scale_fill_okabe_ito() +
  scale_y_continuous(expand = expansion(mult = c(0.075, 0.175))) +
  labs(
    title = "Distance based metrics",
    subtitle = "Across sample sizes",
    x = "Distance based metric",
    y = "Average distance") +
  facet_grid(model_type ~ sample_size) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )


# Exporting --------------------------------------------------------------------
export_path <- "analysis/results/03__multiplicative"

cowplot::save_plot(
  filename = here::here(export_path, "distance_boxplot.png"),
  plot = distance_box,
  base_height = 8, base_width = 10
)

cowplot::save_plot(
  filename = here::here(export_path, "distance_barplot.png"),
  plot = distance_bar,
  base_height = 8, base_width = 10
)

