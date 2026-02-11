# Packages ---------------------------------------------------------------------
pacman::p_load(
  dplyr,
  tidyr,
  stringr,
  data.table,
  ggplot2
)

# Set theme --------------------------------------------------------------------
theme_set(
  theme_minimal(base_size = 12) +
    theme(
      plot.title           = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle        = element_text(face = "bold", size = 12, hjust = 0.5),
      axis.text.y          = element_text(face = "bold"),
      axis.text.x          = element_text(angle = 45, hjust = 1),
      strip.background     = element_rect(fill = "#F0F0F0", colour = NA),
      strip.text           = element_text(face = "bold", size = 10),
      panel.grid.major.y   = element_blank(),
      panel.grid.minor     = element_blank(),
      legend.position      = "bottom",
      legend.background    = element_rect(fill = "transparent"),
      legend.key           = element_blank()
    )
)

# Functions --------------------------------------------------------------------
source(here::here("R/tidy_metrics.R"))
source(here::here("R/highlight_true.R"))

# Data files -------------------------------------------------------------------
message("Setting up data files ... ")

scenarios <- tibble::tibble(
  path = c(
    here::here("analysis/scenario_1/results/matrix_distances_test.fst"),
    here::here("analysis/scenario_2/results/matrix_distances_test.fst"),
    here::here("analysis/scenario_3/results/matrix_distances_test.fst")
  ),
  parent_block = c("Base Models", "Additive Models", "Multiplicative Models")
)

message("Reading in data files ...")

distances <- scenarios |>
  mutate(data = purrr::map2(path, parent_block, function(path, parent) {
    fst::read_fst(path = path, as.data.table = TRUE) |>
      tidy_metrics() |>
      filter(parent_block == parent)
  }))

distances_true <- distances |> pull(data) |> data.table::rbindlist(use.names = TRUE)

# distances_base <- fst::read.fst(
#   path = here::here("analysis/scenario_1/results/matrix_distances.fst"), 
#   as.data.table = TRUE) |> 
#   tidy_metrics() |>
#   filter(parent_block == "Base Models")
# 
# message("Reading in data (additive models) ... ")
# 
# distances_add <- fst::read.fst(
#   path = here::here("analysis/scenario_2/results/matrix_distances.fst"), 
#   as.data.table = TRUE) |> 
#   tidy_metrics() |>
#   filter(parent_block == "Additive Models")
# 
# message("Reading in data (multiplicative models) ... ")
# 
# distances_mult <- fst::read.fst(
#   path = here::here("analysis/scenario_3/results/matrix_distances.fst"), 
#   as.data.table = TRUE) |> 
#   tidy_metrics() |>
#   filter(parent_block == "Multiplicative Models")

# Joining distances together ---------------------------------------------------
message("Joining datasets together ... ")

# distances <- rbindlist(
#   list(distances_base, distances_add, distances_mult),
#   use.names = TRUE, fill = TRUE)

# Summarising ------------------------------------------------------------------
message("Summarising data ... ")

# Grouping and summarizing metrics
dist_sum <- distances_true[, .(value = mean(value)), by = .(parent_block, sub_block, size_label, rep, wave, metric)] |>
  tibble::as_tibble()
  # filter(metric != "Kullback-Leibler Divergence")

message("Finding best model ...")

# Summarizing best model
best_models <- dist_sum |>
  filter(!stringr::str_detect(metric, "Absolute")) |>
  # Collapse the wave column
  group_by(parent_block, sub_block, size_label, rep, metric) |>
  summarise(value = mean(value), .groups = "drop") |>
  # Compute by metric (probably unnecessary ... )
  split(~ metric) |>
  purrr::map_dfr(function(m) {
    m |>
      # Rank the models per repetition
      group_by(parent_block, size_label, rep) |>
      mutate(winning = rank(value)) |>
      ungroup() |>
      # Which model had the lowest metric
      mutate(lowest = ifelse(winning == 1, TRUE, FALSE)) |>
      group_by(parent_block, sub_block, size_label, metric) |>
      # Count each win and summarise
      summarise(n_lowest = sum(lowest), .groups = "drop") |>
      mutate(prop = n_lowest / 200)
  })

# Plotting ---------------------------------------------------------------------
message("Plotting model ... ")

# Plotting ---------------------------------------------------------------------
dis_bar <- best_models |>
  filter(!stringr::str_detect(metric, "Absolute")) |>
  ggplot(aes(x = parent_block, y = prop, fill = sub_block)) +
  geom_col(colour = "black") + 
  geom_text(
    aes(label = ifelse(prop >= 0.04, scales::percent(prop, accuracy = 1), NA)),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  scale_fill_manual(
    values = c(
      "Null Model" = "#E69F00", "Reduced Model 1" = "#56B4E9", 
      "Reduced Model 2" = "#009e73", "True Model" = "#F0E442",
      "Other" = "#F0E44233", "Overfit Model" = "#0072B2"),
    breaks = c("Null Model", "Reduced Model 1", 
               "Reduced Model 2", "True Model", "Overfit Model")) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    # title = "Proportion of times when each model was identified as the best",
    x = "Model Type",
    y = "Proportion of repetitions identified as best", 
    fill = "Sub Model") +
  facet_grid(size_label ~ metric, space = "free", labeller = labeller(
    metric = c("Aic" = "Akaike Information Criterion",
               "Bic" = "Bayesian Information Criterion")
  ))

message("Exporting data ... ")

# Saving -----------------------------------------------------------------------
cowplot::save_plot(
  filename = here::here("paper/plot_test.png"), plot = dis_bar, 
  base_height = 10, base_width = 25)

cowplot::save_plot(
  filename = here::here("paper/plot_test.pdf"), plot = dis_bar, 
  base_height = 10, base_width = 25)

# Saving data for later use
saveRDS(
  object = best_models, 
  file = here::here("paper/best_models.RDS"))

saveRDS(
  object = dist_sum, 
  file = here::here("paper/dis_sum.RDS"))
