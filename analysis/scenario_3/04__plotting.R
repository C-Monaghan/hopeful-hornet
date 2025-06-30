# Scenario 3 -------------------------------------------------------------------
# 
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
      strip.text           = element_text(face = "bold", size = 11),
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

# Data -------------------------------------------------------------------------
path_scenario <- "./analysis/scenario_3/"

true_model <- case_when(
  stringr::str_detect(string = path_scenario, pattern = "scenario_1") ~ "Base",
  stringr::str_detect(string = path_scenario, pattern = "scenario_2") ~ "Additive",
  stringr::str_detect(string = path_scenario, pattern = "scenario_3") ~ "Multiplicative"
)

message("Reading in data ... ")

# ~ 56 million rows (ooof...)
distances <- fst::read.fst(
  path = here::here(path_scenario, "results/matrix_distances_test.fst"), 
  as.data.table = TRUE) |> 
  tidy_metrics()

message("Summarising data ... ")

# Grouping and summarizing metrics
dist_sum <- distances[, .(value = mean(value)), by = .(parent_block, sub_model, size_label, rep, wave, metric)] |>
  tibble::as_tibble() |>
  filter(metric != "Kullback-Leibler Divergence")

# Highlighting the "true model"
dist_sum <- dist_sum |> 
  highlight_true(true_model = true_model, usage = "Parent")

message("Finding best model ...")

# Summarizing best model
best_models <- dist_sum |>
  # Collapse the wave column
  group_by(parent_block, sub_model, size_label, rep, metric) |>
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
      group_by(parent_block, sub_model, size_label, metric) |>
      # Count each win and summarise
      summarise(n_lowest = sum(lowest), .groups = "drop") |>
      mutate(prop = n_lowest / 200)
  }) |>
  highlight_true(true_model = true_model, usage = "Sub")

message("Plotting ... ")

# Plotting ---------------------------------------------------------------------
dis_box <- dist_sum |>
  ggplot(aes(x = value, y = sub_model, fill = fill)) +
  geom_boxplot(
    aes(colour = (fill == "True"), size = (fill == "True")),
    position = ggstance::position_dodgev(height = 0.95, preserve = "single"),
    outlier.size = 1, alpha = 0.7) +
  scale_fill_manual(
    values = c(
      "Base Models" = "#E69F00", "True" = "#8b1a1a",
      "Additive Models" = "#56B4E9", "Multiplicative Models" = "#009E73"),
    breaks = c("Base Models", "Additive Models", "Multiplicative Models")) +
  scale_colour_manual(
    values = c(`TRUE` = "#8b1a1a", `FALSE` = "grey70"),
    guide = "none") +
  scale_size_manual(
    values = c(`TRUE` = 1.2, `FALSE` = 0.5), 
    guide = "none") +
  labs(
    title    =  "Distance Metrics by Sub‑Model and Parent Block",
    subtitle = paste0("True Model from ", true_model, " Models (highlighted in red)"),
    x        =  "Distance",
    y        =  NULL,
    fill     =  NULL
  ) +
  facet_grid(size_label ~ metric, scales = "free_x")

dis_bar <- best_models |>
  ggplot(aes(x = parent_block, y = prop, fill = fill)) +
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
    title = "Proportion of times when each model was identified as the best",
    x = "Model Type",
    y = "Proportion of Repetitions as best", 
    fill = "Sub Model") +
  facet_grid(size_label ~ metric, space = "free")

message("Exporting data ... ")

# Saving -----------------------------------------------------------------------
# As png
cowplot::save_plot(
  filename = here::here(path_scenario, "results/figures/distances_box.png"),
  plot = dis_box,
  base_height = 10, 
  base_width = 25)

cowplot::save_plot(
  filename = here::here(path_scenario, "results/figures/distances_bar.png"),
  plot = dis_bar,
  base_height = 10, 
  base_width = 25)

# As pdf
cowplot::save_plot(
  filename = here::here(path_scenario, "results/figures/distances_box.pdf"),
  plot = dis_box,
  base_height = 10, 
  base_width = 25)

cowplot::save_plot(
  filename = here::here(path_scenario, "results/figures/distances_bar.pdf"),
  plot = dis_bar,
  base_height = 10, 
  base_width = 25)

