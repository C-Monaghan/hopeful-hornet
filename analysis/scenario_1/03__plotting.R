rm(list = ls())

# Packages ---------------------------------------------------------------------
pacman::p_load(
  dplyr,
  tidyr,
  stringr,
  data.table,
  ggplot2
)

# Functions --------------------------------------------------------------------
source(here::here("R/tidy_metrics.R"))

# Data -------------------------------------------------------------------------
path_scenario <- "./analysis/scenario_1/"

# ~ 56 million rows (ooof...)
distances <- data.table::as.data.table(readRDS(
  file = here::here(path_scenario, "results/matrix_distances.RDS")))

# Data manipulation
tidy_metrics(distances)

# Dropping matrices
distances[, ":=" (obs_mat = NULL, sim_mat = NULL)]

# Grouping and summarizing metrics
dist_sum <- distances[, .(value = mean(value)), by = .(parent_block, sub_model, size_label, rep, wave, metric)] |>
  tibble::as_tibble()


    

dist_sum |>
  split(~ parent_block) |>
  purrr::map(function(data) {
    data |>
      filter(wave == "Wave 1 to 2") |>
      ggplot(aes(x = log(value), y = sub_model, fill = sub_model)) +
      geom_boxplot() +
      ggokabeito::scale_fill_okabe_ito() +
      facet_grid(size_label ~ metric, scales = "free_x") +
      theme_bw() +
      theme(legend.position = "none")
  })
  

dist_sum
  