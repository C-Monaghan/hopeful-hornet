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
distances <- fst::read.fst(
  here::here(path_scenario, "results/matrix_distances.fst"), 
  as.data.table = TRUE) |>
  tidy_metrics()

# Grouping and summarizing metrics
dist_sum <- distances[, .(value = mean(value)), by = .(parent_block, sub_model, size_label, rep, wave, metric)] |>
  tibble::as_tibble()

# Highlighting the "true model"
dist_sum <- dist_sum |>
  mutate(
    fill = case_when(
      parent_block == "Base Models" & sub_model == "True Model" ~ "True",
      parent_block == "Base Models" ~ "Base Models",
      parent_block == "Additive Models" ~ "Additive Models",
      parent_block == "Multiplicative Models" ~  "Multiplicative Models"
    ),
    fill = factor(
      fill, 
      levels = c("Base Models", "True", "Additive Models", "Multiplicative Models"))
  )

# Plotting ---------------------------------------------------------------------
dis_plot <- dist_sum |>
  ggplot(aes(x = log(value), y = sub_model, fill = fill)) +
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
    title =  "Distance Metrics by Sub‑Model and Parent Block",
    subtitle = "True Model from Base Models (highlighted in red)",
    x     =  "Log(distance)",
    y     =  NULL,
    fill  =  NULL
  ) +
  facet_grid(size_label ~ metric, scales = "free_x") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y   = element_blank(),
    panel.grid.minor     = element_blank(),
    strip.background     = element_rect(fill = "#F0F0F0", colour = NA),
    strip.text           = element_text(face = "bold", size = 11),
    axis.text.y          = element_text(face = "bold"),
    axis.text.x          = element_text(angle = 45, hjust = 1),
    plot.title           = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle        = element_text(face = "bold", size = 12, hjust = 0.5),
    legend.position      = "bottom",
    legend.background    = element_rect(fill = "transparent"),
    legend.key           = element_blank()
  )

# Stacked bar chart
distances[, .(value = mean(value)), 
          by = .(parent_block, sub_model, size_label, metric)] |>
  tibble::as_tibble() |>
  mutate(
    fill = case_when(
      parent_block == "Base Models" & sub_model == "True Model" ~ "True",
      parent_block == "Base Models" ~ "Base Models",
      parent_block == "Additive Models" ~ "Additive Models",
      parent_block == "Multiplicative Models" ~  "Multiplicative Models"
    ),
    fill = factor(
      fill, 
      levels = c("Base Models", "True", "Additive Models", "Multiplicative Models"))
  ) |>
  mutate(
    parent_block = stringr::str_remove(parent_block, " Models"),
    parent_block = factor(
      parent_block, levels = c("Base", "Additive", "Multiplicative"))) |>
  ggplot(aes(y = sub_model, x = log(value), fill = parent_block)) +
  geom_col(aes(colour = (fill == "True"))) +
  scale_colour_manual(
    values = c(`TRUE` = "#8b1a1a", `FALSE` = "grey70"),
    guide = "none") +
  ggokabeito::scale_fill_okabe_ito() +
  labs(title = "Stacked average distances by sub‐model, sample size & block",
       subtitle = "True model comes from (True model; Base model)",
       x = "Log-transformed distance", y = "Sub Model", fill = NULL) +
  facet_grid(size_label ~ metric, scales = "free_x") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, face = "bold"),
    axis.title = element_text(size = 10, face = "bold"),
    axis.text.x   = element_text(angle = 30, hjust = 1),
    panel.spacing = unit(0.5, "lines"),
    strip.background = element_rect(fill = "grey95", colour = NA),
    legend.position = "bottom"
  )
  


# Saving -----------------------------------------------------------------------
# As png
cowplot::save_plot(
  filename = here::here(path_scenario, "results/distances.png"),
  plot = dis_plot,
  base_height = 10, 
  base_width = 25)

# As pdf
cowplot::save_plot(
  filename = here::here(path_scenario, "results/distances.pdf"),
  plot = dis_plot,
  base_height = 10, 
  base_width = 25)


  
