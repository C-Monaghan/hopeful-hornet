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
path_scenario <- "./analysis/scenario_2/"

# ~ 56 million rows (ooof...)
distances <- fst::read.fst(
  path = here::here(path_scenario, "results/matrix_distances.fst"), 
  as.data.table = TRUE) |> 
  tidy_metrics()

# Grouping and summarizing metrics
dist_sum <- distances[, .(value = mean(value)), by = .(parent_block, sub_model, size_label, rep, wave, metric)] |>
  tibble::as_tibble()

# Highlighting the "true model"
dist_sum <- dist_sum |>
  mutate(
    fill = case_when(
      parent_block == "Additive Models" & sub_model == "True Model" ~ "True",
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
    subtitle = "True Model from Additive Models (highlighted in red)",
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

