# Applying the simulation design to a case study dataset
# The Health and Retirement Study (HRS)

set.seed(86825936)

# Packages ---------------------------------------------------------------------
library(dplyr)
library(purrr)
library(stringr)
library(data.table)
library(nnet)
library(ggplot2)

# Functions --------------------------------------------------------------------
func_files <- list.files(
  path = here::here("R/"),
  pattern = "\\.R$",
  full.names = TRUE
)

purrr::walk(func_files, source)
Rcpp::sourceCpp(file = here::here("R/compare_matrices.cpp"))

# Theme ------------------------------------------------------------------------
theme_simulation <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      plot.subtitle = element_text(face = "bold", size = 12, hjust = 0.5),
      axis.text.y = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_rect(fill = "#F0F0F0", colour = NA),
      strip.text = element_text(face = "bold", size = 10),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.background = element_rect(fill = "transparent"),
      legend.key = element_blank()
    )
}

# Data -------------------------------------------------------------------------
data <- readRDS(here::here("analysis/case_study/data/HRS.RDS")) |>
  janitor::clean_names() |>
  rename(ID = id) |>
  mutate(
    x3 = ifelse(x3 > 186, NA, x3),
    x3 = case_when(
      is.na(x3) & w == 2 ~ 78.44,
      is.na(x3) & w == 3 ~ 79.16,
      TRUE ~ x3
    ),
    x5 = case_when(
      x5 <= 0 ~ NA,
      x5 >= 200 ~ NA,
      TRUE ~ x5
    )
  )

# Applying DTMC model ----------------------------------------------------------
models <- fit_markov_model(
  data = data,
  sample_sizes = c(100, 250, 1000, 5000),
  n_reps = 100,
  parallel = TRUE,
  seed = 125
)

# Extract β‑lists -----------------------------------------------------------
message("Extracting β values ... ")

model_fits <- models[c(
  "base_models",
  "additive_models",
  "multiplicative_models"
)]

model_coefs <- imap(model_fits, function(by_sub_blocks, parent) {
  imap(by_sub_blocks, function(by_sizes, sub_blocks) {
    imap(by_sizes, function(by_fit_list, size_labels) {
      map(by_fit_list, extract_betas)
    })
  })
})

# Extract PIDs into a single tibble -----------------------------------------
pids_df <- imap(models$idv_trans, function(by_reps, size_label) {
  imap(by_reps, function(by_pid_list, rep) {
    tibble(
      ID = as.numeric(stringr::str_remove(names(by_pid_list), "^p_")),
      size_label = size_label,
      rep = as.character(rep)
    )
  })
}) |>
  list_flatten() |>
  bind_rows()

# Save observed transitions (P) ------------------------------------------------
obs_tibble <- imap(models$idv_trans, function(sample_size, size) {
  imap(sample_size, function(rep_list, rep) {
    tibble(
      ID = names(rep_list),
      data = rep_list
    ) |>
      tidyr::unnest_longer(data, values_to = "obs_mat", indices_to = "wave") |>
      mutate(
        ID = stringr::str_remove(ID, "p_"),
        wave = stringr::str_remove(wave, "w_"),
        wave = stringr::str_replace(wave, "_", "-"),
        size_label = size,
        rep = rep,
      ) |>
      relocate(ID, wave, size_label, rep)
  })
}) |>
  bind_rows()

# Create predicted transition matrix (P hat)
## Creating an augmented dataset
augmented_data <- imap(models$sample_data, function(sample_size, size) {
  imap(sample_size, function(data, rep) {
    data_augment <- bind_rows(
      mutate(data, y_prev = factor(1)),
      mutate(data, y_prev = factor(2)),
      mutate(data, y_prev = factor(3))
    ) |>
      arrange(ID, w) |>
      mutate(size = size) |>
      relocate(size)
  })
}) |>
  bind_rows()

## Predicting transition probabilities from this augmented data
prediction_matrices <- imap(model_fits, function(parent_block, parent) {
  imap(parent_block, function(sub_block, sub_model) {
    imap(sub_block, function(sample_size, size_label) {
      ## Filter to the participants used in the sample
      data <- augmented_data |>
        filter(size %in% size_label)

      ## Get their IDS (for later)
      ids <- data |> pull(ID) |> unique()

      imap(sample_size, function(model, reps) {
        ## Predict y value probabilities
        probs <- predict(model, newdata = data, type = "probs")

        ## Split into 3x3 matrices
        split_rows <- split(
          seq_len(nrow(probs)),
          ceiling(seq_along(seq_len(nrow(probs))) / 3)
        )

        ## Building names for matrices
        id_wave_names <- rep(ids, each = 2)
        wave_labels <- rep(c("1-2", "2-3"), times = length(ids))
        matrix_names <- paste0("ID_", id_wave_names, "_", wave_labels)

        ## Create 3x3 matrices per individual per wave
        named_matrices <- setNames(
          lapply(split_rows, function(rows) {
            matrix(probs[rows, ], nrow = 3, ncol = 3, byrow = FALSE)
          }),
          matrix_names
        )
        named_matrices
      })
    })
  })
})

## Creating the predicted dataset
predicted_trans_tibble <- imap_dfr(
  prediction_matrices,
  function(parent_block, parent) {
    imap_dfr(parent_block, function(sub_block, sub_model) {
      imap_dfr(sub_block, function(sample_block, sample_size) {
        imap_dfr(sample_block, function(rep_list, reps) {
          imap_dfr(rep_list, function(probs, ids) {
            tibble(
              ID = stringr::str_extract(ids, "(?<=ID_)\\d+"),
              wave = stringr::str_remove(ids, "ID_\\d+_"),
              parent_block = parent,
              sub_block = sub_model,
              size_label = sample_size,
              rep = reps,
              sim_mat = list(probs)
            )
          })
        })
      })
    })
  }
)

# Join both tibbles
transition_tibble <- predicted_trans_tibble |>
  left_join(obs_tibble, by = c("ID", "wave", "size_label", "rep")) |>
  mutate(obs_mat = unname(obs_mat))

# Matrix distance calculations -------------------------------------------------
num_tasks <- nrow(transition_tibble)

results_list <- vector("list", length = num_tasks)

# 5. Calculating matrix distances ----------------------------------------------
for (i in seq_len(num_tasks)) {
  obs <- transition_tibble$obs_mat[[i]]
  sim <- transition_tibble$sim_mat[[i]]

  # Using C++ code
  results_list[[i]] <- tryCatch(
    compare_matrices_rcpp(obs, sim),
    error = function(e) {
      message(sprintf("Error in row %d: %s", i, e$message))
      NULL
    }
  )
}

results_dt <- data.table::rbindlist(results_list, fill = TRUE, idcol = "row_id")

# Merge with metadata
metadata <- transition_tibble |>
  dplyr::select(-c(obs_mat, sim_mat)) |>
  dplyr::mutate(row_id = dplyr::row_number())

matrix_distances <- metadata |>
  dplyr::left_join(results_dt, by = "row_id") |>
  dplyr::select(-row_id)

model_diagnostics <- imap_dfr(model_fits, function(by_sub_block, parent) {
  imap_dfr(by_sub_block, function(by_size, sub_block) {
    imap_dfr(by_size, function(by_rep, size) {
      imap_dfr(by_rep, function(model, rep_index) {
        tibble(
          parent_block = parent,
          sub_block = sub_block,
          size_label = size,
          rep = rep_index,
          aic = AIC(model),
          bic = BIC(model)
        )
      })
    })
  })
})

distances <- matrix_distances |>
  tidyr::pivot_wider(names_from = metric, values_from = value) |>
  left_join(
    model_diagnostics,
    by = c("parent_block", "sub_block", "size_label", "rep")
  ) |>
  tidyr::pivot_longer(
    cols = c(Frobenius:bic),
    names_to = "metric",
    values_to = "value"
  )

distances <- distances |>
  data.table::as.data.table() |>
  tidy_metrics()

message("Summarising data ... ")

# Grouping and summarizing metrics
dist_sum <- distances[,
  .(value = mean(value)),
  by = .(parent_block, sub_block, size_label, rep, wave, metric)
] |>
  tibble::as_tibble() |>
  filter(metric != "Kullback-Leibler Divergence")

best_models <- dist_sum |>
  # Collapse the wave column
  group_by(parent_block, sub_block, size_label, rep, metric) |>
  summarise(value = mean(value), .groups = "drop") |>
  # Compute by metric (probably unnecessary ... )
  split(~metric) |>
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
      mutate(prop = n_lowest / 1)
  })

case_study_plot <- best_models |>
  ggplot(aes(x = parent_block, y = prop, fill = sub_block)) +
  geom_col(colour = "black") +
  # geom_text(
  #   aes(label = ifelse(prop >= 0.04, scales::percent(prop, accuracy = 1), NA)),
  #   position = position_stack(vjust = 0.5),
  #   size = 3
  # ) +
  scale_fill_manual(
    values = c(
      "Null Model" = "#E69F00",
      "Reduced Model 1" = "#56B4E9",
      "Reduced Model 2" = "#009e73",
      "True Model" = "#F0E442",
      "Other" = "#F0E44233",
      "Overfit Model" = "#0072B2"
    ),
    breaks = c(
      "Null Model",
      "Reduced Model 1",
      "Reduced Model 2",
      "True Model",
      "Overfit Model"
    )
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Proportion of times when each model was identified as the best",
    x = "Model Type",
    y = "Proportion of Repetitions as best",
    fill = "Sub Model"
  ) +
  facet_grid(
    size_label ~ metric,
    space = "free",
    labeller = labeller(
      metric = c(
        "AIC" = "Akaike Information Criterion",
        "BIC" = "Bayesian Information Criterion"
      )
    )
  ) +
  theme_simulation()

cowplot::save_plot(
  filename = here::here("analysis/case_study/results/results.pdf"),
  plot = case_study_plot,
  base_width = 25,
  base_height = 10
)

saveRDS(
  object = case_study_plot,
  here::here("manuscript/files/figures/figure_2.RDS")
)
