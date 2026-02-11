# Applying the simulation design to a case study dataset
# The Health and Retirement Study (HRS)

# rm(list = ls()); gc()

message("Setting up ...")

set.seed(4321)

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
message("Reading in data ...")

data <- readRDS(here::here("analysis/case_study/data/HRS.RDS"))

# Applying DTMC model ----------------------------------------------------------
message("Fitting Markov Model")

models <- fit_markov_model(
  data = data,
  sample_sizes = c(100, 250, 1000, 5000),
  n_reps = 200,
  parallel = TRUE,
  seed = 4321
)

# Extract β‑lists -----------------------------------------------------------
message("Extracting β values ... ")

model_fits <- models[c(
  "base_models",
  "additive_models",
  "multiplicative_models"
)]

# Save observed transitions (P) ------------------------------------------------
message("Calculating observed transitions ...")

obs_tibble <- imap(models$idv_trans, function(sample_size, size) {
  imap(sample_size, function(rep_list, rep) {
    tibble(
      ID = names(rep_list),
      data = rep_list
    ) |>
      tidyr::unnest_longer(data, values_to = "obs_mat", indices_to = "wave") |>
      mutate(
        ID = as.numeric(stringr::str_remove(ID, "p_")),
        wave = stringr::str_remove(wave, "w_"),
        wave = stringr::str_replace(wave, "_", "-"),
        size_label = size,
        rep = rep,
      ) |>
      relocate(ID, wave, size_label, rep)
  })
}) |>
  bind_rows()

# Create predicted transition matrix (P hat) -----------------------------------
## Creating an augmented dataset
message("Creating an augmented dataset ...")

augmented_data <- models |>
  pluck("test_data") |>
  imap(function(sample_size, size) {
  imap(sample_size, function(rep_data, rep) {
    # Augment their data-points
    data_augment <- bind_rows(
      mutate(rep_data, y_prev = factor(1)),
      mutate(rep_data, y_prev = factor(2)),
      mutate(rep_data, y_prev = factor(3))
    ) |>
      arrange(ID, w) |>
      mutate(size_label = size)
  
      return(data_augment)
    })
  })

## Predicting transition probabilities from this augmented data
message("Calculating predicting transition probabilities ...")

prediction_matrices <- imap(model_fits, function(parent_block, parent) {
  imap(parent_block, function(sub_block, sub_model) {
    imap(sub_block, function(sample_size, size) {
      imap(sample_size, function(model, rep_index) {
        
        # Get associated augmented data file & IDs
        pred_data <- augmented_data[[size]][[rep_index]]
        ids <- pred_data |> pull(ID) |> unique()
        
        ## Predict y value probabilities
        probs <- predict(model, newdata = pred_data, type = "probs")

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
message("Creating the predicted dataset ...")

## Creating the predicted dataset
rows <- list()
i <- 1L

iwalk(prediction_matrices, function(parent_block, parent) {
  iwalk(parent_block, function(sub_block, sub_model) {
    iwalk(sub_block, function(sample_block, sample_size) {
      iwalk(sample_block, function(rep_list, reps) {
        iwalk(rep_list, function(probs, ids) {
          rows[[i]] <<- list(
            ID = str_extract(ids, "(?<=ID_)\\d+"),
            wave = str_remove(ids, "ID_\\d+_"),
            parent_block = parent,
            sub_block = sub_model,
            size_label = sample_size,
            rep = reps,
            sim_mat = list(probs)
          )
          
          i <<- i + 1L
        })
      })
    })
  })
})

message("Making into a tibble ...")

predicted_trans_tibble <- tidyr::as_tibble(rbindlist(
  rows, use.names = TRUE, fill = TRUE)) |> 
  mutate(ID = as.numeric(ID))

# Join both tibbles
message("Joining both tibbles ... ")

transition_tibble <- predicted_trans_tibble |>
  left_join(obs_tibble, by = c("ID", "wave", "size_label", "rep")) |>
  mutate(obs_mat = unname(obs_mat))

# Matrix distance calculations -------------------------------------------------
num_tasks <- nrow(transition_tibble)

message("Running matrix distance calculations ... ")

results_list <- vector("list", length = num_tasks)
pb <- txtProgressBar(min = 0, max = num_tasks, style = 3)

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
  
  # Update progress bar
  setTxtProgressBar(pb, i)
}

close(pb) # Close progress bar

message("Joining distances into one tibble ... ")

results_dt <- data.table::rbindlist(results_list, fill = TRUE, idcol = "row_id")

# Merge with metadata
message("Merging meta data ... ")

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

message("Tidying data ... ")

distances <- distances |>
  data.table::as.data.table() |>
  tidy_metrics()

message("Summarising data ... ")

# Grouping and summarizing metrics
dist_sum <- distances[,
  .(value = mean(value)),
  by = .(parent_block, sub_block, size_label, rep, wave, metric)
] |>
  tibble::as_tibble()

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
      mutate(prop = n_lowest / 200)
  })

message("Plotting ... ")

case_study_plot <- best_models |>
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
    ),
    labels = c(
      "Intercept only", 
      "y ~ x1", 
      "y ~ x1 + x2", 
      "y ~ x1 + x2 + x3", 
      "y ~ x1 + x2 + x3 + x4 + x5")
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

message("Exporting ... ")

# Save PDF version of plot
cowplot::save_plot(
  filename = here::here("analysis/case_study/results/figure 2a_red.pdf"),
  plot = case_study_plot,
  base_width = 25,
  base_height = 10
)

# Save RDS version of plot
saveRDS(
  object = case_study_plot,
  here::here("analysis/case_study/results/figure_2.RDS")
)

saveRDS(
  object = best_models, 
  here::here("analysis/case_study/results/best_models.RDS")
  )
