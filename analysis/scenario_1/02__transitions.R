# Calculating original and estimated transitions
# Scenario 1

# 1. Loading packages ----------------------------------------------------------
pacman::p_load(
  dplyr,
  stringr,
  purrr,
  furrr,
  progressr,
  data.table,
  this.path
)

# 2. Functions -----------------------------------------------------------------
source(here::here("R/create_augmented_data.R"))
source(here::here("R/create_predicted_probs.R"))
source(here::here("R/flatten_predictions.R"))
source(here::here("R/flatten_obs_transitions.R"))

# 3. Reading in cached files ---------------------------------------------------
message("Reading in data files ... ")
 
resimulation   <- readRDS(file = file.path(this.dir(), "results/cache/resim.RDS"))
pids_df        <- readRDS(file = file.path(this.dir(), "results/cache/pids.RDS"))
models         <- readRDS(file = file.path(this.dir(), "results/cache/models.RDS"))
obs_trans      <- readRDS(file = file.path(this.dir(), "results/cache/obs_trans.RDS"))

models         <- models[c("base_models", "additive_models", "multiplicative_models")]

# 4. Augmenting datasets, filtered by PIDs ------------------------------------
message("Augmenting data ... ")

# num_tasks <- resimulation |> listr::list_flatten(max_depth = 3) |> length()
# num_taks <- data |> nrow()

# augmented_data <- create_augmented_data(
#   resimulation = resimulation, pids_df = pids_df, num_tasks = num_tasks)

augmented_data <- pids_df |>
  split(~rep) |>
  purrr::imap(function(rep_list, rep_num) {
    rep_list |>
      split(~size_label) |>
      purrr::imap(function(pids, size) {
        data_filter <- data |>
          filter(ID %in% pids$ID)
        
        bind_rows(
          mutate(data_filter, y_prev = factor(1)),
          mutate(data_filter, y_prev = factor(2)),
          mutate(data_filter, y_prev = factor(3))
        ) |>
          arrange(ID, w)
      })
  })

# 5. Creating transition probabilities ----------------------------------------
message("Creating transition probabilities ... ")

# num_tasks <- sum(map_int(
#  augmented_data, ~ sum(purrr::map_int(.x, ~ sum(lengths(.x))))
# ))

predicted_probs <- model_fits |>
  imap(function(parent_block, parent_model) {
    imap(parent_block, function(sub_block, sub_model) {
      imap(sub_block, function(sample_size, size) {
        imap(sample_size, function(betas, rep_index) {
          # Get associated augmented data file & IDs
          pred_data <- augmented_data[[rep_index]][[size]]
          ids <- pred_data |> pull(ID) |> unique()
          
          # Calculate predicted probabilities
          probs <- predict(betas, pred_data, type = "probs")
          
          # Split into 3x3 matrices
          # Split into 3x3 matrices
          split_rows <- split(
            seq_len(nrow(probs)),
            ceiling(seq_along(seq_len(nrow(probs))) / 3)
          )
          
          # Building names for matrices
          id_wave_names <- rep(ids, each = 2)
          wave_labels <- rep(c("1-2", "2-3"), times = length(ids))
          matrix_names <- paste0("ID_", id_wave_names, "_", wave_labels)
          
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



# predicted_probs <- create_predicted_probs(
#   augmented_data = augmented_data, model_fits = models, num_tasks = num_tasks)

# 6. Creating probability tibble ----------------------------------------------
# predicted_probs <- readRDS(file = file.path(this.dir(), "results/cache/predicted_probs.RDS"))

message("Creating transition tibble ... ")

num_tasks <- sum(map_int(
  predicted_probs, ~ sum(purrr::map_int(.x, ~ sum(lengths(.x))))
))

predicted_probs_tibble <- flatten_predictions(
  probs = predicted_probs, num_tasks = num_tasks)

# 7. Create observed tibble ---------------------------------------------------
message("Creating observed tibble ... ")

num_tasks <- sum(map_int(
  obs_trans, ~ sum(purrr::map_int(.x, ~ sum(lengths(.x))))
))

obs_tibble <- flatten_obs_transitions(obs_trans = obs_trans) |>
  tibble::as_tibble() |>
  mutate(wave = case_when(
    wave == "2-2" ~ "1-2", wave == "3-3" ~ "2-3", TRUE ~ wave
  ))

# 8. Joining both transition tibbles ------------------------------------------
message("Joining tibbles ... ")

transition_tibble <- predicted_probs_tibble |>
  left_join(obs_tibble, by = c("ID", "wave", "rep", "size_label"))

# Saving predicted probabilities ----------------------------------------------
message("Exporting ... ")

saveRDS(
  object = transition_tibble,
  file = here(this.dir(), "results/transition_tibble.RDS"))
