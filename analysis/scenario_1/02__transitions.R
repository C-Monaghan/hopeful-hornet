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

num_tasks <- resimulation |> listr::list_flatten(max_depth = 3) |> length()

augmented_data <- create_augmented_data(
  resimulation = resimulation, pids_df = pids_df, num_tasks = num_tasks)

# 5. Creating transition probabilities ----------------------------------------
message("Creating transition probabilities ... ")

num_tasks <- sum(map_int(
 augmented_data, ~ sum(purrr::map_int(.x, ~ sum(lengths(.x))))
))

predicted_probs <- create_predicted_probs(
  augmented_data = augmented_data, model_fits = models, num_tasks = num_tasks)

# 6. Creating probability tibble ----------------------------------------------
predicted_probs <- readRDS(file = file.path(this.dir(), "results/cache/predicted_probs.RDS"))

message("Creating transition tibble ... ")

num_tasks <- sum(map_int(
  predicted_probs, ~ sum(purrr::map_int(.x, ~ sum(lengths(.x))))
))

predicted_probs_tibble <- flatten_predictions(probs = predicted_probs, num_tasks = num_tasks)

# 7. Create observed tibble ---------------------------------------------------
message("Creating observed tibble ... ")

num_tasks <- sum(map_int(
  obs_trans, ~ sum(purrr::map_int(.x, ~ sum(lengths(.x))))
))

obs_tibble <- flatten_obs_transitions(obs_trans = obs_trans) |>
  tibble::as_tibble()

# 8. Joining both transition tibbles ------------------------------------------
message("Joining tibbles ... ")

transition_tibble <- predicted_probs_tibble |>
  left_join(obs_tibble, by = c("ID", "wave", "rep", "size_label"))

# Saving predicted probabilities ----------------------------------------------
message("Exporting ... ")

saveRDS(
  object = transition_tibble,
  file = here(this.dir(), "results/transition_tibble.RDS"))
