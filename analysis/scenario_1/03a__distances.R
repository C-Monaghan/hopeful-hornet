# Create matrix distance tibble
# Scenario 1

rm(list = ls())

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

# 2. Parallel back-end ---------------------------------------------------------
plan(multisession, workers = parallel::detectCores() - 1)

handlers(global = TRUE)

handlers(handler_progress(
  format = "[:bar] :percent (:elapsed elapsed, :eta remaining)",
  clear = FALSE,
  width = 60
))

# 3. Functions -----------------------------------------------------------------
source(here::here("R/extract_transition_pairs.R"))
source(here::here("R/extract_transition_parallel.R"))

# 4. Reading in cached files ---------------------------------------------------
message("Reading in cached data ... ")

indiv_transitions <- readRDS(file = file.path(this.dir(), "results/cache/indiv_trans.RDS"))
obs_trans         <- readRDS(file = file.path(this.dir(), "results/cache/obs_trans.RDS"))

# 5. Compute matrix‐distance metrics -------------------------------------------
# Flatten all transitons into one tibble
message("Computing matrix tibble ... ")

# Where is obs_trans located
obs_path <- here(this.dir(), "results/cache")

# Parallel method (ensure models is available globally, which it will be)
transition_tibble <- extract_transition_parallel(
  indiv_transitions = indiv_transitions, obs_trans = obs_trans)

# Sequential method (I'm too impatient)
transition_tibble <- extract_transition_pairs(
  indiv_transitions = indiv_transitions, obs_trans = obs_trans)

# Resetting to sequential processing
plan(sequential)

# 6. Exporting ----------------------------------------------------------------
message("Saving results ... ")

saveRDS(
  object = transition_tibble,
  file = file.path(this.dir(), "results/transition_tibble.RDS"))
