# Simulation Scenario 2
# Assuming an additive previous response effect
# ------------------------------------------------------------------------------

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

# Scenario setup
scenario <- case_when(
  stringr::str_detect(this.path(), "scenario_1") ~ 1,
  stringr::str_detect(this.path(), "scenario_2") ~ 2,
  stringr::str_detect(this.path(), "scenario_3") ~ 3
)

# 2. Parallel back-end ----------------------------------------------------------
plan(multisession, workers = parallel::detectCores() - 1)

handlers(global = TRUE)

handlers(handler_progress(
  format = "[:bar] :percent (:elapsed elapsed, :eta remaining)",
  clear = FALSE,
  width = 60
))

# 3. Simulation functions ------------------------------------------------------
func_files <- list.files(
  path = here::here("R/"),
  pattern = "\\.R$",
  full.names = TRUE
)

walk(func_files, source)

# 4. Simulating "true" data ----------------------------------------------------
sim <- simulate_data(
  n_subjects = 10000,
  n_waves = 3,
  scenario = scenario,
  resim = FALSE,
  betas = NULL,
  seed = 123,
  verbose = TRUE
)

# Adding previous states
data <- sim$data |> add_previous_status()

# 5. Fit base, additive, multiplicative models ---------------------------------
models <- fit_markov_model(
  data = data,
  sample_sizes = c(100, 250, 500),
  n_reps = 1,
  parallel = TRUE,
  seed = 125
)

# 6. Extract β‑lists -----------------------------------------------------------
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

# 7. Extract PIDs into a single tibble -----------------------------------------
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

# 8. Resimulate from each β‑list in parallel -----------------------------------
message("Resimulating data ... ")

num_tasks <- model_coefs |> listr::list_flatten(max_depth = 3) |> length()

resimulation <- resimulate_data(
  model_coefs = model_coefs,
  sim = data,
  scenario = scenario,
  num_tasks = num_tasks
)

# 9. Saving resimulation components --------------------------------------------
message("Saving resimulation components ... ")

plan(sequential)

# # Resimulation
saveRDS(
  object = resimulation,
  file = file.path(this.dir(), "results/cache/resim.RDS")
)

# Models (obs_trans)
saveRDS(
  object = models$idv_trans,
  file = file.path(this.dir(), "results/cache/obs_trans.RDS")
)

# Models (fits)
saveRDS(
  object = model_fits,
  file = file.path(this.dir(), "results/cache/models.RDS")
)

# PIDs
saveRDS(
  object = pids_df,
  file = file.path(this.dir(), "results/cache/pids.RDS")
)
