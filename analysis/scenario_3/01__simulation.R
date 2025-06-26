# Simulation Scenario 3 
# Assuming a multiplicative previous response effect
# ------------------------------------------------------------------------------

rm(list = ls()) # To annoy Rafael

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
handlers("txtprogressbar")

# 3. Simulation functions ------------------------------------------------------
func_files <- list.files(
  path = here::here("R/"), pattern = "\\.R$", full.names = TRUE)

walk(func_files, source)

# 4. Simulating "true" data ----------------------------------------------------
sim <- simulate_data(
  n_subjects = 10000, n_waves = 3, scenario = scenario, 
  resim = FALSE, betas = NULL, seed = 123, verbose = TRUE)

# Adding previous states
data <- sim$data |> add_previous_status()

# 5. Fit base, additive, multiplicative models ---------------------------------
models <- fit_markov_model(
  data         = data, 
  sample_sizes = c(100, 250, 1000, 5000), 
  n_reps       = 200,
  parallel     = TRUE,
  seed         = 125)

# 6. Extract β‑lists -----------------------------------------------------------
message("Extracting β values ... ")

model_fits <- models[c("base_models", "additive_models", "multiplicative_models")]

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
      ID         = as.numeric(stringr::str_remove(names(by_pid_list), "^p_")),
      size_label = size_label,
      rep        = as.character(rep)
    )
  })
}) |> list_flatten() |> bind_rows()

# 8. Resimulate from each β‑list in parallel -----------------------------------
message("Resimulating data ... ")

num_tasks <- model_coefs |> listr::list_flatten(max_depth = 3) |> length()

resimulation <- with_progress({
  p <- progressor(steps = num_tasks)

  future_imap(model_coefs, function(by_sub_block, parent) {
    # Parent is named
    #   - base_models           = 1
    #   - additive_models       = 2
    #   - multiplicative_models = 3
    future_imap(by_sub_block, function(by_size, sub_block) {
      # sub_block is named
      #   - null_models,
      #   - red_1_models,
      #   - red_2_models,
      #   - true_models,
      #   - of_models
      future_imap(by_size, function(by_beta_lists, size) {
        # size_label is named
        #   - n_100,
        #   - n_250,
        #   - n_1000
        future_map2(by_beta_lists, seq_along(by_beta_lists), function(betas, reps) {

          p()

          tryCatch(
            simulate_data(
              n_subjects = 10000, n_waves = 3, scenario = scenario,
              resim = TRUE, og_data = sim$data, betas = betas,
              seed = 123, verbose = FALSE)$data |>
              mutate(
                parent_block = parent,
                sub_block = sub_block,
                size_label = size,
                rep = as.character(reps)
              ) |>
              add_previous_status(),

            error = function(e) {
              message("Error in simulation (skipping): ", e$message)
              return(NULL)
            })
        }, .options = furrr_options(seed = TRUE))
      }, .options = furrr_options(seed = TRUE))
    }, .options = furrr_options(seed = TRUE))
  }, .options = furrr_options(seed = TRUE))
})

# 9. Saving resimulation components --------------------------------------------
message("Saving resimulation components ... ")

plan(sequential)

# Resimulation
saveRDS(
  object = resimulation,
  file = file.path(this.dir(), "results/cache/resim.RDS"))

# Models
saveRDS(
  object = models,
  file = file.path(this.dir(), "results/cache/models.RDS"))

# PIDs
saveRDS(
  object = pids_df,
  file = file.path(this.dir(), "results/cache/pids.RDS"))
