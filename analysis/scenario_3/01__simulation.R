# Simulation Scenario 3 
# Assuming a multiplicative previous response effect
# -----------------------------------------------------------------------------

# 1. Loading packages ----------------------------------------------------------
pacman::p_load(
  dplyr,
  stringr,
  purrr,
  furrr,
  progressr,
  data.table,
  this.path,
  nnet
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
  path = here::here("R/"), pattern = "\\.R$", full.names = TRUE)

walk(func_files, source)

Rcpp::sourceCpp(file = here::here("R/compare_matrices.cpp"))

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

# model_coefs <- imap(model_fits, function(by_sub_blocks, parent) {
#   imap(by_sub_blocks, function(by_sizes, sub_blocks) {
#     imap(by_sizes, function(by_fit_list, size_labels) {
#       map(by_fit_list, extract_betas)
#     })
#   })
# })

# Saving model fits in case something breaks later -----------------------------
saveRDS(model_fits, file.path(this.dir(), "results/cache/test/model_fits.RDS"))

# 7. Extract PIDs into a single tibble -----------------------------------------
# pids_df <- models |> 
#   pluck("idv_trans") |>
#   imap(function(by_reps, size_label) {
#   imap(by_reps, function(by_pid_list, rep) {
#     tibble(
#       ID         = as.numeric(stringr::str_remove(names(by_pid_list), "^p_")),
#       size_label = size_label,
#       rep        = as.character(rep)
#     )
#   })
# }) |> list_flatten() |> bind_rows()

# 8. Creating augmented dataset ------------------------------------------------
message("Augmenting data ... ")

augmented_data <- models |>
  pluck("test_data") |>
  imap(function(sample_data, size_label) {
    imap(sample_data, function(rep_data, reps) {
      bind_rows(
        mutate(rep_data, y_prev = factor(1)),
        mutate(rep_data, y_prev = factor(2)),
        mutate(rep_data, y_prev = factor(3)),
      ) |>
        arrange(ID, w)
    })
  })

# 9. Calculating predicted probabilities ---------------------------------------
message("Calculating predicted probabilities ... ")

predicted_probs <- model_fits |>
  imap(function(parent_block, parent_model) {
    imap(parent_block, function(sub_block, sub_model) {
      imap(sub_block, function(sample_size, size) {
        imap(sample_size, function(betas, rep_index) {
          # Get associated augmented data file & IDs
          pred_data <- augmented_data[[size]][[rep_index]]
          ids <- pred_data |> pull(ID) |> unique()
          
          if(length(betas$lev) != 3) return(NULL)
          
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


# Turn predictions into a nice tibble
num_tasks <- sum(map_int(
  predicted_probs, ~ sum(purrr::map_int(.x, ~ sum(lengths(.x))))
))

predicted_probs_tibble <- flatten_predictions(
  probs = predicted_probs, num_tasks = num_tasks)

# 10. Create observed tibble ---------------------------------------------------
message("Creating observed tibble ... ")

obs_tibble <- models |>
  pluck("idv_trans") |>
  flatten_obs_transitions() |>
  tibble::as_tibble() |>
  mutate(wave = case_when(
    wave == "2-2" ~ "1-2", 
    wave == "3-3" ~ "2-3", 
    TRUE ~ wave
  ))

# 11. Joining both transition tibbles ------------------------------------------
message("Joining tibbles ... ")

transition_tibble <- predicted_probs_tibble |>
  left_join(obs_tibble, by = c("ID", "wave", "rep", "size_label"))

# Saving transition tibble -----------------------------------------------------
saveRDS(
  transition_tibble,
  file.path(this.dir(), "results/cache/test/transition_tibble.RDS")
)

# 12. Performing distance metrics calculations --------------------------------
message("Calculating matrix distances ... ")

# Progress bar
num_tasks <- nrow(transition_tibble)

pb <- txtProgressBar(min = 0, max = num_tasks, style = 3)

# Preallocate result list
results_list <- vector("list", length = num_tasks)

# 13. Calculating matrix distances ---------------------------------------------
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

# Saving in case of down steam breaking
saveRDS(
  object = results_list,
  file = file.path(this.dir(), "results/cache/test/results_list.RDS"))

# 14. Post-processing ----------------------------------------------------------
# Bind rows
results_dt <- data.table::rbindlist(
  results_list, fill = TRUE, idcol = "row_id")

# Merge with metadata
metadata <- transition_tibble |>
  dplyr::select(-c(obs_mat, sim_mat)) |>
  dplyr::mutate(row_id = dplyr::row_number())

# Join into one dataset
matrix_distances <- metadata |> 
  dplyr::left_join(results_dt, by = "row_id") |>
  dplyr::select(-row_id)

# Temporarily saving matrix distances
saveRDS(
  object = matrix_distances,
  file = file.path(this.dir(), "results/cache/test/matrix_distances_temp.RDS"))

# 15. Getting AIC and BIC data from models -------------------------------------
model_diagnostics <- imap_dfr(model_fits, function(by_sub_block, parent) {
  imap_dfr(by_sub_block, function(by_size, sub_block) {
    imap_dfr(by_size, function(by_rep, size) {
      imap_dfr(by_rep, function(model, rep_index) {
        tibble(
          parent_block = parent,
          sub_block = sub_block,
          size_label = size,
          rep = as.character(rep_index),
          aic = AIC(model),
          bic = BIC(model)
        )
      })
    })
  })
})

# Temporarily saving model diagnostics
saveRDS(
  object = model_diagnostics,
  file = file.path(this.dir(), "results/cache/test/model_diagnostics_temp.RDS"))

# Merging AIC / BIC into metric data
matrix_distances <- matrix_distances |>
  tidyr::pivot_wider(
    names_from = metric, values_from = value) |>
  left_join(model_diagnostics, by = c("parent_block", "sub_block", "size_label", "rep")) |>
  tidyr::pivot_longer(
    cols = c(Frobenius:bic), names_to = "metric", values_to = "value")

# 16. Exporting -----------------------------------------------------------------
message("Saving results ... ")

fst::write.fst(
  x = matrix_distances, 
  path = file.path(this.dir(), "results/matrix_distances_test.fst"))

# 8. Resimulate from each β‑list in parallel -----------------------------------
# message("Resimulating data ... ")

# num_tasks <- model_coefs |> listr::list_flatten(max_depth = 3) |> length()

# resimulation <- resimulate_data(
#   model_coefs = model_coefs,
#   sim = sim,
#   scenario = scenario,
#   num_tasks = num_tasks
# )

# 9. Saving resimulation components --------------------------------------------
# message("Saving resimulation components ... ")
# 
# plan(sequential)
# 
# # Resimulation
# # saveRDS(object = resimulation, file = file.path(this.dir(), "results/cache/resim.RDS"))
# 
# # Observed transitions
# saveRDS(
#   object = models$idv_trans,
#   file = file.path(this.dir(), "results/cache/test/obs_trans.RDS"))
# 
# # Model fits
# saveRDS(
#   object = model_fits,
#   file = file.path(this.dir(), "results/cache/test/models.RDS"))
# 
# # PIDs (not sure I need this anymore)
# saveRDS(
#   object = pids_df,
#   file = file.path(this.dir(), "results/cache/test/pids.RDS"))
# 
# # Data used in sample
# saveRDS(
#   object = models$sample_data,
#   file = file.path(this.dir(), "results/cache/test/sample_data.RDS")
# )
