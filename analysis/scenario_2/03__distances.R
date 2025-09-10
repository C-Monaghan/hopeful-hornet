# Matrix distance calculation - scenario 2

# 1. Packages ------------------------------------------------------------------
pacman::p_load(
  dplyr,
  purrr,
  furrr,
  progressr,
  this.path,
  here,
  nnet
)

# 2. Functions -----------------------------------------------------------------
# Using a C++ function instead of an R one
Rcpp::sourceCpp(file = here::here("R/compare_matrices.cpp"))

# 3. Reading in data files -----------------------------------------------------
message("Reading in data ... ")

transitions    <- readRDS(here(this.dir(), "results/transition_tibble.RDS"))
models         <- readRDS(here(this.dir(), "results/cache/models.RDS"))

# Only interested in the fits
models         <- models[c("base_models", "additive_models", "multiplicative_models")]

# 4. Setting up distances ------------------------------------------------------
# Progress bar
num_tasks <- nrow(transitions)

pb <- txtProgressBar(min = 0, max = num_tasks, style = 3)

# Preallocate result list
results_list <- vector("list", length = num_tasks)

# 5. Calculating matrix distances ----------------------------------------------
for (i in seq_len(num_tasks)) {
  obs <- transitions$obs_mat[[i]]
  sim <- transitions$sim_mat[[i]]
  
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
  file = file.path(this.dir(), "results/cache/results_list.RDS"))

# 6. Post-processing -----------------------------------------------------------
# Bind rows
results_dt <- data.table::rbindlist(results_list, fill = TRUE, idcol = "row_id")

# Merge with metadata
metadata <- transitions |>
  dplyr::select(-c(obs_mat, sim_mat)) |>
  dplyr::mutate(row_id = dplyr::row_number())

# Join into one dataset
matrix_distances <- metadata |> 
  dplyr::left_join(results_dt, by = "row_id") |>
  dplyr::select(-row_id)

# Temporarily saving matrix distances
saveRDS(
  object = matrix_distances,
  file = file.path(this.dir(), "results/cache/matrix_distances_temp.RDS"))

# Getting AIC and BIC data from models
model_diagnostics <- imap_dfr(models, function(by_sub_block, parent) {
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
  file = file.path(this.dir(), "results/cache/model_diagnostics_temp.RDS"))

# Merging AIC / BIC into metric data
matrix_distances <- matrix_distances |>
  tidyr::pivot_wider(names_from = metric, values_from = value) |>
  left_join(model_diagnostics, by = c("parent_block", "sub_block", "size_label", "rep")) |>
  tidyr::pivot_longer(cols = c(Frobenius:bic), names_to = "metric", values_to = "value")

# 7. Exporting -----------------------------------------------------------------
message("Saving results ... ")

fst::write.fst(
  x = matrix_distances, 
  path = file.path(this.dir(), "results/matrix_distances_2.fst"))
