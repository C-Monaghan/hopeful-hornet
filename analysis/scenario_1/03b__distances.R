# Matrix distance calculation - scenario 1

# 1. Packages ------------------------------------------------------------------
pacman::p_load(
  progressr,
  this.path,
  here
)

# 2. Functions -----------------------------------------------------------------
# Using a C++ function instead of an R one
Rcpp::sourceCpp(file = here::here("R/compare_matrices.cpp"))

source(here::here("R/pad_matrix_to_3x3.R"))

# 3. Reading in transition data ------------------------------------------------
message("Reading in data ... ")

transitions <- readRDS(here(this.dir(), "results/transition_tibble.RDS"))

# 4. Setting up distances ------------------------------------------------------
# Progress bar
num_tasks <- nrow(transitions)

pb <- txtProgressBar(min = 0, max = num_tasks, style = 3)

# Preallocate result list
results_list <- vector("list", length = num_tasks)

# 5. Calculating matrix distances ----------------------------------------------
for (i in seq_len(num_tasks)) {
  obs <- transitions$obs_mat[[i]] |> pad_matrix_to_3x3()
  sim <- transitions$sim_mat[[i]] |> pad_matrix_to_3x3()
  
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

# 7. Exporting -----------------------------------------------------------------
message("Saving results ... ")

fst::write.fst(
  x = matrix_distances, 
  path = file.path(this.dir(), "results/matrix_distances.fst"))