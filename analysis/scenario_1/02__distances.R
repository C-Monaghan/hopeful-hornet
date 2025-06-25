# Matrix distance calculation (scenario 1)

rm(list = ls())

# Packages ---------------------------------------------------------------------
pacman::p_load(
  progressr,
  this.path,
  here
)

# Functions --------------------------------------------------------------------
# Using a C++ function instead of an R one
Rcpp::sourceCpp(file = here("R/compare_matrices.cpp"))

# Reading in transition data ---------------------------------------------------
message("Reading in data ... ")

path_scenario <- this.dir()

transitions <- readRDS(here(path_scenario, "results/transition_tibble.RDS"))

# Setting up distances ---------------------------------------------------------
# Preallocate result list
results_list <- vector("list", length = num_tasks)

# Progress bar
num_tasks <- nrow(transitions)

pb <- txtProgressBar(min = 0, max = num_tasks, style = 3)

# Calculating matrix distances -------------------------------------------------
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

# Bind rows
results_dt <- data.table::rbindlist(results_list, fill = TRUE, idcol = "row_id")

# Merge with metadata
metadata <- transitions |>
  dplyr::select(-c(obs_mat, sim_mat)) |>
  dplyr::mutate(row_id = row_number())

# Join into one dataset
matrix_distances <- metadata |> 
  dplyr::left_join(results_dt, by = "row_id") |>
  dplyr::select(-row_id)

# Exporting --------------------------------------------------------------------
message("Saving results ... ")

fst::write.fst(
  x = matrix_distances, 
  path = here(path_scenario, "results/matrix_distances.fst"))