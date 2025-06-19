# Matrix distance calculation

rm(list = ls())

# Packages ---------------------------------------------------------------------
pacman::p_load(
  dplyr,
  purrr,
  furrr,
  progressr
)

# Functions --------------------------------------------------------------------
# Using a C++ function instead of an R one
Rcpp::sourceCpp(file = here::here("R/compare_matrices.cpp"))

# Reading in transition data ---------------------------------------------------
message("Reading in data ... ")

path_scenario <- "./analysis/scenario_3/"

transitions <- readRDS(here::here(path_scenario, "results/transition_tibble.RDS"))

# Calculating matrix distances -------------------------------------------------
message("Calculating distances ... ")

num_tasks <- nrow(transitions)

# C++ functions cannot be parallelised so we use pmap() instead of future_pmap()
matrix_distances <- with_progress({
  
  p <- progressor(steps = num_tasks)
  
  transitions |>
    mutate(
      results = pmap(list(obs_mat, sim_mat), function(obs_mat, sim_mat) {
        
        p()
        
        tryCatch(
          compare_matrices_rcpp(Obs = obs_mat, Sim = sim_mat),
          error = function(e) {
            message("Transition error: ", e$message)
            return(NULL)
          })
      })
    ) |>
    tidyr::unnest(results)
})

# Removing large transition columns
matrix_distances <- matrix_distances |> select(-c(sim_mat, obs_mat))

# Exporting --------------------------------------------------------------------
message("Saving results ... ")

fst::write.fst(
  x = matrix_distances, 
  path = here::here(path_scenario, "results/matrix_distances.fst"))