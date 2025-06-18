rm(list = ls())

# Packages ---------------------------------------------------------------------
pacman::p_load(
  dplyr,
  purrr,
  furrr,
  progressr
)

source(here::here("R/compare_matrices.R"))

# Reading in transition data ---------------------------------------------------
message("Reading in data ... ")

path_scenario <- "./analysis/scenario_3/"

transitions <- readRDS(here::here(path_scenario, "results/transition_tibble.RDS"))

# Calculating matrix distances -------------------------------------------------
message("Calculating distances ... ")

num_tasks <- nrow(transitions)

matrix_distances <- with_progress({
  
  p <- progressor(steps = num_tasks)
  
  transitions |>
    mutate(
      results = future_pmap(list(obs_mat, sim_mat), function(obs_mat, sim_mat) {
        
        p()
        
        tryCatch(
          compare_matrices(obs_mat = obs_mat, sim_mat = sim_mat),
          error = function(e) {
            message("Transition error: ", e$message)
            return(NULL)
          })
      }, .options = furrr_options(seed = TRUE))
    ) |>
    tidyr::unnest(results)
})

# Removing transition matrices
matrix_distances <- matrix_distances |> 
  select(-c(sim_mat, obs_mat))

# Exporting --------------------------------------------------------------------
message("Saving results ... ")

fst::write.fst(
  x = matrix_distances, 
  path = here::here(path_scenario, "results/matrix_distances.fst"))