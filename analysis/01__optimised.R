# Simulation Scenario 1 
# Assuming no previous response effect
# ------------------------------------------------------------------------------

rm(list = ls()) # To annoy Rafael

# 1. Loading packages ----------------------------------------------------------
pacman::p_load(
  dplyr,
  purrr,
  furrr,
  progressr,
  ggplot2
)

# 2. Parallel back-end ----------------------------------------------------------
plan(multisession, workers = parallel::detectCores() - 1)

handlers("txtprogressbar")

# Increasing allowed usage of future 
options(future.globals.maxSize = 3000 * 1024^2) # 3GB Limit

# 3. Simulation functions ------------------------------------------------------
func_files <- list.files(
  path = here::here("R/"), pattern = "\\.R$", full.names = TRUE)

walk(func_files, source)

# 4. Simulating "true" data ----------------------------------------------------
sim <- simulate_data(
  n_subjects = 2000, n_waves = 3, scenario = 1, 
  resim = FALSE, betas = NULL, seed = 123, verbose = TRUE)

# Adding previous states
data <- sim$data |> add_previous_status()

# 5) Fit base, additive, multiplicative models ---------------------------------
models <- fit_markov_model(
  data         = data, 
  sample_sizes = c(100, 250, 1000), 
  n_reps       = 200,
  parallel     = FALSE,
  seed         = 125)

# 6) Extract β‑lists -----------------------------------------------------------
message("Extracting β values ... ")

model_coefs <- models[c("base_models", "additive_models", "multiplicative_models")] |>
  map_depth(3, function(fit) map(fit, extract_betas))

# 7) Extract PIDs into a single tibble -----------------------------------------
pids_df <- imap(models$idv_trans, function(by_reps, size_label) {
  imap(by_reps, function(by_pid_list, rep) {
    tibble(
      ID         = as.numeric(stringr::str_remove(names(by_pid_list), "^p_")),
      size_label = size_label,
      rep        = as.character(rep)
    )
  })
}) |> list_flatten() |> bind_rows()


# 7) Resimulate from each β‑list in parallel -----------------------------------
message("Resimulating data and computing distances ")

num_tasks <- model_coefs |>
  map_depth(3, length) |>
  unlist() |>
  sum()

distances <- with_progress({
  p <- progressor(steps = num_tasks)
  
  future_imap_dfr(model_coefs, function(by_sub_block, parent) {
    future_imap_dfr(by_sub_block, function(by_size, sub_block) {
      future_imap_dfr(by_size, function(by_beta_list, size_label) {
        future_map2_dfr(by_beta_list, seq_along(by_beta_list), function(betas, rep_index) {
          # 1. Update progress
          p()
          
          # 2. Resimulate + add status
          resim_df <- simulate_data(
            n_subjects = 2000,
            n_waves    = 3,
            scenario   = 1,
            resim      = TRUE,
            betas      = betas,
            seed       = 123,
            verbose    = FALSE
          )$data |>
            add_previous_status()
          
          # 3. Filter to observed PIDs
          current_pids <- pids_df |>
            filter(size_label == !!size_label, rep == !!as.character(rep_index)) |>
            pull(ID)
          
          resim_df <- resim_df |>
            filter(ID %in% current_pids)
          
          # Allowing for no observed data (shouldn't happen though?)
          if(nrow(resim_df) == 0) return(NULL)
          
          # 4. Build individual transitions
          sim_trans <- tryCatch(
            create_individual_transition_matrices(data = resim_df),
            error = function(e) {
              message("Transition error (skipping): ", e$message)
              return(NULL)
            })
          
          # 5. Compute distances on the fly
          obs_trans <- models$idv_trans[[size_label]][[rep_index]]
          
          if(is.null(sim_trans)) return(NULL)
          
          imap_dfr(sim_trans, function(waves_mats, pid) {
            pid_clean <- stringr::str_remove(pid, "^p_")
            
            if(!pid %in% names(obs_trans)) return(NULL)
            
            common_waves <- intersect(names(waves_mats), names(obs_trans[[pid]]))
            
            map_dfr(common_waves, function(wv) {
              
              obs_mat <- as.matrix(obs_trans[[pid]][[wv]])
              sim_mat <- as.matrix(waves_mats[[wv]])
              
              compare_matrices(obs_mat = obs_mat, sim_mat = sim_mat) |>
                mutate(
                  ID   = pid_clean,
                  wave = wv
                )
            }) # End of common waves
          }) |> mutate( # End of sim_trans
            parent_block = parent,
            sub_block = sub_block,
            size_label = size_label,
            rep = as.character(rep_index)
          )
        }, .options = furrr_options(seed = TRUE)) # End of by_beta_list
      }, .options = furrr_options(seed = TRUE)) # End of by_size
    }, .options = furrr_options(seed = TRUE)) # End of by_sub_block
  }, .options = furrr_options(seed = TRUE)) # End of model_coefs
})

# 8) Exporting ----------------------------------------------------------------
message("Saving results ... ")

saveRDS(
  object = distances, 
  file = here::here("analysis/results/matrix_distances.RDS"))
