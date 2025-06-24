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
  ggplot2,
  data.table
)

# 2. Parallel back-end ----------------------------------------------------------
plan(multisession, workers = parallel::detectCores() - 1)

handlers("txtprogressbar")

# 3. Simulation functions ------------------------------------------------------
func_files <- list.files(
  path = here::here("R/"), pattern = "\\.R$", full.names = TRUE)

walk(func_files, source)

# 4. Simulating "true" data ----------------------------------------------------
sim <- simulate_data(
  n_subjects = 10000, n_waves = 3, scenario = 1, 
  resim = FALSE, betas = NULL, og_data = NULL, seed = 123, verbose = TRUE)

# Adding previous states
data <- sim$data |> add_previous_status()

# 5) Fit base, additive, multiplicative models ---------------------------------
models <- fit_markov_model(
  data         = data, 
  sample_sizes = c(100, 250, 1000, 5000), 
  n_reps       = 200,
  parallel     = TRUE,
  seed         = 125)

# 6) Extract β‑lists -----------------------------------------------------------
message("Extracting β values ... ")

model_fits <- models[c("base_models", "additive_models", "multiplicative_models")]

model_coefs <- imap(model_fits, function(by_sub_blocks, parent) {
  imap(by_sub_blocks, function(by_sizes, sub_blocks) {
    imap(by_sizes, function(by_fit_list, size_labels) {
      map(by_fit_list, extract_betas)
    })
  })
})

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

# 8) Resimulate from each β‑list in parallel -----------------------------------
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
              n_subjects = 10000, n_waves = 3, scenario = 1, 
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

# Saving resimulation data (for later use) -------------------------------------
message("Saving resimulation data ... ")

saveRDS(
  object = resimulation, 
  file = here::here("analysis/scenario_1/results/resim.RDS"))

# 9) Compute individual transition matrices, filtered by PIDs ------------------
message("Computing individual transitions ... ")

num_tasks <- resimulation |> listr::list_flatten(max_depth = 3) |> length()

indiv_transitions <- with_progress({
  p <- progressor(steps = num_tasks)

  future_imap(resimulation, function(by_sub_blocks, parent) {
    future_imap(by_sub_blocks, function(by_sizes, sub_block) {
      future_imap(by_sizes, function(by_data_list, size_label) {
        future_map(by_data_list, function(df) {

          p()

          # Filtering down to same participants used in model sample
          df <- df |>
            semi_join(pids_df, by = c("ID", "size_label", "rep"))

          # Return NULL if no one is on data
          if (nrow(df) == 0) return(NULL)

          # Calculating individual transitions (with error handling)
          tryCatch(
            create_individual_transition_matrices(data = df),
            error = function(e) {
              message("Transition error: ", e$message)
              return(NULL)
            })
        }, .options = furrr_options(seed = TRUE)) # end of by_data_list
      }, .options = furrr_options(seed = TRUE)) # End of by_sizes
    }, .options = furrr_options(seed = TRUE)) # End of by_sub_blocks
  }, .options = furrr_options(seed = TRUE)) # End of resimulation
})

# Memory cleaning
rm(model_fits, model_coefs, resimulation)

# 10) Compute matrix‐distance metrics ------------------------------------------
# Flatten all transitons into one tibble
message("Computing matrix tibble ... ")

transition_tibble <- imap_dfr(indiv_transitions, function(by_sub_model, parent) {
  imap_dfr(by_sub_model, function(by_size, sub_model) {
    imap_dfr(by_size, function(reps, size_label) {
      imap_dfr(reps, function(sim_list, rep_index) {
        obs_list <- models$idv_trans[[size_label]][[rep_index]]

        # for each PID and wave, extract sim and obs matrices
        imap_dfr(sim_list, function(sim_pid_list, pid) {
          common_waves <- intersect(names(sim_pid_list), names(obs_list[[pid]]))
          tibble(
            parent_block = parent,
            sub_model    = sub_model,
            size_label   = size_label,
            rep          = rep_index,
            ID           = stringr::str_remove(pid, "^p_"),
            wave         = common_waves,
            sim_mat      = sim_pid_list[common_waves] |> map(as.matrix),
            obs_mat      = obs_list[[pid]][common_waves] |> map(as.matrix)
          )
        }) # End of sim_list
      }) # End of reps
    }) # End of by_size
  }) # End of by_sub_model
})

# Memory cleaning
rm(data, indiv_transitions, models, pids_df, sim)

# Resetting to sequential processing
plan(sequential)

# 11) Exporting ----------------------------------------------------------------
message("Saving results ... ")

saveRDS(
  object = transition_tibble,
  file = here::here("analysis/scenario_1/results/transition_tibble.RDS"))

