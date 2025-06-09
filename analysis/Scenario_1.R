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

# 3. Simulation functions ------------------------------------------------------
func_files <- list.files(path = here::here("R/"), 
                         pattern = "\\.R$", 
                         full.names = TRUE)

walk(func_files, source)

# 4. Simulating "true" data ----------------------------------------------------
sim <- simulate_data(
  n_subjects = 10000, n_waves = 3, scenario = 1, 
  resim = FALSE, betas = NULL, seed = 123, verbose = TRUE)

# Adding previous states
data <- sim$data |>
  add_previous_status()

# 5) Fit base, additive, multiplicative models ---------------------------------
models <- fit_markov_model(
  data = data, 
  sample_sizes = c(100, 250, 1000), 
  n_reps = 250,
  parallel = FALSE,
  seed = 125)

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

# 7) Resimulate from each β‑list in parallel -----------------------------------
message("Resimulating data ... ")

num_tasks <- model_coefs |> listr::list_flatten(max_depth = 2) |> length()

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
          
          resim_data <- simulate_data(
            n_subjects = 10000, n_waves = 3, scenario = 1, 
            resim = TRUE, betas = betas, seed = 123, verbose = FALSE
          )
          
          resim_data$data |>
            mutate(
              parent_block = parent,
              sub_block = sub_block,
              size_label = size,
              rep = as.character(reps)
            ) |>
            add_previous_status()
        }, .options = furrr_options(seed = TRUE))
      })
    })
  })
})

# 8) Extract PIDs into a single tibble -----------------------------------------
pids_df <- imap(models$idv_trans, function(by_reps, size_label) {
  imap(by_reps, function(by_pid_list, rep) {
    tibble(
      ID         = as.numeric(stringr::str_remove(names(by_pid_list), "^p_")),
      size_label = size_label,
      rep        = as.character(rep)
    )
  })
}) |> list_flatten() |> bind_rows()

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
          
          df |>
            semi_join(pids_df, by = c("ID", "size_label", "rep")) |>
            create_individual_transition_matrices()
        }) # end of by_data_list
      }) # End of by_sizes
    }) # End of by_sub_blocks
  }) # End of resimulation
})

# 10. Compute matrix‐distance metrics ------------------------------------------
# Flatten all transitons into one tibble
message("Computing matrix distances (step 1) ... ")

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

message("Computing matrix distances (step 2) ... ")

num_tasks <- nrow(transition_tibble)

# 10.1. Comparing matrices
matrix_distances <- with_progress({
  p <- progressor(steps = num_tasks)
  
  transition_tibble |>
    mutate(
      results = future_pmap(list(obs_mat, sim_mat), function(obs_mat, sim_mat) {
        p()
        
        compare_matrices(obs_mat = obs_mat, sim_mat = sim_mat)
      }, .options = furrr_options(seed = TRUE))
    ) |>
    tidyr::unnest(results)
})

# 11. Exporting ----------------------------------------------------------------
message("Saving results ... ")

saveRDS(
  object = matrix_distances, 
  file = here::here("analysis/results/matrix_distances.RDS"))

# transition_difference <- purrr::imap(indiv_transitions, function(by_sub_model, parent) {
#   # by_sub_model = indiv_transitions[[parent]] 
#   purrr::imap(by_sub_model, function(by_size, sub_model) {
#     # by_size = indiv_transitions[[parent]][[sub_model]]
#     purrr::imap(by_size, function(by_rep_list, size) {
#       # by_rep_list = indiv_transitions[[parent]][[sub_model]][[size]]
#       # and obs[[size]] is the observed list-of-reps per sample size
#       obs_reps <- obs[[size]]
#       
#       purrr::map2(by_rep_list, obs_reps, function(sim_list, obs_list) {
#         common_pids <- intersect(names(sim_list), names(obs_list))
#         
#         # For each PID, subtract sub‐matrices:
#         # pid$w_1_2
#         # pid$w_2_3
#         pid_diffs <- purrr::map(common_pids, function(pid){
#           sim_pid_list <- sim_list[[pid]]
#           obs_pid_list <- obs_list[[pid]]
#           
#           common_wave <- intersect(names(sim_pid_list), names(obs_pid_list))
#           
#           # Finally, we can subtract the sub matrices
#           wave_diffs <- purrr::map(common_wave, function(wave) {
#             sim_mat <- as.matrix(sim_pid_list[[wave]])
#             obs_mat <- as.matrix(obs_pid_list[[wave]])
#             
#             # Simulated - Observed
#             sim_mat - obs_mat
#           })
#           
#           # Naming the waves
#           names(wave_diffs) <- common_wave
#           wave_diffs
#         })
#         
#         # Naming the pids
#         names(pid_diffs) <- common_pids
#         pid_diffs
#       })
#     })
#   })
# })

# # Estimated transition matrices
# estimate_matrices <- estimate_transition_matrices(models, models$test_data)
# 
# # Splitting into each respective model
# matrices <- list(
#   "Observed"  = models$obs_trans,
#   "Null"      = estimate_matrices$estimated_transitions_null,
#   "Reduced 1" = estimate_matrices$estimated_transitions_red_1,
#   "Reduced 2" = estimate_matrices$estimated_transitions_red_2,
#   "True"      = estimate_matrices$estimated_transitions_true,
#   "Overfit"   = estimate_matrices$estimated_transitions_overfit
# )
# 
# # Plotting ---------------------------------------------------------------------
# compare_transitions(
#   transition_list = matrices, sample_size = "n_1000", rep = 1,
#   obs = FALSE, model_names = NULL)
# 
# # Calculating distance based metrics
# distances <- calculate_matrix_distances(
#   results = estimate_matrices, 
#   sample_size = c("n_100", "n_250", "n_1000"),
#   rep = 1:200)
# 
# # Plotting distances -----------------------------------------------------------
# ## Using boxplots
# distance_box <- distances |>
#   mutate(
#     sample_size = stringr::str_replace(sample_size, "_", " = "),
#     sample_size = factor(sample_size, levels = c("n = 100", "n = 250", "n = 1000"))
#   ) |>
#   ggplot(aes(x = model_type, y = log(value), fill = model_type)) +
#   geom_boxplot() +
#   ggokabeito::scale_fill_okabe_ito() +
#   labs(
#     title = "Distance based metrics",
#     subtitle = "Across sample sizes",
#     x = "Distance based metric",
#     y = "Distance Value") +
#   facet_grid(metric ~ sample_size, scales = "free_y") +
#   theme_bw() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
#     axis.title = element_text(face = "bold", size = 12),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "none"
#   )
# 
# ## Using bar charts
# distance_bar <- distances |>
#   group_by(sample_size, model_type, metric) |>
#   summarise(mean_distance = mean(value)) |> 
#   mutate(
#     mean_distance = round(mean_distance, digits = 2),
#     sample_size = stringr::str_replace(sample_size, "_", " = "),
#     sample_size = factor(sample_size, levels = c("n = 100", "n = 250", "n = 1000"))) |>
#   ggplot(aes(x = metric, y = mean_distance, fill = metric)) +
#   geom_col(colour = "black") +
#   geom_text(aes(label = mean_distance, vjust = -0.5)) +
#   ggokabeito::scale_fill_okabe_ito() +
#   scale_y_continuous(expand = expansion(mult = c(0.075, 0.275))) +
#   labs(
#     title = "Distance based metrics",
#     subtitle = "Across sample sizes",
#     x = "Distance based metric",
#     y = "Average distance") +
#   facet_grid(model_type ~ sample_size) +
#   theme_bw() +
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold"),
#     axis.title = element_text(face = "bold", size = 12),
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "none"
#   )
# 
# # Exporting --------------------------------------------------------------------
# export_path <- "analysis/results/01__base"
# 
# cowplot::save_plot(
#   filename = here::here(export_path, "distance_boxplot.png"),
#   plot = distance_box,
#   base_height = 8, base_width = 10
# )
# 
# cowplot::save_plot(
#   filename = here::here(export_path, "distance_barplot.png"),
#   plot = distance_bar,
#   base_height = 8, base_width = 10
# )
