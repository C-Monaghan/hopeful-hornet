extract_transition_pairs <- function(indiv_transitions, models) {
  # Estimate total number of reps to use as progress steps
  num_reps <- sum(map_int(
    indiv_transitions, ~ sum(map_int(.x, ~ sum(map_int(.x, length))))
  ))
  
  # Creating transition tibble (with progress)
  with_progress({
    p <- progressor(steps = num_reps)
    
    future_imap_dfr(indiv_transitions, function(by_sub_model, parent_block) {
      imap_dfr(by_sub_model, function(by_size, sub_model) {
        imap_dfr(by_size, function(reps, size_label) {
          
          future_imap_dfr(reps, function(sim_list, rep_index) {
            p()  # Tick the progress bar
            
            obs_list <- models$idv_trans[[size_label]][[rep_index]]
            
            imap_dfr(sim_list, function(sim_pid_list, pid) {
              pid_clean <- str_remove(pid, "^p_")
              common_waves <- intersect(names(sim_pid_list), names(obs_list[[pid]]))
              
              if (length(common_waves) == 0) return(tibble())
              
              tibble(
                parent_block = parent_block,
                sub_model    = sub_model,
                size_label   = size_label,
                rep          = rep_index,
                ID           = pid_clean,
                wave         = common_waves,
                sim_mat      = map(common_waves, ~ as.matrix(sim_pid_list[[.x]])),
                obs_mat      = map(common_waves, ~ as.matrix(obs_list[[pid]][[.x]]))
              )
            })
          }, .options = furrr_options(seed = TRUE))  # End future_imap_dfr reps
        })  # End by_size
      })  # End by_sub_model
    }, .options = furrr_options(seed = TRUE))  # End indiv_transitions
  })  # End with_progress
}
