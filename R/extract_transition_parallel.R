extract_transition_parallel <- function(indiv_transitions, obs_trans) {
  # Estimate total number of reps to use as progress steps
  num_reps <- sum(map_int(
    indiv_transitions, ~ sum(map_int(.x, ~ sum(map_int(.x, length))))
  ))
  
  # Read in obs_trans
  # obs_trans <- tryCatch({
  #   readRDS(file.path(obs_path, "obs_trans.rds"))
  # }, error = function(e) {
  #   stop("Failed to load obs_trans: ", e$message)
  # })
  
  with_progress({
    p <- progressor(steps = num_reps)
    
    future_imap_dfr(indiv_transitions, function(by_sub_model, parent_block) {
      imap_dfr(by_sub_model, function(by_size, sub_model) {
        imap_dfr(by_size, function(reps, size_label) {
          
          future_imap_dfr(reps, function(sim_list, rep_index) {
            p()  # Tick progress
            
            result <- tryCatch({
              obs_list <- obs_trans[[size_label]][[rep_index]]
              
              imap_dfr(sim_list, function(sim_pid_list, pid) {
                tryCatch({
                  pid_clean <- str_remove(pid, "^p_")
                  obs_pid <- obs_list[[pid]]
                  if (is.null(obs_pid)) return(tibble())
                  
                  common_waves <- intersect(names(sim_pid_list), names(obs_pid))
                  if (length(common_waves) == 0) return(tibble())
                  
                  # To reduce memory: just return dimensions
                  tibble(
                    parent_block = parent_block,
                    sub_model    = sub_model,
                    size_label   = size_label,
                    rep          = rep_index,
                    ID           = pid_clean,
                    wave         = common_waves,
                    sim_mat      = map(common_waves, ~ as.matrix(sim_pid_list[[.x]])),
                    obs_mat      = map(common_waves, ~ as.matrix(obs_pid[[.x]]))
                  )
                }, error = function(e) {
                  message(sprintf("Error processing PID %s: %s", pid, e$message))
                  return(tibble())
                })
              })
              
            }, error = function(e) {
              message(sprintf("Failed rep %s for size %s: %s", rep_index, size_label, e$message))
              return(tibble())
            })
            
            result
          }, .options = furrr_options(seed = TRUE, globals = FALSE))
          
        })
      })
    }, .options = furrr_options(seed = TRUE))
  })
}