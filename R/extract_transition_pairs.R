extract_transition_pairs <- function(indiv_transitions, obs_trans) {
  
  # Estimate total number of reps to use as progress steps
  num_reps <- sum(map_int(
    indiv_transitions, ~ sum(map_int(.x, ~ sum(map_int(.x, length))))
  ))
  
  # Initialize utils-based progress bar
  pb <- txtProgressBar(min = 0, max = num_reps, style = 3)
  pb_tick <- 0
  
  result <- imap_dfr(indiv_transitions, function(by_sub_model, parent_block) {
    imap_dfr(by_sub_model, function(by_size, sub_model) {
      imap_dfr(by_size, function(reps, size_label) {
        
        imap_dfr(reps, function(sim_list, rep_index) {
          # Tick progress bar
          pb_tick <<- pb_tick + 1
          setTxtProgressBar(pb, pb_tick)
          
          obs_list <- obs_trans[[size_label]][[rep_index]]
          
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
        })  # reps
      })  # size
    })  # model
  })  # indiv_trans
  
  close(pb)
  return(result)
}
