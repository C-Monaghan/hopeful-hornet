flatten_obs_transitions <- function(obs_trans) {
  out <- list()
  i <- 1
  
  size_labels <- names(obs_trans)
  
  # Count total iterations for the progress bar
  total <- sum(
    sapply(obs_trans, function(reps_list) {
      sum(sapply(reps_list, function(persons) {
        sum(sapply(persons, function(p) length(p)))
      }))
    })
  )
  
  pb <- txtProgressBar(min = 0, max = total, style = 3)
  counter <- 0
  
  for (size_label in size_labels) {
    reps_list <- obs_trans[[size_label]]
    
    for (rep_index in seq_along(reps_list)) {
      persons <- reps_list[[rep_index]]
      
      for (pid in names(persons)) {
        waves <- persons[[pid]]
        
        for (wave_name in names(waves)) {
          out[[i]] <- list(
            ID = str_remove(pid, "^p_"),
            wave = str_replace(wave_name, "^w_", "") |> str_replace_all("_", "-"),
            rep = as.character(rep_index),
            size_label = size_label,
            obs_mat = list(waves[[wave_name]])
          )
          i <- i + 1
          
          # Update progress bar
          counter <- counter + 1
          setTxtProgressBar(pb, counter)
        }
      }
    }
  }
  
  return(rbindlist(out))
}