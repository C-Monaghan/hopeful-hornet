create_augmented_data <- function(
    resimulation, 
    pids_df, 
    num_tasks = NULL) {
  
  # Pre-calculate number of tasks
  if(is.null(num_tasks)) {
    num_tasks <- sum(
      map_int(resimulation, ~sum(map_int(.x, ~sum(map_int(.x, ~length(.x)))))
      ))
  }
  
  # Create progress bar
  pb      <- txtProgressBar(min = 0, max = num_tasks, style = 3)
  counter <- 0
  
  # Augmenting data
  augmented_data <- imap(resimulation, function(by_sub_blocks, parent) {
      imap(by_sub_blocks, function(by_sizes, sub_block) {
        imap(by_sizes, function(by_data_list, size_label) {
          map(by_data_list, function(df) {
            
            # Update progress bar
            counter <<- counter + 1
            setTxtProgressBar(pb, counter)
            
            # NULL check and empty df check
            if(is.null(df) || nrow(df) == 0) return(NULL)
            
            # Efficient data augmentation
            tryCatch({
              df_filtered <- semi_join(
                df, pids_df, by = c("ID", "size_label", "rep"), copy = FALSE)
              
              if(nrow(df_filtered) == 0) return(NULL)
              
              # Vectorized augmentation
              augmented <- bind_rows(
                mutate(df_filtered, y_prev = factor(1)),
                mutate(df_filtered, y_prev = factor(2)),
                mutate(df_filtered, y_prev = factor(3)))
                
                arrange(augmented, ID, w)
            }, error = function(e) {
              message("Error: ", e$message)
              return(NULL)
            })
          })
        })
      })
    })
  
  return(augmented_data)
}