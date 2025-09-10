process_structure_2 <- function(resim, pids) {
  # Flatten the nested keys into a list of all combinations
  all_combos <- list()
  
  for (top_name in names(resim)) {
    top_level <- resim[[top_name]]
    for (cat_name in names(top_level)) {
      if (cat_name != "true_models")
        next
      category <- top_level[[cat_name]]
      for (model_name in names(category)) {
        all_combos[[length(all_combos) + 1]] <- list(top = top_name,
                                                     cat = cat_name,
                                                     model = model_name)
      }
    }
  }
  
  # Set up progress bar
  pb <- txtProgressBar(min = 0,
                       max = length(all_combos),
                       style = 3)
  i <- 0
  
  # Results list
  all_results <- vector("list", length = length(all_combos))
  
  for (combo_idx in seq_along(all_combos)) {
    combo <- all_combos[[combo_idx]]
    i <- i + 1
    setTxtProgressBar(pb, i)
    
    # Loop through all combinations
    result <- tryCatch({
      datasets      <- resim[[combo$top]][[combo$cat]][[combo$model]]
      
      # For now (only running on 1 / 3 of data)
      subset_index  <- seq_len(ceiling(length(datasets) / 3))
      datasets      <- datasets[subset_index]
      
      results <- purrr::map(datasets, function(data) {
        data <- dplyr::semi_join(data, pids, by = c("ID", "size_label", "rep"))
        process_dataset(data = data)
      }) # End of mapping function
      
      tibble(
        top_level = combo$top,
        model_category = combo$cat,
        model_type = combo$model,
        pct_outside = unlist(results)
      )
    }, error = function(e) {
      message("Error in combo ", combo$model, ": ", conditionMessage(e))
      
      tibble(
        top_level = combo$top,
        model_category = combo$cat,
        model_type = combo$model,
        pct_outside = NA_real_
      )
    }) # End of tryCatch
    
    all_results[[combo_idx]] <- result
  } # End of for loop
  
  close(pb)
  
  return(dplyr::bind_rows(all_results))
}