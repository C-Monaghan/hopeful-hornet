process_structure_2 <- function(resim) {
  # Flatten the nested keys into a list of all combinations
  all_combos <- list()
  
  for (top_name in names(resim)) {
    top_level <- resim[[top_name]]
    for (cat_name in names(top_level)) {
      if(cat_name != "true_models") next
      category <- top_level[[cat_name]]
      for (model_name in names(category)) {
        all_combos[[length(all_combos) + 1]] <- list(
          top = top_name,
          cat = cat_name,
          model = model_name
        )
      }
    }
  }
  
  # Set up progress bar
  pb <- txtProgressBar(min = 0, max = length(all_combos), style = 3)
  i <- 0  # Progress counter
  
  # Loop through all combinations
  results <- map_dfr(all_combos, function(combo) {
    i <<- i + 1
    setTxtProgressBar(pb, i)
    
    datasets <- resim[[combo$top]][[combo$cat]][[combo$model]]
    
    results <- map(datasets, function(data) {
      tryCatch({
        if (!is.data.frame(data)) stop("Invalid data format")
        process_dataset(data)
      }, error = function(e) {
        message("Error in ", combo$model, ": ", conditionMessage(e))
        NA_real_
      })
    })
    
    tibble(
      top_level = combo$top,
      model_category = combo$cat,
      model_type = combo$model,
      pct_outside = unlist(results)
    )
  })
  
  close(pb)
  return(results)
}
