process_structure <- function(resim) {
  map_dfr(names(resim), function(top_name) {
    top_level <- resim[[top_name]]
    map_dfr(names(top_level), function(cat_name) {
      category <- top_level[[cat_name]]
      map_dfr(names(category), function(model_name) {
        datasets <- category[[model_name]]
        
        results <- map(datasets, function(data) {
          tryCatch({
            if (!is.data.frame(data)) stop("Invalid data format")
            process_dataset(data)
          }, error = function(e) {
            message("Error in ", model_name, ": ", conditionMessage(e))
            NA_real_
          })
        })
        
        # successful <- results[!is.na(results)]
        
        tibble(
          top_level = top_name,
          model_category = cat_name,
          model_type = model_name,
          pct_outside = unlist(results)
          # pct_outside = if (length(successful)) mean(unlist(successful)) else NA_real_,
          # success_rate = length(successful)/length(datasets),
          # n_datasets = length(datasets)
        )
      })
    })
  })
}
