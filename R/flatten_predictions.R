flatten_predictions <- function(probs, num_tasks = NULL) {
  
  if(is.null(num_tasks)) {
    num_tasks <- sum(map_int(
      probs, ~ sum(purrr::map_int(.x, ~ sum(lengths(.x))))
    ))
  }
  
  # Create progress bar
  pb      <- txtProgressBar(min = 0, max = num_tasks, style = 3)
  counter <- 0
  
  imap_dfr(predicted_probs, function(by_sub_block, parent) {
    imap_dfr(by_sub_block, function(by_size_label, sub_block) {
      imap_dfr(by_size_label, function(by_rep, size) {
        imap_dfr(by_rep, function(mat, rep_num) {
          
          # Update progress bar
          counter <<- counter + 1
          setTxtProgressBar(pb, counter)
          
          mat_names <- names(mat)
          
          tibble(
            ID = str_extract(string = mat_names, "(?<=ID_)\\d+"),
            wave = str_extract(mat_names, "\\d+-\\d+"),
            parent_block = parent,
            sub_block = sub_block,
            size_label = size,
            rep = as.character(rep_num),
            sim_mat = unname(mat))
        })
      })
    })
  })
}