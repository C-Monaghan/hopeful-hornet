extract_distance <- function(obj, data, obs_mat) {
  # 1. Get predicted probabilities
  pred_probs <- predict(obj, type = "probs")
  
  # 2. Split into a 3x3 estimated transition matrix
  split_rows <- split(
    seq_len(nrow(pred_probs)), 
    ceiling(seq_along(seq_len(nrow(pred_probs))) / 3))
    
    ids <- data |> pull(ID) |> unique()
  
    # Building names for matrices
    id_wave_names <- rep(ids, each = 2)
    wave_labels   <- rep(c("1-2", "2-3"), times = length(ids))
    matrix_names  <- paste0("ID_", id_wave_names, "_", wave_labels)
    
    # Setting names
    named_matrices <- setNames(
      lapply(split_rows, function(rows) {
        matrix(pred_probs[rows, ], nrow = 3, ncol = 3, byrow = FALSE)
      }),
      matrix_names
    )
    
    # For the tibble (only way I can think of doing this)
    rep_index  <- data |> pull(rep) |> unique()
    size_label <- data |> pull(size_label) |> unique()
    
    # 3. Converting to a named tibble
    transitions <- tibble(
      ID = str_extract(string = matrix_names, "(?<=ID_)\\d+"),
      wave = str_extract(matrix_names, "\\d+-\\d+"),
      rep = rep(rep_index, each = nrow(data) / 3),
      size_label = rep(size_label, each = nrow(data) / 3),
      sim_mat = unname(named_matrices)
    )
    
    transitions <- transitions |>
      left_join(obs_mat, by = c("ID", "wave", "rep", "size_label"))
    
    # 4. Setting up distances ------------------------------------------------------
    num_tasks <- nrow(transitions)
    
    # Preallocate result list
    results_list <- vector("list", length = num_tasks)
    
    # 5. Calculating matrix distances ----------------------------------------------
    for (i in seq_len(num_tasks)) {
      obs <- transitions$obs_mat[[i]]
      sim <- transitions$sim_mat[[i]]
      
      # Using C++ code
      results_list[[i]] <- tryCatch(
        # compare_matrices_rcpp(obs, sim),
        sum(abs(obs - sim)),
        error = function(e) {
          message(sprintf("Error in row %d: %s", i, e$message))
          NULL
        }
      )
    }
    
    # 6. Extract Manhatten distance
    # manhattan_dist <- data.table::rbindlist(
    #   results_list, fill = TRUE, idcol = "row_id") |> 
    #   tibble() |> 
    #   filter(metric == "Manhattan") |> 
    #   pull(value)
    # 
    
    manhattan_dist <- unlist(results_list)
  return(manhattan_dist)
}


# # Get observed categories as matrix
# obs        <- model.response(model.frame(obj))
# obs_matrix <- model.matrix(~ obs - 1)
# 
# # Calculate Pearson residuals: (
# # - Observed - Predicted) / sqrt(Predicted * (1 - Predicted))
# residuals <- (obs_matrix - pred_probs) / sqrt(pred_probs * (1 - pred_probs))
# 
# # Manhattan distance = sum of absolute residuals per observation
# manhattan_dist <- rowSums(abs(residuals))
