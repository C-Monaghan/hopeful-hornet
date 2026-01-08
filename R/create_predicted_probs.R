create_predicted_probs <- function(augmented_data, model_fits, num_tasks) {
  # Create progress bar
  if (!is.null(num_tasks)) {
    pb <- txtProgressBar(min = 0, max = num_tasks, style = 3)
    counter <- 0
  }

  predicted_probs <- imap(augmented_data, function(by_sub_blocks, parent) {
    imap(by_sub_blocks, function(by_sizes, sub_block) {
      imap(by_sizes, function(data_list, size_label) {
        map2(
          data_list,
          model_fits[[parent]][[sub_block]][[size_label]],
          function(data_augment, model) {
            # Update progress bar
            if (!is.null(num_tasks)) {
              counter <<- counter + 1
              setTxtProgressBar(pb, counter)
            }

            if (is.null(data_augment) || is.null(model)) {
              return(NULL)
            }

            require(nnet)

            ids <- data_augment |> pull(ID) |> unique()

            tryCatch(
              {
                probs <- predict(model, newdata = data_augment, type = "probs")
              },
              error = function(e) {
                message("Prediction error: ", e$message)
                return(NULL)
              }
            )

            tryCatch(
              {
                # Split into 3x3 matrices
                split_rows <- split(
                  seq_len(nrow(probs)),
                  ceiling(seq_along(seq_len(nrow(probs))) / 3)
                )

                # Building names for matrices
                id_wave_names <- rep(ids, each = 2)
                wave_labels <- rep(c("1-2", "2-3"), times = length(ids))
                matrix_names <- paste0("ID_", id_wave_names, "_", wave_labels)

                named_matrices <- setNames(
                  lapply(split_rows, function(rows) {
                    matrix(probs[rows, ], nrow = 3, ncol = 3, byrow = FALSE)
                  }),
                  matrix_names
                )
                named_matrices
              },
              error = function(e) {
                message("Splitting error: ", e$message)
                return(NULL)
              }
            ) # end of TryCatch
          }
        ) # End of map2
      }) # End of imap
    }) # End of imap
  }) # End of imap

  return(predicted_probs)
}
