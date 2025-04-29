# Computes multiple distance metrics between observed and estimated transition matrices.
# Compares both well-specified and misspecified models across specified sample sizes
# and repetitions. Handles input validation and provides informative error messages.
#
# Arguments:
#   - results: List containing three components:
#     * obs_trans: Observed transition matrices
#     * estimated_transitions_good: Well-specified model estimates  
#     * estimated_transitions_bad: Misspecified model estimates
#   - sample_size: Specific sample size(s) to analyze (NULL processes all available)
#   - rep: Specific repetition(s) to analyze (NULL processes all available)
#   - epsilon: Small constant to avoid division by zero in KL divergence
#   - type: Placeholder for future visualization type specification
#
# Returns:
#   - Tidy dataframe with seven distance metrics for each model type, including:
#     * Frobenius, Manhattan, and Maximum distances
#     * Mean absolute difference and RMSE  
#     * Correlation distance and KL divergence
#     * Properly formatted factor variables for visualization
#
# Validation Steps:
#   1. Checks for required input components
#   2. Identifies available sample sizes
#   3. Validates requested sample sizes
#   4. Validates requested repetitions
#
# Error Handling:
#   - Provides specific warnings for missing data
#   - Skips problematic cases while continuing processing
#   - Returns meaningful error if no metrics calculated
# 
calculate_matrix_distances <- function(results, sample_size = NULL, 
                                       rep = NULL, epsilon = 1e-10, type = "bar") {
  
  # Validations ----------------------------------------------------------------
  
  # 1. Validate input structure
  
  if (!all(c("obs_trans", 
             "estimated_transitions_good", 
             "estimated_transitions_bad") %in% names(results))) {
    stop("Input results missing required components. Expected: obs_trans, estimated_transitions_good, estimated_transitions_bad")
  }
  
  # 2. Get available sample sizes
  available_sizes <- names(results$obs_trans)
  if (is.null(available_sizes)) stop("No sample sizes found in results")
  
  # 3. Set sample sizes to analyze
  if (is.null(sample_size)) {
    sample_sizes <- available_sizes
    message("Analyzing all sample sizes: ", paste(available_sizes, collapse = ", "))
  } else {
    if (!any(sample_size %in% available_sizes)) {
      stop("Requested sample_size not found. Available: ", paste(available_sizes, collapse = ", "))
    }
    sample_sizes <- sample_size
  }
  
  # 4. Set repetitions to analyze
  if (is.null(rep)) {
    reps <- seq_along(results$obs_trans[[1]])
    message("Analyzing all repetitions: ", length(reps))
  } else {
    max_rep <- length(results$obs_trans[[1]])
    if (any(rep > max_rep)) {
      invalid <- rep[rep > max_rep]
      stop("Requested rep(s) ", paste(invalid, collapse = ", "), 
           " exceed available repetitions (", n_reps, ") for ", size)
    }
    reps <- rep
  }
  
  # Initialize results storage
  metrics_list <- list()
  
  # User message
  message("Calculating distances...")
  
  for(size in sample_size) {
    for(r in rep) {
      tryCatch({
        # Get observed and estimated (good and bad) matrices
        p <- results$obs_trans[[size]][[r]]
        good <- results$estimated_transitions_good[[size]][[r]]
        bad <- results$estimated_transitions_bad[[size]][[r]]
      
        # Verify matrices
        if (is.null(p) || is.null(good) || is.null(bad)) {
          warning("Missing matrix for ", size, " rep ", r)
          next
        }
      
        # Calculate distance based metrics
        for(model in c("good", "bad")) {
          p_hat <- if(model == "good") good else bad
        
          distances <- tibble::tibble(
            sample_size = size,
            repitition = r,
            model_type = model,
            metric = c("Frobenius", "Manhattan", "Max", "MeanAbs", "RMSE", "Correlation", "KL"),
            value = c(
              norm(p - p_hat, type = "F"),
              sum(abs(p - p_hat)),
              max(abs(p - p_hat)),
              mean(abs(p - p_hat)),
              sqrt(mean((p - p_hat)^2)),
              1 - cor(c(p), c(p_hat)),
              sum((p + epsilon) * log((p + epsilon) / (p_hat + epsilon)))
            ))
        
          metrics_list[[paste(size, r, model)]] <- distances
        } # End of for(model in c("good", "bad"))
      }, error = function(e) {
        message("Error processing ", size, " rep ", r, ": ", e$message)
        })
    } # End of for(r in rep) 
  } # End of for(size in sample_size)

  # Check if we got any results
  if (length(metrics_list) == 0) {
    stop("No distance metrics calculated. Check your input data structure.")
  }
  
  # Combine all metrics
    metrics_df <- dplyr::bind_rows(metrics_list) |>
      dplyr::mutate(
        metric = dplyr::case_when(
          metric == "Frobenius" ~ "Frobenius Distance",
          metric == "Manhattan" ~ "Manhattan Distance",
          metric == "Max" ~ "Max Difference",
          metric == "MeanAbs" ~ "Mean Absolute Difference",
          metric == "RMSE" ~ "Root Mean Square Error",
          metric == "Correlation" ~ "Correlation Distance",
          metric == "KL" ~ "Kullback-Leibler Divergence"
        ),
        model_type = ifelse(model_type == "good", "Well-Specified Model", "Misspecified Model")
      )
    
    metrics_df <- metrics_df |>
      dplyr::mutate(
        sample_size = factor(sample_size, levels = sample_sizes),
        model_type = factor(model_type, levels = c("Well-Specified Model", "Misspecified Model")),
        metric = factor(metric, levels = c(
          "Manhattan Distance", 
          "Frobenius Distance", 
          "Max Difference",
          "Mean Absolute Difference",
          "Root Mean Square Error",
          "Correlation Distance",
          "Kullback-Leibler Divergence"))
      )
  
  return(metrics_df)
}
