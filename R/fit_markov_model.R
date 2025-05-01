# Fits and compares well-specified and misspecified Markov models across multiple sample sizes.
# Performs repeated model fitting with different training samples to assess model performance
# and stability. Creates a holdout test set for external validation.
#
# Arguments:
#   - data: Dataset containing transition data with:
#     * ID: Subject identifier
#     * y_prev: Previous state (factor)
#     * y: Current state (factor)
#     * x1-x5: Predictor variables
#   - sample_sizes: Vector of sample sizes to evaluate model performance
#   - n_reps: Number of repetitions per sample size
#   - parallel: Logical indicating if parallel processing should be used
#   - n_cores: Number of cores to use for parallel processing (default is all but one)
#   - seed: Optional random seed for reproducibility
#
# Returns:
#   - List containing three components:
#     1. good_fits: Well-specified models (using all predictors)
#     2. bad_fits: Misspecified models (using subset of predictors)
#     3. obs_trans: Observed transition matrices for each sample
#     4. test_data: Holdout dataset (20% of observations)
#
# Process:
#   1. Splits data into training (80%) and test (20%) sets
#   2. For each sample size:
#     a) Samples n individuals from training set
#     b) Calculates observed transition matrix
#     c) Fits both model specifications
#   3. Repeats process n_reps times per sample size
#
# Notes:
#   - Uses multinomial logistic regression via nnet::multinom
#   - Well-specified model includes all predictors (x1-x5 + y_prev)
#   - Misspecified model uses reduced predictor set (x1 + x3 + y_prev)
#   - Results organized by sample size for easy comparison

fit_markov_model <- function(data,
                             sample_sizes = c(100, 250, 500, 1000, 5000),
                             n_reps = 5,
                             parallel = FALSE,
                             n_cores = parallel::detectCores() - 1,
                             seed = NULL) {
  
  # For reproducibility --------------------------------------------------------
  if(!is.null(seed)) set.seed(seed)
  
  # Create a test set (20% of data) --------------------------------------------
  ids        <- unique(data$ID)
  test_id    <- sample(ids, size = round(0.2 * length(ids)))
  
  test_data  <- data[data$ID %in% test_id, ]
  train_data <- data[!data$ID %in% test_id, ]
  
  unique_train_ids <- unique(train_data$ID)
  
  # For storing end results ----------------------------------------------------
  results <- list(
    good_fits = vector(mode = "list", length = length(sample_sizes)),
    bad_fits  = vector(mode = "list", length = length(sample_sizes)),
    obs_trans = vector("list", length(sample_sizes)),
    test_data = test_data
  )
  
  # Naming lists for easy understanding ----------------------------------------
  names(results$good_fits) <- paste0("n_", sample_sizes)
  names(results$bad_fits)  <- paste0("n_", sample_sizes)
  names(results$obs_trans) <- paste0("n_", sample_sizes)
  
  # Setting up a worker for both parallel and sequential processing ------------
  fit_worker <- function(n) {
    replicate(n_reps, {
      sample_ids <- sample(unique_train_ids, size = n)
      sample_data <- train_data[train_data$ID %in% sample_ids, ]
      
      transitions <- table(From = sample_data$y_prev, To = sample_data$y)
      obs_matrix <- round(prop.table(transitions, margin = 1), 2)
      
      list(
        good_fit = nnet::multinom(y ~ x1 + x2 + x3 + x4 + x5 + y_prev, data = sample_data, trace = FALSE),
        bad_fit = nnet::multinom(y ~ x1 + x3 + y_prev, data = sample_data, trace = FALSE),
        obs_matrix = obs_matrix
      )
    }, simplify = FALSE)
  }
  
  # Parallel Execution
  if(parallel) {
    require(foreach)
    require(doSNOW)
    require(doRNG)
    
    cl <- parallel::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)
    
    # Progress bar
    pb      <- utils::txtProgressBar(max = length(sample_sizes) * n_reps, style = 3)
    counter <- 0
    opts    <- list(progress = function(n) {
      counter <<- counter + 1
      utils::setTxtProgressBar(pb, counter)
    })
    
    parallel::clusterExport(cl, varlist = c("train_data", "unique_train_ids"), envir = environment())
    
    # Setting seed for parallel processing
    if(!is.null(seed)) doRNG::registerDoRNG(seed)
      
    for(i in seq_along(sample_sizes)) {
      n <- sample_sizes[i]
      
      rep_results <- foreach::foreach(
        reps = 1:n_reps, .packages = "nnet", .options.snow = opts) %dorng% {
          
          sample_ids <- sample(unique_train_ids, size = n)
          sample_data <- train_data[train_data$ID %in% sample_ids, ]
          
          transitions <- table(From = sample_data$y_prev, To = sample_data$y)
          obs_matrix <- round(prop.table(transitions, margin = 1), 2)
          
          list(
            good_fit = nnet::multinom(y ~ x1 + x2 + x3 + x4 + x5 + y_prev, data = sample_data, trace = FALSE),
            bad_fit = nnet::multinom(y ~ x1 + x3 + y_prev, data = sample_data, trace = FALSE),
            obs_matrix = obs_matrix
          )
        } # End of %dopar%
      
      results$good_fits[[i]] <- lapply(rep_results, `[[`, "good_fit")
      results$bad_fits[[i]] <- lapply(rep_results, `[[`, "bad_fit")
      results$obs_trans[[i]] <- lapply(rep_results, `[[`, "obs_matrix")
  } # End of for(i in seq_along(sample_sizes))
    
    parallel::stopCluster(cl)
    
  } else {
    # Serial processing
    for (i in seq_along(sample_sizes)) {
      message("Running sample size ", sample_sizes[i], " (", i, "/", length(sample_sizes), ")")
      rep_results <- fit_worker(sample_sizes[i])
      results$good_fits[[i]] <- lapply(rep_results, `[[`, "good_fit")
      results$bad_fits[[i]] <- lapply(rep_results, `[[`, "bad_fit")
      results$obs_trans[[i]] <- lapply(rep_results, `[[`, "obs_matrix")
    } # End of for (i in seq_along(sample_sizes))
  }
  
  return(results)
}