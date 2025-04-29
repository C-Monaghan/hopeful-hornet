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
  
  # For reproducibility
  if(!is.null(seed)) set.seed(seed)
  
  # Create a test set (20% of data)
  test_id    <- sample(unique(data$ID), size = round(0.2 * length(unique(data$ID))))
  
  test_data  <- data[data$ID %in% test_id, ]
  train_data <- data[!data$ID %in% test_id, ]
  
  # For storing end results
  results <- list(
    good_fits = vector(mode = "list", length = length(sample_sizes)),
    bad_fits  = vector(mode = "list", length = length(sample_sizes)),
    obs_trans = vector("list", length(sample_sizes)),
    test_data = test_data
  )
  
  # Naming lists for easy understanding
  names(results$good_fits) <- paste0("n_", sample_sizes)
  names(results$bad_fits)  <- paste0("n_", sample_sizes)
  names(results$obs_trans) <- paste0("n_", sample_sizes)
  
  # Set up parallel processing if requested
  if(parallel == TRUE) {
    require(foreach)
    
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
  }
  
  if(parallel == TRUE) {
    # Loop over sample sizes
    for(i in seq_along(sample_sizes)) {
      n <- sample_sizes[i]
      
      # Process repetitions in parallel
      rep_results <- foreach(reps = 1:n_reps, .packages = "nnet") %dopar% {
        # Sample subset of training data
        sample_ids <- sample(unique(train_data$ID), size = n)
        sample_data <- train_data[train_data$ID %in% sample_ids, ]
        
        # Calculate observed transition matrix
        transitions <- table(From = sample_data$y_prev, To = sample_data$y)
        obs_matrix <- round(prop.table(transitions, margin = 1), 2)
        
        # Fit models
        good_fit <- nnet::multinom(
          y ~ x1 + x2 + x3 + x4 + x5 + y_prev,
          data = sample_data, trace = FALSE)
        
        bad_fit <- nnet::multinom(
          y ~ x1 + x3 + y_prev,
          data = sample_data, trace = FALSE)
        
        list(good_fit = good_fit, bad_fit = bad_fit, obs_matrix = obs_matrix)
      }
      
      # Extract results from parallel processing
      results$good_fits[[i]] <- lapply(rep_results, function(x) x$good_fit)
      results$bad_fits[[i]] <- lapply(rep_results, function(x) x$bad_fit)
      results$obs_trans[[i]] <- lapply(rep_results, function(x) x$obs_matrix)
    }
  } else {
  # Loop over sample sizes
  for(i in seq_along(sample_sizes)) {
    n         <- sample_sizes[i]
    good_fits <- list()
    bad_fits  <- list()
    obs_trans <- list()
    
    # Loop of number of repetitions
    for(reps in 1:n_reps) {
      
      message("Running model ", reps, " for sample size category ", i)
      
      # Sample subset of training data
      sample_ids  <- sample(unique(train_data$ID), size = n)
      sample_data <- train_data[train_data$ID %in% sample_ids, ]
      
      # Calculate an observed transition matrix for this sample
      transitions <- table(From = sample_data$y_prev, To = sample_data$y)
      obs_matrix <- round(x = prop.table(transitions, margin = 1), digits = 2)
      
      # A good markov model
      good_fit <- nnet::multinom(
        y ~ x1 + x2 + x3 + x4 + x5 + y_prev,
        data = sample_data, trace = FALSE)
      
      # A bad markov model
      bad_fit <- nnet::multinom(
        y ~ x1 + x3 + y_prev,
        data = sample_data, trace = FALSE)
      
      good_fits[[reps]] <- good_fit
      bad_fits[[reps]]  <- bad_fit
      obs_trans[[reps]] <- obs_matrix
    }
    
    results$good_fits[[i]] <- good_fits
    results$bad_fits[[i]]  <- bad_fits
    results$obs_trans[[i]] <- obs_trans
  }
  }
  
  return(results)
}
