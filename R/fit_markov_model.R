# Fit and Compare Markov Models with Varying Specifications and Sample Sizes
# 
# Performs repeated model fitting across different sample sizes to assess model performance,
# stability, and specification sensitivity. Creates a holdout test set for external validation.
# Supports three different modeling approaches (Base, Additive, Multiplicative) with varying
# complexity levels from null to overfitted models
#
# Arguments:
#   - data: Dataset containing transition data with:
#     * ID: Subject identifier
#     * y_prev: Previous state (factor)
#     * y: Current state (factor)
#     * x1-x3: Predictor variables
#   - sample_sizes: Vector of sample sizes to evaluate model performance
#   - n_reps: Number of repetitions per sample size
#   - method: Character specifying modeling approach
#     1. "Base": Model without Markov dependency
#     2. "Additive": Model with additive previous state effect
#     3. "Multiplicative": Model with interaction between predictors and previous state
#   - parallel: Logical indicating if parallel processing should be used
#   - n_cores: Number of cores to use for parallel processing (default is all but one)
#   - seed: Optional random seed for reproducibility
#
# Returns:
#   - List containing three components:
#     1. null_models: List of null models (intercept-only or y_prev-only) by
#     2. red_1_models: List of reduced models (x1 only) by sample size
#     3. red_2_models: List of reduced models (x1+x2) by sample size
#     4. true_models: List of correctly specified models (x1+x2+x3) by sample size
#     5. of_models: List of overfitted models (all predictors) by sample size
#     6. obs_trans: List of observed transition matrices by sample size
#     7. test_data: Holdout dataset (20% of observations)
#
# Process:
# The function implements a comprehensive model evaluation workflow:
#   1. Splits data into training (80%) and test (20%) sets
#   2. For each sample size:
#     a) Samples n individuals from training set
#     b) Calculates observed transition matrix
#     c) Fits five model specifications of increasing complexity
#   3. Repeats process n_reps times per sample size
#   4. Supports parallel execution for computationally intensive scenarios
#

fit_markov_model <- function(
    data,
    sample_sizes = c(100, 250, 1000),
    n_reps = 200,
    method = c("Base", "Additive", "Multiplicative"),
    parallel = FALSE,
    n_cores = parallel::detectCores() - 1,
    seed = NULL) {
  
  # Input validation -----------------------------------------------------------
  method <- match.arg(method)
  stopifnot(
    is.data.frame(data),
    all(c("ID", "y_prev", "y") %in% names(data)),
    is.numeric(sample_sizes),
    is.numeric(n_reps) && n_reps > 0,
    is.logical(parallel),
    is.numeric(n_cores) && n_cores > 0
  )
  
  # Initialize random seed if provided -----------------------------------------
  if(!is.null(seed)) set.seed(seed); message("Random seed set to: ", seed)
  
  # Data partitioning ----------------------------------------------------------
  # Split data into training (80%) and test (20%) sets while keeping all 
  # observations for each subject together
  ids        <- unique(data$ID)
  test_id    <- sample(ids, size = round(0.2 * length(ids)))
  
  test_data  <- data[data$ID %in% test_id, ]
  train_data <- data[!data$ID %in% test_id, ]
  
  unique_train_ids <- unique(train_data$ID)
  
  # Results structure initialization -------------------------------------------
  # Pre-allocate lists for storing results with meaningful names
  results <- list(
    null_models  = vector(mode = "list", length = length(sample_sizes)),
    red_1_models = vector(mode = "list", length = length(sample_sizes)),
    red_2_models = vector(mode = "list", length = length(sample_sizes)),
    true_models  = vector(mode = "list", length = length(sample_sizes)),
    of_models    = vector(mode = "list", length = length(sample_sizes)),
    obs_trans    = vector("list", length(sample_sizes)),
    test_data    = test_data,
    meta_data    = list(
      sample_sizes = sample_sizes,
      n_reps       = n_reps,
      method       = method,
      train_size   = length(unique_train_ids),
      test_size    = length(test_id)
    )
  )
  
  # Naming lists for easy understanding
  size_names                  <- paste0("n_", sample_sizes)
  
  names(results$null_models)  <- size_names
  names(results$red_1_models) <- size_names
  names(results$red_2_models) <- size_names
  names(results$true_models)  <- size_names
  names(results$of_models)    <- size_names
  
  names(results$obs_trans)   <- paste0("n_", sample_sizes)
  
  # Model fitting worker function ----------------------------------------------
  # Centralized function to handle model fitting for both parallel and 
  # serial execution
  fit_worker <- function(n, method) {
    replicate(n_reps, {
      # Sample subjects (not individual observations) to maintain data structure
      sample_ids <- sample(unique_train_ids, size = n)
      sample_data <- train_data[train_data$ID %in% sample_ids, ]
      
      # Calculate empirical transition probabilities
      transitions <- table(From = sample_data$y_prev, To = sample_data$y)
      obs_matrix <- round(prop.table(transitions, margin = 1), 2)
      
      # Fit models based on specified method
      if(method == "Base") { # Scenario 1
      list(
        null_model  = nnet::multinom(y ~ 1, data = sample_data, trace = FALSE),
        red_1_model = nnet::multinom(y ~ x1, data = sample_data, trace = FALSE,),
        red_2_model = nnet::multinom(y ~ x1 + x2, data = sample_data, trace = FALSE,),
        true_model  = nnet::multinom(y ~ x1 + x2 + x3, data = sample_data, trace = FALSE,),
        of_model    = nnet::multinom(y ~ x1 + x2 + x3 + x4 + x5, data = sample_data, trace = FALSE,),
        obs_matrix  = obs_matrix
      )} else if(method == "Additive") { # Scenario 2
        list(
          null_model  = nnet::multinom(y ~ y_prev, data = sample_data, trace = FALSE),
          red_1_model = nnet::multinom(y ~ x1 + y_prev, data = sample_data, trace = FALSE,),
          red_2_model = nnet::multinom(y ~ x1 + x2 + y_prev, data = sample_data, trace = FALSE,),
          true_model  = nnet::multinom(y ~ x1 + x2 + x3 + y_prev, data = sample_data, trace = FALSE,),
          of_model    = nnet::multinom(y ~ x1 + x2 + x3 + x4 + x5 + y_prev, data = sample_data, trace = FALSE,),
          obs_matrix  = obs_matrix
        )
      } else if(method == "Multiplicative") { # Scenario 3
        list(
          null_model  = nnet::multinom(y ~ y_prev, data = sample_data, trace = FALSE),
          red_1_model = nnet::multinom(y ~ (x1 * y_prev), data = sample_data, trace = FALSE,),
          red_2_model = nnet::multinom(y ~ (x1 + x2) * y_prev, data = sample_data, trace = FALSE,),
          true_model  = nnet::multinom(y ~ (x1 + x2 + x3) * y_prev, data = sample_data, trace = FALSE,),
          of_model    = nnet::multinom(y ~ (x1 + x2 + x3 + x4 + x5) * y_prev, data = sample_data, trace = FALSE,),
          obs_matrix  = obs_matrix
        )
      }
    }, simplify = FALSE)
  }
  
  # Parallel execution setup ---------------------------------------------------
  if(parallel) {
    require(foreach)
    require(doSNOW)
    require(doRNG)
    
    message("Initializing parallel processing with ", n_cores, " cores")
    
    # Setting up parallel backend
    cl <- parallel::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)
    
    # Setting up progress bar
    pb      <- utils::txtProgressBar(max = length(sample_sizes) * n_reps, style = 3)
    counter <- 0
    opts    <- list(progress = function(n) {
      counter <<- counter + 1
      utils::setTxtProgressBar(pb, counter)
    })
    
    # Export data to clusters
    parallel::clusterExport(
      cl, 
      varlist = c("train_data", "unique_train_ids"), 
      envir = environment())
    
    # Set RNG seed for reproducible parallel execution
    if(!is.null(seed)) doRNG::registerDoRNG(seed)
      
    # Process each sample size
    for(i in seq_along(sample_sizes)) {
      n <- sample_sizes[i]
      
      rep_results <- foreach::foreach(
        reps = 1:n_reps, 
        .packages = "nnet", 
        .options.snow = opts
        ) %dorng% {
          
          # Sample subjects (not individual observations) to maintain data structure
          sample_ids <- sample(unique_train_ids, size = n)
          sample_data <- train_data[train_data$ID %in% sample_ids, ]
          
          # Calculate empirical transition probabilities
          transitions <- table(From = sample_data$y_prev, To = sample_data$y)
          obs_matrix <- round(prop.table(transitions, margin = 1), 2)
          
          # Model fitting (same as in fit_worker)
          if(method == "Base") {
            list(
              null_model  = nnet::multinom(y ~ 1, data = sample_data, trace = FALSE),
              red_1_model = nnet::multinom(y ~ x1, data = sample_data, trace = FALSE,),
              red_2_model = nnet::multinom(y ~ x1 + x2, data = sample_data, trace = FALSE,),
              true_model  = nnet::multinom(y ~ x1 + x2 + x3, data = sample_data, trace = FALSE,),
              of_model    = nnet::multinom(y ~ x1 + x2 + x3 + x4 + x5, data = sample_data, trace = FALSE,),
              obs_matrix  = obs_matrix
            )} else if(method == "Additive") {
              list(
                null_model  = nnet::multinom(y ~ y_prev, data = sample_data, trace = FALSE),
                red_1_model = nnet::multinom(y ~ x1 + y_prev, data = sample_data, trace = FALSE,),
                red_2_model = nnet::multinom(y ~ x1 + x2 + y_prev, data = sample_data, trace = FALSE,),
                true_model  = nnet::multinom(y ~ x1 + x2 + x3 + y_prev, data = sample_data, trace = FALSE,),
                of_model    = nnet::multinom(y ~ x1 + x2 + x3 + x4 + x5 + y_prev, data = sample_data, trace = FALSE,),
                obs_matrix  = obs_matrix
              )
            } else if(method == "Multiplicative") {
              list(
                null_model  = nnet::multinom(y ~ y_prev, data = sample_data, trace = FALSE),
                red_1_model = nnet::multinom(y ~ (x1 * y_prev), data = sample_data, trace = FALSE,),
                red_2_model = nnet::multinom(y ~ (x1 + x2) * y_prev, data = sample_data, trace = FALSE,),
                true_model  = nnet::multinom(y ~ (x1 + x2 + x3) * y_prev, data = sample_data, trace = FALSE,),
                of_model    = nnet::multinom(y ~ (x1 + x2 + x3 + x4 + x5) * y_prev, data = sample_data, trace = FALSE,),
                obs_matrix  = obs_matrix
              )
            }
        } # End of %dopar%
      
      # Storing results
      results$null_models[[i]]  <- lapply(rep_results, `[[`, "null_model")
      results$red_1_models[[i]] <- lapply(rep_results, `[[`, "red_1_model")
      results$red_2_models[[i]] <- lapply(rep_results, `[[`, "red_2_model")
      results$true_models[[i]]  <- lapply(rep_results, `[[`, "true_model")
      results$of_models[[i]]    <- lapply(rep_results, `[[`, "of_model")
      
      results$obs_trans[[i]]    <- lapply(rep_results, `[[`, "obs_matrix")
  } # End of for(i in seq_along(sample_sizes))
    
    parallel::stopCluster(cl)
    
  } else {
    # Serial execution ---------------------------------------------------------
    message("Running in serial mode ... ")
    
    for (i in seq_along(sample_sizes)) {
      
      message("Processing sample size ", sample_sizes[i], " (", i, "/", length(sample_sizes), ")")
      
      rep_results <- fit_worker(sample_sizes[i], method = method)
      
      results$null_models[[i]]  <- lapply(rep_results, `[[`, "null_model")
      results$red_1_models[[i]] <- lapply(rep_results, `[[`, "red_1_model")
      results$red_2_models[[i]] <- lapply(rep_results, `[[`, "red_2_model")
      results$true_models[[i]]  <- lapply(rep_results, `[[`, "true_model")
      results$of_models[[i]]    <- lapply(rep_results, `[[`, "of_model")
      
      results$obs_trans[[i]]    <- lapply(rep_results, `[[`, "obs_matrix")
    } # End of for (i in seq_along(sample_sizes))
  }
  
  # Add execution metadata to results
  # Add execution metadata to results
  results$metadata$completion_time <- Sys.time()
  results$metadata$seed_used <- seed
  
  message("\nModel fitting completed successfully")
  message("Training samples used: ", length(unique_train_ids))
  message("Test samples held out: ", length(test_id))
  
  return(results)
}