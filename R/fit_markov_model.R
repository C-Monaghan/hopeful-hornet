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
#     c) Fits fifteen model specifications of increasing complexity
#   3. Repeats process n_reps times per sample size
#   4. Supports parallel execution for computationally intensive scenarios
#

fit_markov_model <- function(
  data,
  sample_sizes = c(100, 250, 1000),
  n_reps = 200,
  parallel = FALSE,
  n_cores = parallel::detectCores() - 1,
  seed = NULL
) {
  # Loading function for parallel workers to access
  source(here::here("R/create_individual_transition_matrices.R"))

  # Input validation -----------------------------------------------------------
  stopifnot(
    is.data.frame(data),
    all(c("ID", "y_prev", "y") %in% names(data)),
    is.numeric(sample_sizes),
    is.numeric(n_reps) && n_reps > 0,
    is.logical(parallel),
    is.numeric(n_cores) && n_cores > 0
  )

  # Initialize random seed if provided -----------------------------------------
  if (!is.null(seed)) {
    set.seed(seed)
  }
  message("Random seed set to: ", seed)

  # Data partitioning ----------------------------------------------------------
  # Split data into training (80%) and test (20%) sets while keeping all
  # observations for each subject together
  ids <- unique(data$ID)
  # test_id    <- sample(ids, size = round(0.2 * length(ids)))
  #
  # test_data  <- data[data$ID %in% test_id, ]
  # train_data <- data[!data$ID %in% test_id, ]
  #
  # unique_train_ids <- unique(train_data$ID)

  # Pre-allocate lists for storing results with meaningful names ---------------
  results <- list(
    base_models = structure(
      list(
        null_models = vector("list", length(sample_sizes)),
        red_1_models = vector("list", length(sample_sizes)),
        red_2_models = vector("list", length(sample_sizes)),
        true_models = vector("list", length(sample_sizes)),
        of_models = vector("list", length(sample_sizes))
      ),
      .Names = c(
        "null_models",
        "red_1_models",
        "red_2_models",
        "true_models",
        "of_models"
      )
    ),

    additive_models = structure(
      list(
        null_models = vector("list", length(sample_sizes)),
        red_1_models = vector("list", length(sample_sizes)),
        red_2_models = vector("list", length(sample_sizes)),
        true_models = vector("list", length(sample_sizes)),
        of_models = vector("list", length(sample_sizes))
      ),
      .Names = c(
        "null_models",
        "red_1_models",
        "red_2_models",
        "true_models",
        "of_models"
      )
    ),

    multiplicative_models = structure(
      list(
        null_models = vector("list", length(sample_sizes)),
        red_1_models = vector("list", length(sample_sizes)),
        red_2_models = vector("list", length(sample_sizes)),
        true_models = vector("list", length(sample_sizes)),
        of_models = vector("list", length(sample_sizes))
      ),
      .Names = c(
        "null_models",
        "red_1_models",
        "red_2_models",
        "true_models",
        "of_models"
      )
    ),

    obs_trans = structure(
      vector("list", length(sample_sizes)),
      .Names = paste0("n_", sample_sizes)
    ),
    idv_trans = structure(
      vector("list", length(sample_sizes)),
      .Names = paste0("n_", sample_sizes)
    ),
    sample_data = structure(
      vector("list", length(sample_sizes)),
      .Names = paste0("n_", sample_sizes)
    ),
    meta_data = list(
      sample_sizes = sample_sizes,
      n_reps = n_reps
    )
  )

  # Name the top-level components
  names(results) <- c(
    "base_models",
    "additive_models",
    "multiplicative_models",
    "obs_trans",
    "idv_trans",
    "sample_data",
    "meta_data"
  )

  # Name the sample size elements for each model type
  size_names <- paste0("n_", sample_sizes)

  for (model_type in c(
    "base_models",
    "additive_models",
    "multiplicative_models"
  )) {
    for (model_complexity in names(results[[model_type]])) {
      names(results[[model_type]][[model_complexity]]) <- size_names
    }
  }

  # names(results$obs_trans)   <- size_names

  # Model fitting worker function ----------------------------------------------
  # Centralized function to handle model fitting for both parallel and
  # serial execution
  fit_worker <- function(n) {
    replicate(
      n_reps,
      {
        # Sample subjects (not individual observations) to maintain data structure
        sample_ids <- sample(ids, size = n)
        sample_data <- data[data$ID %in% sample_ids, ]

        # Calculate empirical transition probabilities
        transitions <- table(From = sample_data$y_prev, To = sample_data$y)
        obs_matrix <- round(prop.table(transitions, margin = 1), 2)

        # Fitting all 15 models
        list(
          # Base models (no Markov dependency)
          base_null = nnet::multinom(y ~ 1, data = sample_data, trace = FALSE),
          base_red1 = nnet::multinom(y ~ x1, data = sample_data, trace = FALSE),
          base_red2 = nnet::multinom(
            y ~ x1 + x2,
            data = sample_data,
            trace = FALSE
          ),
          base_true = nnet::multinom(
            y ~ x1 + x2 + x3,
            data = sample_data,
            trace = FALSE
          ),
          base_of = nnet::multinom(
            y ~ x1 + x2 + x3 + x4 + x5,
            data = sample_data,
            trace = FALSE
          ),

          # Additive models (with y_prev column)
          add_null = nnet::multinom(
            y ~ y_prev,
            data = sample_data,
            trace = FALSE
          ),
          add_red1 = nnet::multinom(
            y ~ x1 + y_prev,
            data = sample_data,
            trace = FALSE
          ),
          add_red2 = nnet::multinom(
            y ~ x1 + x2 + y_prev,
            data = sample_data,
            trace = FALSE
          ),
          add_true = nnet::multinom(
            y ~ x1 + x2 + x3 + y_prev,
            data = sample_data,
            trace = FALSE
          ),
          add_of = nnet::multinom(
            y ~ x1 + x2 + x3 + x4 + x5 + y_prev,
            data = sample_data,
            trace = FALSE
          ),

          # Multiplicative models (with interactions)
          mult_null = nnet::multinom(
            y ~ y_prev,
            data = sample_data,
            trace = FALSE
          ),
          mult_red1 = nnet::multinom(
            y ~ (x1 * y_prev),
            data = sample_data,
            trace = FALSE
          ),
          mult_red2 = nnet::multinom(
            y ~ (x1 + x2) * y_prev,
            data = sample_data,
            trace = FALSE
          ),
          mult_true = nnet::multinom(
            y ~ (x1 + x2 + x3) * y_prev,
            data = sample_data,
            trace = FALSE
          ),
          mult_of = nnet::multinom(
            y ~ (x1 + x2 + x3 + x4 + x5) * y_prev,
            data = sample_data,
            trace = FALSE
          ),

          # Observed transition matrix
          obs_matrix = obs_matrix,

          # Individual transitions
          idv_trans = create_individual_transition_matrices(sample_data),

          # Sample data
          sample_data = sample_data
        )
      },
      simplify = FALSE
    )
  }

  # Parallel execution setup ---------------------------------------------------
  if (parallel) {
    require(foreach)
    require(doSNOW)
    require(doRNG)

    message("Initializing parallel processing with ", n_cores, " cores")

    # Setting up parallel backend
    cl <- parallel::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)

    # Setting up progress bar
    pb <- utils::txtProgressBar(max = length(sample_sizes) * n_reps, style = 3)
    counter <- 0
    opts <- list(progress = function(n) {
      counter <<- counter + 1
      utils::setTxtProgressBar(pb, counter)
    })

    # Export data to clusters
    parallel::clusterExport(
      cl,
      varlist = c("data", "ids", "create_individual_transition_matrices"),
      envir = environment()
    )

    # Set RNG seed for reproducible parallel execution
    if (!is.null(seed)) {
      doRNG::registerDoRNG(seed)
    }

    # Process each sample size
    for (i in seq_along(sample_sizes)) {
      n <- sample_sizes[i]

      rep_results <- foreach::foreach(
        reps = 1:n_reps,
        .packages = "nnet",
        .options.snow = opts
      ) %dorng%
        {
          # Sample subjects (not individual observations) to maintain data structure
          sample_ids <- sample(ids, size = n)
          sample_data <- data[data$ID %in% sample_ids, ]

          # Calculate empirical transition probabilities
          transitions <- table(From = sample_data$y_prev, To = sample_data$y)
          obs_matrix <- round(prop.table(transitions, margin = 1), 2)

          # Create individual transition matrices for each subject
          # idv_trans   = create_individual_transition_matrices(sample_data)

          # Fit all models
          list(
            # Base models (no Markov dependency)
            base_null = nnet::multinom(
              y ~ 1,
              data = sample_data,
              trace = FALSE
            ),
            base_red1 = nnet::multinom(
              y ~ x1,
              data = sample_data,
              trace = FALSE
            ),
            base_red2 = nnet::multinom(
              y ~ x1 + x2,
              data = sample_data,
              trace = FALSE
            ),
            base_true = nnet::multinom(
              y ~ x1 + x2 + x3,
              data = sample_data,
              trace = FALSE
            ),
            base_of = nnet::multinom(
              y ~ x1 + x2 + x3 + x4 + x5,
              data = sample_data,
              trace = FALSE
            ),

            # Additive models (with y_prev)
            add_null = nnet::multinom(
              y ~ y_prev,
              data = sample_data,
              trace = FALSE
            ),
            add_red1 = nnet::multinom(
              y ~ x1 + y_prev,
              data = sample_data,
              trace = FALSE
            ),
            add_red2 = nnet::multinom(
              y ~ x1 + x2 + y_prev,
              data = sample_data,
              trace = FALSE
            ),
            add_true = nnet::multinom(
              y ~ x1 + x2 + x3 + y_prev,
              data = sample_data,
              trace = FALSE
            ),
            add_of = nnet::multinom(
              y ~ x1 + x2 + x3 + x4 + x5 + y_prev,
              data = sample_data,
              trace = FALSE
            ),

            # Multiplicative models (with interactions)
            mult_null = nnet::multinom(
              y ~ y_prev,
              data = sample_data,
              trace = FALSE
            ),
            mult_red1 = nnet::multinom(
              y ~ (x1 * y_prev),
              data = sample_data,
              trace = FALSE
            ),
            mult_red2 = nnet::multinom(
              y ~ (x1 + x2) * y_prev,
              data = sample_data,
              trace = FALSE
            ),
            mult_true = nnet::multinom(
              y ~ (x1 + x2 + x3) * y_prev,
              data = sample_data,
              trace = FALSE
            ),
            mult_of = nnet::multinom(
              y ~ (x1 + x2 + x3 + x4 + x5) * y_prev,
              data = sample_data,
              trace = FALSE
            ),

            obs_matrix = obs_matrix,
            idv_trans = create_individual_transition_matrices(sample_data),

            sample_data = sample_data
          )
        } # End of %dopar%

      # Store results in organized structure
      results$base_models$null_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "base_null"
      )
      results$base_models$red_1_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "base_red1"
      )
      results$base_models$red_2_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "base_red2"
      )
      results$base_models$true_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "base_true"
      )
      results$base_models$of_models[[i]] <- lapply(rep_results, `[[`, "base_of")

      results$additive_models$null_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "add_null"
      )
      results$additive_models$red_1_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "add_red1"
      )
      results$additive_models$red_2_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "add_red2"
      )
      results$additive_models$true_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "add_true"
      )
      results$additive_models$of_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "add_of"
      )

      results$multiplicative_models$null_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "mult_null"
      )
      results$multiplicative_models$red_1_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "mult_red1"
      )
      results$multiplicative_models$red_2_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "mult_red2"
      )
      results$multiplicative_models$true_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "mult_true"
      )
      results$multiplicative_models$of_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "mult_of"
      )

      results$obs_trans[[i]] <- lapply(rep_results, `[[`, "obs_matrix")
      results$idv_trans[[i]] <- lapply(rep_results, `[[`, "idv_trans")
      results$sample_data[[i]] <- lapply(rep_results, `[[`, "sample_data")
    } # End of for(i in seq_along(sample_sizes))

    # closeAllConnections()
    parallel::stopCluster(cl)
  } else {
    # Serial execution ---------------------------------------------------------
    message("Running in serial mode ... ")

    require(progress)

    pb <- progress::progress_bar$new(
      format = " fitting [:bar] :percent (:current/:total) in :elapsed",
      total = length(sample_sizes),
      clear = FALSE,
      width = 60
    )

    for (i in seq_along(sample_sizes)) {
      pb$tick()
      n <- sample_sizes[i]

      rep_results <- fit_worker(n)

      # Store results in organized structure
      results$base_models$null_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "base_null"
      )
      results$base_models$red_1_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "base_red1"
      )
      results$base_models$red_2_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "base_red2"
      )
      results$base_models$true_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "base_true"
      )
      results$base_models$of_models[[i]] <- lapply(rep_results, `[[`, "base_of")

      results$additive_models$null_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "add_null"
      )
      results$additive_models$red_1_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "add_red1"
      )
      results$additive_models$red_2_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "add_red2"
      )
      results$additive_models$true_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "add_true"
      )
      results$additive_models$of_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "add_of"
      )

      results$multiplicative_models$null_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "mult_null"
      )
      results$multiplicative_models$red_1_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "mult_red1"
      )
      results$multiplicative_models$red_2_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "mult_red2"
      )
      results$multiplicative_models$true_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "mult_true"
      )
      results$multiplicative_models$of_models[[i]] <- lapply(
        rep_results,
        `[[`,
        "mult_of"
      )

      results$obs_trans[[i]] <- lapply(rep_results, `[[`, "obs_matrix")
      results$idv_trans[[i]] <- lapply(rep_results, `[[`, "idv_trans")
      results$sample_data[[i]] <- lapply(rep_results, `[[`, "sample_data")
    } # End of for (i in seq_along(sample_sizes))
  }

  # Add execution metadata to results
  # Add execution metadata to results
  results$meta_data$completion_time <- Sys.time()
  results$meta_data$seed_used <- seed

  message("\nModel fitting completed successfully")

  return(results)
}
