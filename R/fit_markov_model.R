fit_markov_model <- function(data,
                             sample_sizes = c(100, 250, 500, 1000, 5000),
                             n_reps = 5,
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
  
  return(results)
}
