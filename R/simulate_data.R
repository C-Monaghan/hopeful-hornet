# Simulates longitudinal panel data with state transitions and covariate effects.
# Creates realistic multi-state Markov chain data with time-invariant and time-varying
# covariates that influence transition probabilities between states.
#
# Arguments:
#   - n_subjects: Number of individuals to simulate
#   - n_waves: Number of observation waves per individual
#   - y: Possible states (default 1:3 for 3-state model)
#   - transition_matrix: Optional custom transition matrix (default generates stationary matrix)
#   - initial_probs: Starting probabilities for each state
#   - state_means: List of state-dependent means for time-varying covariates
#   - covariate_effects: List specifying how covariates influence transitions
#   - seed: Random seed for reproducibility
#
# Returns:
#   - List containing:
#     * data: Simulated panel dataset with:
#       - ID, wave, state (y)
#       - Time-invariant covariates (x1, x3)
#       - Time-varying covariates (x2, x4, x5)
#     * transition_matrix: Used transition probabilities
#     * initial_probs: Used initial state probabilities
#     * adjusted_probs: Final adjusted probabilities
#     * trans_probs: Final transition probabilities
#     * state_means: State-dependent means used
#     * covariate_effects: Covariate effects applied
#
# Features:
#   - Realistic aging trajectory simulation (x2 increases over time)
#   - Bounded time-varying covariates (x4: 0-8, x5: 0-60)
#   - Multiple covariate effects on transitions:
#     * x1: Gender (weak effect)
#     * x2: Age (strong effect)
#     * x3: Education (moderate effect)
#     * x4/x5: State-dependent time-varying effects
#   - Automatic transition matrix generation if none provided
#   - Comprehensive input validation

simulate_data <- function(
    n_subjects = 100,                 # Number of individuals
    n_waves = 3,                      # Number of waves
    y = 1:3,                          # Number of different states (possible transitions)
    initial_probs = rep(1 / length(y), length(y)), # Initial probabilities
    true_betas = list(
      alpha  = c(-5.152, -5.402),   # True intercepts
      beta_1 = c(0.008, -0.034),    # True beta 1
      beta_2 = c(0.036, 0.017),     # True beta 2
      beta_3 = c(0.028, 0.045)      # True beta 3
    ),
    # state_means = c(10, 25, 17),     # State dependent means (normal mu)
    # covariate_effects = list(        # Covariate effects
      # x2 = c(0, 0.5, 0.1),           # Effect of x2 on state transitions
      # x3 = c(0, 0.4, 0.7)),          # Effect of x3 on state transitions
    seed = NULL) {                   # Seed (for reproducibility)
  
  # Set seed for reproducibility -----------------------------------------------
  if(!is.null(seed)) set.seed(seed)
  
  # Simulating data ------------------------------------------------------------
  # Simulate subject-level characteristics (time-invariant)
  # ID = ID
  # x1 = Gender
  # x2 = Age (will become time-variariant)
  # x3 = Procrastination (will also become time-variariant)
  subject_data <- data.frame(
    ID = 1:n_subjects,
    
    # Predictors
    x1 = sample(0:1, n_subjects, replace = TRUE),
    x2 = round(rnorm(n_subjects, mean = 70, sd = 5)),
    x3 = round(pmin(60, pmax(0, rnorm(n_subjects, mean = 25, sd = 15)))),
    
    # Noise
    x4 = round(runif(n = n_subjects, min = 0, max = 1), digits = 2),
    x5 = round(runif(n = n_subjects, min = 0, max = 1), digits = 2)
  )
  
  # Preallocating results (for better optimization)
  total_rows <- n_subjects * n_waves
  panel_list <- vector("list", total_rows)
  pi_values <- vector("list", total_rows)
  row_index  <- 1
  
  # For outputting the probabilities used for the multinomial regression
  # starting_probs   <- vector("list", total_rows)
  # transition_probs <- vector("list", total_rows)
  
  # Simulate time-varying outcomes
  # Progress bar
  pb <- utils::txtProgressBar(min = 0, max = n_subjects, style = 3)
  
  for(id in 1:n_subjects) {
    
    # Start progress
    utils::setTxtProgressBar(pb, id)
    
    # Get individual level subject data
    subj <- subject_data[id, ]
    
    # Get probabilities from true beta values ----------------------------------
    # Calculating pi_1
    denom <- 1 + 
      exp(true_betas$alpha[1] + 
            true_betas$beta_1[1] * subj$x1 + 
            true_betas$beta_2[1] * subj$x2 + 
            true_betas$beta_3[1] * subj$x3) + 
      exp(true_betas$alpha[2] + 
            true_betas$beta_1[2] * subj$x1 + 
            true_betas$beta_2[2] * subj$x2 + 
            true_betas$beta_3[2] * subj$x3)

    
    pi_1 <- 1 / denom
    
    # Calculating pi_2
    pi_2 <- pi_1 * exp(true_betas$alpha[1] + 
                         true_betas$beta_1[1] * subj$x1 + 
                         true_betas$beta_2[1] * subj$x2 + 
                         true_betas$beta_3[1] * subj$x3)
    
    # Calculating pi_3
    pi_3 <- pi_1 * exp(true_betas$alpha[2] + 
                         true_betas$beta_1[2] * subj$x1 + 
                         true_betas$beta_2[2] * subj$x2 + 
                         true_betas$beta_3[2] * subj$x3)
    
    # Making (and saving) a vector of probabilities used in multinomial draws
    probs <- c(pi_1, pi_2, pi_3)
    pi_values[[row_index]] <- probs
    
    # Simulate initial state from a multinomial distribution
    draw_init <- rmultinom(n = 1, size = 1, prob = probs)
    y <- which(draw_init == 1)

    # 🎶 I love them double for loops baby 🎶
    for(wave in 1:n_waves) {
      
      if(wave != 1)  {
      # Variables change across time
      x2  <- subj$x2 + (wave - 1) * 2
      x3  <- round(pmin(60, pmax(0, subj$x3 + rnorm(1, 5, 2))))
      
      # Noise changes across time too
      x4 <- round(pmin(1, pmax(0, subj$x4 + runif(1, 0, 0.25))), digits = 2)
      x5 <- round(pmin(1, pmax(0, subj$x5 + runif(1, 0, 0.25))), digits = 2)
      } else {
        x2 <- subj$x2
        x3 <- subj$x3
        x4 <- subj$x4
        x5 <- subj$x5
      }
      
      panel_list[[row_index]] <- list(
        ID = id, 
        w  = wave, 
        y  = y,
        x1 = subj$x1, 
        x2 = x2, 
        x3 = x3,
        x4 = x4, 
        x5 = x5
      )
      
      # Simulating transitions for next wave based off time varying predictors
      if(wave %in% c(2:tail(n_waves))) {
        # Get new probabilities from true beta values --------------------------
        # Calculating pi_1
        denom <- 1 +
          exp(true_betas$alpha[1] +
                true_betas$beta_1[1] * panel_list[[row_index]]$x1 +
                true_betas$beta_2[1] * panel_list[[row_index]]$x2 +
                true_betas$beta_3[1] * panel_list[[row_index]]$x3) +
          exp(true_betas$alpha[2] +
                true_betas$beta_1[2] * panel_list[[row_index]]$x1 +
                true_betas$beta_2[2] * panel_list[[row_index]]$x2 +
                true_betas$beta_3[2] * panel_list[[row_index]]$x3)
        
        pi_1 <- 1 / denom
        
        # Calculating pi_2
        pi_2 <- pi_1 * exp(true_betas$alpha[1] +
                             true_betas$beta_1[1] * panel_list[[row_index]]$x1 +
                             true_betas$beta_2[1] * panel_list[[row_index]]$x2 +
                             true_betas$beta_3[1] * panel_list[[row_index]]$x3)
        
        # Calculating pi_3
        pi_3 <- pi_1 * exp(true_betas$alpha[2] +
                             true_betas$beta_1[2] * panel_list[[row_index]]$x1 +
                             true_betas$beta_2[2] * panel_list[[row_index]]$x2 +
                             true_betas$beta_3[2] * panel_list[[row_index]]$x3)
        
        # These are new pi values based off the now time varying predictors
        probs <- c(pi_1, pi_2, pi_3)
        pi_values[[row_index]] <- probs
        
        # Simulate next state from a multinomial distribution
        draw_init <- rmultinom(n = 1, size = 1, prob = probs)
        
        panel_list[[row_index]]$y <- which(draw_init == 1)
      } # End of if(wave < n_waves)
      
      # Do next row
      row_index <- row_index + 1
      
    } # End of for(wave in 1:n_waves)
  } # End of for(id in 1:n_subjects)
  
  # End progress
  close(pb)
  
  # Turn the nested lists into one data set
  panel_data <- data.table::rbindlist(panel_list) |> as.data.frame()
  
  # Converting certain rows to factors
  panel_data$y  <- factor(panel_data$y)
  panel_data$x1 <- factor(panel_data$x1)
  panel_data$w  <- factor(panel_data$w)
  
  # Turn the pi values into a dataframe
  pi_matrix <- do.call(rbind, pi_values)
  colnames(pi_matrix) <- c("pi_1", "pi_2", "pi_3")
  
  pi_df <- cbind(panel_data[, c("ID", "w")], pi_matrix)
  
  # Returning data
  return(list(
    data              = panel_data,
    true_betas        = true_betas,
    pi_values         = pi_df
  ))
}
