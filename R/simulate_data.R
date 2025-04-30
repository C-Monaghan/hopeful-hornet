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
#
# Notes:
#   - Uses double for-loop structure for clarity in transition simulation
#   - Converts categorical variables to factors automatically
#   - Returns all simulation parameters for reproducibility

simulate_data <- function(
  n_subjects = 100,                 # Number of individuals
  n_waves = 3,                      # Number of waves
  y = 1:3,                          # Number of different states (possible transitions)
  transition_matrix = NULL,         # Transition matrix
  initial_probs = rep(1 / length(y), length(y)), # Initial probabilities
  state_means = list(               # State dependent means
    x4 = c(2, 4, 3),                # Poisson Lambda
    x5 = c(25, 35, 50)),            # Normal Mu
  covariate_effects = list(         # Covariate effects
    x2 = c(0, 0.5, 0.1),            # Effect of x2 on state transitions
    x3 = c(0.5, -0.25, -0.08),      # Effect of x3 on state transitions
    x4 = c(0, 0.5, 0.6),            # Effect of x4 on state transitions
    x5 = c(0, 0.4, 0.7)),           # Effect of x5 on state transitions
  seed = NULL) {                    # Seed (for reproducibility)
  
  # Set seed for reproducibility -----------------------------------------------
  if(!is.null(seed)) set.seed(seed)

  # Generate an initial (stationary) transition matrix (if none provided) ------
  if(is.null(transition_matrix)) {
    transition_matrix <- matrix(0.1 / (length(y) - 1), nrow = length(y), ncol = length(y), byrow = TRUE)
    diag(transition_matrix) <- 0.7
  }
  rownames(transition_matrix) <- paste("from", y)
  colnames(transition_matrix) <- paste("to", y)
  
  # Some validations -----------------------------------------------------------
  if(length(initial_probs) != length(y)){
    stop("Length of initial probs must match length of y")
  }
  if(dim(transition_matrix)[1] != length(y) & dim(transition_matrix)[2] != length(y)) {
    stop("Transition matrix must be a square matrix matching length of y")
  }
  
  # Simulating data ------------------------------------------------------------
  # Simulate subject-level characteristics (time-invariant)
  # ID = ID
  # x1 = Gender (poor predictor)
  # x2 = Age (will become time-variariant later) (will be a good predictor)
  # x3 = Education level (will be an ok predictor)
  
  subject_data <- data.frame(
    ID = 1:n_subjects,
    x1 = sample(0:1, n_subjects, replace = TRUE),
    x2 = round(rnorm(n_subjects, mean = 70, sd = 5)),
    x3 = sample(0:2, n_subjects, replace = TRUE, prob = c(0.3, 0.5, 0.2))
  )
  
  # Simulate time-varying outcomes
  panel_data <- data.frame()
  
  # Progress bar
  pb <- utils::txtProgressBar(min = 0, max = n_subjects, style = 3)
  
  for(id in 1:n_subjects) {
    
    # Start progress
    utils::setTxtProgressBar(pb, id)
    
    # Initialize state with some covariate effects
    # - Older individuals more likely to be in state 2
    # - Those with higher education more likely to be in state 1
    adjusted_probs <- initial_probs *
      c(1 + covariate_effects$x3[1:length(y)] * subject_data$x3[id] / 2 +  # Effect of x3
        covariate_effects$x2[1:length(y)] * subject_data$x2[id] / 50)      # Effect of x2
    
    # Normalizing
    adjusted_probs <- pmax(adjusted_probs, 0)
    adjusted_probs <- adjusted_probs / sum(adjusted_probs)
    
    # Simulating initial state
    current_y <- sample(x = y, size = 1, prob = adjusted_probs)
    
    # Simulate time-varying covariates with state-dependent means
    base_x4 <- rpois(1, lambda = state_means$x4[current_y])
    base_x5 <- round(rnorm(1, mean = state_means$x5[current_y], sd = 8))
    
    # 🎶 I love them double for loops baby 🎶
    for(wave in 1:n_waves) {
      
      # Outputting message
      # message("Generating data for person ", id, " in wave ", wave)
      
      panel_data <- rbind(panel_data, data.frame(
        ID   = id,
        w    = wave,
        y    = current_y,
        x1   = subject_data$x1[id],
        x2   = subject_data$x2[id] + (wave - 1) * 2,       # x2 increases
        x3   = subject_data$x3[id],
        x4   = pmax(0, pmin(8, base_x4 + rpois(1, 0.5))),  # bounded between (0 - 8)
        x5   = pmax(0, pmin(60, base_x5 + rnorm(1, 0, 2))) # bounded between (0 - 60)
      ))
      
      # Simulate transition to next state with covariate effects
      if(wave < n_waves) {
        trans_probs <- transition_matrix[current_y, ]
        
        # Modify based on covariates using the predefined effects
        current_x4 <- panel_data$x4[nrow(panel_data)]
        current_x5 <- panel_data$x5[nrow(panel_data)]
        
        # Apply all covariate effects
        trans_probs <- trans_probs * 
          (1 + 
             covariate_effects$x4 * current_x4 / 8 +           # x4 effect
             covariate_effects$x5 * current_x5 / 60 +          # x5 effect
             covariate_effects$x3 * subject_data$x3[id] / 2 +  # x3 effect
             covariate_effects$x2 * subject_data$x2[id] / 100) # x2 effect
        
        # Ensure probabilities are valid
        trans_probs <- pmax(trans_probs, 0)
        trans_probs <- trans_probs/sum(trans_probs)
        
        current_y <- sample(y, 1, prob = trans_probs)
        } # End of if statement
    } # End of for(wave in 1:n_waves)
  } # End of for(id in 1:n_subjects)
  
  # End progress
  close(pb)
  
  # Converting certain rows to factors
  panel_data$y  <- factor(panel_data$y)
  panel_data$x1 <- factor(panel_data$x1)
  panel_data$x3 <- factor(panel_data$x3)
  panel_data$w  <- factor(panel_data$w)
  
  # Rounding x5
  panel_data$x5 <- round(x = panel_data$x5, digits = 0)
  
  # Returning data
  return(list(
    data              = panel_data,
    transition_matrix = transition_matrix,
    initial_probs     = initial_probs,
    adjusted_probs    = adjusted_probs,
    trans_probs       = trans_probs,
    state_means       = state_means,
    covariate_effects = covariate_effects
  ))
}
