#' Simulate Panel Data with State Transitions and Covariate Effects
#'
#' Generates a simulated panel dataset with discrete state transitions where 
#' covariates differentially affect state probabilities. The function creates 
#' data where:
#' - Older individuals are more likely to start in state 2
#' - Higher education is associated with state 1
#' - Depression and procrastination scores vary by state and affect 
#'   transition probabilities
#'
#' @param n_subjects Integer. Number of individuals (default: 100).
#' @param n_waves Integer. Number of time waves/measurements (default: 3).
#' @param y Numeric vector. Possible states (default: `1:3`).
#' @param transition_matrix Square matrix. Transition probabilities between states.
#'   If `NULL` (default), generates a stationary matrix with strong diagonal.
#' @param initial_probs Numeric vector. Initial state probabilities (default: `c(0.7, 0.2, 0.1)`).
#' @param seed Integer. Random seed for reproducibility (default: `NULL`).
#'
#' @return A list with three components:
#' \itemize{
#'   \item `data` - A `data.frame` containing:
#'     \itemize{
#'       \item `ID`: Subject identifier
#'       \item `w`: Wave/time point (factor)
#'       \item `y`: Current state (factor)
#'       \item `x1`: Gender (0/1, factor)
#'       \item `x2`: Age (increases by 2 each wave)
#'       \item `x3`: Education level (0-2, factor)
#'       \item `x4`: Depression score (0-8, state-dependent Poisson)
#'       \item `x5`: Procrastination score (0-60, state-dependent normal)
#'     }
#'   \item `transition_matrix` - The transition matrix used (with row/col names)
#'   \item `initial_probs` - The initial state probabilities
#' }
#'
#' @section Covariate Effects:
#' \strong{Initial State Probabilities:}
#' \itemize{
#'   \item Older individuals (higher x2) more likely to start in state 2
#'   \item Higher education (higher x3) more likely to start in state 1
#' }
#'
#' \strong{State-Dependent Covariate Means:}
#' \itemize{
#'   \item \strong{State 1}: Low depression (λ=2), low procrastination (μ=25)
#'   \item \strong{State 2}: High depression (λ=4), high procrastination (μ=40)
#'   \item \strong{State 3}: Moderate depression (λ=3), moderate procrastination (μ=35)
#' }
#'
#' \strong{Transition Probability Effects:}
#' \itemize{
#'   \item \strong{Depression (x4)}: Stronger effect on state 3, moderate on state 2
#'   \item \strong{Procrastination (x5)}: Stronger effect on state 3, moderate on state 2
#'   \item \strong{Education (x3)}: Increases probability of state 1
#'   \item \strong{Age (x2)}: Increases probability of state 2
#' }
#'
#' @examples
#' # Default simulation
#' dat <- simulate_data(seed = 123)
#' head(dat$data)
#'
#' # Check state-dependent means
#' aggregate(x4 ~ y, data = dat$data, mean)
#' aggregate(x5 ~ y, data = dat$data, mean)
#'
#' # Custom transition matrix
#' trans_mat <- matrix(c(0.7, 0.2, 0.1,
#'                      0.1, 0.8, 0.1,
#'                      0.05, 0.15, 0.8), nrow = 3)
#' dat2 <- simulate_data(transition_matrix = trans_mat, n_waves = 5)
#' 
#' @export

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
  
  for(id in 1:n_subjects) {
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
