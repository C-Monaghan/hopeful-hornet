simulate_data <- function(
    n_subjects = 100,                 # Number of individuals
    n_waves = 3,                      # Number of waves
    scenario = 1:3,                   # What simulation scenario to run
    seed = NULL) {                    # Seed (for reproducibility)
  
  # Set seed for reproducibility -----------------------------------------------
  if(!is.null(seed)) set.seed(seed)
  
  # True beta values to be used in the simulation ------------------------------
  beta_scenario_1 <- list(
    alpha  = c(-5.152, -5.402),
    beta_1 = c(0.008, -0.034),
    beta_2 = c(0.036, 0.017),
    beta_3 = c(0.028, 0.045)
  )
  
  beta_scenario_2 <- list(
    alpha  = c(-5.370, -7.907),
    beta_1 = c(0.020, -0.011),
    beta_2 = c(0.036, 0.039),
    beta_3 = c(0.022, 0.016),
    beta_4 = c(1.711, 2.790),
    beta_5 = c(-0.776, 20.914)
  )
  
  beta_scenario_3 <- list(
    alpha  = c(-5.885, -10.613),
    beta_1 = c(0.102, 0.615),
    beta_2 = c(0.043, 0.076),
    beta_3 = c(0.019, -0.002),
    beta_4 = c(3.845, 7.901),
    beta_5 = c(-0.638, 12.753),
    beta_6 = c(-0.292, -1.019),
    beta_7 = c(-0.474, -1.268),
    beta_8 = c(-0.031, -0.072),
    beta_9 = c(0.093, 0.100), 
    beta_10 = c(0.008, 0.029),
    beta_11 = c(-0.082, -0.021)
  )
  
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
  
  # Simulating a wave 0 for use in scenario 2 and 3 ----------------------------
  if(scenario != 1) {
    
    wave_0 <- vector("list", n_subjects)
    
    # Using scenario 1 betas to simulate y
    for(id in 1:n_subjects) {
      subj <- subject_data[id, ]
      
      probs <- get_probabilities(
        x1 = subj$x1, x2 = subj$x2, x3 = subj$x3, 
        y_prev = NULL, betas = beta_scenario_1, scenario = 1)
      
      draw_0 <- rmultinom(n = 1, size = 1, prob = probs)
      y_0 <- which(draw_0 == 1)
      
      wave_0[[id]] <- data.frame(
        ID = id, w = 0, y = factor(y_0, levels = 1:3),
        x1 = subj$x1, x2 = subj$x2, x3 = subj$x3, 
        x4 = subj$x4, x5 = subj$x5)
    }
    
    wave_0_df <- bind_rows(wave_0)
  }
  
  # Preallocating results (for better optimization)
  total_rows <- n_subjects * n_waves
  panel_list <- vector("list", total_rows)
  pi_values <- vector("list", total_rows)
  row_index  <- 1
  
  # Progress bar
  pb <- utils::txtProgressBar(min = 0, max = n_subjects, style = 3)
  
  # Simulate time-varying outcomes ---------------------------------------------
  for(id in 1:n_subjects) {
    
    # Start progress
    utils::setTxtProgressBar(pb, id)
    
    # Get individual level subject data
    subj <- subject_data[id, ]
    
    # Getting scenario specific data (previous y and beta values)
    y_prev <- if(scenario != 1) wave_0_df$y[wave_0_df$ID == id] else NULL
    
    betas <- if(scenario == 2) beta_scenario_2 else if(scenario == 3) beta_scenario_3 else beta_scenario_1
    
    # Get probabilities from true beta values ----------------------------------
    probs <- get_probabilities(
      x1 = subj$x1, x2 = subj$x2, x3 = subj$x3,
      y_prev = y_prev, betas = betas, scenario = scenario)
    
    # Making (and saving) a vector of probabilities used in multinomial draws
    pi_values[[row_index]] <- probs
    
    # Simulate initial state from a multinomial distribution
    draw_init <- rmultinom(n = 1, size = 1, prob = probs)
    y <- factor(which(draw_init == 1), levels = 1:3)

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
        
        # For use in scenario 2 and 3
        y_prev <- panel_list[[row_index]]$y
        
        # Get new probabilities from true beta values --------------------------
        probs <- get_probabilities(
          x1 = panel_list[[row_index]]$x1, x2 = panel_list[[row_index]]$x2,
          x3 = panel_list[[row_index]]$x3, y_prev = y_prev, betas = betas, scenario = scenario)
        
        # These are new pi values based off the now time varying predictors
        pi_values[[row_index]] <- probs
        
        # Simulate next state from a multinomial distribution
        draw_init <- rmultinom(n = 1, size = 1, prob = probs)
        
        panel_list[[row_index]]$y <- factor(which(draw_init == 1), levels = 1:3)
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
    true_betas        = betas,
    pi_values         = pi_df
  ))
}
