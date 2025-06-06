create_individual_transition_matrices <- function(data) {
  
  # Get unique states
  states   <- sort(unique(c(data$y, data$y_prev)))
  n_states <- length(states)
  
  # Create a transition list to store results
  transitions <- list()
  
  for(id in unique(data$ID)) {
    
    individual <- data |>
      dplyr::filter(ID == id) |>
      dplyr::arrange(w)
    
    individual_transitions <- list()
    
    # Making transition matrix for each wave pair (1 - 2; 2 - 3)
    for(i in 1:(nrow(individual))) {
      
      # Getting waves
      from_wave  <- as.character(as.numeric(individual$w[i]) - 1)
      to_wave    <- as.character(individual$w[i])
      
      # Getting states
      from_state <- as.numeric(individual$y_prev[i])
      to_state   <- as.numeric(individual$y[i])
      
      # Creating an empty transition matrix
      trans_matrix <- matrix(
        0, nrow = n_states, ncol = n_states,
        dimnames = list(
          paste("From", states), paste("To", states))
      )
      
      # Filling in the observed transition cell
      trans_matrix[from_state, to_state] <- 1
      
      # Store into their matrix list
      wave_pair <- paste0("w_", from_wave, "_", to_wave)
      individual_transitions[[wave_pair]] <- trans_matrix
    } # End of for(i in 1:(nrow(individual) - 1))
    
    # Add to results if we found any transitions
    if (length(individual_transitions) > 0) {
      transitions[[paste0("p_", as.character(id))]] <- individual_transitions
    }
  } # End of for(id in unique(data$ID))
  
  return(transitions)
}
