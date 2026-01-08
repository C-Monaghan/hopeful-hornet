create_individual_transition_matrices <- function(data) {
  # Identify the full set of possible states across all transitions
  states <- sort(unique(c(data$y, data$y_prev)))
  n_states <- length(states)

  # Pre-split the dataset by ID for efficient access per individual
  data_by_id <- split(data, data$ID)

  # Initialize a named list to hold transition matrices for all individuals
  transitions <- vector("list", length(data_by_id))
  names(transitions) <- paste0("p_", names(data_by_id))

  # Loop over each individual's data
  for (i in seq_along(data_by_id)) {
    # Sort individual's data by wave to ensure correct time order
    # Prepare a list to store that individual's transition matrices
    individual <- data_by_id[[i]][order(data_by_id[[i]]$w), ]
    individual_transitions <- vector("list", nrow(individual))

    # Loop over each row (observation) for this individual
    for (j in seq_len(nrow(individual))) {
      # Define wave pair: from (previous wave) to (current wave)
      from_wave <- as.character(as.numeric(individual$w[j]) - 1)
      # from_wave <- as.character(as.numeric(individual$w[j]))
      to_wave <- as.character(individual$w[j])

      # Extract the observed state transition
      from_state <- as.numeric(individual$y_prev[j])
      to_state <- as.numeric(individual$y[j])

      # Only create the matrix if transition is valid
      if (!is.na(from_state) && !is.na(to_state)) {
        # Initialise matrix
        trans_matrix <- matrix(
          0,
          nrow = n_states,
          ncol = n_states,
          dimnames = list(paste("From", states), paste("To", states))
        )

        # Fill in a 1 for the transition (from -> to state)
        trans_matrix[from_state, to_state] <- 1
        wave_pair <- paste0("w_", from_wave, "_", to_wave)
        individual_transitions[[wave_pair]] <- trans_matrix
      } # End of if statement
    } # end of for(j ... )

    # Remove any NULLs from missing transitions (e.g., due to NA values)
    individual_transitions <- Filter(Negate(is.null), individual_transitions)

    # Save to overall transitions list if any transitions exist
    if (length(individual_transitions) > 0) {
      transitions[[i]] <- individual_transitions
    } else {
      transitions[[i]] <- NULL # Clean up empty list slots
    } # End of if statement
  } # End of for(i ... )

  # Remove entries for individuals with no valid transitions
  transitions <- Filter(Negate(is.null), transitions)

  return(transitions)
}
