# Adds previous state (t-1) information to longitudinal panel data for transition analysis.
# Prepares data for Markov modeling by creating lagged state variable and removing
# initial observations with no previous state.
#
# Arguments:
#   - data: Input dataset containing longitudinal state observations
#   - id: Column name for subject identifier (default: "ID")
#   - w: Column name for wave/time variable (default: "w")
#   - y: Column name for state variable (default: "y")
#
# Returns:
#   - Modified dataset with:
#     * y_prev column containing previous state (placed after y column)
#     * First wave observations removed (no previous state available)
#     * Original factor structure preserved for state variables
#
# Process:
#   1. Arranges data by ID and wave for proper lag calculation
#   2. Creates y_prev column using lagged y values
#   3. Maintains factor levels if y is a factor
#   4. Removes rows with missing y_prev (first wave observations)
#
# Notes:
#   - Essential for transition probability calculations
#   - Preserves original factor levels when present
#   - Uses tidyverse syntax for consistency with pipeline workflows
#   - Handles both numeric and factor state variables appropriately

add_previous_status <- function(data, id = "ID", w = "w", y = "y") {
  
  # Make sure data is correctly arranged
  data <- data |>
    dplyr::arrange(!!rlang::sym(id), !!rlang::sym(w)) 
  
  # Create a t-1 column
  data <- data |>
    dplyr::group_by(!!rlang::sym(id)) |>
    dplyr::mutate(y_prev = dplyr::lag(!!rlang::sym(y), n = 1)) |>
    dplyr::ungroup() |>
    dplyr::relocate(y_prev, .after = y)
  
  # Convert to factor
  # If y is a factor, then logically prev_y should also be a factor
  if(is.factor(data$y)) {
    data$y_prev <- factor(data$y_prev, levels = levels(data$y))
  }
  
  # Removing wave 1 (w = 1) from data as there is no prev_y column
  data <- data |>
    dplyr::filter(!is.na(y_prev))
  
  return(data)
}
