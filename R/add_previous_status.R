#' Add Previous State to Panel Data
#'
#' Adds a column containing the previous state (`y_prev`) to longitudinal/panel data.
#' Useful for transition analysis (e.g., Markov models). Drops the first time point
#' for each subject since no previous state exists.
#'
#' @param data A `data.frame` containing panel data with IDs, time points, and states.
#' @param id Character. Column name for subject IDs (default: `"ID"`).
#' @param w Character. Column name for time/wave variable (default: `"w"`).
#' @param y Character. Column name for state variable (default: `"y"`).
#'
#' @return A modified `data.frame` with:
#' \itemize{
#'   \item New column `y_prev` (the state at t-1) inserted after `y`
#'   \item Rows where `y_prev` is `NA` (first observations) removed
#'   \item If `y` is a factor, `y_prev` inherits the same factor levels
#'   \item Original ordering preserved within subjects
#' }
#'
#' @details
#' The function:
#' 1. Arranges data by `id` and `w` to ensure correct lagging
#' 2. Groups by `id` and creates `y_prev` using `dplyr::lag()`
#' 3. Drops initial observations (where `y_prev` is `NA`)
#' 4. Preserves factor levels if `y` is a factor
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' 
#' # Simulate data
#' dat <- simulate_data(n_subjects = 5, n_waves = 3, seed = 123)$data
#' 
#' # Add previous states
#' dat_with_prev <- add_previous_status(dat)
#' 
#' # Compare before/after
#' dat %>% arrange(ID, w) %>% select(ID, w, y)
#' dat_with_prev %>% arrange(ID, w) %>% select(ID, w, y, y_prev)
#' }
#'
#' @importFrom dplyr arrange group_by mutate ungroup relocate filter
#' @importFrom rlang sym
#' @export

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
