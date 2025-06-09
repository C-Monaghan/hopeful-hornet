# parent_block_name is one of:
#   "base_models", "additive_models", "multiplicative_models"
# Return:
#   - 1 for cross‐sectional (no y_prev),
#   - 2 for additive‐Markov,
#   - 3 for multiplicative‐Markov.
#   
get_scenario_number <- function(parent_block_name) {
  case_when(
    parent == "base_models"         ~ 1L,
    parent == "additive_models"     ~ 2L,
    parent == "multiplicative_models" ~ 3L,
    TRUE ~ stop("Unknown parent: ", parent)
  )
}
