# parent_block_name is one of:
#   "base_models", "additive_models", "multiplicative_models"
# Return 1 for cross‐sectional (no y_prev),
#        2 for additive‐Markov,
#        3 for multiplicative‐Markov.
#
get_scenario_number <- function(parent_block_name) {
  if (parent_block_name == "base_models") {
    return(1L)
  }
  if (parent_block_name == "additive_models") {
    return(2L)
  }
  if (parent_block_name == "multiplicative_models") {
    return(3L)
  }
  stop("Unknown parent block: ", parent_block_name)
}
