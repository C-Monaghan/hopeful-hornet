# Generates a panel of transition matrix plots for multiple repetitions of a given sample size.
# Arranges individual transition plots in a grid layout for easy comparison across replicates.
#
# Arguments:
#   - transition_list: List containing transition matrices (output from fit_markov_model)
#   - sample_size: Specific sample size to visualize (e.g., 100, 250)
#   - reps: Range of repetitions to include in panel (default: first 4 reps)
#   - obs: Logical indicating whether to include observed transitions (TRUE) or just estimated
#
# Returns:
#   - A ggarrange plot object containing:
#     * Multiple transition matrix heatmaps arranged in grid
#     * Shared legend and consistent formatting
#     * 2-row layout with dynamic column number
#
# Dependencies:
#   - Requires ggplot2 and ggpubr packages
#
# Behavior:
#   - Creates individual plots using plot_transitions() for each repetition
#   - Arranges plots in optimal grid layout (2 rows)
#   - Maintains consistent legend and color scheme across all plots
#   - Handles any number of repetitions up to the specified maximum

plot_multiple_transitions <- function(
    transition_list, 
    sample_size, 
    reps = 1:4,
    obs = TRUE){
  
  require(ggplot2)
  require(ggpubr)
  
  plot_list <- list()
  
  for(rep in reps) {
    plot_list[[rep]] <- plot_transitions(
      transition_list = transition_list,
      sample_size = sample_size,
      rep = rep, 
      obs = obs)
  }
  
  ggpubr::ggarrange(
    plotlist = plot_list, 
    nrow = 2, 
    ncol = max((reps / 2)), 
    common.legend = TRUE,
    legend = "right")
}
