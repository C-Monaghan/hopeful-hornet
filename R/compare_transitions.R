# Creates a heatmap visualization of state transition probabilities from Markov model results.
# Generates either observed or estimated transition matrices with consistent formatting and
# comprehensive labeling for interpretability.
#
# Arguments:
#   - transition_list: List containing transition matrices (output from fit_markov_model)
#   - sample_size: Specific sample size to visualize (e.g., "n_100", "n_500")
#   - rep: Replication number to plot (default: 1)
#   - obs: Logical flag for observed (TRUE) vs estimated (FALSE) transitions
#
# Returns:
#   - A ggplot heatmap object showing:
#     * Transition probabilities as color-coded tiles
#     * Numeric probability values displayed in each cell
#     * Properly formatted axes with state labels
#     * Consistent Blue-Red color scheme (0-1 scale)
#
# Features:
#   - Input validation for sample size existence
#   - Clean visualization with:
#     * Diagonal white grid lines
#     * Bold probability labels
#     * Rotated x-axis labels
#     * Informative title and subtitle
#   - Theme customization for publication quality
#
# Dependencies:
#   - Requires reshape2, ggplot2, stringr, and colorspace packages
#
# Throws:
#   - Error if specified sample_size not found in transition_list
#   - Error if sample_size argument is NULL
compare_transitions <- function(
    transition_list, 
    sample_size, 
    rep = 1,
    obs = TRUE,
    model_names) {
  
  # Required packages ----------------------------------------------------------
  require(reshape2)
  require(ggplot2)
  require(stringr)
  require(colorspace)
  require(patchwork)
  
  # Validation -----------------------------------------------------------------
  # Check if sample size is specified and valid
  if(is.null(sample_size)) {
    stop("Please specify a sample size (example: sample_size = 'n_100'")
  }
  
  # Check if all lists contain the specified sample size
  for(i in seq_along(transition_list)) {
    if(!sample_size %in% names(transition_list[[i]])) {
      stop(paste0(
        "Sample size not found in model ", i,
        ". Available options: ", paste(names(transition_list[[i]]), collapse = ", ")))
    }
  }
  
  # If model names not provided, use list names or default names
  if(is.null(model_names)) {
    if(!is.null(names(transition_list))) {
      model_names <- names(transition_list)
    } else {
      model_names <- paste("Model", seq_along(transition_list))
    }
  }
  
  # if(obs == TRUE) title = "Observed Transition Matrix"
  
  # Create a list to store individual plots
  plot_list <- list()
  
  # Generate each plot ---------------------------------------------------------
  for(i in seq_along(transition_list)) {
    # Extract the transition matrix
    tran_matrix <- transition_list[[i]][[sample_size]][[rep]]
    
    # Convert to tidy format
    tran_df <- tran_matrix |>
      reshape2::melt(varnames = c("From", "To"), value.name = "Probability")
    
    # Create plot
    p <- ggplot(tran_df, aes(x = From, y = To, fill = Probability)) +
      geom_tile(colour = "white", linewidth = 0.5) +
      geom_text(aes(label = round(Probability, 2)), size = 3.5, 
                colour = "#212427", fontface = "bold") +
      colorspace::scale_fill_continuous_diverging(
        palette = "Blue-Red 3", mid = 0.50, alpha = 0.5, 
        limits = c(0, 1), name = "Transition \nProbability") +
      labs(
        title = model_names[i],
        x = "Previous State (t - 1)",
        y = "Current State (t)") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.4, size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "right",
        legend.text = element_text(size = 8, face = "bold"),
        panel.grid = element_blank()
      )
    
    plot_list[[i]] <- p
  }
  
  # Combine plots --------------------------------------------------------------
  combined_plot <- wrap_plots(plot_list, nrow = 2) + 
    plot_annotation(
      title = "Comparison of Transition Matrices",
      subtitle = paste0(
        "Sample size: ", stringr::str_remove(sample_size, "n_"),
        " | Replicate: ", rep),
      theme = theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12))
      ) +
    plot_layout(guides = "collect", axes = "collect")
      
  return(combined_plot)
}
