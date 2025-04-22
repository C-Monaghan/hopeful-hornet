plot_transitions <- function(
    transition_list, 
    sample_size, 
    rep = 1,
    obs = TRUE) {
  
  # Required packages ----------------------------------------------------------
  require(reshape2)
  require(ggplot2)
  require(stringr)
  require(colorspace)
  
  # Validation -----------------------------------------------------------------
  # Check if sample size is specified and valid
  if(is.null(sample_size)) {
    stop("Please specify a sample size (example: sample_size = 'n_100'")
  }
  if(!sample_size %in% names(transition_list)) {
    stop(paste0("Sample size not found. Available options:"), paste(names(transition_list), collapse = ", "))
  }
  
  if(obs == TRUE) {
    title = "Observed Transition Matrix"
  } else {
    title = "Estimated Transition Matrix"
  }
  
  # Extract observed matrix
  obs_matrix <- transition_list[[sample_size]][[rep]]
  
  # Convert to tidy format
  obs_df <- obs_matrix |>
    reshape2::melt(varnames = c("From", "To"), value.name = "Probability")
  
  # Plotting transition matrix
  plot <- obs_df |>
    ggplot(aes(x = From, y = To, fill = Probability)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = Probability), size = 4.5, 
              colour = "#212427", fontface = "bold") +
    colorspace::scale_fill_continuous_diverging(
      palette = "Blue-Red 3", mid = 0.50, alpha = 0.5, 
      limits = c(0, 1), name = "Transition \nProbability") +
    labs(
      title = title,
      subtitle = paste0("Sample size: ", stringr::str_remove(sample_size, "n_"), " | Replicate: ", rep),
      x = "Previous State (t - 1)",
      y = "Current State (t)") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      legend.position = "right",
      legend.text = element_text(size = 9, face = "bold"),
      panel.grid = element_blank()
    )
  
  return(plot)
} 
