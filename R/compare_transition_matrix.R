compare_transition_matrix <- function(
    models, 
    results,
    sample_size = NULL, 
    rep = NULL, 
    type = "good") {
  
  # Validations ----------------------------------------------------------------
  if(!type %in% c("good", "bad")) {
    stop("type must be either 'good' or 'bad'")
  }
  
  # Get all sample sizes and reps if not specified
  if (is.null(sample_size)) {
    sample_sizes <- names(models$obs_trans)
  } else {
    sample_sizes <- sample_size
  }
  
  # Get all reps if not specified
  if (is.null(rep)) {
    reps <- seq_along(models$obs_trans[[1]])
  } else {
    reps <- rep
  }
  
  # Initialize plot list
  plots <- list()
  
  for(size in sample_size) {
    for(r in rep) {
      # Getting observed transition matrix
      obs <- models$obs_trans[[sample_size]][[r]]
      
      # Getting model type
      if(type == "good") {
        est   <- results[[size]][[r]]
        label <- "Good Model"
      } else{
        est   <- results[[size]][[r]]
        label <- "Bad Model"
      }
      
      # Combine observed and estimated
      plot_data <- rbind(
        data.frame(
          From = rownames(obs)[row(obs)],
          To = colnames(obs)[col(obs)],
          Probability = c(obs),
          Matrix = "Observed"),
        data.frame(
          From = rownames(est)[row(est)],
          To = colnames(est)[col(est)],
          Probability = c(est),
          Matrix = label) 
      )
      
      # Plotting
      p <- plot_data |>
        ggplot(aes(x = From, y = To, fill = Probability)) +
        geom_tile(color = "white", linewidth = 0.5) +
        geom_text(aes(label = Probability), size = 4.5, 
                  colour = "#212427", fontface = "bold") +
        colorspace::scale_fill_continuous_diverging(
          palette = "Blue-Red 3", mid = 0.50, alpha = 0.5, 
          limits = c(0, 1), name = "Transition \nProbability") +
        facet_wrap(~ Matrix, nrow = 1) +
        theme_minimal() +
        labs(
          title = "Comparison of estimated and observed transition matrix",
          subtitle = paste("Sample size", stringr::str_remove(size, "n_"), " | Replicate", r),
          x = "Previous State (t - 1)",
          y = "Current State (t)"
        ) +
        theme(
          plot.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(face = "bold", size = 10)
        )
      
      # Store or print plot
      if (is.null(sample_size) || is.null(rep)) {
        plots[[paste(size, "Rep", r)]] <- p
      } else {
        print(p)
        return(invisible(p))
      }
    } # End of for r in reps
  } # End of for size in sample_size

  
  if (length(plots) > 0) {
    # Arrange multiple plots if needed
    if (length(plots) <= 4) {
      do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
    } else {
      warning("Many plots generated. Returning list of plots rather than displaying.")
    }
    return(invisible(plots))
  }
}