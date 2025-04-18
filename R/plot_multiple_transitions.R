plot_multiple_transitions <- function(transition_list, sample_size, reps = 1:4){
  
  plot_list <- list()
  
  for(rep in reps) {
    plot_list[[rep]] <- plot_transitions(
      transition_list = transition_list,
      sample_size = sample_size,
      rep = rep)
  }
  
  # cowplot::plot_grid(plotlist = plot_list, ncol = 2)
  
  ggpubr::ggarrange(
    plotlist = plot_list, 
    nrow = 2, 
    ncol = 2, 
    common.legend = TRUE, 
    legend = "right")
}
