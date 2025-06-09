compare_matrices <- function(obs_mat, sim_mat) {
  diff <- obs_mat - sim_mat
  tibble(
    metric     = c("Frobenius", "Manhattan", "Max", "MeanAbs", "RMSE", "Correlation", "KL"),
    value      = c(
      norm(diff, type = "F"),
      sum(abs(diff)),
      max(abs(diff)),
      mean(abs(diff)),
      sqrt(mean(diff^2)),
      1 - suppressWarnings(cor(c(obs_mat), c(sim_mat))),
      sum((obs_mat + 1e-10) * log((obs_mat + 1e-10) / (sim_mat + 1e-10)))
    )
  )
}