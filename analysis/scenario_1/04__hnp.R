rm(list = ls())

# Packages ---------------------------------------------------------------------
pacman::p_load(
  dplyr,                 # Data manipulation
  hnp                    # Half normal plots
)

# Functions --------------------------------------------------------------------

# Data -------------------------------------------------------------------------
path_scenario <- "analysis/scenario_1"

resim <- readRDS(file = here::here(path_scenario, "results/resim.RDS"))

# Testing HNP ------------------------------------------------------------------
fit_1 <- nnet::multinom(y ~ 1, data = test, verbose = FALSE)
fit_2 <- nnet::multinom(y ~ 1, data = data, verbose = FALSE)

max_diff <- function(fit_real, fit_sim) {
  r1 <- resid(fit_real) 
  r2 <- resid(fit_sim)
  
  diff <- r2 - r1
  
  max(abs(diff))
}

refit_function1 <- function(resp_vector) {
  fit <- lm(resp_vector ~ 1)
  return(fit)
}

simulate_function <- function(obj) {
  probs <- predict(fit_1, type = "probs")
  
  apply(probs, 1, function(p) {
    sample(1:3, size = 1, prob = p)
  }) |> as.numeric()
}

hnp(
  fit_1, newclass = TRUE,
  diagfun = max_diff(fit_real = fit_2),
  fitfun = refit_function1,
  simfun = simulate_function,
  how.many.out = TRUE,
  plot = FALSE)
