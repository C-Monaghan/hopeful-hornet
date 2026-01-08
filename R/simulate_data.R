# Simulate Longitudinal Panel Data with Markov Transitions
#
# Generates synthetic longitudinal data with time-varying covariates and
# Markov-dependent multinomial outcomes across multiple waves. Supports three
# distinct simulation scenarios with varying complexity levels.
#
# Inputs
#   - n_subjects Integer number of individuals to simulate (default: 100)
#   - n_waves Integer number of observation waves per subject (default: 3)
#   - scenario Integer (1-3) specifying simulation scenario:
#     1. Basic model without Markov dependence
#     2. Model with first-order Markov dependence
#     3. Complex model with interactions and additional effects
#   - seed Optional random seed for reproducibility (default: NULL)
#
# Output
# A list containing:
#   - Data.frame with simulated panel data including:
#     1. ID: Subject identifier
#     2. w: Wave indicator (factor)
#     3. y: Multinomial outcome (factor, levels 1-3)
#     4. x1: Binary covariate (gender, factor)
#     5. x2: Continuous covariate (age, numeric)
#     6. x3: Continuous covariate (procrastination, numeric)
#     7. x4-x5: Noise variables (numeric)
#   - true_betas: List of true beta coefficients used for simulation
#   - pi_values: Data.frame containing true transition probabilities for each observation
#
# Details
# The function implements a sophisticated data generation process:
#   - Generates baseline subject characteristics (time-invariant and time-varying)
#   - For scenarios 2-3, simulates a "wave 0" using scenario 1 parameters
#   - Simulates longitudinal outcomes using multinomial transitions
#   - Incorporates time-varying covariate effects
#   - Tracks true transition probabilities for validation

simulate_data <- function(
  n_subjects = 100, # Number of individuals
  n_waves = 3, # Number of waves
  scenario = 1:3, # What simulation scenario to run
  resim = FALSE, # Is this a resimulation
  og_data = NULL, # Orginal simulated data
  betas = NULL, # For scenario 4: either a list-of-coefs or list-of-multinom-objects
  seed = NULL, # Seed (for reproducibility)
  verbose = FALSE
) {
  # Should messages appear

  if (verbose) {
    message("Running validations ... ")
  }

  if (verbose) {
    Sys.sleep(time = 1)
  }

  # ----- 1) Input checks ------------------------------------------------------
  scenario <- match.arg(as.character(scenario), choices = 1:3)

  stopifnot(
    is.numeric(n_subjects) && n_subjects > 0,
    is.numeric(n_waves) && n_waves > 0,
    scenario %in% 1:3,
    is.null(seed) || is.numeric(seed)
  )

  if (resim) {
    stopifnot(!is.null(betas), inherits(x = og_data, what = "data.frame"))
  }

  # If scenario 4 but no betas supplied, throw an error:
  if (resim == TRUE && is.null(betas)) {
    stop("`resim = TRUE` requires you to pass a non‐NULL `betas` argument.")
  }

  if (!is.null(seed)) {
    set.seed(seed)
    if (verbose) message("Random seed set to: ", seed)
  }

  if (verbose) {
    Sys.sleep(time = 1)
  }

  # ----- 2) Original simulation -----------------------------------------------
  # Derived from empirical studies
  beta_scenario_1 <- list(
    alpha = c(-5.152, -5.402), # Intercepts for y = 2 and y = 3
    beta_1 = c(0.008, -0.034), # x1 effects
    beta_2 = c(0.036, 0.017), # x2 effects
    beta_3 = c(0.028, 0.045) # x3 effects
  )

  beta_scenario_2 <- list(
    alpha = c(-5.370, -7.907), # Intercepts for y = 2 and y = 3
    beta_1 = c(0.020, -0.011), # x1 effects
    beta_2 = c(0.036, 0.039), # x2 effects
    beta_3 = c(0.022, 0.016), # x3 effects
    beta_4 = c(1.711, 2.790), # y_prev = 2 effects
    beta_5 = c(-0.776, 20.914) # y_prev = 3 effects
  )

  beta_scenario_3 <- list(
    alpha = c(-5.885, -10.613), # Intercepts for y = 2 and y = 3
    beta_1 = c(0.102, 0.615), # x1 effects
    beta_2 = c(0.043, 0.076), # x2 effects
    beta_3 = c(0.019, -0.002), # x3 effects
    beta_4 = c(3.845, 7.901), # y_prev = 2 effects
    beta_5 = c(-0.638, 12.753), # y_prev = 3 effects
    beta_6 = c(-0.292, -1.019), # x1 * y_prev = 2 effects
    beta_7 = c(-0.474, -1.268), # x1 * y_prev = 3 effects
    beta_8 = c(-0.031, -0.072), # x2 * y_prev = 2 effects
    beta_9 = c(0.093, 0.100), # x2 * y_prev = 3 effects
    beta_10 = c(0.008, 0.029), # x3 * y_prev = 2 effects
    beta_11 = c(-0.082, -0.021) # x3 * y_prev = 3 effects
  )

  # ----- 3) Resimulation branch -----------------------------------------------
  if (resim == TRUE) {
    if (verbose) {
      message("Resimulating data ... ")
    }

    # As a datatable object
    df <- copy(og_data)
    setDT(df)

    # Ensure columns are factors
    df[, `:=`(
      w = factor(w, levels = 1:n_waves),
      x1 = factor(x1),
      y = factor(y, levels = levels(og_data$y))
    )]

    # Pre-allocate
    n <- nrow(df)
    pi_list <- vector("list", n)
    y_drawn <- integer(n)

    # Extract vectors for fast access
    ids <- df$ID
    waves <- as.integer(as.character(df$w))
    x1_vals <- as.integer(as.character(df$x1))
    x2_vals <- df$x2
    x3_vals <- df$x3

    for (i in seq_len(n)) {
      wave_i <- waves[i]
      id_i <- ids[i]

      # a) get correct previous outcome
      y_prev <- if (wave_i == 1 & scenario != 1) {
        probs <- get_probabilities(
          x1 = x1_vals[i],
          x2 = x2_vals[i],
          x3 = x3_vals[i],
          y_prev = NULL,
          betas = beta_scenario_1,
          scenario = 1
        )

        which.max(rmultinom(n = 1, size = 1, prob = probs))
      } else {
        # look up the newly drawn y from wave (i − 1)
        prev_i <- which(ids == id_i & waves == wave_i)
        as.integer(as.character(y_drawn[prev_i]))
      }

      # b) recompute transition probabilities
      probs <- get_probabilities(
        x1 = x1_vals[i],
        x2 = x2_vals[i],
        x3 = x3_vals[i],
        y_prev = y_prev,
        betas = betas,
        scenario = scenario
      )

      pi_list[[i]] <- probs

      # c) draw a new y
      y_drawn[i] <- which.max(rmultinom(n = 1, size = 1, prob = probs))
    }

    # Making y a factor
    df$y <- factor(y_drawn, levels = levels(df$y))

    # rebuild the pi_values data.frame
    pi_mat <- do.call(rbind, pi_list)
    colnames(pi_mat) <- paste0("pi_", seq_len(ncol(pi_mat)))
    pi_df <- cbind(df[, c("ID", "w")], pi_mat) |> as_tibble()

    return(list(
      data = tibble::as_tibble(df),
      true_betas = betas,
      pi_values = pi_df,
      metadata = list(
        resim = TRUE,
        scenario = scenario,
        n_subjects = n_subjects,
        n_waves = n_waves,
        seed = seed,
        timestamp = Sys.time()
      )
    ))
  }

  if (verbose) {
    message("Generating subject level data ... ")
  }

  if (verbose) {
    Sys.sleep(time = 1)
  }

  # ── 4) BASELINE SUBJECT CHARACTERISTICS ────────────────────────────────────
  # Generate time-invariant and initial time-varying covariates
  subject_data <- data.frame(
    ID = 1:n_subjects,

    # Core predictors
    # - x1 simulates gender
    # - x2 simulates age (mean 70 ± 5)
    # - x3 simulates procrastination (bounded between 0 - 60)
    x1 = sample(0:1, n_subjects, replace = TRUE),
    x2 = round(rnorm(n_subjects, mean = 70, sd = 5)),
    x3 = round(pmin(60, pmax(0, rnorm(n_subjects, mean = 25, sd = 15)))),

    # Noise variables (generated from uniform distribution)
    x4 = round(runif(n = n_subjects, min = 0, max = 1), digits = 2),
    x5 = round(runif(n = n_subjects, min = 0, max = 1), digits = 2)
  )

  # ── 5) WAVE 0 SIMULATION (FOR MARKOV SCENARIOS 2–4) ─────────────────────────
  if (scenario != 1) {
    if (verbose) {
      message(
        "Generating wave 0 dataframe for Markov scenario: ",
        scenario,
        " ..."
      )
    }

    wave_0 <- vector("list", n_subjects)

    # Using scenario 1 betas to simulate y
    for (id in 1:n_subjects) {
      subj <- subject_data[id, ]

      probs <- get_probabilities(
        x1 = subj$x1,
        x2 = subj$x2,
        x3 = subj$x3,
        y_prev = NULL,
        betas = beta_scenario_1,
        scenario = 1
      )

      draw_0 <- rmultinom(n = 1, size = 1, prob = probs)
      y_0 <- which(draw_0 == 1)

      wave_0[[id]] <- data.frame(
        ID = id,
        w = 0,
        y = factor(y_0, levels = 1:3),
        x1 = subj$x1,
        x2 = subj$x2,
        x3 = subj$x3,
        x4 = subj$x4,
        x5 = subj$x5
      )
    }

    wave_0_df <- bind_rows(wave_0)
  }

  # ── 6) MAIN SIMULATION SETUP ────────────────────────────────────────────────
  total_rows <- n_subjects * n_waves
  panel_list <- vector("list", total_rows)
  pi_values <- vector("list", total_rows)
  row_index <- 1

  # Progress tracking
  if (verbose) {
    message("Simulating ", n_waves, " waves for ", n_subjects, " subjects...")
    pb <- utils::txtProgressBar(min = 0, max = n_subjects, style = 3)
  }

  # Longitudinal Data Simulation ----------------------------------------------
  for (id in 1:n_subjects) {
    # Start progress
    if (verbose) {
      utils::setTxtProgressBar(pb, id)
    }

    # Get individual level subject data
    subj <- subject_data[id, ]

    # Getting scenario specific data (previous y and beta values)
    y_prev <- if (scenario != 1) wave_0_df$y[wave_0_df$ID == id] else NULL

    betas <- switch(
      as.character(scenario),
      "1" = beta_scenario_1,
      "2" = beta_scenario_2,
      "3" = beta_scenario_3
    )

    # ── 6.1) INITIAL STATE (wave = 1) ─────────────────────────────────────────
    probs <- get_probabilities(
      x1 = subj$x1,
      x2 = subj$x2,
      x3 = subj$x3,
      y_prev = y_prev,
      betas = betas,
      scenario = scenario
    )

    # Making (and saving) a vector of probabilities used in multinomial draws
    pi_values[[row_index]] <- probs

    # Simulate initial state from a multinomial distribution
    draw_init <- rmultinom(n = 1, size = 1, prob = probs)
    y <- factor(which(draw_init == 1), levels = 1:3)

    # ── 6.2) LOOP OVER WAVES 1..n_waves ──────────────────────────────────────
    # 🎶 I love them double for loops baby 🎶
    for (wave in 1:n_waves) {
      # Time-varying covariate updates
      if (wave != 1) {
        x2 <- subj$x2 + (wave - 1) * 2 # Linear increase
        x3 <- round(pmin(60, pmax(0, subj$x3 + rnorm(1, 5, 2)))) # Random walk

        # Noise changes across time too
        x4 <- round(pmin(1, pmax(0, subj$x4 + runif(1, 0, 0.25))), digits = 2)
        x5 <- round(pmin(1, pmax(0, subj$x5 + runif(1, 0, 0.25))), digits = 2)
      } else {
        x2 <- subj$x2
        x3 <- subj$x3
        x4 <- subj$x4
        x5 <- subj$x5
      }

      # Store current observation
      panel_list[[row_index]] <- list(
        ID = id,
        w = wave,
        y = y,
        x1 = subj$x1,
        x2 = x2,
        x3 = x3,
        x4 = x4,
        x5 = x5
      )

      # ── 6.3) UPDATE FOR NEXT WAVE (IF ANY) ─────────────────────────────────
      if (wave %in% c(2:tail(n_waves))) {
        # For use in scenario 2 and 3
        y_prev <- if (scenario != 1) panel_list[[row_index]]$y else NULL

        # Get new probabilities from true beta values --------------------------
        probs <- get_probabilities(
          x1 = panel_list[[row_index]]$x1,
          x2 = panel_list[[row_index]]$x2,
          x3 = panel_list[[row_index]]$x3,
          y_prev = y_prev,
          betas = betas,
          scenario = scenario
        )

        # These are new pi values based off the now time varying predictors
        pi_values[[row_index]] <- probs

        # Simulate next state from a multinomial distribution
        draw_init <- rmultinom(n = 1, size = 1, prob = probs)

        panel_list[[row_index]]$y <- factor(which(draw_init == 1), levels = 1:3)
      } # End of if(wave < n_waves)

      # Do next row
      row_index <- row_index + 1
    } # End of for(wave in 1:n_waves)
  } # End of for(id in 1:n_subjects)

  # ── 7) FINAL PROCESSING ─────────────────────────────────────────────────────
  if (verbose) {
    close(pb)
  }

  if (verbose) {
    message("Running final processing ...")
  }

  if (verbose) {
    Sys.sleep(time = 1)
  }

  # Combine results into data.frame
  panel_data <- data.table::rbindlist(panel_list) |> as.data.frame()

  # Converting certain rows to factors
  panel_data$y <- factor(panel_data$y)
  panel_data$x1 <- factor(panel_data$x1)
  panel_data$w <- factor(panel_data$w)

  # Turn the pi values into a dataframe
  pi_matrix <- do.call(rbind, pi_values)
  colnames(pi_matrix) <- c("pi_1", "pi_2", "pi_3")

  pi_df <- cbind(panel_data[, c("ID", "w")], pi_matrix) |>
    as_tibble()

  # Returning data
  return(list(
    data = panel_data,
    true_betas = betas,
    pi_values = pi_df,
    metadata = list(
      n_subjects = n_subjects,
      n_waves = n_waves,
      scenario = scenario,
      completition_time = Sys.time(),
      seed = seed
    )
  ))
}
