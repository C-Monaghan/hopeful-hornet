# Simulation Master Script - Scenario 3 ----------------------------------------

rm(list = ls())

# Load Packages ----------------------------------------------------------------
pacman::p_load(
  here,
  this.path,
  dplyr,
  stringr,
  emoji
)

# Define Scripts ---------------------------------------------------------------
scripts <- c(
  # "01__simulation.R",
  # "02__transitions.R",
  # "03a__distances.R",
  "03b__distances.R",
  "04__plotting.R"
)

# Detect Scenario from Directory Name ------------------------------------------
my_scenario <- case_when(
  str_detect(this.dir(), "scenario_1") ~ 1,
  str_detect(this.dir(), "scenario_2") ~ 2,
  str_detect(this.dir(), "scenario_3") ~ 3,
  TRUE ~ NA_real_
)

# Announce Start ---------------------------------------------------------------
message(
  emoji_glue(":robot: Starting simulation pipeline for Scenario ", my_scenario),
  "\n------------------------------------------------------------"
)

# Run Scripts ------------------------------------------------------------------
for (i in seq_along(scripts)) {
  script <- scripts[i]
  message(emoji_glue(":hourglass_flowing_sand: ", i, "/", length(scripts), " Running ", script))
  source(here(this.dir(), script))
  message(emoji_glue(":white_check_mark: Finished ", script, "\n"))
}

# Done -------------------------------------------------------------------------
message(
  emoji_glue(":tada: All steps complete!"),
  "\n------------------------------------------------------------"
)
