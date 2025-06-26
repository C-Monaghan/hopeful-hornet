# Calculating original and estimated transitions
# Scenario 3

rm(list = ls())

# 1. Loading packages ----------------------------------------------------------
pacman::p_load(
  dplyr,
  stringr,
  purrr,
  furrr,
  progressr,
  data.table,
  this.path
)

# 2. Parallel back-end ----------------------------------------------------------
plan(multisession, workers = parallel::detectCores() - 1)

handlers(global = TRUE)
handlers("txtprogressbar")

# 3. Simulation functions ------------------------------------------------------
func_files <- list.files(
  path = here::here("R/"), pattern = "\\.R$", full.names = TRUE)

walk(func_files, source)

# 4. Reading in cached files ---------------------------------------------------
message("Reading in data files ... ")

resimulation   <- readRDS(file = file.path(this.dir(), "results/cache/resim.RDS"))
models         <- readRDS(file = file.path(this.dir(), "results/cache/models.RDS"))
pids_df        <- readRDS(file = file.path(this.dir(), "results/cache/pids.RDS"))

# 5. Compute individual transition matrices, filtered by PIDs ------------------
message("Computing individual transitions ... ")

num_tasks <- resimulation |> listr::list_flatten(max_depth = 3) |> length()

indiv_transitions <- with_progress({
  p <- progressor(steps = num_tasks)
  
  future_imap(resimulation, function(by_sub_blocks, parent) {
    future_imap(by_sub_blocks, function(by_sizes, sub_block) {
      future_imap(by_sizes, function(by_data_list, size_label) {
        future_map(by_data_list, function(df) {
          
          p()
          
          # Check if df exists
          if(is.null(df)) return(NULL)
          
          df <- tryCatch({
            df |>
              semi_join(pids_df, by = c("ID", "size_label", "rep"))
          }, error = function(e) {
            message("Joining error: ", e$message)
            return(NULL)
          })
          
          if(is.null(df) || nrow(df) == 0) return(NULL)
          
          # Calculating individual transitions (with error handling)
          tryCatch(
            create_individual_transition_matrices(data = df),
            error = function(e) {
              message("Transition error: ", e$message)
              return(NULL)
            }
          )
        }, .options = furrr_options(seed = TRUE)) # end of by_data_list
      }, .options = furrr_options(seed = TRUE)) # End of by_sizes
    }, .options = furrr_options(seed = TRUE)) # End of by_sub_blocks
  }, .options = furrr_options(seed = TRUE)) # End of resimulation
})

# Memory cleaning
rm(resimulation)

# 6. Compute matrix‐distance metrics ------------------------------------------
# Flatten all transitons into one tibble
message("Computing matrix tibble ... ")

transition_tibble <- extract_transition_pairs(
  indiv_transitions = indiv_transitions, models = models)

# Memory cleaning
rm(indiv_transitions, models, pids_df)

# Resetting to sequential processing
plan(sequential)

# 7. Exporting ----------------------------------------------------------------
message("Saving results ... ")

saveRDS(
  object = transition_tibble,
  file = file.path(this.dir(), "results/transition_tibble.RDS"))
