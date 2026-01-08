resimulate_data <- function(model_coefs, sim, scenario, num_tasks) {
  # Loading simulation function (this may not be necessary)
  source(here::here("R/simulate_data.R"))

  # Use a text-based progress bar (works in Jobs pane)
  handlers(handler_txtprogressbar(enable = TRUE))

  with_progress({
    p <- progressor(steps = num_tasks)

    results <- future_imap(
      model_coefs,
      function(by_sub_block, parent) {
        future_imap(
          by_sub_block,
          function(by_size, sub_block) {
            future_imap(
              by_size,
              function(by_beta_lists, size) {
                future_map2(
                  by_beta_lists,
                  seq_along(by_beta_lists),
                  function(betas, reps) {
                    # Update progress (with minimal text to reduce overhead)
                    p()

                    tryCatch(
                      simulate_data(
                        n_subjects = 549,
                        n_waves = 4,
                        scenario = scenario,
                        resim = TRUE,
                        og_data = sim,
                        betas = betas,
                        seed = 123,
                        verbose = FALSE
                      )$data |>
                        mutate(
                          parent_block = parent,
                          sub_block = sub_block,
                          size_label = size,
                          rep = as.character(reps)
                        ) |>
                        add_previous_status(),

                      error = function(e) {
                        message("Error in simulation (skipping): ", e$message)
                        return(NULL)
                      }
                    )
                  },
                  .options = furrr_options(seed = TRUE)
                )
              },
              .options = furrr_options(seed = TRUE)
            )
          },
          .options = furrr_options(seed = TRUE)
        )
      },
      .options = furrr_options(seed = TRUE)
    )
  })

  return(results)
}
