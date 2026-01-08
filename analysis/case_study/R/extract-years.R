extract_years <- function(
  data,
  years,
  impute = TRUE,
  cog_total = FALSE,
  absorbing = TRUE
) {
  # Extracts cognitive function data for specified years from a dataset.
  # Converts numeric cognitive status codes (1, 2, 3) into descriptive labels
  # ("Normal Cognition", "MCI", "Dementia") for easier interpretation.
  # Arguments:
  #   - data: The input dataset containing cognitive function data.
  #   - years: A vector of years for which data should be extracted.
  # Returns:
  #   - A dataset with ID and cognitive status columns for the specified years.

  # Create dynamic column names based on the years provided
  cogfunction_cols <- paste0("cogfunction", years)
  cogtotal_cols <- paste0("cogtot27_imp", years)

  if (cog_total == FALSE) {
    data <- data |>
      # Select only the ID column and cognitive function columns for the specified years
      dplyr::select(ID, dplyr::any_of(cogfunction_cols)) |>
      dplyr::mutate(across(
        !c(ID),
        ~ dplyr::case_when(
          .x == 1 ~ "Normal Cognition",
          .x == 2 ~ "MCI",
          .x == 3 ~ "Dementia",
          TRUE ~ NA_character_ # To handle missing/other cases
        )
      ))
  } else {
    data <- data |>
      # Select only the ID column and cognitive function columns for the specified years
      dplyr::select(ID, any_of(cogtotal_cols)) |>
      dplyr::rename_with(
        .cols = !ID,
        .fn = ~ stringr::str_replace(
          string = .x,
          pattern = "cogtot27_imp",
          replacement = "cog_score_"
        )
      )
  }

  if (impute == TRUE) {
    data <- data |>
      tidyr::pivot_longer(
        cols = !ID,
        names_to = "Wave",
        values_to = "Status"
      ) |>
      dplyr::group_by(ID) |>
      tidyr::fill(Status, .direction = "down") |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(names_from = "Wave", values_from = "Status")
  }

  if (absorbing == TRUE) {
    data <- data |>
      tidyr::pivot_longer(
        cols = !ID,
        names_to = "Wave",
        values_to = "Status"
      ) |>
      dplyr::group_by(ID) |>
      dplyr::mutate(
        Status = ifelse(
          dplyr::cumany(Status == "Dementia" & !is.na(Status)),
          "Dementia",
          Status
        )
      ) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(names_from = "Wave", values_from = "Status")
  }

  return(data)
}
