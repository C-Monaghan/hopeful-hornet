highlight_true <- function(distances, true_model = NULL, usage = NULL) {
  if(usage == "Parent") {
    distances |>
      mutate(
        fill = case_when(
          parent_block == paste0(true_model, " Models") & sub_block == "True Model" ~ "True",
          parent_block == "Base Models" ~ "Base Models",
          parent_block == "Additive Models" ~ "Additive Models",
          parent_block == "Multiplicative Models" ~  "Multiplicative Models"
        ),
        fill = factor(
          fill, 
          levels = c("Base Models", "True", "Additive Models", "Multiplicative Models"))
      )  
  } else if(usage == "Sub") {
      distances |>
      mutate(
        fill = case_when(
          parent_block == paste0(true_model, " Models") & sub_block == "True Model" ~ "True Model",
          sub_block == "Null Model" ~ "Null Model",
          sub_block == "Reduced Model 1" ~ "Reduced Model 1",
          sub_block == "Reduced Model 2" ~ "Reduced Model 2",
          sub_block == "True Model" ~ "Other",
          sub_block == "Overfit Model" ~ "Overfit Model"
        ),
        fill = factor(
          fill,
          levels = c("Null Model", "Reduced Model 1", "Reduced Model 2", "True Model", "Other", "Overfit Model")
        )
      )
    }
}