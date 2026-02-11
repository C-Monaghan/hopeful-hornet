tidy_metrics <- function(metrics) {
  # Ensures our object is stored as a data.table object ------------------------
  if (!is.data.table(metrics)) {
    metrics <- as.data.table(metrics)
  }

  # Renaming cells -------------------------------------------------------------
  metrics[, parent_block := str_replace(parent_block, "_", " ")]
  metrics[, parent_block := str_to_title(parent_block)]

  metrics[,
    sub_block := fcase(
      sub_block == "null_models"  , "Null Model"      ,
      sub_block == "red_1_models" , "Reduced Model 1" ,
      sub_block == "red_2_models" , "Reduced Model 2" ,
      sub_block == "true_models"  , "True Model"      ,
      sub_block == "of_models"    , "Overfit Model"   ,
      default = sub_block
    )
  ]

  metrics[, size_label := str_replace(size_label, "n_", "n = ")]
  # metrics[, wave := str_replace(wave, "w_", "Wave ")]
  metrics[, wave := str_replace(wave, "-", " to ")]
  metrics[, wave := paste0("Wave ", wave)]

  metrics[,
    metric := fcase(
      metric == "Frobenius"   , "Frobenius Distance"          ,
      metric == "Manhattan"   , "Manhattan Distance"          ,
      metric == "Max"         , "Max Difference"              ,
      metric == "MeanAbs"     , "Mean Absolute Difference"    ,
      metric == "RMSE"        , "Root Mean Square Error"      ,
      metric == "Correlation" , "Correlation Distance"        ,
      metric == "KL"          , "Kullback-Leibler Divergence" ,
      metric == "Determinent" , "Determinent"                 ,
      metric == "aic"         , "AIC"                         ,
      metric == "bic"         , "BIC"                         ,
      default = metric
    )
  ]

  # Factorise columns with explicit levels
  metrics[,
    parent_block := factor(
      parent_block,
      levels = c("Base Models", "Additive Models", "Multiplicative Models")
    )
  ]

  metrics[,
    sub_block := factor(
      sub_block,
      levels = c(
        "Null Model",
        "Reduced Model 1",
        "Reduced Model 2",
        "True Model",
        "Overfit Model"
      )
    )
  ]

  metrics[,
    size_label := factor(
      size_label,
      levels = c("n = 100", "n = 250", "n = 500", "n = 1000", "n = 5000")
    )
  ]

  metrics[,
    wave := factor(
      wave,
      levels = c("Wave 1 to 2", "Wave 2 to 3", "Wave 3 to 4")
    )
  ]

  metrics[,
    metric := factor(
      metric,
      levels = c(
        "Frobenius Distance",
        "Manhattan Distance",
        "Max Difference",
        "Mean Absolute Difference",
        "Root Mean Square Error",
        "Correlation Distance",
        "Kullback-Leibler Divergence",
        "Determinent",
        "AIC",
        "BIC"
      )
    )
  ]

  return(metrics)
}
