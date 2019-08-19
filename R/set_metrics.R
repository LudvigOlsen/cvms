
# Returns a list of metric names
# With default values unless
set_metrics <- function(family_, metrics_list = NULL){

  if (family_ == "gaussian"){

    default_metrics <- list(
      "RMSE" = TRUE,
      "MAE" = TRUE,
      "r2m" = TRUE,
      "r2c" = TRUE,
      "AIC" = TRUE,
      "AICc" = TRUE,
      "BIC" = TRUE
    )

  } else if (family_ == "binomial"){

    default_metrics <- list(
      "Balanced Accuracy" = TRUE,
      "Accuracy" = FALSE,
      "F1" = TRUE,
      "Sensitivity" = TRUE,
      "Specificity" = TRUE,
      "Pos Pred Value" = TRUE,
      "Neg Pred Value" = TRUE,
      "AUC" = TRUE,
      "Lower CI" = TRUE,
      "Upper CI" = TRUE,
      "Kappa" = TRUE,
      "MCC" = TRUE,
      "Detection Rate" = TRUE,
      "Detection Prevalence" = TRUE,
      "Prevalence" = TRUE
    )

  } else if (family_ == "multinomial"){

    default_metrics <- list(
      "Overall Accuracy" = TRUE,
      "Balanced Accuracy" = TRUE,
      "Accuracy" = FALSE,
      "F1" = TRUE,
      "Sensitivity" = TRUE,
      "Specificity" = TRUE,
      "Pos Pred Value" = TRUE,
      "Neg Pred Value" = TRUE,
      "AUC" = TRUE,
      "Lower CI" = TRUE,
      "Upper CI" = TRUE,
      "Kappa" = TRUE,
      "MCC" = TRUE,
      "Detection Rate" = TRUE,
      "Detection Prevalence" = TRUE,
      "Prevalence" = TRUE
    ) # TODO Add weigted macro metrics?

  }

  metrics <- default_metrics

  if (!is.null(metrics_list) && length(metrics_list) > 0){

    # Check for unknown metric names
    unknown_metric_names <- setdiff(names(metrics_list), names(metrics))
    if (length(unknown_metric_names) > 0) {
      stop(paste0(
        "'metrics_list' contained unknown metric names: ",
        unknown_metric_names,
        "."
      ))
    }

    # Update metrics as specified by user
    for (met in seq_along(metrics_list)){
      if (is.null(metrics_list[[met]])){
        stop("metrics in 'metrics_list' should be logical (TRUE/FALSE) not NULL.")
      }
      metrics[[names(metrics_list)[[met]]]] <- metrics_list[[met]]
    }

  }

  # Extract the metric names
  # We need to provide these,
  # as the whole conversion below adds dots instead of spaces
  metric_names <- names(metrics)

  # Extract and return names of the metrics set to TRUE
  dplyr::as_tibble(
    t(data.frame(metrics)),
    rownames = "metric", .name_repair = ~paste0("include")) %>%
    dplyr::mutate(metric = metric_names) %>%
    dplyr::filter(.data$include) %>%
    dplyr::pull(.data$metric)

}

