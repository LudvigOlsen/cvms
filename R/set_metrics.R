
# Returns a list of metric names
# With default values unless
set_metrics <- function(family, metrics_list = NULL){

  if (family == "gaussian"){

    default_metrics <- list(
      "RMSE" = TRUE,
      "MAE" = TRUE,
      "r2m" = TRUE,
      "r2c" = TRUE,
      "AIC" = TRUE,
      "AICc" = TRUE,
      "BIC" = TRUE
    )

  } else if (family == "binomial"){

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

  } else if (family == "multinomial"){

    default_metrics <- list(
      "Overall Accuracy" = TRUE,
      "Balanced Accuracy" = TRUE,
      "Weighted Balanced Accuracy" = FALSE,
      "Accuracy" = FALSE,
      "Weighted Accuracy" = FALSE,
      "F1" = TRUE,
      "Weighted F1" = FALSE,
      "Sensitivity" = TRUE,
      "Weighted Sensitivity" = FALSE,
      "Specificity" = TRUE,
      "Weighted Specificity" = FALSE,
      "Pos Pred Value" = TRUE,
      "Weighted Pos Pred Value" = FALSE,
      "Neg Pred Value" = TRUE,
      "Weighted Neg Pred Value" = FALSE,
      "AUC" = TRUE,
      "Weighted AUC" = FALSE,
      "Lower CI" = TRUE,
      "Weighted Lower CI" = FALSE,
      "Upper CI" = TRUE,
      "Weighted Upper CI" = FALSE,
      "Kappa" = TRUE,
      "Weighted Kappa" = FALSE,
      "MCC" = TRUE,
      "Weighted MCC" = FALSE,
      "Detection Rate" = TRUE,
      "Weighted Detection Rate" = FALSE,
      "Detection Prevalence" = TRUE,
      "Weighted Detection Prevalence" = FALSE,
      "Prevalence" = TRUE,
      "Weighted Prevalence" = FALSE
    )

  }

  metrics <- default_metrics

  if (!is.list(metrics_list) && metrics_list == "all"){

    # Set all metrics to TRUE
    for (met in seq_along(metrics)){
      metrics[[met]] <- TRUE
    }

  } else if (!is.null(metrics_list) && length(metrics_list) > 0){

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

