
# Returns a list of metric names
# With default values unless
set_metrics <- function(family, metrics_list = NULL, include_model_object_metrics = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_choice(
    x = family,
    choices = c("gaussian", "binomial", "multinomial"),
    add = assert_collection
  )

  if (!checkmate::test_string(x = metrics_list,
                              pattern = "^all$")) {
    checkmate::assert_list(
      x = metrics_list,
      types = c("logical"),
      names = "named",
      any.missing = FALSE,
      null.ok = TRUE,
      add = assert_collection
    )
  }
  checkmate::assert_flag(x = include_model_object_metrics,
                         add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (family == "gaussian") {
    default_metrics <- list(
      "RMSE" = TRUE,
      "MAE" = TRUE,
      "NRMSE(RNG)" = FALSE,
      "NRMSE(IQR)" = TRUE,
      "NRMSE(STD)" = FALSE,
      "NRMSE(AVG)" = FALSE,
      "RSE" = FALSE,
      "RRSE" = TRUE,
      "RAE" = TRUE,
      "RMSLE" = TRUE,
      "MALE" = FALSE,
      "MAPE" = FALSE,
      "MSE" = FALSE,
      "TAE" = FALSE,
      "TSE" = FALSE,
      "r2m" = FALSE,
      "r2c" = FALSE,
      "AIC" = FALSE,
      "AICc" = FALSE,
      "BIC" = FALSE
    )
  } else if (family == "binomial") {
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
      "Prevalence" = TRUE,
      "False Neg Rate" = FALSE,
      "False Pos Rate" = FALSE,
      "False Discovery Rate" = FALSE,
      "False Omission Rate" = FALSE,
      "Threat Score" = FALSE,
      "AIC" = FALSE,
      "AICc" = FALSE,
      "BIC" = FALSE
    )
  } else if (family == "multinomial") {
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
      "AUC" = FALSE,
      "Kappa" = TRUE,
      "Weighted Kappa" = FALSE,
      "MCC" = TRUE,
      "Detection Rate" = TRUE,
      "Weighted Detection Rate" = FALSE,
      "Detection Prevalence" = TRUE,
      "Weighted Detection Prevalence" = FALSE,
      "Prevalence" = TRUE,
      "Weighted Prevalence" = FALSE,
      "False Neg Rate" = FALSE,
      "Weighted False Neg Rate" = FALSE,
      "False Pos Rate" = FALSE,
      "Weighted False Pos Rate" = FALSE,
      "False Discovery Rate" = FALSE,
      "Weighted False Discovery Rate" = FALSE,
      "False Omission Rate" = FALSE,
      "Weighted False Omission Rate" = FALSE,
      "Threat Score" = FALSE,
      "Weighted Threat Score" = FALSE,
      "AIC" = FALSE,
      "AICc" = FALSE,
      "BIC" = FALSE
    )
  }

  metrics <- default_metrics

  if (!is.null(metrics_list) && length(metrics_list) > 0) {
    if (length(metrics_list) > 1 && !is.list(metrics_list)) {
      stop("'metrics' must be either a named list or 'all'.")
    }

    if (!is.list(metrics_list)) {
      if (metrics_list == "all") {
        # Set all metrics to TRUE
        metrics <- set_all(metrics, to = TRUE)
      } else {
        stop("'metrics' must be either a named list or 'all'.")
      }
    } else {

      # Check for duplicates
      if (length(unique(names(metrics_list))) != length(names(metrics_list))) {
        stop("'metrics' cannot contain duplicate names.")
      }

      # Check for unknown metric names
      unknown_metric_names <- c(setdiff(names(metrics_list), c(names(metrics), "all")))
      if (length(unknown_metric_names) > 0) {
        stop(paste0(
          "'metrics_list' contained unknown metric names: ",
          paste0(unknown_metric_names, collapse = ", "),
          "."
        ))
      }

      # Check for unknown values (Those not TRUE/FALSE)
      if (any(unlist(lapply(metrics_list, function(x) {
        !(is.logical(x) && !is.na(x))
      })))) {
        stop("The values in the 'metrics' list must be either TRUE or FALSE.")
      }

      # If "all" is a key in metrics_list
      # apply its value (TRUE/FALSE) to every element
      if ("all" %in% names(metrics_list)) {
        metrics <- set_all(metrics, to = metrics_list[["all"]])
        if (length(metrics_list) == 1) {
          metrics_list <- list()
        } else {
          metrics_list <- metrics_list %>% purrr::list_modify("all" = NULL)
        }
      }

      # Update metrics as specified by user
      for (met in seq_along(metrics_list)) {
        if (is.null(metrics_list[[met]])) {
          stop("metrics in 'metrics_list' should be logical (TRUE/FALSE) not NULL.")
        }
        metrics[[names(metrics_list)[[met]]]] <- metrics_list[[met]]
      }
    }
  }

  if (!isTRUE(include_model_object_metrics)) {

    # Remove the metrics that require model objects
    if (family == "gaussian") {
      metrics[["r2m"]] <- FALSE
      metrics[["r2c"]] <- FALSE
      metrics[["AIC"]] <- FALSE
      metrics[["AICc"]] <- FALSE
      metrics[["BIC"]] <- FALSE
    } else if (family %in% c("binomial", "multinomial")) {
      metrics[["AIC"]] <- FALSE
      metrics[["AICc"]] <- FALSE
      metrics[["BIC"]] <- FALSE
    }
  }

  # Extract and return names of the metrics set to TRUE
  names(
    which(
      sapply(metrics, function(y) isTRUE(y))
    )
  )
}

set_all <- function(metrics, to = TRUE) {
  # Set all metrics to TRUE/FALSE
  for (met in seq_along(metrics)) {
    metrics[[met]] <- to
  }
  metrics
}
