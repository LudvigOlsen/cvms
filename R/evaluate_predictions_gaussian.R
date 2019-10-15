evaluate_predictions_gaussian <- function(data,
                                          predictions_col,
                                          targets_col,
                                          model_was_null_col,
                                          id_col,
                                          id_method,
                                          fold_info_cols = list(rel_fold = "rel_fold",
                                                                abs_fold = "abs_fold",
                                                                fold_column = "fold_column"),
                                          model_specifics,
                                          metrics,
                                          include_fold_columns,
                                          include_predictions,
                                          na.rm = TRUE) {

  # Create fold and fold column map
  fold_and_fold_col <- create_fold_and_fold_column_map(data, fold_info_cols)

  # Nest predictions, will be NA
  # if any model_was_null is TRUE and
  # include_predictions is TRUE
  # If include_predictions is FALSE,
  # will always return NULL
  predictions_nested <- nest_predictions(
    data = data,
    predictions_col = predictions_col,
    targets_col = targets_col,
    model_was_null_col = model_was_null_col,
    type = "gaussian",
    id_col = id_col,
    id_method = id_method,
    fold_info_cols = fold_info_cols,
    include_fold_columns = include_fold_columns,
    include_predictions = include_predictions)


  # Group the data frame to prepare for calculating RMSE and MAE
  data <- data %>%
    dplyr::group_by(!!as.name(fold_info_cols[["abs_fold"]]),
                    !!as.name(fold_info_cols[["fold_column"]]),
                    !!as.name(fold_info_cols[["rel_fold"]]))

  # Calculate RMSE
  rmse_results <- calculate_gaussian_prediction_metric(
    data = data,
    predictions_col = predictions_col,
    targets_col = targets_col,
    model_was_null_col = model_was_null_col,
    metric_fn = calculate_RMSE,
    metric_name = "RMSE",
    metrics = metrics,
    fold_info_cols = fold_info_cols,
    fold_and_fold_col = fold_and_fold_col,
    na.rm = na.rm)

  rmse_per_fold <- rmse_results[["metric_per_fold"]]
  avg_rmse <- rmse_results[["avg_metric"]]

  # Calculate MAE
  mae_results <- calculate_gaussian_prediction_metric(
    data = data,
    predictions_col = predictions_col,
    targets_col = targets_col,
    model_was_null_col = model_was_null_col,
    metric_fn = calculate_MAE,
    metric_name = "MAE",
    metrics = metrics,
    fold_info_cols = fold_info_cols,
    fold_and_fold_col = fold_and_fold_col,
    na.rm = na.rm)

  mae_per_fold <- mae_results[["metric_per_fold"]]
  avg_mae <- mae_results[["avg_metric"]]

  data <- data %>% dplyr::ungroup()

  # Join the metrics
  if (!is.null(rmse_per_fold) && !is.null(mae_per_fold)){

    results_per_fold <- dplyr::full_join(rmse_per_fold, mae_per_fold,
                                         by = c(fold_info_cols[["abs_fold"]],
                                                fold_info_cols[["fold_column"]],
                                                fold_info_cols[["rel_fold"]]))
  } else if (!is.null(rmse_per_fold)){
    results_per_fold <- rmse_per_fold
  } else if (!is.null(mae_per_fold)){
    results_per_fold <- mae_per_fold
  } else {
    results_per_fold <- NULL
  }

  # Join the average metrics
  if (is.null(avg_rmse) && is.null(avg_mae)){
    avg_results <- NULL
  } else {
    avg_results <- dplyr::as_tibble(dplyr::bind_cols(avg_rmse, avg_mae))
  }

  # Rename the fold info columns
  # And nest fold results in avg results
  if (!is.null(results_per_fold)){

    results_per_fold <- results_per_fold %>%
      dplyr::ungroup() %>%
      dplyr::rename(`Fold Column` = fold_info_cols[["fold_column"]],
                    Fold = fold_info_cols[["rel_fold"]]) %>%
      dplyr::arrange(.data$abs_fold) %>%
      dplyr::select(-.data$abs_fold)

    if (!is.null(avg_results)){
      # nest fold results and add to result tibble
      avg_results[["Results"]] <- nest_results(results_per_fold)[["results"]]
    } else {
      avg_results <- tibble::tibble(
        "Results" = nest_results(results_per_fold)[["results"]])
    }

  }

  if (!is.null(predictions_nested)){
    if (!is.null(avg_results)){
      if (is.na(predictions_nested)){
        avg_results[["Predictions"]] <- NA
      } else {
        avg_results[["Predictions"]] <- predictions_nested[["predictions"]]
      }

    } else {

      if (is.na(predictions_nested)){
        avg_results <- tibble::tibble(
          "Predictions" = NA)
      } else {
        avg_results <- tibble::tibble(
          "Predictions" = predictions_nested[["predictions"]])
      }
    }
  }

  avg_results

}


calculate_gaussian_prediction_metric <- function(data,
                                        predictions_col,
                                        targets_col,
                                        model_was_null_col,
                                        metric_fn,
                                        metric_name,
                                        metrics,
                                        fold_info_cols,
                                        fold_and_fold_col,
                                        na.rm){

  if (any(data[[model_was_null_col]])){
    # If one or more of the model objects were NULL
    # Return NA results
    return(
      calculate_gaussian_prediction_metric_NA(data = data,
                                     metric_name = metric_name,
                                     metrics = metrics,
                                     fold_info_cols = fold_info_cols,
                                     fold_and_fold_col = fold_and_fold_col)
    )
  }

  if (metric_name %in% metrics) {
    # Calculate the metric per fold
    metric_per_fold <- data %>%
      dplyr::summarize(m = metric_fn(!!as.name(predictions_col), !!as.name(targets_col)))

    # First average per fold column, then average those
    avg_metric <- metric_per_fold %>%
      dplyr::group_by(!!as.name(fold_info_cols[["fold_column"]])) %>%
      dplyr::summarize(m = mean(.data$m, na.rm = na.rm)) %>%
      dplyr::summarize(m = mean(.data$m, na.rm = na.rm))

    # Set colnames to the metric name
    names(metric_per_fold)[names(metric_per_fold) == "m"] <- metric_name
    names(avg_metric)[names(avg_metric) == "m"] <- metric_name

  } else {
    metric_per_fold <- NULL
    avg_metric <- NULL
  }

  list("metric_per_fold" = metric_per_fold,
       "avg_metric" = avg_metric)

}

calculate_gaussian_prediction_metric_NA <- function(data,
                                           metric_name,
                                           metrics,
                                           fold_info_cols,
                                           fold_and_fold_col){

  if (metric_name %in% metrics) {

    # Find number of columns
    num_folds <- length(unique(data[[ fold_info_cols[["abs_fold"]] ]]))

    # NA metric per fold
    metric_per_fold <- tibble::tibble("metric" = rep(NA, num_folds))
    colnames(metric_per_fold) <- metric_name

    metric_per_fold[[fold_info_cols[["fold_column"]]]] <- fold_and_fold_col[[fold_info_cols[["fold_column"]]]]
    metric_per_fold[[fold_info_cols[["abs_fold"]]]] <- fold_and_fold_col[[fold_info_cols[["abs_fold"]]]]
    metric_per_fold[[fold_info_cols[["rel_fold"]]]] <- fold_and_fold_col[[fold_info_cols[["rel_fold"]]]]

    # Average NA metric
    avg_metric <- tibble::tibble("metric" = NA)
    colnames(avg_metric) <- metric_name

  } else {
    metric_per_fold <- NULL
    avg_metric <- NULL
  }

  list("metric_per_fold" = metric_per_fold,
       "avg_metric" = avg_metric)

}
