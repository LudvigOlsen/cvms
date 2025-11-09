evaluate_predictions_gaussian <- function(
  data,
  prediction_col,
  target_col,
  model_was_null_col,
  id_col,
  id_method,
  fold_info_cols = list(
    rel_fold = "rel_fold",
    abs_fold = "abs_fold",
    fold_column = "fold_column"
  ),
  fold_and_fold_col = NULL,
  group_info = NULL,
  stds_col = NULL,
  model_specifics,
  metrics,
  include_fold_columns,
  include_predictions,
  na.rm = TRUE
) {
  if (is.null(fold_and_fold_col)) {
    # Map of fold column, abs_fold and rel_fold
    fold_and_fold_col <- create_fold_and_fold_column_map(data, fold_info_cols)
  }

  # Nest predictions, will be NA
  # if any model_was_null is TRUE and
  # include_predictions is TRUE
  # If include_predictions is FALSE,
  # will always return NULL
  predictions_nested <- nest_predictions(
    data = data,
    prediction_col = prediction_col,
    target_col = target_col,
    model_was_null_col = model_was_null_col,
    type = "gaussian",
    id_col = id_col,
    id_method = id_method,
    stds_col = stds_col,
    fold_info_cols = fold_info_cols,
    group_info = group_info,
    include_fold_columns = include_fold_columns,
    include_predictions = include_predictions
  )

  # Group the data frame to prepare for calculating RMSE and MAE
  data <- data %>%
    dplyr::group_by(
      !!as.name(fold_info_cols[["abs_fold"]]),
      !!as.name(fold_info_cols[["fold_column"]]),
      !!as.name(fold_info_cols[["rel_fold"]])
    )

  # Calculate residual errors
  results_per_fold <- call_evaluate_residuals(
    data = data,
    target_col = target_col,
    prediction_col = prediction_col,
    metrics = metrics,
    return_nas = any(data[[model_was_null_col]])
  )

  if (!is.null(results_per_fold)) {
    # Average metrics by first fold column then fold
    avg_results <- mean_residual_errors_by_fcol_fold(
      metrics_per_folds = results_per_fold,
      fold_info_cols = fold_info_cols,
      metrics = metrics,
      na.rm = na.rm
    )
  } else {
    avg_results <- NULL
  }

  # Rename the fold info columns
  # And nest fold results in avg results
  if (!is.null(results_per_fold)) {
    results_per_fold <- results_per_fold %>%
      dplyr::ungroup()
    results_per_fold <- results_per_fold[
      order(results_per_fold[[fold_info_cols[["abs_fold"]]]],
        method = "radix"
      ),
    ]

    # Remove abs_fold
    results_per_fold[[fold_info_cols[["abs_fold"]]]] <- NULL

    # Rename columns (faster than dplyr::rename)
    results_per_fold <- base_rename(results_per_fold,
      before = fold_info_cols[["fold_column"]],
      after = "Fold Column"
    )
    results_per_fold <- base_rename(results_per_fold,
      before = fold_info_cols[["rel_fold"]],
      after = "Fold"
    )

    if (!is.null(avg_results)) {
      # nest fold results and add to result tibble
      avg_results[["Results"]] <- nest_results(results_per_fold)[["results"]]
    } else {
      avg_results <- tibble::tibble(
        "Results" = nest_results(results_per_fold)[["results"]]
      )
    }
  }

  if (!is.null(predictions_nested)) {
    if (!is.null(avg_results)) {
      if (is.na(predictions_nested)) {
        avg_results[["Predictions"]] <- NA
      } else {
        avg_results[["Predictions"]] <- predictions_nested[["predictions"]]
      }
    } else {
      if (is.na(predictions_nested)) {
        avg_results <- tibble::tibble(
          "Predictions" = NA
        )
      } else {
        avg_results <- tibble::tibble(
          "Predictions" = predictions_nested[["predictions"]]
        )
      }
    }
  }

  # Add process information
  avg_results[["Process"]] <- list(
    process_info_gaussian(
      data = data,
      target_col = target_col,
      prediction_cols = prediction_col,
      id_col = model_specifics[["for_process"]][["id_col"]]
    )
  )

  avg_results
}


mean_residual_errors_by_fcol_fold <- function(metrics_per_folds,
                                              fold_info_cols,
                                              metrics,
                                              na.rm) {
  metrics <- intersect(metrics, colnames(metrics_per_folds))

  # First average per fold column, then average those
  metrics_per_folds %>%
    base_select(c(fold_info_cols[["fold_column"]], metrics)) %>%
    dplyr::group_by(!!as.name(fold_info_cols[["fold_column"]])) %>%
    dplyr::summarize_all(~ mean(., na.rm = na.rm)) %>%
    base_deselect(fold_info_cols[["fold_column"]]) %>%
    dplyr::summarize_all(~ mean(., na.rm = na.rm))
}
