# Functions for nesting predictions

nest_predictions <- function(data,
                             prediction_col,
                             predicted_class_col = NULL,
                             target_col,
                             model_was_null_col,
                             type,
                             id_col,
                             id_method,
                             stds_col,
                             fold_info_cols,
                             group_info,
                             include_fold_columns,
                             include_predictions) {
  if (!any(data[[model_was_null_col]]) &&
    isTRUE(include_predictions)) {

    # Nest predictions and targets
    predictions_nested <- nesting_predictions(
      data = data,
      prediction_col = prediction_col,
      predicted_class_col = predicted_class_col,
      target_col = target_col,
      id_col = id_col,
      id_method = id_method,
      stds_col = stds_col,
      fold_info_cols = fold_info_cols,
      group_info = group_info,
      include_fold_columns = include_fold_columns
    )
  } else {
    if (isTRUE(include_predictions)) {
      predictions_nested <- NA
    } else {
      predictions_nested <- NULL
    }
  }

  predictions_nested
}


nesting_predictions <- function(data,
                                prediction_col,
                                predicted_class_col = NULL,
                                # not used
                                target_col,
                                id_col,
                                id_method,
                                stds_col,
                                fold_info_cols,
                                group_info,
                                include_fold_columns) {
  predictions_for_nesting <- tibble::tibble(
    "Fold Column" = data[[fold_info_cols[["fold_column"]]]],
    "Fold" = data[[fold_info_cols[["rel_fold"]]]],
    "Target" = data[[target_col]],
    "Prediction" = data[[prediction_col]]
  )

  if (!is.null(stds_col))
    predictions_for_nesting[["SD"]] <- data[[stds_col]]

  if (!is.null(group_info)) {
    predictions_for_nesting <- group_info %>%
      dplyr::bind_cols(predictions_for_nesting)
  }

  if (!is.null(predicted_class_col)) {
    predictions_for_nesting[["Predicted Class"]] <- data[[predicted_class_col]]
  }

  if ("observation" %in% names(data)) {
    predictions_for_nesting <- predictions_for_nesting %>%
      tibble::add_column(
        "Observation" = data[["observation"]],
        .before = "Target"
      )
  }

  # If ID evaluation, add ID and method to nested predictions
  if (!is.null(id_col)) {
    if (is.null(id_method)) {
      stop("when 'id_col' is specified, 'id_method' must be specified as well.")
    }

    predictions_for_nesting[[id_col]] <- data[[id_col]]
    predictions_for_nesting[["id_method"]] <- id_method

  }

  if (!isTRUE(include_fold_columns)) {
    predictions_for_nesting[["Fold"]] <- NULL
    predictions_for_nesting[["Fold Column"]] <- NULL
  }

  predictions_nested <- predictions_for_nesting %>%
    dplyr::group_nest(.key = "predictions")

  predictions_nested
}
