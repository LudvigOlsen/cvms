# Functions for nesting predictions

nest_predictions <- function(data,
                             predictions_col,
                             predicted_class_col = NULL,
                             targets_col,
                             model_was_null_col,
                             type,
                             id_col,
                             id_method,
                             fold_info_cols,
                             include_fold_columns,
                             include_predictions){

  if (!any(data[[model_was_null_col]]) &&
      isTRUE(include_predictions)) {

    # Nest predictions and targets
    predictions_nested <- nesting_predictions(
      data = data,
      predictions_col = predictions_col,
      predicted_class_col = predicted_class_col,
      targets_col = targets_col,
      id_col = id_col,
      id_method = id_method,
      fold_info_cols = fold_info_cols,
      include_fold_columns = include_fold_columns)

  } else {

    if (isTRUE(include_predictions)) {
      predictions_nested <- NA
    } else {
      predictions_nested <- NULL
    }

  }

  predictions_nested

}


nesting_predictions<- function(data,
                               predictions_col,
                               predicted_class_col = NULL,
                               # not used
                               targets_col,
                               id_col,
                               id_method,
                               fold_info_cols,
                               include_fold_columns) {

  predictions_for_nesting <- tibble::tibble(
    "Fold Column" = data[[fold_info_cols[["fold_column"]]]],
    "Fold" = data[[fold_info_cols[["rel_fold"]]]],
    "Target" = data[[targets_col]],
    "Prediction" = data[[predictions_col]]
  )

  if (!is.null(predicted_class_col)){
    predictions_for_nesting[["Predicted Class"]] <- data[[predicted_class_col]]
  }

  if ("observation" %in% names(data)){
    predictions_for_nesting <- predictions_for_nesting %>%
      tibble::add_column("Observation" = data[["observation"]],
                         .before = "Target")
  }

  # If ID evaluation, add ID and method to nested predictions
  if (!is.null(id_col)){
    if (is.null(id_method))
      stop("when 'id_col' is specified, 'id_method' must be specified as well.")

    predictions_for_nesting[[id_col]] <- data[[id_col]]
    predictions_for_nesting[["id_method"]] <- id_method

  }

  if (!isTRUE(include_fold_columns)){
    predictions_for_nesting[["Fold"]] <- NULL
    predictions_for_nesting[["Fold Column"]] <- NULL
  }

  predictions_nested <- predictions_for_nesting %>%
    dplyr::group_nest(.key = "predictions")

  predictions_nested
}
