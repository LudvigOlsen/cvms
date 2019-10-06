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

    # Get nesting function
    if (type == "gaussian"){
      nest_fn <- nesting_predictions_gaussian
    } else if (type == "binomial"){
      nest_fn <- nesting_predictions_binomial
    } else if (type == "multinomial"){
      nest_fn <- nesting_predictions_multinomial
    }

    # Nest predictions and targets
    predictions_nested <- nest_fn(
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



nesting_predictions_gaussian <- function(data,
                                         predictions_col,
                                         predicted_class_col = NULL, # not used
                                         targets_col,
                                         id_col,
                                         id_method,
                                         fold_info_cols,
                                         include_fold_columns){

  predictions_for_nesting <- tibble::as_tibble(data) %>%
    dplyr::select(!! as.name(fold_info_cols[["fold_column"]]),
                  !! as.name(fold_info_cols[["rel_fold"]]),
                  !! as.name(targets_col),
                  !! as.name(predictions_col)
    ) %>%
    dplyr::rename(Fold = fold_info_cols[["rel_fold"]],
                  `Fold Column` = fold_info_cols[["fold_column"]],
                  Target = !! as.name(targets_col),
                  Prediction = !! as.name(predictions_col)
    )

  # If ID evaluation, add ID and method to nested predictions
  if (!is.null(id_col)){
    if (is.null(id_method))
      stop("when 'id_col' is specified, 'id_method' must be specified as well.")

    predictions_for_nesting[[id_col]] <- data[[id_col]]
    predictions_for_nesting[["id_method"]] <- id_method

  }

  if (!isTRUE(include_fold_columns)){
    predictions_for_nesting <- predictions_for_nesting %>%
      dplyr::select(-dplyr::one_of("Fold", "Fold Column"))
  }

  predictions_nested <- predictions_for_nesting %>%
    legacy_nest(seq_len(ncol(predictions_for_nesting))) %>%
    dplyr::rename(predictions = data)

  predictions_nested
}


nesting_predictions_binomial <- function(data,
                                         predictions_col,
                                         predicted_class_col,
                                         targets_col,
                                         id_col,
                                         id_method,
                                         fold_info_cols,
                                         include_fold_columns
                                         ){

  # Nest predictions and targets
  predictions_for_nesting <- tibble::as_tibble(data) %>%
    dplyr::select(!! as.name(fold_info_cols[["fold_column"]]),
                  !! as.name(fold_info_cols[["rel_fold"]]),
                  !! as.name(targets_col),
                  !! as.name(predictions_col),
                  !! as.name(predicted_class_col)
    ) %>%
    dplyr::rename(Fold = fold_info_cols[["rel_fold"]],
                  `Fold Column` = fold_info_cols[["fold_column"]],
                  Target = !! as.name(targets_col),
                  Prediction = !! as.name(predictions_col),
                  `Predicted Class` = !! as.name(predicted_class_col))

  # If ID evaluation, add ID and method to nested predictions
  if (!is.null(id_col)){
    if (is.null(id_method))
      stop("when 'id_col' is specified, 'id_method' must be specified as well.")

    predictions_for_nesting[[id_col]] <- data[[id_col]]
    predictions_for_nesting[["id_method"]] <- id_method

  }

  if (!isTRUE(include_fold_columns)){
    predictions_for_nesting <- predictions_for_nesting %>%
      dplyr::select(-dplyr::one_of("Fold","Fold Column"))
  }

  # Nest predictions
  predictions_nested <- predictions_for_nesting %>%
    legacy_nest(seq_len(ncol(predictions_for_nesting))) %>%
    dplyr::rename(predictions = data)

  predictions_nested

}


nesting_predictions_multinomial <- function(data,
                                            predictions_col,
                                            predicted_class_col,
                                            targets_col,
                                            id_col,
                                            id_method,
                                            fold_info_cols,
                                            include_fold_columns){

  # Prepare predictions and targets for nesting
  # TODO This should not be included in class level results, only for average metrics
  predictions_for_nesting <- tibble::as_tibble(data) %>%
    dplyr::select(!! as.name(fold_info_cols[["fold_column"]]),
                  !! as.name(fold_info_cols[["rel_fold"]]),
                  !! as.name(targets_col),
                  !! as.name(predictions_col),
                  !! as.name(predicted_class_col)
    )

  # If ID evaluation, add ID and method to nested predictions
  if (!is.null(id_col)){
    if (is.null(id_method))
      stop("when 'id_col' is specified, 'id_method' must be specified as well.")

    predictions_for_nesting[[id_col]] <- data[[id_col]]
    predictions_for_nesting[["id_method"]] <- id_method

  }

  # Rename some columns and nest, nest, nest
  predictions_for_nesting <- predictions_for_nesting %>%
    dplyr::rename(Fold = fold_info_cols[["rel_fold"]],
                  `Fold Column` = fold_info_cols[["fold_column"]],
                  Target = !! as.name(targets_col),
                  Prediction = !! as.name(predictions_col),
                  `Predicted Class` = !! as.name(predicted_class_col)
    )

  # Remove fold columns if they should not be included
  if (!isTRUE(include_fold_columns)){
    predictions_for_nesting <- predictions_for_nesting %>%
      dplyr::select(-dplyr::one_of("Fold","Fold Column"))
  }

  # Nest predictions
  predictions_nested <- predictions_for_nesting %>%
    legacy_nest(seq_len(ncol(predictions_for_nesting))) %>%
    dplyr::rename(predictions = data)

  predictions_nested

}
