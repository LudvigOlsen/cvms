# Functions for nesting predictions

nesting_predictions_gaussian <- function(data,
                                         predictions_col,
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
    legacy_nest(1:ncol(predictions_for_nesting)) %>%
    dplyr::rename(predictions = data)

  predictions_nested
}


nesting_predictions_binomial <- function(data,
                                         predictions_col,
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
                  .data$predicted_class
    ) %>%
    dplyr::rename(Fold = fold_info_cols[["rel_fold"]],
                  `Fold Column` = fold_info_cols[["fold_column"]],
                  Target = !! as.name(targets_col),
                  Prediction = !! as.name(predictions_col),
                  `Predicted Class` = .data$predicted_class)

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
    legacy_nest(1:ncol(predictions_for_nesting)) %>%
    dplyr::rename(predictions = data)

  predictions_nested

}


nesting_predictions_multinomial <- function(data,
                                            predictions_col,
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
                  .data$predicted_class
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
                  `Predicted Class` = .data$predicted_class
    )

  # Remove fold columns if they should not be included
  if (!isTRUE(include_fold_columns)){
    predictions_for_nesting <- predictions_for_nesting %>%
      dplyr::select(-dplyr::one_of("Fold","Fold Column"))
  }

  # Nest predictions
  predictions_nested <- predictions_for_nesting %>%
    legacy_nest(1:ncol(predictions_for_nesting)) %>%
    dplyr::rename(predictions = data)

  predictions_nested

}
