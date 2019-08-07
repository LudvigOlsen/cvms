# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

binomial_classification_eval <- function(data,
                                         predictions_col,
                                         targets_col,
                                         fold_info_cols = list(
                                           rel_fold = "rel_fold",
                                           abs_fold = "abs_fold",
                                           fold_column = "fold_column"),
                                         models = NULL,
                                         cutoff = 0.5,
                                         positive = 2){
  # Note: predictions are floats (e.g. 0.7), targets are 0 or 1

  # Check if there are NAs in predictions
  na_in_predictions <- sum(is.na(data[[predictions_col]])) > 0
  na_in_targets <- sum(is.na(data[[targets_col]])) > 0

  # Map of fold column, abs_fold and rel_fold
  fold_and_fold_col <- create_fold_and_fold_column_map(data, fold_info_cols)

  # Unique fold columns
  unique_fold_cols <- unique(fold_and_fold_col[["fold_column"]])

  if (!na_in_targets && !na_in_predictions){

    # Find the levels in the categorical target variable
    cat_levels = levels_as_characters(data[[targets_col]])

    if (length(cat_levels) > 2){ stop("The target column must maximally contain 2 levels.") }

    # Create a column with the predicted class based on the chosen cutoff
    data[["predicted_class"]] <- ifelse(data[[predictions_col]] < cutoff, cat_levels[1], cat_levels[2])

    # Nest predictions and targets
    predictions_nested <- tibble::as_tibble(data) %>%
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
                    `Predicted Class` = .data$predicted_class
                    ) %>%
      tidyr::nest(1:5) %>%
      dplyr::rename(predictions = data)

    results <-
      binomial_eval(
        data = data,
        targets_col = targets_col,
        predictions_col = predictions_col,
        predicted_class_col = "predicted_class",
        unique_fold_cols = unique_fold_cols,
        cat_levels = cat_levels,
        positive = positive,
        fold_info_cols = fold_info_cols,
        fold_and_fold_col = fold_and_fold_col,
        predictions_nested = predictions_nested,
        models = models
      )

  } else {

    results <- binomial_classification_NA_results_tibble()

    if (!is.null(models))
      results[["Coefficients"]] <- get_nested_model_coefficients(NULL)

    if (length(unique_fold_cols) > 1){
      results[["Results"]] <- NA
    }
  }

  return(results)

}

