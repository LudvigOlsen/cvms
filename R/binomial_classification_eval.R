# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

binomial_classification_eval <- function(data,
                                         predictions_col,
                                         targets_col,
                                         id_col = NULL,
                                         id_method = NULL,
                                         fold_info_cols = list(
                                           rel_fold = "rel_fold",
                                           abs_fold = "abs_fold",
                                           fold_column = "fold_column"),
                                         models = NULL,
                                         cutoff = 0.5,
                                         positive = 2,
                                         metrics,
                                         include_fold_columns = TRUE,
                                         include_predictions = TRUE,
                                         na.rm = TRUE){
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

    if (isTRUE(include_predictions)){

      # Nest predictions and targets
      predictions_nested <- nesting_predictions_binomial(
        data = data,
        predictions_col = predictions_col,
        targets_col = targets_col,
        id_col = id_col,
        id_method = id_method,
        fold_info_cols = fold_info_cols,
        include_fold_columns = include_fold_columns
      )

    } else {

      predictions_nested <- NULL

    }

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
        models = models,
        metrics = metrics,
        include_fold_columns = include_fold_columns,
        include_predictions = include_predictions,
        na.rm = na.rm
      )

  } else {

    results <- binomial_classification_NA_results_tibble(
      metrics = metrics, include_predictions = include_predictions)

    if (!is.null(models))
      results[["Coefficients"]] <- get_nested_model_coefficients(
        models = NULL, include_fold_columns = include_fold_columns)

    if (length(unique_fold_cols) > 1){
      results[["Results"]] <- NA
    }
  }

  return(results)

}

