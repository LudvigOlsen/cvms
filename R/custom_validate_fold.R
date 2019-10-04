
custom_validate_fold <- function(data,
                                 fold_info,
                                 evaluation_type = "gaussian",
                                 model_specifics = list(),
                                 model_specifics_update_fn = NULL,
                                 metrics,
                                 fold_cols = ".folds"){

  if (evaluation_type %ni% c("gaussian","binomial","multinomial")){
    stop("evaluation_type must be either 'gaussian', 'binomial', or 'multinomial'.")
  }

  # Check arguments
  # Check model_specifics arguments
  if (!is.null(model_specifics_update_fn)){
    model_specifics <- model_specifics_update_fn(model_specifics)
  }

  # Extract train and test sets
  data_subset <- subset_data(data = data,
                             fold_info = fold_info,
                             fold_cols = fold_cols)
  train_data <- data_subset[["train"]]
  test_data <- data_subset[["test"]]

  # Fit model, predict test set, and collect warnings
  fitted_model_process <- fit_predict_model_fn(
    train_data = train_data,
    test_data = test_data,
    fold_info = fold_info,
    model_specifics = model_specifics
  )

  # Extract models
  model <- fitted_model_process[["model"]]

  # Extract predictions and targets
  predictions_and_targets <- fitted_model_process[["predictions_and_targets"]] %>%
    dplyr::mutate(model_was_null = is.null(model))

  # Extract warnings and messages
  warnings_and_messages <- fitted_model_process[["warnings_and_messages"]]

  # Nest warnings and messages tibble
  nested_warnings_and_messages <- warnings_and_messages %>%
    legacy_nest(seq_len(ncol(warnings_and_messages))) %>%
    dplyr::pull(.data$data)

  # Model metrics
  # Predictions will be evaluated in parent function

  model_metrics <- internal_evaluate_model(
    model = model,
    train_data = train_data,
    test_data = test_data,
    type = evaluation_type,
    fold_info = fold_info,
    model_specifics = model_specifics,
    metrics = metrics) %>%
    mutate(`Convergence Warnings` = fitted_model_process[['n_convergence_warnings']],
           `Singular Fit Messages` = fitted_model_process[['n_singular_fit_messages']],
           `Other Warnings` = fitted_model_process[['n_unknown_warnings']] +
             fitted_model_process[['n_prediction_warnings']],
           `Warnings and Messages` = nested_warnings_and_messages)

  # TODO: These should be added in parent
  # Folds = n_folds,
  # `Fold Columns` = length(fold_cols),

  list("predictions_and_targets" = predictions_and_targets,
       "model_metrics" = model_metrics)

}


subset_data <- function(data, fold_info, fold_cols){

  # Important to ensure it is character instead of factor
  # As that will match the rel_fold with the level index column instead
  current_fold_column <- as.character(fold_info[["fold_column"]])

  # Create training set for this iteration
  train_data <- data[data[[current_fold_column]] != fold_info[["rel_fold"]],]
  # Create test set for this iteration
  test_data <- data[data[[current_fold_column]] == fold_info[["rel_fold"]],]

  # Remove folds column(s) from subsets, so we can use "y ~ ." method
  # when defining the model formula.
  train_data <- train_data %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::one_of(fold_cols))

  test_data <- test_data %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::one_of(fold_cols))

  list("train" = train_data,
       "test" = test_data)
}
