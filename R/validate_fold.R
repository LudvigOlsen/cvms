
validate_fold <- function(data,
                          fold_info,
                          fold_info_cols = fold_info_cols,
                          evaluation_type = "gaussian",
                          model_specifics = list(),
                          model_specifics_update_fn = NULL,
                          metrics,
                          fold_cols = ".folds",
                          err_nc = FALSE,
                          include_fold_columns = TRUE,
                          return_model = FALSE) {

  if (evaluation_type %ni% c("gaussian", "binomial", "multinomial")) {
    stop("evaluation_type must be either 'gaussian', 'binomial', or 'multinomial'.")
  }

  # Check arguments
  # Check model_specifics arguments
  if (!is.null(model_specifics_update_fn)){
    model_specifics <- model_specifics_update_fn(model_specifics)
  }

  if (isTRUE(model_specifics[["preprocess_once"]])){

    # The preprocessing will have happened in the parent function
    # so we simply need to extract it

    train_test <- fold_info %>%
      dplyr::as_tibble() %>%
      dplyr::left_join(data, by = c("fold_column", "abs_fold", "rel_fold"))

    train_data <- train_test[["train"]][[1]]
    test_data <- train_test[["test"]][[1]]
    preprocess_params <- train_test[["preprocess_parameters"]][[1]]
    preprocess_warnings_and_messages <- train_test[["warnings_and_messages"]][[1]]
    preprocess_n_unknown_warnings <- train_test[["n_unknown_warnings"]][[1]]

  } else {

    # Subset and preprocess train/test split
    train_test <- prepare_train_test(
      data = data,
      fold_info = fold_info,
      fold_cols = fold_cols,
      model_specifics = model_specifics)

    train_data <- train_test[["train"]]
    test_data <- train_test[["test"]]
    preprocess_params <- train_test[["preprocess_parameters"]]
    preprocess_warnings_and_messages <- train_test[["warnings_and_messages"]]
    preprocess_n_unknown_warnings <- train_test[["n_unknown_warnings"]]

  }

  # Fit model, predict test set, and collect warnings
  fitted_model_process <- fit_predict_model_fn(
    train_data = train_data,
    test_data = test_data,
    fold_info = fold_info,
    model_specifics = model_specifics,
    include_fold_columns = include_fold_columns
  )

  # Extract models
  model <- fitted_model_process[["model"]]

  # Extract predictions and targets
  predictions_and_targets <- fitted_model_process[["predictions_and_targets"]] %>%
    dplyr::mutate(model_was_null = is.null(model))

  # Extract warnings and messages
  warnings_and_messages <- fitted_model_process[["warnings_and_messages"]] %>%
    dplyr::bind_rows(preprocess_warnings_and_messages)

  if (!isTRUE(include_fold_columns)){

    # If required, remove fold columns from tibbles that go in the output

    warnings_and_messages <- warnings_and_messages %>%
      dplyr::select(-dplyr::one_of(
        intersect(colnames(warnings_and_messages),
                  c("Fold Column", "Fold"))))

    preprocess_params <- preprocess_params %>%
      dplyr::select(-dplyr::one_of(
        intersect(colnames(preprocess_params),
                  c("Fold Column", "Fold"))))
  }

  # Nest warnings and messages tibble
  nested_warnings_and_messages <- warnings_and_messages %>%
    legacy_nest(seq_len(ncol(warnings_and_messages))) %>%
    dplyr::pull(.data$data)

  # Model metrics
  # Predictions will be evaluated in parent function

  model_evaluation <- internal_evaluate_model(
    model = model,
    train_data = train_data,
    test_data = test_data,
    type = evaluation_type,
    fold_info = fold_info,
    fold_info_cols = fold_info_cols,
    model_specifics = model_specifics,
    metrics = metrics,
    include_fold_columns = include_fold_columns) %>%
    dplyr::mutate(
      `Convergence Warnings` = fitted_model_process[['n_convergence_warnings']],
      `Singular Fit Messages` = fitted_model_process[['n_singular_fit_messages']],
      `Other Warnings` = fitted_model_process[['n_unknown_warnings']] +
        fitted_model_process[['n_prediction_warnings']] +
        preprocess_n_unknown_warnings,
      `Warnings and Messages` = nested_warnings_and_messages)

  if (isTRUE(err_nc) && model_evaluation[["Convergence Warnings"]] != 0) {
    stop("Model did not converge.")
  }

  if (!isTRUE(return_model)){
    model <- NULL
  }

  list("predictions_and_targets" = predictions_and_targets,
       "model_evaluation" = model_evaluation,
       "preprocess_parameters" = preprocess_params,
       "model" = model)

}


