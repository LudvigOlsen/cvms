fit_predict_model_fn <- function(
  train_data,
  test_data,
  fold_info = list(
    rel_fold = NULL,
    abs_fold = NULL,
    fold_column = NULL
  ),
  include_fold_columns = TRUE,
  model_specifics = list(
    model_formula = NULL,
    family = NULL,
    link = NULL,
    control = NULL,
    REML = NULL,
    positive = 2,
    cutoff = 0.5,
    model_verbose = FALSE,
    model_fn = NULL,
    predict_fn = NULL,
    caller = NULL
  )
) {
  # Make sure, a model function was actually passed
  if (is.null(model_specifics[["model_fn"]])) {
    stop("'model_fn' was NULL.")
  }

  y_col <- extract_y(model_specifics[["model_formula"]]) # Name of target column
  if (is.null(y_col)) stop("The model formula does not contain a dependent variable.")

  user_predict_fn <- model_specifics[["predict_fn"]]

  # Check task/evaluation type
  if (model_specifics[["family"]] %ni% c("gaussian", "binomial", "multinomial")) {
    stop(paste0("Does not recognize '", model_specifics[["family"]], "'."))
  }

  # Fit model
  fitted_model <- run_model_fitting(
    model_fitting_fn = fit_model,
    model_specifics = model_specifics,
    train_data = train_data,
    warn_info = list(
      model_formula = model_specifics[["model_formula"]],
      fold_info = fold_info,
      model_verbose = model_specifics[["model_verbose"]],
      caller = model_specifics[["caller"]]
    )
  )

  model <- fitted_model[["model"]]

  # Predict test set and catch warnings and messages
  prediction_process <- run_prediction_process(
    test_data = test_data,
    train_data = train_data,
    model = model,
    model_formula = model_specifics[["model_formula"]],
    y_col = y_col,
    user_predict_fn = user_predict_fn,
    model_specifics = model_specifics,
    fold_info = fold_info
  )

  # Create tibble with predictions, targets and fold info
  predictions_and_targets <- tibble::tibble(
    "observation" = test_data[[model_specifics[["observation_id_col"]]]],
    "target" = test_data[[y_col]]
  ) %>%
    dplyr::bind_cols(prediction_process[["predictions"]])

  if (isTRUE(include_fold_columns)) {
    predictions_and_targets <- predictions_and_targets %>%
      dplyr::mutate(
        rel_fold = fold_info[["rel_fold"]],
        abs_fold = fold_info[["abs_fold"]],
        fold_column = fold_info[["fold_column"]]
      )
  }


  # Concatenate warnings and messages from
  # the model_fn and predict_fn processes
  warnings_and_messages <- fitted_model[["warnings_and_messages"]] %>%
    dplyr::bind_rows(prediction_process[["warnings_and_messages"]]) %>%
    dplyr::mutate(
      Fold = fold_info[["rel_fold"]],
      `Fold Column` = as.character(fold_info[["fold_column"]])
    ) %>%
    base_select(cols = c("Fold Column", "Fold", "Function", "Type", "Message"))

  if (!isTRUE(include_fold_columns)) {
    warnings_and_messages <- warnings_and_messages %>%
      base_deselect(cols = c("Fold Column", "Fold"))
  }

  list(
    predictions_and_targets = predictions_and_targets,
    model = model,
    warnings_and_messages = warnings_and_messages,
    n_singular_fit_messages = sum(fitted_model[["threw_singular_fit_message"]]),
    n_unknown_messages = sum(fitted_model[["threw_unknown_message"]]),
    n_convergence_warnings = sum(fitted_model[["threw_convergence_warning"]]),
    n_unknown_warnings = sum(fitted_model[["threw_unknown_warning"]]),
    n_prediction_warnings = ifelse(is.null(prediction_process[["n_unknown_warnings"]]), 0,
      prediction_process[["n_unknown_warnings"]]
    )
  )
}
