# Model function for cross-validating lm, lmer, glm, and glmer

custom_model_fn <- function(train_data,
                            test_data,
                            fold_info = list(rel_fold=NULL, abs_fold=NULL, fold_column=NULL),
                            model_specifics = list(model_formula=NULL, family=NULL, link=NULL,
                                                   control=NULL, REML=FALSE, positive=2,
                                                   cutoff=0.5, model_verbose=FALSE, model_fn=NULL)){

  # Make sure, a model function was actually passed
  if (is.null(model_specifics[["model_fn"]])){
    stop("'model_fn' was NULL.")
  }

  y_col <- extract_y(model_specifics[["model_formula"]]) # Name of target column
  if (is.null(y_col)) stop("The model formula does not contain a dependent variable.")

  # Check if there are random effects (Logical)
  contains_random_effects = rand_effects(model_specifics[["model_formula"]])

  # Choose model_type
  if (model_specifics[["family"]] %ni% c("gaussian","binomial","multinomial")){
    stop(paste0("Does not recognize '", model_specifics[["family"]], "'."))
  }

  # Fit model
  fitted_model <- run_model_fitting(
    custom_fit_model,
    model_specifics = model_specifics,
    train_data = train_data,
    warn_info = list(
      model_formula = model_specifics[["model_formula"]],
      fold_info = fold_info,
      model_verbose = model_specifics[["model_verbose"]]
    ))

  model <- fitted_model[["model"]]

  # Predict test set
  # If models is NULL (e.g. didn't converge)
  #   Create a list of NA predictions the length of y_column

  if (is.null(model)){
    predictions <- rep(NA, length(test_data[[y_col]]))
  } else {
    if (model_specifics[["family"]] == "gaussian")
      predictions <- stats::predict(model, test_data, allow.new.levels=TRUE)
    if (model_specifics[["family"]] == "binomial")
      predictions <- stats::predict(model, test_data, type="response", allow.new.levels=TRUE)
    if (model_specifics[["family"]] == "multinomial")
      predictions <- stats::predict(model, test_data, type="probs", allow.new.levels=TRUE)
  }

  predictions_and_targets <- tibble::tibble("target" = test_data[[y_col]]) %>%
    dplyr::bind_cols(predictions) %>%
    dplyr::mutate(rel_fold = fold_info[["rel_fold"]],
                  abs_fold = fold_info[["abs_fold"]],
                  fold_column = fold_info[["fold_column"]])

  list(
    predictions_and_targets = predictions_and_targets,
    model = model,
    threw_singular_fit_message = fitted_model[["threw_singular_fit_message"]],
    threw_convergence_warning = fitted_model[["threw_convergence_warning"]],
    threw_unknown_warning = fitted_model[["threw_unknown_warning"]]
  )
}


