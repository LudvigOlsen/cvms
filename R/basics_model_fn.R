# Model function for cross-validating lm, lmer, glm, and glmer

basics_model_fn <- function(train_data,
                            test_data,
                            fold_info = list(rel_fold=NULL, abs_fold=NULL, fold_column=NULL),
                            model_specifics = list(model_formula=NULL, family=NULL, link=NULL,
                                                   control=NULL, REML=FALSE, positive=2,
                                                   cutoff=0.5, model_verbose=FALSE)){

  y_col <- extract_y(model_specifics[["model_formula"]]) # Name of target column
  if (is.null(y_col)) stop("The model formula does not contain a dependent variable.")

  # Check if there are random effects (Logical)
  contains_random_effects = rand_effects(model_specifics[["model_formula"]])

  # Choose model_type
  if (model_specifics[["family"]] == "gaussian")
    model_type <- "lm"
  if (model_specifics[["family"]] == "binomial")
    model_type <- "glm"
  if (TRUE %in% contains_random_effects) model_type <- paste0(model_type, "er") # makes them either lmer or glmer

  model_specifics[["model_type"]] <- model_type

  # Fit model
  model <- basics_run_model_fitting(basics_fit_model, model_specifics = model_specifics,
                                    train_data = train_data,
                                    warn_info=list(model_formula=model_specifics[["model_formula"]],
                                                   fold_info=fold_info,
                                                   model_verbose=model_specifics[["model_verbose"]]))

  if (is.list(model) && isTRUE(model[["yielded_singular_fit_message"]])){
    yielded_singular_fit_message <- TRUE
    model <- model[["model"]]
  } else {
    yielded_singular_fit_message <- FALSE
  }

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
  }

  predictions_and_targets <- tibble::tibble("target" = test_data[[y_col]],
                                            "prediction" = predictions,
                                            "rel_fold" = fold_info[["rel_fold"]],
                                            "abs_fold" = fold_info[["abs_fold"]],
                                            "fold_column" = fold_info[["fold_column"]])

  return(list(predictions_and_targets=predictions_and_targets,
              model=model,
              yielded_singular_fit_message = yielded_singular_fit_message))

}


