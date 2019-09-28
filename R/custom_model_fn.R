# Model function for cross-validating custom model functions like svm, randomForest, etc.

custom_model_fn <- function(train_data,
                            test_data,
                            fold_info = list(rel_fold = NULL,
                                             abs_fold = NULL,
                                             fold_column = NULL),
                            model_specifics = list(
                              model_formula = NULL, family = NULL, link = NULL,
                              control = NULL, REML = FALSE, positive = 2,
                              cutoff = 0.5, model_verbose = FALSE, model_fn = NULL,
                              predict_type = NULL, predict_fn = NULL,
                              caller = NULL)){

  # Make sure, a model function was actually passed
  if (is.null(model_specifics[["model_fn"]])){
    stop("'model_fn' was NULL.")
  }

  y_col <- extract_y(model_specifics[["model_formula"]]) # Name of target column
  if (is.null(y_col)) stop("The model formula does not contain a dependent variable.")

  # Check if there are random effects (Logical)
  contains_random_effects = rand_effects(model_specifics[["model_formula"]])

  user_predict_fn <- model_specifics[["predict_fn"]]

  # Check task/evaluation type
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
      model_verbose = model_specifics[["model_verbose"]],
      caller = model_specifics[["caller"]]
    ))

  model <- fitted_model[["model"]]

  # Predict test set
  # If models is NULL (e.g. didn't converge)
  #   Create a list of NA predictions the length of y_column

  if (is.null(model)){

    predictions <- tibble::tibble("prediction" = rep(NA, length(test_data[[y_col]])))

  } else {

    if (!is.null(user_predict_fn)){

      # Use user's predict function
      predictions <- run_user_predict_fn(
        user_predict_fn = user_predict_fn,
        test_data = test_data,
        model = model,
        formula = model_specifics[["model_formula"]],
        caller = model_specifics[["caller"]])

    } else {

      # Use default predict function
      predictions <- internal_predict_fn(
        model = model,
        test_data = test_data,
        family = model_specifics[["family"]],
        predict_type = model_specifics[["predict_type"]],
        caller = model_specifics[["caller"]])

    }

    if (is.null(predictions)){
      stop(paste0(model_specifics[["caller"]],": predictions were NULL."))
    }

    if (model_specifics[["family"]] %in% c("gaussian","binomial")){

      if (is.matrix(predictions)){
        if (ncol(predictions) > 1){

          stop(paste0(model_specifics[["caller"]],": When type/family is ", model_specifics[["family"]],
                      ", the predictions must be a vector or matrix / data frame with one column but was a matrix with ",
                      ncol(predictions), " columns. ",
                      "Did you specify 'predict_type' or 'predict_fn' correctly?"))
        }

        # Convert column to vector then tibble
        predictions <- tibble::enframe(as.vector(predictions),
                        value = "prediction",
                        name = NULL)

      } else if (is.data.frame(predictions)){
        if (ncol(predictions) > 1){

          stop(paste0(model_specifics[["caller"]],": When type/family is ", model_specifics[["family"]],
                      ", the predictions must be a vector or matrix / data frame with one column but was a data frame with ",
                      ncol(predictions), " columns. ",
                      "Did you specify 'predict_type' or 'predict_fn' correctly?"))
        }

        # Make sure the data frame has the correct column name
        colnames(predictions) <- "prediction"
        # Convert to tibble
        predictions <- dplyr::as_tibble(predictions)

      } else {

        predictions <- tryCatch({
          tibble::enframe(as.vector(predictions),
                          value = "prediction",
                          name = NULL)
          }, error = function(e){
            stop(paste0(model_specifics[["caller"]],": Could not use the obtained predictions. ",
                        "Did you specify 'predict_type' or 'predict_fn' correctly? ",
                        "The original error was: ", e))
          })

      }

      if (nrow(predictions) != nrow(test_data)){
        stop(paste0(model_specifics[["caller"]],
                    ": The number of predictions did not match the number of rows in the test set."))
      }

      # Force type numeric
      predictions[["prediction"]] <- force_numeric(
        predictions_vector = predictions[["prediction"]],
        caller = model_specifics[["caller"]])

      # Select prediction column
      predictions <- predictions %>%
        dplyr::select(.data$prediction)

    } else if (model_specifics[["family"]] == "multinomial"){

      # TODO DO ALL SORTS OF CHECKS HERE

      # Convert to tibble
      predictions <- dplyr::as_tibble(predictions) %>%
        dplyr::mutate_all(~ force_numeric(predictions_vector = .,
                                          caller = model_specifics[["caller"]])) %>%
        nest_probabilities_rowwise() %>%
        tibble::enframe(value = "prediction", name = NULL)
    }
  }

  predictions_and_targets <- tibble::tibble("target" = test_data[[y_col]]) %>%
    dplyr::bind_cols(predictions) %>%
    dplyr::mutate(rel_fold = fold_info[["rel_fold"]],
                  abs_fold = fold_info[["abs_fold"]],
                  fold_column = fold_info[["fold_column"]])

  warnings_and_messages <- fitted_model[["warnings_and_messages"]] %>%
    dplyr::mutate(Fold = fold_info[["rel_fold"]],
                  `Fold Column` = fold_info[["fold_column"]]) %>%
    dplyr::select(dplyr::one_of(
      c("Fold Column","Fold","Type", "Message")))

  list(
    predictions_and_targets = predictions_and_targets,
    model = model,
    warnings_and_messages = warnings_and_messages,
    threw_singular_fit_message = fitted_model[["threw_singular_fit_message"]],
    threw_unknown_message = fitted_model[["threw_unknown_message"]],
    threw_convergence_warning = fitted_model[["threw_convergence_warning"]],
    threw_unknown_warning = fitted_model[["threw_unknown_warning"]]
  )
}


force_numeric <- function(predictions_vector, caller = ""){

  if (!is.numeric(predictions_vector)) {
    predictions_vector <- tryCatch({
      if (is.factor(predictions_vector)) {
        predictions_vector <- as.numeric(as.character(predictions_vector))
      } else {
        as.numeric(predictions_vector)
      }
    }, error = function(e) {
      stop(paste0(
        caller,
        ": Could not convert predictions to type numeric."
      ))
    }, warning = function(w) {
      stop(paste0(
        caller,
        ": Could not convert predictions to type numeric."
      ))
    })
  }

  predictions_vector
}

internal_predict_fn <- function(model, test_data, family, predict_type = NULL, caller = ""){
  # If predict_type is specified by user
  if (!is.null(predict_type)){

    preds <- try_predicting(
      fn = function() {
        stats::predict(model,
                       test_data,
                       type = predict_type,
                       allow.new.levels = TRUE)
      },
      caller = caller,
      predict_type = predict_type
    )

  } else {

    # Default predict_type per family
    if (family == "gaussian"){
      preds <- try_predicting(
        fn = function() {
          stats::predict(model,
                         test_data,
                         allow.new.levels = TRUE)
        },
        caller = caller,
        predict_type = predict_type
      )

    } else if (family == "binomial"){
      preds <- try_predicting(
        fn = function() {
          stats::predict(model,
                         test_data,
                         type = "response",
                         allow.new.levels = TRUE)
        },
        caller = caller,
        predict_type = predict_type
      )

    } else if (family == "multinomial"){
      preds <- try_predicting(
        fn = function() {
          stats::predict(model,
                         test_data,
                         type = "probs",
                         allow.new.levels = TRUE)
        },
        caller = caller,
        predict_type = predict_type
      )
    }
  }
  preds
}

try_predicting <- function(fn, caller, predict_type){
  tryCatch({
    fn()
  }, error = function(e){
    if (grepl("'arg' should be", as.character(e), ignore.case = TRUE)){
      if (is.null(predict_type)){
        stop(paste0(caller, ": Could not use the default 'predict_type' in stats::predict(). ",
                    "Specify 'predict_type' or pass a custom 'predict_fn'. ",
                    "The original error was: ", e))
      } else {
        stop(paste0(caller, ": Could not use the specified 'predict_type.' ",
                    "Try changing 'predict_type' or pass a custom 'predict_fn'. ",
                    "The original error was: ", e))
      }
    } else {
      stop(paste0(caller, ": Could not call stats::predict() with current settings (typically 'predict_type'). ",
                  "Try changing 'predict_type' or pass a custom 'predict_fn'. ",
                  "The original error was: ", e))
    }
  }, warning = function(w){
    warning(paste0(caller, ": ", w))
  })
}

run_user_predict_fn <- function(user_predict_fn, test_data, model, formula, caller = ""){

  tryCatch({
    # Use user's predict function
    user_predict_fn(test_data = test_data,
                    model = model,
                    formula = stats::as.formula(formula))

  }, error = function(e){

    stop(paste0(
      caller,
      ": ",
      "Got the following error while using specified 'predict_fn': ",
      e
    ))

  }, warning = function(w){

    warning(paste0(
      caller,
      ": ",
      "Got the following warning while using specified 'predict_fn': ",
      w
    ))

    return(user_predict_fn(test_data = test_data,
                           model = model))
  })
}
