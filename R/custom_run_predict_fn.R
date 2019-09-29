custom_process_predictions <- function(test_data,
                                       model,
                                       model_formula,
                                       y_col,
                                       user_predict_fn,
                                       model_specifics,
                                       fold_info) {


  prediction_process <- tryCatch({
    purrr::map(.x = 1, .f = purrr::quietly(function(.x){
      custom_run_predict_fn(test_data = test_data,
                            model = model,
                            model_formula = model_formula,
                            y_col = y_col,
                            user_predict_fn = user_predict_fn,
                            model_specifics = model_specifics)
    }))
  }, error = function(e){
    stop(paste('',
               '-------------------------------------',
               paste0(model_specifics[["caller"]], ': Error:'),
               'In formula:',
               model_formula,
               'For fold column:',
               fold_info[["fold_column"]],
               'In fold:',
               fold_info[["rel_fold"]],
               e, sep = "\n"))
  })

  predictions <- prediction_process[[1]][["result"]]
  warnings <- prediction_process[[1]][["warnings"]]
  messages <- prediction_process[[1]][["messages"]]

  # Create tibble with warnings and messages
  warnings_and_messages <- create_warnings_and_messages_tibble(warnings = warnings,
                                                               messages = messages,
                                                               fn = "predict_fn")

  # Message the caught messages to the user
  for (m in messages){

    # purrr::quietly adds \n to end of messages, which we're not interested in here
    m <- gsub('\\\n$', '', m)

    message(paste('',
                  '-----------------------------',
                  paste0(model_specifics[["caller"]], ': Message:'),
                  'In formula:',
                  model_formula,
                  'For fold column:',
                  fold_info[["fold_column"]],
                  'In fold:',
                  fold_info[["rel_fold"]],
                  m, sep = "\n"))
  }

  # Throw the caught warnings
  for(w in warnings){

    warning(paste('',
                  '---------------------------------------',
                  paste0(model_specifics[["caller"]], ': Warning:'),
                  'In formula:',
                  model_formula,
                  'For fold column:',
                  fold_info[["fold_column"]],
                  'In fold:',
                  fold_info[["rel_fold"]],
                  w, sep = "\n"))

  }

  return(
    list(
      "predictions" = predictions,
      "warnings_and_messages" = warnings_and_messages,
      "n_unknown_messages" = length(messages),
      "n_unknown_warnings" = length(warnings)
    )
  )

}


custom_run_predict_fn <- function(test_data,
                                  model,
                                  model_formula,
                                  y_col,
                                  user_predict_fn,
                                  model_specifics){

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

          stop(paste0("When type/family is ", model_specifics[["family"]],
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

          stop(paste0("When type/family is ", model_specifics[["family"]],
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
          stop(paste0("Could not use the obtained predictions. ",
                      "Did you specify 'predict_type' or 'predict_fn' correctly? ",
                      "The original error was: ", e))
        })

      }

      if (nrow(predictions) != nrow(test_data)){
        stop(paste0("The number of predictions did not match the number of rows in the test set."))
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

  predictions

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
      stop("Could not convert predictions to type numeric.")
    }, warning = function(w) {
      stop("Could not convert predictions to type numeric.")
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
      predict_type = predict_type
    )

  } else {

    # Default predict_type per family
    if (family == "gaussian"){
      preds <- try_predicting(
        fn <- function() {
          stats::predict(model,
                         test_data,
                         allow.new.levels = TRUE)
        },
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
        predict_type = predict_type
      )
    }
  }
  preds
}

try_predicting <- function(fn, predict_type){
  tryCatch({
    fn()
  }, error = function(e){
    if (grepl("'arg' should be", as.character(e), ignore.case = TRUE)){
      if (is.null(predict_type)){
        stop(paste0("Could not use the default 'predict_type' in stats::predict(). ",
                    "Specify 'predict_type' or pass a custom 'predict_fn'. ",
                    "The original error was: ", e))
      } else {
        stop(paste0("Could not use the specified 'predict_type.' ",
                    "Try changing 'predict_type' or pass a custom 'predict_fn'. ",
                    "The original error was: ", e))
      }
    } else {
      stop(paste0("Could not call stats::predict() with current settings (typically 'predict_type'). ",
                  "Try changing 'predict_type' or pass a custom 'predict_fn'. ",
                  "The original error was: ", e))
    }
  }, warning = function(w){
    warning(w)
    fn()
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
      "Got the following error while using specified 'predict_fn': ",
      e
    ))

  }, warning = function(w){

    warning(paste0(
      "Got the following warning while using specified 'predict_fn': ",
      w
    ))

    return(user_predict_fn(test_data = test_data,
                           model = model))
  })
}
