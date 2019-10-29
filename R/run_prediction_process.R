run_prediction_process <- function(test_data,
                                       model,
                                       model_formula,
                                       y_col,
                                       user_predict_fn,
                                       model_specifics,
                                       fold_info) {

  prediction_process <- tryCatch({
    purrr::map(.x = 1, .f = purrr::quietly(function(.x){
      run_predict_fn(test_data = test_data,
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


run_predict_fn <- function(test_data,
                                  model,
                                  model_formula,
                                  y_col,
                                  user_predict_fn,
                                  model_specifics){

  # Predict test set

  if (is.null(model)){

    # If model is NULL (e.g. didn't converge)
    # Create a list of NA predictions the length of y_column

    predictions <- tibble::tibble("prediction" = rep(NA, length(test_data[[y_col]])))

  } else {

    if (!is.null(user_predict_fn)){

      # Use user's predict function
      predictions <- run_user_predict_fn(
        user_predict_fn = user_predict_fn,
        test_data = test_data,
        model = model,
        formula = model_specifics[["model_formula"]],
        hyperparameters = extract_hparams(model_specifics),
        caller = model_specifics[["caller"]])

    } else {
      stop("'predict_fn' was NULL")
    }

    if (is.null(predictions)){
      stop(paste0(model_specifics[["caller"]],": predictions were NULL."))
    }

    if (model_specifics[["family"]] %in% c("gaussian", "binomial")){

      if (is.matrix(predictions)){
        if (ncol(predictions) > 1){

          stop(paste0("When type/family is ", model_specifics[["family"]],
                      ", the predictions must be a vector or matrix / data frame ",
                      "with one column but was a matrix with ",
                      ncol(predictions), " columns. ",
                      "Did you specify 'predict_fn' correctly?"))
        }

        # Convert column to vector then tibble
        predictions <- tibble::enframe(as.vector(predictions),
                                       value = "prediction",
                                       name = NULL)

      } else if (is.data.frame(predictions)){
        if (ncol(predictions) > 1){

          stop(paste0("When type/family is ", model_specifics[["family"]],
                      ", the predictions must be a vector or matrix / data frame ",
                      "with one column but was a data frame with ",
                      ncol(predictions), " columns. ",
                      "Did you specify 'predict_fn' correctly?"))
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
                      "Did you specify 'predict_fn' correctly? ",
                      "The original error was: ", e))
        })

      }

      if (nrow(predictions) != nrow(test_data)){
        stop(paste0("The number of predictions did not match the ",
                    "number of rows in the test set."))
      }

      # Force type numeric
      predictions[["prediction"]] <- force_numeric(
        predictions_vector = predictions[["prediction"]],
        caller = model_specifics[["caller"]])

      # Select prediction column
      predictions <- predictions %>%
        base_select(cols = "prediction")

    } else if (model_specifics[["family"]] == "multinomial"){

      # TODO DO ALL SORTS OF CHECKS HERE

      # Convert to tibble
      predictions <- dplyr::as_tibble(predictions) %>%
        dplyr::mutate_all(~ force_numeric(
          predictions_vector = ., caller = model_specifics[["caller"]])) %>%
        nest_rowwise() %>%
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

run_user_predict_fn <- function(user_predict_fn,
                                test_data,
                                model,
                                formula,
                                hyperparameters,
                                caller = ""){

  tryCatch({
    # Use user's predict function
    user_predict_fn(test_data = test_data,
                    model = model,
                    formula = stats::as.formula(formula),
                    hyperparameters = hyperparameters)

  }, error = function(e){

    stop(paste0(
      "Got the following error while using specified 'predict_fn': ",
      e
    ))
  })
  # When we don't catch warnings, the output is returned and the warning is thrown
  # In this case, that's what we want, as they will be caught in parent function
}
