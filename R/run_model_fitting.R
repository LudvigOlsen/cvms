

run_model_fitting <- function(model_fitting_fn, model_specifics, train_data,
                              warn_info = list(
                                model_formula = NULL,
                                fold_info = list(
                                  rel_fold = NULL,
                                  abs_fold = NULL,
                                  fold_column = NULL
                                ),
                                model_verbose = FALSE,
                                caller = "cross_validate_fn()"
                              )) {

  # Tries to fit the given model with the given model_type
  # .. If it gives a warning
  # .... it checks if it's a convergence warning
  # ...... If it is
  # ........ it issues the warning using warning()
  # ........ and returns NULL to model
  # ...... If it is not
  # ........ it issues the warning using warning()
  # ........ and returns the output of lm() / lmer()

  # Check that warn_info contains all three named arguments
  if (length(setdiff(names(warn_info), c("model_formula", "fold_info", "model_verbose", "caller"))) > 0) {
    stop("warn_info must contain all named arguments. Be sure to name arguments.")
  }

  model_formula <- assign_if_not_null_named_lists(warn_info[["model_formula"]], "model_formula", "warn_info")
  model_verbose <- assign_if_not_null_named_lists(warn_info[["model_verbose"]], "model_verbose", "warn_info")
  caller <- assign_if_not_null_named_lists(warn_info[["caller"]], "caller", "warn_info")

  fold_info <- assign_if_not_null_named_lists(warn_info[["fold_info"]], "fold_info", "warn_info")
  rel_fold <- assign_if_not_null_named_lists(fold_info[["rel_fold"]], "rel_fold", "fold_info")
  abs_fold <- assign_if_not_null_named_lists(fold_info[["abs_fold"]], "abs_fold", "fold_info")
  fold_column <- assign_if_not_null_named_lists(fold_info[["fold_column"]], "fold_column", "fold_info")

  # We run the model fitting function once, but use map, so we can use the
  # quietly function from purrr. Perhaps there's a way to avoid map() ?
  fitted_model_process <- tryCatch(
    {
      purrr::map(.x = 1, .f = purrr::quietly(function(.x) {
        model_fitting_fn(model_specifics, train_data)
      }))
    },
    error = function(e) {
      stop(
        create_message(
          m = e,
          caller = caller,
          formula = model_formula,
          fold_col = fold_column,
          fold = rel_fold,
          hyperparameters = extract_hparams(model_specifics)
        )
      )
    }
  )

  model <- fitted_model_process[[1]][["result"]]
  warnings <- fitted_model_process[[1]][["warnings"]]
  messages <- fitted_model_process[[1]][["messages"]]

  # Check that model is not NULL
  if (is.null(model)){
    stop(
      create_message(
        m = "'model_fn' returned 'NULL'. Must return a fitted model object.",
        caller = caller,
        formula = model_formula,
        fold_col = fold_column,
        fold = rel_fold,
        hyperparameters = extract_hparams(model_specifics),
        note = ifelse(caller %in% c("cross_validate()", "validate()"),
                      "Boundary (Singular) Fit Message", "")
      )
    )
  }

  # Init flags
  threw_singular_message <- FALSE
  threw_unknown_message <- FALSE
  threw_convergence_warning <- FALSE
  threw_unknown_warning <- FALSE

  # Check messages
  # We assume there will never be a lot of messages
  # and use a basic for loop
  for (m in messages) {

    # purrr::quietly adds \n to end of messages, which we're not interested in here
    m <- gsub("\\\n$", "", m)

    if (grepl("boundary \\(singular\\) fit", as.character(m), ignore.case = TRUE)) {
      message(
        create_message(
          m = m,
          caller = caller,
          formula = model_formula,
          fold_col = fold_column,
          fold = rel_fold,
          hyperparameters = extract_hparams(model_specifics),
          note = "Boundary (Singular) Fit Message"
        ))

      threw_singular_message <- TRUE
    } else if (grepl("Model function: Used", as.character(m), ignore.case = TRUE)) {
      # Happens on purpose, when model_verbose is TRUE
      message(m)
    } else {
      threw_unknown_message <- TRUE
      message(
        create_message(
          m = m,
          caller = caller,
          formula = model_formula,
          fold_col = fold_column,
          fold = rel_fold,
          hyperparameters = extract_hparams(model_specifics)
        ))
    }
  }

  # We assume only a few warnings will occur at once
  # why we use a basic for loop
  for (w in warnings) {

    # Check if the warning contains some words that would
    # indicate that it is a convergence warning
    # TODO This part could be improved upon, as it's currently only based on
    # the warnings that we got with a limited dataset and set of models
    if (grepl("checkConv", as.character(w), ignore.case = TRUE) ||
      grepl("convergence", as.character(w), ignore.case = TRUE) ||
      grepl("converge", as.character(w), ignore.case = TRUE)) {

      # If it seemed to be a convergence warning:
      # .. message the user of the failed model and fold
      # .. issue the warning
      # .. and return NULL to model

      warning(
        create_message(
          m = w,
          caller = caller,
          formula = model_formula,
          fold_col = fold_column,
          fold = rel_fold,
          hyperparameters = extract_hparams(model_specifics),
          note = "Convergence Warning"
        ))

      threw_convergence_warning <- TRUE
      # We don't want to evaluate the non-converged models
      model <- NULL
    } else {

      # If it didn't seem to be a convergence warning
      # .. message the user of the failed model and fold
      # .. issue the warning
      # .. and return the fitted model

      warning(
        create_message(
          m = w,
          caller = caller,
          formula = model_formula,
          fold_col = fold_column,
          fold = rel_fold,
          hyperparameters = extract_hparams(model_specifics)
        ))

      threw_unknown_warning <- TRUE
    }
  }

  # Create tibble with warnings and messages
  warnings_and_messages <- create_warnings_and_messages_tibble(
    warnings = warnings,
    messages = messages,
    fn = "model_fn"
  )

  # If it threw a message or warning, we want to count that.
  return(
    list(
      "model" = model,
      "warnings_and_messages" = warnings_and_messages,
      "threw_singular_fit_message" = threw_singular_message,
      "threw_unknown_message" = threw_unknown_message,
      "threw_convergence_warning" = threw_convergence_warning,
      "threw_unknown_warning" = threw_unknown_warning
    )
  )
}

paste_hparams <- function(hparams) {
  if (is.null(hparams)) {
    return("")
  }
  paste(names(hparams), hparams, sep = " : ", collapse = ", ")
}

# Extract hparams, either as list or tibble, or NULL if
# hparams wasn't passed originally
extract_hparams <- function(model_specifics) {
  hparams <- dplyr::bind_rows(model_specifics[["hparams"]])
  if (".__NA__" %in% names(hparams) && length(hparams) == 1) {
    return(NULL)
  }
  hparams
}
