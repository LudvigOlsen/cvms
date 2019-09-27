

run_model_fitting <- function(
  model_fitting_fn, model_specifics, train_data,
  warn_info = list(
    model_formula = NULL,
    fold_info = list(
      rel_fold = NULL,
      abs_fold = NULL,
      fold_column = NULL),
    model_verbose = FALSE,
    caller = "cross_validate()"
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

  fitted_model_process <- tryCatch({
    purrr::map(.x = 1, .f = purrr::quietly(function(.x){
      model_fitting_fn(model_specifics, train_data)
    }))
  }, error = function(e){
    stop(paste('',
               '-------------------------------------',
               paste0(caller, ': Error:'),
               'In model:',
               model_formula,
               'For fold column:',
               fold_column,
               'In fold:',
               rel_fold,
               e, sep = "\n"))
  })

  model <- fitted_model_process[[1]][["result"]]
  warnings <- fitted_model_process[[1]][["warnings"]]
  messages <- fitted_model_process[[1]][["messages"]]

  # Init flags
  threw_singular_message <- FALSE
  threw_unknown_message <- FALSE
  threw_convergence_warning <- FALSE
  threw_unknown_warning <- FALSE

  # Check messages
  # We assume there will never be a lot of messages
  # and use a basic for loop
  for(m in messages){

    # purrr::quietly adds \n to end of messages, which we're not interested in here
    m <- gsub('\\\n$', '', m)

    if (grepl('boundary \\(singular\\) fit', as.character(m), ignore.case = TRUE)){
      message(paste('',
                    '--------------------------------------------------',
                    paste0(caller, ': Boundary (Singular) Fit Message:'),
                    'In model:',
                    model_formula,
                    'For fold column:',
                    fold_column,
                    'In fold:',
                    rel_fold,
                    m, sep = "\n"))

      threw_singular_message <- TRUE

    } else if (grepl('Model function: Used', as.character(m), ignore.case = TRUE)) {
      # Happens on purpose, when model_verbose is TRUE
      message(m)
    } else {
      threw_unknown_message <- TRUE
      message(paste('',
                    '--------------------------',
                    paste0(caller, ': Message:'),
                    'In model:',
                    model_formula,
                    'For fold column:',
                    fold_column,
                    'In fold:',
                    rel_fold,
                    m, sep = "\n"))
    }
  }

  # We assume only a few warnings will occur at once
  # why we use a basic for loop
  for(w in warnings){

    # Check if the warning contains some words that would
    # indicate that it is a convergence warning
    # TODO This part could be improved upon, as it's currently only based on
    # the warnings that we got with a limited dataset and set of models
    if (grepl('checkConv', as.character(w), ignore.case = TRUE) ||
        grepl('convergence', as.character(w), ignore.case = TRUE) ||
        grepl('converge', as.character(w), ignore.case = TRUE)){

      # If it seemed to be a convergence warning:
      # .. message the user of the failed model and fold
      # .. issue the warning
      # .. and return NULL to model

      warning(paste('',
                    '-------------------------------------',
                    paste0(caller, ': Convergence Warning:'),
                    'In model:',
                    model_formula,
                    'For fold column:',
                    fold_column,
                    'In fold:',
                    rel_fold,
                    w, sep = "\n"))

      threw_convergence_warning <- TRUE
      # We don't want to evaluate the non-converged models
      model <- NULL

    } else {

      # If it didn't seem to be a convergence warning
      # .. message the user of the failed model and fold
      # .. issue the warning
      # .. and return the fitted model

      warning(paste('',
                    '-------------------------------------',
                    paste0(caller, ': Warning:'),
                    'In model:',
                    model_formula,
                    'For fold column:',
                    fold_column,
                    'In fold:',
                    rel_fold,
                    w, sep = "\n"))

      threw_unknown_warning <- TRUE
    }
  }

  # Add warnings and messages to nested tibble
  warnings_and_messages <-
    tibble::tibble("Message" = warnings,
           "Type" = "warning") %>%
    dplyr::bind_rows(
      tibble::tibble("Message" = messages,
             "Type" = "message")
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
