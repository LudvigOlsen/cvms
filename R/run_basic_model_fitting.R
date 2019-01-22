run_basic_model_fitting <- function(model_fitting_fn, model_specifics, train_data,
                            warn_info = list(model_formula=NULL, fold=NULL, model_verbose=FALSE)){

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
  if (length(setdiff(names(warn_info), c("model_formula", "fold", "model_verbose"))) > 0) {
    stop("warn_info must contain all named arguments. Be sure to name arguments.")
  }

  model_formula <- assign_if_not_null_named_lists(warn_info[["model_formula"]], "model_formula", "warn_info")
  fold <- assign_if_not_null_named_lists(warn_info[["fold"]], "fold", "warn_info")
  model_verbose <- assign_if_not_null_named_lists(warn_info[["model_verbose"]], "model_verbose", "warn_info")

  model = tryCatch({

    # Fit model
    model_fitting_fn(model_specifics, train_data)

  }, warning = function(w){

    # A warning occured!

    # Check if the warning contains some words that would
    # indicate that it is a convergence warning
    # --> This part could be improved upon, as it's currently only based on
    # the warnings that we got with a limited dataset and set of models
    if (grepl('checkConv', as.character(w)) ||
        grepl('convergence', as.character(w)) ||
        grepl('converge', as.character(w))){

      # If it seemed to be a convergence warning:
      # .. message the user of the failed model and fold
      # .. issue the warning
      # .. and return NULL to model

      warning(paste('',
                    '-------------------------------------',
                    'cross_validate(): Convergence Warning:',
                    'In model:',
                    model_formula,
                    'In fold:',
                    fold,
                    w, sep = "\n"))


      return(NULL)

    } else {

      # If it didn't seem to be a convergence warning
      # .. message the user of the failed model and fold
      # .. issue the warning
      # .. and return the fitted model

      warning(paste('',
                    '-------------------------------------',
                    'cross_validate(): Warning:',
                    'In model:',
                    model_formula,
                    'In fold:',
                    fold,
                    w, sep = "\n"))

      # Return the fitted model
      model_specifics[["model_verbose"]] = FALSE # It printed the first time, so we don't need it again
      return(model_fitting_fn(model_specifics, train_data))

    }

  })

  return(model)

}
