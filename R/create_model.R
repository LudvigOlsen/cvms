create_model_ = function(model, model_type, training_set, family, REML, fold, model_verbose){

  # Tries to fit the given model with the given model_type
  # .. If it gives a warning
  # .... it checks if it's a convergence warning
  # ...... If it is
  # ........ it issues the warning using warning()
  # ........ and returns NULL to model_temp
  # ...... If it is not
  # ........ it issues the warning using warning()
  # ........ and returns the output of lm() / lmer()

  model_temp = tryCatch({

    # Fit model
    fit_model_(model, model_type, training_set, family, REML, model_verbose)

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
      # .. and return NULL to model_temp

      warning(paste('-------------------------------------',
                    'cross_validate(): Convergence Warning:',
                    'In model:',
                    model,
                    'In fold:',
                    fold,
                    w, sep = "\n"))


      return(NULL)

    } else {

      # If it didn't seem to be a convergence warning
      # .. message the user of the failed model and fold
      # .. issue the warning
      # .. and return the fitted model

      warning(paste('-------------------------------------',
                    'cross_validate(): Warning:',
                    'In model:',
                    model,
                    'In fold:',
                    fold,
                    w, sep = "\n"))

      # Return the fitted model
      # model_verbose = FALSE; It printed the first time, so we don't need it again
      return(fit_model_(model, model_type, training_set, family, REML, model_verbose=FALSE))

    }

  })

  return(model_temp)

}

