fold_evaluation_lm_lmer <- function(fold_output, fitted_model, fold){

  # If model_temp returned NULL
  # .. set the output variables to NA
  # .. set converged to "no"
  # Else
  # .. set it to the right values
  # .. set converged to "yes"

  if (is.null(fitted_model)){

    rmse = NA
    r2m = NA
    r2c = NA
    AIC = NA
    AICc = NA
    BIC = NA
    converged = "No"
    model_tidied = data.frame('term'=NA, 'estimate'=NA, 'std.error'=NA, 'statistic'=NA)

  } else {

    predictions <- fold_output[["prediction"]]
    true_values <- fold_output[["true_value"]]

    # Find the values rmse, r2m, r2c, AIC, and BIC
    rmse <- tryCatch({
      hydroGOF::rmse(predictions, true_values)
    }, error = function(e){
      return(NA)
    })

    r2m <- tryCatch({
      suppressWarnings(MuMIn::r.squaredGLMM(fitted_model)[1])
    }, error = function(e){
      return(NA)
    })

    r2c <- tryCatch({
      suppressWarnings(MuMIn::r.squaredGLMM(fitted_model)[2])
    }, error = function(e){
      return(NA)
    })

    AIC <- tryCatch({
      stats::AIC(fitted_model)
    }, error = function(e){
      return(NA)
    })

    AICc <- tryCatch({
      AICcmodavg::AICc(fitted_model, return.K = REML)
    }, error = function(e){
      return(NA)
    })

    BIC <- tryCatch({
      stats::BIC(fitted_model)
    }, error = function(e){
      return(NA)
    })

    converged = "Yes"
    print(fitted_model)
    model_tidied = broom::tidy(fitted_model, effects = c("fixed"))

  }

  # Return a list with
  # .. a tidied summary of the model
  # .. the model object
  # .. a dataframe with the found values
  return(list(
    'model_tidy' = cbind(model_tidied, fold),
    'model' = fitted_model,
    'result' = tibble::tibble('RMSE' = rmse, 'r2m' = r2m, 'r2c' = r2c,
                              'AIC' = AIC, 'AICc' = AICc, 'BIC' = BIC,
                              'converged' = converged, "fold" = fold)
  ))
}
