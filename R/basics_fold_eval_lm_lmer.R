basics_fold_eval_lm_lmer <- function(fold_output, fitted_model, fold, model_specifics){

  # If model_temp returned NULL
  # .. set the output variables to NA
  # .. set converged to "no"
  # Else
  # .. set it to the right values
  # .. set converged to "yes"

  # Check that model_specifics contains all named arguments
  check_model_specifics(model_specifics, c("model_formula", "control",
                                           "REML", "positive", "cutoff","model_verbose",
                                           "link", "family"))


  if (is.null(fitted_model)){

    rmse_ = NA
    r2m_ = NA
    r2c_ = NA
    AIC_ = NA
    AICc_ = NA
    BIC_ = NA
    converged = "No"
    # model_tidied = data.frame('term'=NA, 'estimate'=NA, 'std.error'=NA, 'statistic'=NA)

  } else {

    predictions <- fold_output[["prediction"]]
    targets <- fold_output[["target"]]

    # Find the values rmse, r2m, r2c, AIC, and BIC
    rmse_ <- RMSE(predictions, targets)
    r2m_ <- r2m(fitted_model)
    r2c_ <- r2c(fitted_model)
    AIC_ <- AIC(fitted_model)
    AICc_ <- AICc(fitted_model, model_specifics[["REML"]])
    BIC_ <- BIC(fitted_model)

    converged = "Yes"
    #model_tidied = broom::tidy(fitted_model, effects = c("fixed"))

  }

  # Return a list with
  # .. a tidied summary of the model
  # .. the model object
  # .. a dataframe with the found values
  return(
    # 'model_tidy' = cbind(model_tidied, fold),
    # 'model' = fitted_model,
    tibble::tibble('RMSE' = rmse_, 'r2m' = r2m_, 'r2c' = r2c_,
                   'AIC' = AIC_, 'AICc' = AICc_, 'BIC' = BIC_,
                   'converged' = converged, "fold" = fold)
  )
}
