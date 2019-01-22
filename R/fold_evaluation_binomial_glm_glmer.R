fold_evaluation_binomial_glm_glmer <- function(fold_output, fitted_model, fold, model_specifics){

  # Check that model_specifics contains all named arguments
  check_model_specifics(model_specifics, c("model_formula", "control",
                                           "REML", "positive", "cutoff","model_verbose",
                                           "link", "family"))

  if (is.null(fitted_model)){
    converged = "No"
    model_tidied = data.frame('term'=NA, 'estimate'=NA, 'std.error'=NA, 'statistic'=NA)
  }
  else {
    converged = "Yes"
    model_tidied = broom::tidy(fitted_model, effects = c("fixed"))
  }

  # Return a list with
  # .. a tidied summary of the model
  # .. the model object
  # .. a dataframe with the predictions and targets
  return(list(
    'model_tidy' = cbind(model_tidied, fold),
    'model' = fitted_model,
    'predictions_and_targets' = fold_output,
    'converged' = converged
  ))

}
