eval_aggregation_lm_lmer <- function(fold_evaluations, n_folds, model_specifics){

  # Check that model_specifics contains all named arguments
  check_model_specifics(model_specifics, c("model_formula", "control",
                                           "REML", "positive", "cutoff","model_verbose",
                                           "link", "family"))

  # Extract model dataframe from fold_lists_list
  models_list = fold_evaluations %c% 'model_tidy'
  models = dplyr::bind_rows(models_list)

  # Extract result dataframe from fold_lists_list
  results_list = fold_evaluations %c% 'result'
  results = dplyr::bind_rows(results_list)

  # Count the convergence warnings
  conv_warns <- count_convergence_warnings(results$converged)

  # Make results and models into a tibble
  iter_results <- nest_results(results)
  iter_models <- nest_models(models)

  return(tibble::tibble(
    'RMSE' = mean(na.omit(results$RMSE)),
    'r2m' = mean(na.omit(results$r2m)),
    'r2c' = mean(na.omit(results$r2c)),
    'AIC' = mean(na.omit(results$AIC)),
    'AICc' = mean(na.omit(results$AICc)),
    'BIC' = mean(na.omit(results$BIC)),
    "Folds"=n_folds,
    "Convergence Warnings" = as.integer(conv_warns),
    "Family" = model_specifics[["family"]],
    "Link" = model_specifics[["link"]],
    "Results" = iter_results$results,
    "Coefficients" = iter_models$coefficients))

}
