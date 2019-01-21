eval_aggregation_lm_lmer <- function(fold_evaluations, n_folds, model_specifics = list(link=NULL, family=NULL)){

  # Check that model_specifics contains all named arguments
  if (length(setdiff(names(model_specifics), c("link", "family"))) > 0) {
    stop("model_specifics must (only) contain all named arguments. Be sure to name arguments.")
  }

  # Extract model dataframe from fold_lists_list
  models_list = fold_evaluations %c% 'model_tidy'
  models = dplyr::bind_rows(models_list)

  # Extract result dataframe from fold_lists_list
  results_list = fold_evaluations %c% 'result'
  results = dplyr::bind_rows(results_list)

  # Count the convergence warnings
  conv_warns <- count_convergence_warnings(results$converged)

  # Make results into a tibble
  iter_results <- tibble::as_tibble(results)
  rownames(iter_results) <- NULL
  iter_results <- iter_results %>%
    tidyr::nest() %>%
    dplyr::rename(results = data)

  # Make models into a tibble
  iter_models <- tibble::as_tibble(models)
  iter_models <- iter_models %>%
    mutate(p.value = ifelse(exists('p.value', where = iter_models), p.value, NA)) %>%
    tidyr::nest() %>%
    dplyr::rename(coefficients = data)

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
