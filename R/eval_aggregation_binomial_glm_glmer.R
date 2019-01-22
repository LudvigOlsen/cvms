eval_aggregation_binomial_glm_glmer <- function(fold_evaluations, n_folds,
                                                model_specifics = list(link=NULL, family=NULL, positive=NULL, cutoff=NULL)){

  # Check that model_specifics contains all named arguments
  check_model_specifics(model_specifics, c("link", "family", "positive", "cutoff"))

  # Get content of model_specifics
  # They cannot be NULL, which happens when the list is passed without naming the arguments
  # (i.e. "link" instead "link=link")
  link = assign_if_not_null_named_lists(model_specifics[["link"]], "link", "model_specifics")
  family = assign_if_not_null_named_lists(model_specifics[["family"]], "family", "model_specifics")
  positive = assign_if_not_null_named_lists(model_specifics[["positive"]], "positive", "model_specifics")
  cutoff = assign_if_not_null_named_lists(model_specifics[["cutoff"]], "cutoff", "model_specifics")

  # Extract model dataframe from fold_lists_list
  models_list = fold_evaluations %c% 'model_tidy'
  models = dplyr::bind_rows(models_list)

  # Extract predictions and observations dataframe from fold_lists_list
  predictions_and_targets_list = fold_evaluations %c% 'predictions_and_targets'
  predictions_and_targets = dplyr::bind_rows(predictions_and_targets_list)

  # Count the convergence warnings
  convergences = fold_evaluations %c% 'converged'
  conv_warns <- count_convergence_warnings(convergences)              ##### TODO does this work?

  # Make models into a tibble
  iter_models <- nest_models(models)

  # Check if there are NAs in predictions
  na_in_predictions <- sum(is.na(predictions_and_targets$prediction)) > 0

  if (conv_warns == 0 && !na_in_predictions){
    # Find the levels in the categorical target variable
    cat_levels = levels_as_characters(predictions_and_targets[["target"]])

    # Create a column with the predicted class based on the chosen cutoff
    predictions_and_targets = predictions_and_targets %>%
      dplyr::mutate(predicted_class = ifelse(prediction < cutoff, cat_levels[1],
                                             cat_levels[2])) # e.g. "ASD", "TD"

    # Try to use fit a confusion matrix with the predictions and targets
    conf_mat = tryCatch({
      caret::confusionMatrix(factor(predictions_and_targets$predicted_class, levels=c(0,1)),
                             factor(predictions_and_targets$target, levels=c(0,1)),
                             positive=cat_levels[positive])

    }, error = function(e) {
      stop(paste0('Confusion matrix error: ',e))

    })

    # Try to fit a ROC curve on the data
    roc_curve = tryCatch({
      pROC::roc(response =  predictions_and_targets$target,
                predictor =  predictions_and_targets$prediction)

    }, error = function(e) {
      stop(paste0('Receiver Operator Characteristic (ROC) Curve error: ',e))

    })

    # If both conf_mat and roc_curve are not NULL
    # .. Return
    # .... AUC, CI, Kappa, Sensitivity, Specificity
    # .... Pos Pred Value, Neg Pred Value, Precision,
    # .... Recall, F1, Prevalence, Detection Rate,
    # .... Detection Prevalence, Balanced Accuracy,
    # .... number of folds, and the count of convergence warnings
    # Else
    # .. Return the same variables but with NA in all but
    # .. folds and convergence warnings

    # Predictions and targets
    predictions_nested <- predictions_and_targets %>%
      #dplyr::rename(target = y_column) %>%
      dplyr::select(prediction, predicted_class, target) %>%
      tibble::as_tibble() %>%
      tidyr::nest(1:3) %>%
      dplyr::rename(predictions = data)

    # ROC sensitivities and specificities
    roc_nested <- tibble::tibble(sensitivities = roc_curve$sensitivities,
                                 specificities = roc_curve$specificities) %>%
      tidyr::nest(1:2) %>%
      dplyr::rename(roc = data)

    return(binomial_results_tibble(roc_curve, roc_nested, conf_mat, predictions_nested, n_folds, conv_warns, family, link))

  } else {
    warning(paste0('The model didn\'t converge on ', conv_warns,
                   ' fold', ifelse(conv_warns>1, 's',''), '.'))
    return(binomial_NA_results_tibble(n_folds, conv_warns, family, link))
  }

}

binomial_NA_results_tibble <- function(n_folds, conv_warns, family, link){

  return(tibble::tibble("AUC" = NA, "Lower CI" = NA, "Upper CI" = NA,
                        "Kappa" = NA, 'Sensitivity' = NA, 'Specificity' = NA,
                        'Pos Pred Value' = NA,"Neg Pred Value"=NA,"F1" = NA,
                        "Prevalence" = NA,"Detection Rate" = NA,
                        "Detection Prevalence" = NA,"Balanced Accuracy" = NA,
                        "Folds" = n_folds, "Convergence Warnings" = conv_warns,
                        "Family" = family,
                        "Link" = link,
                        "Predictions" = NA,
                        "ROC" = NA))

}

binomial_results_tibble <- function(roc_curve, roc_nested, conf_mat, predictions_nested,
                                    n_folds, conv_warns, family, link){

  tibble::tibble("AUC" = pROC::auc(roc_curve)[1],
                "Lower CI" = pROC::ci(roc_curve)[1],
                "Upper CI" = pROC::ci(roc_curve)[3],
                "Kappa" = unname(conf_mat$overall['Kappa']),
                "Sensitivity" = unname(conf_mat$byClass['Sensitivity']),
                'Specificity' = unname(conf_mat$byClass['Specificity']),
                'Pos Pred Value' = unname(conf_mat$byClass['Pos Pred Value']),
                'Neg Pred Value' = unname(conf_mat$byClass['Neg Pred Value']),
                'F1' = unname(conf_mat$byClass['F1']),
                'Prevalence' = unname(conf_mat$byClass['Prevalence']),
                'Detection Rate' = unname(conf_mat$byClass['Detection Rate']),
                'Detection Prevalence' = unname(conf_mat$byClass['Detection Prevalence']),
                'Balanced Accuracy' = unname(conf_mat$byClass['Balanced Accuracy']),
                "Folds"=n_folds, "Convergence Warnings" = conv_warns,
                "Family" = family,
                "Link" = link,
                "Predictions" = predictions_nested$predictions,
                "ROC" = roc_nested$roc)

}
