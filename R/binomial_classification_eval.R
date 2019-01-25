binomial_classification_eval <- function(predictions, targets, models=NULL, cutoff=0.5, positive=1){
  # Note: predictions are floats (e.g. 0.7), targets are 0 or 1

  # Check if there are NAs in predictions
  na_in_predictions <- sum(is.na(predictions)) > 0
  na_in_targets <- sum(is.na(targets)) > 0

  if (!na_in_targets && !na_in_predictions){

    # Find the levels in the categorical target variable
    cat_levels = levels_as_characters(targets)

    # Create a column with the predicted class based on the chosen cutoff

    predicted_classes <- ifelse(predictions < cutoff, cat_levels[1], cat_levels[2]) # e.g. "ASD", "TD"

    # Try to use fit a confusion matrix with the predictions and targets
    conf_mat = tryCatch({
      caret::confusionMatrix(factor(predicted_classes, levels=c(0,1)),
                             factor(targets, levels=c(0,1)),
                             positive=cat_levels[positive])

    }, error = function(e) {
      stop(paste0('Confusion matrix error: ',e))

    })

    # Try to fit a ROC curve on the data
    roc_curve = tryCatch({
      pROC::roc(response = targets,
                predictor = predictions)

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
    predictions_nested <- tibble::tibble("prediction" = predictions,
                                         "predicted_class" = predicted_classes,
                                         "target" = targets) %>%
      tidyr::nest(1:3) %>%
      dplyr::rename(predictions = data)

    # ROC sensitivities and specificities
    roc_nested <- tibble::tibble(sensitivities = roc_curve$sensitivities,
                                 specificities = roc_curve$specificities) %>%
      tidyr::nest(1:2) %>%
      dplyr::rename(roc = data)

    results <- binomial_classification_results_tibble(roc_curve, roc_nested, conf_mat, predictions_nested)

    if (!is.null(models)){
      # Get model coefficients
      # If broom::tidy does not work with the model objects, return NAs.
      nested_coefficients <- tryCatch({
        get_nested_model_coefficients(models)
      }, error = function(e){
        get_nested_model_coefficients(NULL)
      })

      results[["Coefficients"]] <- nested_coefficients
    }


  } else {

    warning(paste0('The model didn\'t converge'))# on ', conv_warns, # TODO conv_warns is not specified here xD
                   #' fold', ifelse(conv_warns>1, 's',''), '.'))

    results <- binomial_classification_NA_results_tibble()

    if (!is.null(models))
      results[["Coefficients"]] <- get_nested_model_coefficients(NULL)
  }

  return(results)

}

binomial_classification_NA_results_tibble <- function(){

  return(tibble::tibble("AUC" = NA, "Lower CI" = NA, "Upper CI" = NA,
                        "Kappa" = NA, 'Sensitivity' = NA, 'Specificity' = NA,
                        'Pos Pred Value' = NA,"Neg Pred Value"=NA,"F1" = NA,
                        "Prevalence" = NA,"Detection Rate" = NA,
                        "Detection Prevalence" = NA,"Balanced Accuracy" = NA,
                        "Predictions" = NA,
                        "ROC" = NA))

}

binomial_classification_results_tibble <- function(roc_curve, roc_nested, conf_mat, predictions_nested){

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
                 "Predictions" = predictions_nested$predictions,
                 "ROC" = roc_nested$roc)

}
