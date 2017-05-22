

#' @importFrom dplyr %>%
cross_validate_single = function(data, model, folds_col = '.folds',
                          family='gaussian', REML=FALSE,
                          cutoff=0.5, positive=1,
                          model_verbose=FALSE){

  # model: ("y~a+b+(1|c)")
  # data: Dataframe
  # family: gaussian or binomial
  # REML: Restricted maximum likelihood
  # cutoff: For deciding prediction class from prediction (binomial)
  # positive: Level from dependent variable to predict (1/2 - alphabetically) (binomial)
  # model_verbose: Printed feedback on the used model (lm() / lmer() / glm() / glmer()) (BOOL)

  # Set errors if input variables aren't what we expect / can handle
  # WORK ON THIS SECTION!
  stopifnot(!is.null(model),
            is.data.frame(data),
            is.factor(data[[folds_col]]),
            positive %in% c(1,2),
            family %in% c("gaussian", "binomial")
  )

  # Extract y_column from model
  y_column = unlist(strsplit(model, '\\s*~'))[1]

  # Check if there are random effects (yields a list e.g. (False, False, True))
  random_effects = grepl('\\(\\d', model, perl=TRUE)

  # get number of folds - aka. number of levels in folds column
  nfolds <- nlevels(data[[folds_col]])

  # Loop through the folds
  # .. Create a test_set and a training_set
  # .. Train the model on the training_set
  # .. Test the model on the test_set

  fold_lists_list <- plyr::llply(1:nfolds, function(fold){

    # Create training set for this iteration
    training_set = data[data[[folds_col]] != fold,]
    # Create test set for this iteration
    test_set = data[data[[folds_col]] == fold,]

    # Train and test the model

    if (family=='gaussian'){

      # Call cv_gaussian_
      # Returns list with
      # .. a results dataframe
      # .. whether the model converged (yes / no)
      gaussian_fold = cv_gaussian_(model, test_set, training_set,
                                   y_column, fold, random_effects,
                                   REML=REML, model_verbose)

    } else if (family == 'binomial'){

      # Call cv_binomial_
      # Returns a list with
      # .. a dataframe with predictions and observations
      # .. whether the model converged (yes / no)
      binomial_fold = cv_binomial_(model, test_set, training_set,
                                   y_column, fold, random_effects,
                                   family, model_verbose)

    }

  })

  # Returning

  if (family == 'gaussian'){

    # Extract model dataframe from fold_lists_list
    models_list = fold_lists_list %c% 'model'
    models = do.call("rbind", models_list)

    # Extract result dataframe from fold_lists_list
    results_list = fold_lists_list %c% 'result'
    results = do.call("rbind", results_list)

    # Count the convergence warnings

    conv_warns = as.integer(table(results$converged)['No'])
    if (is.na(conv_warns)){
      conv_warns = 0
    }

    # Return a list with
    # .. the means of the found RMSE, r2m, r2c, AIC, AICc, and BIC
    # .. the used number of folds
    # .. the count of convergence warnings
    # .. list of plot objects

    # Make results into a tibble
    iter_results <- tibble::as_tibble(results)
    rownames(iter_results) <- NULL
    iter_results <- iter_results %>%
      tidyr::nest(1:length(colnames(.))) %>%
      dplyr::rename(results = data)

    # Make models into a tibble
    iter_models <- tibble::as_tibble(models)
    iter_models <- mutate(iter_models,
                          p.value = if (exists('p.value', where = iter_models)) p.value else NA) %>%
      tidyr::nest(1:length(colnames(.))) %>%
      dplyr::rename(coefficients = data)

    return(tibble::tibble(
      'RMSE' = mean(na.omit(results$RMSE)),
      'r2m' = mean(na.omit(results$r2m)),
      'r2c' = mean(na.omit(results$r2c)),
      'AIC' = mean(na.omit(results$AIC)),
      'AICc' = mean(na.omit(results$AICc)),
      'BIC' = mean(na.omit(results$BIC)),
      "Folds"=nfolds,
      "Convergence Warnings" = as.integer(conv_warns),
      "Family" = family,
      "Results" = iter_results$results,
      "Coefficients" = iter_models$coefficients))


  } else if (family == 'binomial'){

    # Extract model dataframe from fold_lists_list
    pred_obs_list = fold_lists_list %c% 'predictions_and_observations'
    pred_obs = do.call("rbind", pred_obs_list)

    # Extract converged list from fold_lists_list
    conv_list = unlist(fold_lists_list %c% 'converged')

    # Count the convergence warnings

    # Count how many 'No's are in conv_list
    conv_warns = as.integer(table(conv_list)['No'])

    # If conv_warns is NA, it means there were no 'No's in conv_warns
    if (is.na(conv_warns)){

      conv_warns = 0

    }

    # Find the levels in the categorical dependent variable
    cat_levels = c(as.character(levels(factor(data[[y_column]]))[1]),
                   as.character(levels(factor(data[[y_column]]))[2]))

    # Create a new dataframe based on the dataframe with predictions and observations
    # Create a column with the predicted class based on the chosen cutoff
    binomial_pred_obs_class = pred_obs %>%
      dplyr::mutate(predicted_class = ifelse(.[[1]] < cutoff, cat_levels[1],
                                             cat_levels[2])) # e.g. "ASD", "TD"

    # Try to use a confusion matrix with the data
    # This will fail if one of the folds didn't converge
    conf_mat = tryCatch({
      caret::confusionMatrix(binomial_pred_obs_class$predicted_class,
                      factor(binomial_pred_obs_class$y_column),
                      positive=cat_levels[positive])

    }, error = function(e) {

      # If there are any NAs in the prediction, the model didn't converge
      # .. message the user and return NULL
      # else stop the execution of cross_validate()

      if (sum(is.na(binomial_pred_obs_class$prediction)) != 0){

        warning('Confusion Matrix error as the model didn\'t converge.')

      } else {

        stop(paste0('Confusion matrix error: ',e))

      }

      return(NULL)

    }
    )

    # Try to fit a ROC curve on the data
    # This will fail if one of the folds didn't converge

    roc_curve = tryCatch({
      pROC::roc(response =  binomial_pred_obs_class$y_column,
                predictor =  binomial_pred_obs_class$prediction)

    }, error = function(e) {

      # If there are any NAs in the prediction, the model didn't converge
      # .. message the user and return NULL
      # else stop the execution of cross_validate()

      if (sum(is.na(binomial_pred_obs_class$prediction)) != 0){

        warning('Receiver Operator Characteristic (ROC) Curve error as the model didn\'t converge.')

      } else {

        stop(paste0('Receiver Operator Characteristic (ROC) Curve error: ',e))

      }

      return(NULL)

    }
    )

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

    if (!(is.null(conf_mat)) && !(is.null(roc_curve))){

      # Predictions and targets
      predictions_ <- binomial_pred_obs_class %>%
        dplyr::rename(target = y_column) %>%
        dplyr::select(prediction, predicted_class, target) %>%
        tibble::as_tibble() %>%
        tidyr::nest(1:3) %>%
        dplyr::rename(predictions = data)

      # ROC sensitivities and specificities
      roc_ <- tibble::tibble(sensitivities = roc_curve$sensitivities,
                             specificities = roc_curve$specificities) %>%
        tidyr::nest(1:2) %>%
        dplyr::rename(roc = data)

      return(tibble::tibble("AUC" = pROC::auc(roc_curve)[1],
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
                           "Folds"=nfolds, "Convergence Warnings" = conv_warns,
                           "Family" = family,
                           "Predictions" = predictions_$predictions,
                           "ROC" = roc_$roc))
    } else {

      return(tibble::tibble("AUC" = NA, "Lower CI" = NA, "Upper CI" = NA,
                           "Kappa" = NA, 'Sensitivity' = NA, 'Specificity' = NA,
                           'Pos Pred Value' = NA,"Neg Pred Value"=NA,"F1" = NA,
                           "Prevalence" = NA,"Detection Rate" = NA,
                           "Detection Prevalence" = NA,"Balanced Accuracy" = NA,
                           "Folds" = nfolds, "Convergence Warnings" = conv_warns,
                           "Family" = family,
                           "Predictions" = NA,
                           "ROC" = NA))

    }

  }

}

