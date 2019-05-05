binomial_classification_eval <- function(data,
                                         predictions_col,
                                         targets_col,
                                         fold_info_cols = list(rel_fold="rel_fold",
                                                               abs_fold="abs_fold",
                                                               fold_column="fold_column"),
                                         models=NULL, cutoff=0.5, positive=1){
  # Note: predictions are floats (e.g. 0.7), targets are 0 or 1

  # Check if there are NAs in predictions
  na_in_predictions <- sum(is.na(data[[predictions_col]])) > 0
  na_in_targets <- sum(is.na(data[[targets_col]])) > 0

  # Map of fold column, abs_fold and rel_fold
  fold_and_fold_col <- create_fold_and_fold_column_map(data, fold_info_cols)

  # Unique fold columns
  unique_fold_cols <- unique(fold_and_fold_col[["fold_column"]])

  if (!na_in_targets && !na_in_predictions){

    # Find the levels in the categorical target variable
    cat_levels = levels_as_characters(data[[targets_col]])

    # Create a column with the predicted class based on the chosen cutoff
    data[["predicted_classes"]] <- ifelse(data[[predictions_col]] < cutoff, cat_levels[1], cat_levels[2])

    # Nest predictions and targets
    predictions_nested <- tibble::as_tibble(data) %>%
      dplyr::select(!! as.name(fold_info_cols[["fold_column"]]),
                    !! as.name(fold_info_cols[["rel_fold"]]),
                    !! as.name(targets_col),
                    !! as.name(predictions_col),
                    predicted_classes
      ) %>%
      dplyr::rename(Fold = fold_info_cols[["rel_fold"]],
                    `Fold Column` = fold_info_cols[["fold_column"]],
                    Target = !! as.name(targets_col),
                    Prediction = !! as.name(predictions_col),
                    `Predicted Class` = predicted_classes
                    ) %>%
      tidyr::nest(1:5) %>%
      dplyr::rename(predictions = data)

    # Confusion matrices
    if (length(unique_fold_cols) > 1){
      fold_col_confusion_matrices <- plyr::llply(unique_fold_cols, function(fcol){

        # Subset data
        fcol_data <- data %>% dplyr::filter(!! as.name(fold_info_cols[["fold_column"]]) == fcol)

        # Create confusion matrix and add to list
        fcol_conf_mat <- list("x" = fit_confusion_matrix(predicted_classes = fcol_data[["predicted_classes"]],
                             targets = fcol_data[[targets_col]],
                             cat_levels = cat_levels,
                             positive = positive))
        # Rename list element to the fold column name
        names(fcol_conf_mat) <- fcol

        fcol_conf_mat

      }) %>% unlist(recursive=FALSE)

      nested_confusion_matrices <- nest_confusion_matrices(fold_col_confusion_matrices, cat_levels, unique_fold_cols)

    } else {
      conf_mat <- fit_confusion_matrix(predicted_classes = data[["predicted_classes"]],
                                       targets = data[[targets_col]],
                                       cat_levels = cat_levels,
                                       positive = positive)

      nested_confusion_matrices <- nest_confusion_matrices(list(conf_mat), cat_levels, unique_fold_cols)
    }

    # ROC curves
    if (length(unique_fold_cols) > 1){
      fold_col_roc_curves <- plyr::llply(unique_fold_cols, function(fcol){

        # Subset data
        fcol_data <- data %>% dplyr::filter(!! as.name(fold_info_cols[["fold_column"]]) == fcol)

        # Create confusion matrix and add to list
        fcol_roc_curve <- list("x" = fit_roc_curve(predicted_probabilities = fcol_data[[predictions_col]],
                                                   targets = fcol_data[[targets_col]]))
        # Rename list element to the fold column name
        names(fcol_roc_curve) <- fcol

        fcol_roc_curve

      }) %>% unlist(recursive=FALSE)

      # Fold column level results
      fold_col_results <- plyr::ldply(unique_fold_cols, function(fcol){

        binomial_classification_results_tibble(roc_curve = fold_col_roc_curves[[fcol]],
                                               roc_nested = NULL,
                                               conf_mat = fold_col_confusion_matrices[[fcol]],
                                               predictions_nested = NULL) %>%
          dplyr::mutate(`Fold Column` = fcol)
        }) %>%
        dplyr::select(`Fold Column`, dplyr::everything())

      # Nest fold column results
      fold_col_results_nested <- fold_col_results %>%
        dplyr::select(-c(Predictions, ROC)) %>%
        tidyr::nest(1 : (ncol(fold_col_results) - 2) ) %>% # -2 as we just remove two cols
        dplyr::rename(fold_col_results = data)

      # Average fold column results for reporting
      average_metrics <- fold_col_results %>%
        dplyr::select(-`Fold Column`) %>%
        dplyr::summarise_all(list(~mean(., na.rm=FALSE)))

      # ROC sensitivities and specificities
      roc_nested <- plyr::ldply(1:length(fold_col_roc_curves), function(i){
        tibble::tibble(`Fold Column` = names(fold_col_roc_curves)[[i]],
                       Sensitivities = fold_col_roc_curves[[i]]$sensitivities,
                       Specificities = fold_col_roc_curves[[i]]$specificities)
        }) %>%
        tidyr::nest(1:3) %>%
        dplyr::rename(roc = data)

      # Gather the various results
      results <- average_metrics
      results[["Predictions"]] <- predictions_nested$predictions
      results[["ROC"]] <- roc_nested$roc

    } else {
      roc_curve <- fit_roc_curve(predicted_probabilities = data[[predictions_col]],
                                 targets = data[[targets_col]])

      # ROC sensitivities and specificities
      roc_nested <- tibble::tibble(Sensitivities = roc_curve$sensitivities,
                                   Specificities = roc_curve$specificities) %>%
        tidyr::nest(1:2) %>%
        dplyr::rename(roc = data)

      results <- binomial_classification_results_tibble(roc_curve, roc_nested, conf_mat, predictions_nested)
    }

    results[["Confusion Matrix"]] <- nested_confusion_matrices$confusion_matrices

    # Add model coefficients
    if (!is.null(models)){
      # Get model coefficients
      # If broom::tidy does not work with the model objects, return NAs.
      nested_coefficients <- tryCatch({
        get_nested_model_coefficients(models,
                                      fold_info = list(folds = fold_and_fold_col[["rel_fold"]],
                                                       fold_columns = fold_and_fold_col[["fold_column"]]))
      }, error = function(e){
        get_nested_model_coefficients(NULL)
      })

      results[["Coefficients"]] <- nested_coefficients
    }

    if (length(unique_fold_cols) > 1){
      results[["Results"]] <- fold_col_results_nested$fold_col_results
    }
  } else {

    results <- binomial_classification_NA_results_tibble()

    if (!is.null(models))
      results[["Coefficients"]] <- get_nested_model_coefficients(NULL)

    if (length(unique_fold_cols) > 1){
      results[["Results"]] <- NA
    }
  }

  return(results)

}

binomial_classification_NA_results_tibble <- function(){

  return(tibble::tibble("Balanced Accuracy" = NA,
                        "F1" = NA, 'Sensitivity' = NA, 'Specificity' = NA,
                        'Pos Pred Value' = NA, "Neg Pred Value"=NA,
                        "AUC" = NA, "Lower CI" = NA, "Upper CI" = NA,
                        "Kappa" = NA,
                        "MCC"=NA,
                        "Detection Rate" = NA,
                        "Detection Prevalence" = NA,
                        "Prevalence" = NA,
                        "Predictions" = NA,
                        "ROC" = NA))
}

binomial_classification_results_tibble <- function(roc_curve, roc_nested, conf_mat, predictions_nested){

  tibble::tibble('Balanced Accuracy' = unname(conf_mat$byClass['Balanced Accuracy']),
                 'F1' = unname(conf_mat$byClass['F1']),
                 "Sensitivity" = unname(conf_mat$byClass['Sensitivity']),
                 'Specificity' = unname(conf_mat$byClass['Specificity']),
                 'Pos Pred Value' = unname(conf_mat$byClass['Pos Pred Value']),
                 'Neg Pred Value' = unname(conf_mat$byClass['Neg Pred Value']),
                 "AUC" = pROC::auc(roc_curve)[1],
                 "Lower CI" = pROC::ci(roc_curve)[1],
                 "Upper CI" = pROC::ci(roc_curve)[3],
                 "Kappa" = unname(conf_mat$overall['Kappa']),
                 'MCC' = mltools::mcc(TP=conf_mat$table[1], FP=conf_mat$table[3],
                                      TN=conf_mat$table[4], FN=conf_mat$table[2]),
                 'Detection Rate' = unname(conf_mat$byClass['Detection Rate']),
                 'Detection Prevalence' = unname(conf_mat$byClass['Detection Prevalence']),
                 'Prevalence' = unname(conf_mat$byClass['Prevalence']),
                 "Predictions" = ifelse(!is.null(predictions_nested), predictions_nested$predictions, logical()),
                 "ROC" = ifelse(!is.null(roc_nested), roc_nested$roc, logical()))
}

fit_confusion_matrix <- function(predicted_classes, targets, cat_levels, positive){

  if (is.numeric(positive)) positive <- cat_levels[positive]
  else if (is.character(positive) && positive %ni% cat_levels){
    stop(paste0("When 'positive' is a character, it must correspond to a factor level in the dependent variable.",
                "\n'positive' is ", positive, " and levels are ", paste(cat_levels, collapse = " and "),"."))
  }

  # Try to use fit a confusion matrix with the predictions and targets
  conf_mat = tryCatch({
    caret::confusionMatrix(factor(predicted_classes), #levels=c(0,1)),
                           factor(targets), #levels=c(0,1)),
                           positive=positive)

  }, error = function(e) {
    stop(paste0('Confusion matrix error: ',e))

  })

  conf_mat
}

fit_roc_curve <- function(predicted_probabilities, targets){

  # Try to fit a ROC curve on the data
  roc_curve = tryCatch({
    pROC::roc(response = targets,
              predictor = predicted_probabilities)

  }, error = function(e) {
    stop(paste0('Receiver Operator Characteristic (ROC) Curve error: ',e))

  })

  roc_curve
}

nest_confusion_matrices <- function(confusion_matrices, cat_levels=c("0","1"), fold_cols=".folds"){

  if (length(fold_cols) == 1) {
    fold_cols <- rep(fold_cols, length(confusion_matrices))
  }

  plyr::ldply(1:length(confusion_matrices), function(i){

    dplyr::as_tibble(confusion_matrices[[i]]$table) %>%
      dplyr::mutate(Pos0 = c("TP","FN","FP","TN"),
                    Pos1 = c("TN","FP","FN","TP"),
                    `Fold Column` = fold_cols[[i]])
  }) %>%
    dplyr::rename(N=n) %>%
    dplyr::select(c(`Fold Column`, Prediction, Reference, Pos0, Pos1, N)) %>%
    dplyr::rename_at(dplyr::vars(c("Pos0","Pos1")), ~ c(paste0("Pos_",cat_levels[[1]]),
                                                        paste0("Pos_",cat_levels[[2]]))) %>%
    tidyr::nest(1:6) %>%
    dplyr::rename(confusion_matrices = data)


}
