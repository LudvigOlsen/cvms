# Whereas binomial_classification_eval.R and multinomial_classification_eval.R
# prepares the actual evaluation, these function do the actual evaluation

binomial_eval <- function(data,
                          targets_col,
                          predictions_col,
                          predicted_class_col,
                          unique_fold_cols,
                          cat_levels,
                          positive,
                          fold_info_cols,
                          fold_and_fold_col,
                          predictions_nested = NULL,
                          models = NULL) {


  confusion_matrices_list <- binomial_eval_confusion_matrices(
    data = data,
    targets_col = targets_col,
    predicted_class_col = predicted_class_col,
    unique_fold_cols = unique_fold_cols,
    cat_levels = cat_levels,
    positive = positive,
    fold_info_cols = fold_info_cols
  )

  roc_curves_list <- binomial_eval_roc_curves(
    data = data,
    targets_col = targets_col,
    predictions_col = predictions_col,
    unique_fold_cols = unique_fold_cols,
    cat_levels = cat_levels,
    positive = positive,
    fold_info_cols = fold_info_cols
  )

  results <- binomial_eval_collect(unique_fold_cols = unique_fold_cols,
                                   roc_curves_list = roc_curves_list,
                                   confusion_matrices_list = confusion_matrices_list,
                                   predictions_nested = predictions_nested,
                                   models = models)

  if (!is.null(models)){
    results[["Coefficients"]] <- binomial_add_model_coefficients(models, fold_and_fold_col)
  }

  results

}

binomial_eval_confusion_matrices <- function(data,  targets_col, predicted_class_col, unique_fold_cols, cat_levels, positive, fold_info_cols){

  # Confusion matrices
  if (length(unique_fold_cols) > 1){
    confusion_matrices <- plyr::llply(unique_fold_cols, function(fcol){

      # Subset data
      fcol_data <- data %>% dplyr::filter(!! as.name(fold_info_cols[["fold_column"]]) == fcol)

      # Create confusion matrix and add to list
      fcol_conf_mat <- list("x" = fit_confusion_matrix(predicted_classes = fcol_data[[predicted_class_col]],
                                                       targets = fcol_data[[targets_col]],
                                                       cat_levels = cat_levels,
                                                       positive = positive))
      # Rename list element to the fold column name
      names(fcol_conf_mat) <- fcol

      fcol_conf_mat

    }) %>% unlist(recursive=FALSE)

    nested_confusion_matrices <- nest_confusion_matrices(confusion_matrices, cat_levels, unique_fold_cols)

  } else {

    # NOT repeated

    confusion_matrix <- fit_confusion_matrix(predicted_classes = data[[predicted_class_col]],
                                             targets = data[[targets_col]],
                                             cat_levels = cat_levels,
                                             positive = positive)

    nested_confusion_matrices <- nest_confusion_matrices(list(confusion_matrix), cat_levels, unique_fold_cols)

    confusion_matrices <- list(confusion_matrix)
  }

  list("nested_confusion_matrices" = nested_confusion_matrices,
       "confusion_matrices" = confusion_matrices)

}

binomial_eval_roc_curves <- function(data, targets_col, predictions_col, unique_fold_cols, cat_levels, positive, fold_info_cols){

  # ROC curves

  # Prepare roc_cat_levels order first
  # Note that if this order is reverse of cat_levels,
  # the positive class will be correct when the probability is
  # smaller than a threshold (closer to 0).
  roc_cat_levels <- c(cat_levels[cat_levels != positive], cat_levels[cat_levels == positive])
  roc_direction <- ifelse(all(roc_cat_levels == rev(cat_levels)), ">", "<")

  if (length(unique_fold_cols) > 1){
    roc_curves <- plyr::llply(unique_fold_cols, function(fcol){

      # Subset data
      fcol_data <- data %>% dplyr::filter(!! as.name(fold_info_cols[["fold_column"]]) == fcol)

      # Create confusion matrix and add to list
      fcol_roc_curve <- list("x" = fit_roc_curve(predicted_probabilities = fcol_data[[predictions_col]],
                                                 targets = fcol_data[[targets_col]],
                                                 levels = roc_cat_levels,
                                                 direction = roc_direction))
      # Rename list element to the fold column name
      names(fcol_roc_curve) <- fcol

      fcol_roc_curve

    }) %>% unlist(recursive=FALSE)

    # ROC sensitivities and specificities
    roc_nested <- plyr::ldply(1:length(roc_curves), function(i){
      tibble::tibble(`Fold Column` = names(roc_curves)[[i]],
                     Sensitivities = roc_curves[[i]]$sensitivities,
                     Specificities = roc_curves[[i]]$specificities)
    }) %>%
      tidyr::nest(1:3) %>%
      dplyr::rename(roc = data)


  } else {
    roc_curve <- fit_roc_curve(predicted_probabilities = data[[predictions_col]],
                               targets = data[[targets_col]],
                               levels = roc_cat_levels,
                               direction = roc_direction)

    # ROC sensitivities and specificities
    roc_nested <- tibble::tibble(Sensitivities = roc_curve$sensitivities,
                                 Specificities = roc_curve$specificities) %>%
      tidyr::nest(1:2) %>%
      dplyr::rename(roc = data)

    # Put in list
    roc_curves <- list(roc_curve)
  }

  list("roc_curves" = roc_curves,
       "roc_nested" = roc_nested)

}

binomial_eval_collect <- function(unique_fold_cols, roc_curves_list, confusion_matrices_list, predictions_nested, models){

  # Unpack args
  roc_curves <- roc_curves_list[["roc_curves"]]
  roc_nested <- roc_curves_list[["roc_nested"]]
  confusion_matrices <- confusion_matrices_list[["confusion_matrices"]]
  nested_confusion_matrices <- confusion_matrices_list[["nested_confusion_matrices"]]

  if (length(unique_fold_cols) > 1){

    # Fold column level results
    fold_col_results <- plyr::ldply(unique_fold_cols, function(fcol){

      binomial_classification_results_tibble(roc_curve = roc_curves[[fcol]],
                                             roc_nested = NULL,
                                             confusion_matrix = confusion_matrices[[fcol]],
                                             predictions_nested = NULL) %>%
        dplyr::mutate(`Fold Column` = fcol)
    }) %>%
      dplyr::select(.data$`Fold Column`, dplyr::everything())

    # Nest fold column results
    fold_col_results_nested <- fold_col_results %>%
      dplyr::select(-c(.data$Predictions, .data$ROC)) %>%
      tidyr::nest(1 : (ncol(fold_col_results) - 2) ) %>% # -2 as we just remove two cols
      dplyr::rename(fold_col_results = data)

    # Average fold column results for reporting
    average_metrics <- fold_col_results %>%
      dplyr::select(-.data$`Fold Column`) %>%
      dplyr::summarise_all(list(~mean(., na.rm=FALSE)))

    # Gather the various results
    results <- average_metrics
    if (!is.null(predictions_nested)){
      results[["Predictions"]] <- predictions_nested$predictions
    }
    results[["Results"]] <- fold_col_results_nested$fold_col_results
    results[["ROC"]] <- roc_nested$roc

  } else {

    results <- binomial_classification_results_tibble(roc_curve = roc_curves[[1]],
                                                      roc_nested = roc_nested,
                                                      confusion_matrix = confusion_matrices[[1]],
                                                      predictions_nested = predictions_nested)
  }

  results[["Confusion Matrix"]] <- nested_confusion_matrices$confusion_matrices

  results
}

binomial_add_model_coefficients <- function(models, fold_and_fold_col){

  if (is.null(models)){
    stop("'models' is NULL.")
  }

  # Add model coefficients

  # Get model coefficients
  # If broom::tidy does not work with the model objects, return NAs.
  nested_coefficients <- tryCatch({
    get_nested_model_coefficients(models,
                                  fold_info = list(folds = fold_and_fold_col[["rel_fold"]],
                                                   fold_columns = fold_and_fold_col[["fold_column"]]))
  }, error = function(e){
    get_nested_model_coefficients(NULL)
  })

  nested_coefficients

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

binomial_classification_results_tibble <- function(roc_curve, roc_nested, confusion_matrix, predictions_nested){

  tibble::tibble('Balanced Accuracy' = unname(confusion_matrix$byClass['Balanced Accuracy']),
                 'F1' = unname(confusion_matrix$byClass['F1']),
                 "Sensitivity" = unname(confusion_matrix$byClass['Sensitivity']),
                 'Specificity' = unname(confusion_matrix$byClass['Specificity']),
                 'Pos Pred Value' = unname(confusion_matrix$byClass['Pos Pred Value']),
                 'Neg Pred Value' = unname(confusion_matrix$byClass['Neg Pred Value']),
                 "AUC" = pROC::auc(roc_curve)[1],
                 "Lower CI" = pROC::ci(roc_curve)[1],
                 "Upper CI" = pROC::ci(roc_curve)[3],
                 "Kappa" = unname(confusion_matrix$overall['Kappa']),
                 'MCC' = mltools::mcc(
                   TP = confusion_matrix$table[1],
                   FP = confusion_matrix$table[3],
                   TN = confusion_matrix$table[4],
                   FN = confusion_matrix$table[2]
                 ),
                 'Detection Rate' = unname(confusion_matrix$byClass['Detection Rate']),
                 'Detection Prevalence' = unname(confusion_matrix$byClass['Detection Prevalence']),
                 'Prevalence' = unname(confusion_matrix$byClass['Prevalence']),
                 "Predictions" = ifelse(!is.null(predictions_nested),
                                        predictions_nested$predictions,
                                        logical()),
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
    caret::confusionMatrix(factor(predicted_classes, levels = cat_levels),
                           factor(targets, levels = cat_levels),
                           positive = positive)

  }, error = function(e) {
    stop(paste0('Confusion matrix error: ',e))

  })

  conf_mat
}

# levels must be ordered such that the positive class is last c(neg, pos)
fit_roc_curve <- function(predicted_probabilities, targets, levels = c(0,1), direction = "<"){

  # Try to fit a ROC curve on the data
  roc_curve = tryCatch({
    pROC::roc(
      response = targets,
      predictor = predicted_probabilities,
      direction = direction,
      levels = levels
    )
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
    dplyr::rename(N=.data$n) %>%
    dplyr::select(c(.data$`Fold Column`, .data$Prediction, .data$Reference,
                    .data$Pos0, .data$Pos1, .data$N)) %>%
    dplyr::rename_at(dplyr::vars(c("Pos0","Pos1")), ~ c(paste0("Pos_",cat_levels[[1]]),
                                                        paste0("Pos_",cat_levels[[2]]))) %>%
    tidyr::nest(1:6) %>%
    dplyr::rename(confusion_matrices = data)

}

# Note, if only one confusion matrix object (from caret::confusionMatrix()),
# pass it in a list
nest_multiclass_confusion_matrices <- function(confusion_matrices,
                                               fold_cols = ".folds") {

  if (length(fold_cols) == 1) {
    fold_cols <- rep(fold_cols, length(confusion_matrices))
  }

  plyr::ldply(1:length(confusion_matrices), function(i){

    dplyr::as_tibble(confusion_matrices[[i]]$table) %>%
      dplyr::mutate(`Fold Column` = fold_cols[[i]])
  }) %>%
    dplyr::rename(N=.data$n) %>%
    dplyr::select(c(.data$`Fold Column`, .data$Prediction,
                    .data$Reference, .data$N)) %>%
    tidyr::nest(1:4) %>%
    dplyr::rename(confusion_matrices = data)

}
