evaluate_predictions_binomial <- function(data,
                                          predictions_col,
                                          targets_col,
                                          model_was_null_col,
                                          id_col,
                                          id_method,
                                          type,
                                          fold_info_cols = list(rel_fold = "rel_fold",
                                                                abs_fold = "abs_fold",
                                                                fold_column = "fold_column"),
                                          model_specifics,
                                          metrics,
                                          include_fold_columns,
                                          include_predictions,
                                          na.rm = TRUE) {

  # Map of fold column, abs_fold and rel_fold
  fold_and_fold_col <- create_fold_and_fold_column_map(data, fold_info_cols)
  # Unique fold columns
  unique_fold_cols <- unique(fold_and_fold_col[["fold_column"]])

  # Check if there are NAs in predictions or targets
  na_in_predictions <- contains_na(data[[predictions_col]])
  na_in_targets <- contains_na(data[[targets_col]])

  # Warn if NA in predictions
  if (!isTRUE(na_in_predictions)){
    warning(paste0(
      model_specifics[["caller"]], ": ",
      "predictions contained NA."
    ))
  }

  # Warn if NA in targets
  if (!isTRUE(na_in_targets)){
    warning(paste0(
      model_specifics[["caller"]], ": ",
      "targets contained NA."
    ))
  }

  if (!na_in_targets && !na_in_predictions){

    # Find the levels in the categorical target variable
    cat_levels <- levels_as_characters(data[[targets_col]])

    if (length(cat_levels) > 2){ stop("The target column must maximally contain 2 levels.") }

    # Create a column with the predicted class based on the chosen cutoff
    data[["predicted_class"]] <- ifelse(data[[predictions_col]] < model_specifics[["cutoff"]],
                                        cat_levels[1], cat_levels[2])

    # Nest predictions and targets
    # Will be NA if any model_was_null is TRUE and
    # include_predictions is TRUE
    # If include_predictions is FALSE,
    # will always return NULL
    predictions_nested <- nest_predictions(
      data = data,
      predictions_col = predictions_col,
      predicted_class_col = "predicted_class",
      targets_col = targets_col,
      model_was_null_col = model_was_null_col,
      type = type,
      id_col = id_col,
      id_method = id_method,
      fold_info_cols = fold_info_cols,
      include_fold_columns = include_fold_columns,
      include_predictions = include_predictions)

    # Create confusion matrices
    confusion_matrices_list <- binomial_eval_confusion_matrices(
      data = data,
      targets_col = targets_col,
      predicted_class_col = predicted_class_col,
      unique_fold_cols = unique_fold_cols,
      cat_levels = cat_levels,
      positive = model_specifics[["positive"]],
      fold_info_cols = fold_info_cols,
      include_fold_columns = include_fold_columns
    )

    # Create ROC curves
    roc_curves_list <- binomial_eval_roc_curves(
      data = data,
      targets_col = targets_col,
      predictions_col = predictions_col,
      unique_fold_cols = unique_fold_cols,
      cat_levels = cat_levels,
      positive = model_specifics[["positive"]],
      fold_info_cols = fold_info_cols
    )

    # TODO: Should probably rename "extra_metrics" to something more meaningful
    # Currently calculates the regular accuracy
    extra_metrics <- binomial_eval_extra_metrics(data = data,
                                                 targets_col = targets_col,
                                                 predicted_class_col = predicted_class_col,
                                                 unique_fold_cols = unique_fold_cols,
                                                 fold_info_cols = fold_info_cols)

    # Combine the different metrics and nested tibbles
    results <- binomial_eval_collect(unique_fold_cols = unique_fold_cols,
                                     roc_curves_list = roc_curves_list,
                                     confusion_matrices_list = confusion_matrices_list,
                                     predictions_nested = predictions_nested,
                                     extra_metrics = extra_metrics,
                                     metrics = metrics,
                                     include_fold_columns = include_fold_columns,
                                     include_predictions = include_predictions,
                                     na.rm = na.rm)

  } else {

    results <- binomial_classification_NA_results_tibble(
      metrics = metrics, include_predictions = include_predictions)

    if (!is.null(models))
      results[["Coefficients"]] <- get_nested_model_coefficients(
        models = NULL, include_fold_columns = include_fold_columns)

    if (length(unique_fold_cols) > 1){
      results[["Results"]] <- NA
    }
  }

  return(results)

}


binomial_eval_confusion_matrices <- function(
  data,
  targets_col,
  predicted_class_col,
  unique_fold_cols,
  cat_levels,
  positive,
  fold_info_cols,
  include_fold_columns) {

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

    nested_confusion_matrices <- nest_confusion_matrices(confusion_matrices, cat_levels, unique_fold_cols,
                                                         include_fold_columns = include_fold_columns)

  } else {

    # NOT repeated

    confusion_matrix <- fit_confusion_matrix(predicted_classes = data[[predicted_class_col]],
                                             targets = data[[targets_col]],
                                             cat_levels = cat_levels,
                                             positive = positive)

    nested_confusion_matrices <- nest_confusion_matrices(confusion_matrices = list(confusion_matrix),
                                                         cat_levels = cat_levels, fold_cols = unique_fold_cols,
                                                         include_fold_columns = include_fold_columns)

    confusion_matrices <- list(confusion_matrix)
  }

  list("nested_confusion_matrices" = nested_confusion_matrices,
       "confusion_matrices" = confusion_matrices)

}

binomial_eval_roc_curves <- function(data, targets_col, predictions_col,
                                     unique_fold_cols, cat_levels,
                                     positive, fold_info_cols){

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

      # Create ROC curve and add to list
      fcol_roc_curve <- list("x" = fit_roc_curve(predicted_probabilities = fcol_data[[predictions_col]],
                                                 targets = fcol_data[[targets_col]],
                                                 levels = roc_cat_levels,
                                                 direction = roc_direction))
      # Rename list element to the fold column name
      names(fcol_roc_curve) <- fcol

      fcol_roc_curve

    }) %>% unlist(recursive=FALSE)

    # ROC sensitivities and specificities
    roc_nested <- plyr::ldply(seq_len(length(roc_curves)), function(i){
      if (is.null(roc_curves[[i]])){
        return(
          tibble::tibble(`Fold Column` = NA,
                         Sensitivities = NA,
                         Specificities = NA)
        )
      }
      tibble::tibble(`Fold Column` = names(roc_curves)[[i]],
                     Sensitivities = roc_curves[[i]]$sensitivities,
                     Specificities = roc_curves[[i]]$specificities)
    }) %>%
      legacy_nest(1:3) %>%
      dplyr::rename(roc = data)


  } else {
    roc_curve <- fit_roc_curve(predicted_probabilities = data[[predictions_col]],
                               targets = data[[targets_col]],
                               levels = roc_cat_levels,
                               direction = roc_direction)

    # ROC sensitivities and specificities

    if (is.null(roc_curve)){
      roc_for_nesting <- tibble::tibble(Sensitivities = NA,
                                        Specificities = NA)
    } else {
      roc_for_nesting <- tibble::tibble(
        Sensitivities = roc_curve$sensitivities,
        Specificities = roc_curve$specificities)
    }

    roc_nested <- roc_for_nesting %>%
      legacy_nest(1:2) %>%
      dplyr::rename(roc = data)

    # Put in list
    roc_curves <- list(roc_curve)
  }

  list("roc_curves" = roc_curves,
       "roc_nested" = roc_nested)

}

binomial_eval_extra_metrics <- function(data, targets_col,
                                        predicted_class_col,
                                        unique_fold_cols,
                                        fold_info_cols){

  extra_metrics <- plyr::llply(unique_fold_cols, function(fcol) {

    # Subset data
    fcol_data <-
      data %>% dplyr::filter(!!as.name(fold_info_cols[["fold_column"]]) == fcol)

    # Regular old accuracy
    fcol_accuracy <- list("x" = list("accuracy" = calculate_accuracy(
      predictions = data[[predicted_class_col]],
      targets = data[[targets_col]]
    )))

    # Rename list element to the fold column name
    names(fcol_accuracy) <- fcol

    fcol_accuracy

  }) %>% unlist(recursive = FALSE)

  extra_metrics

}

binomial_eval_collect <- function(unique_fold_cols,
                                  roc_curves_list,
                                  confusion_matrices_list,
                                  predictions_nested,
                                  extra_metrics,
                                  metrics,
                                  include_fold_columns = TRUE,
                                  include_predictions = TRUE,
                                  na.rm = FALSE){

  # Unpack args
  roc_curves <- roc_curves_list[["roc_curves"]]
  roc_nested <- roc_curves_list[["roc_nested"]]
  confusion_matrices <- confusion_matrices_list[["confusion_matrices"]]
  nested_confusion_matrices <- confusion_matrices_list[["nested_confusion_matrices"]]

  # Whether we want to save the averages both with and without removing
  # the NAs
  # NOTE: na.rm == "both" is not currently used in this part!!
  both_keep_and_remove_NAs <- is.character(na.rm) && na.rm == "both"

  if (length(unique_fold_cols) > 1){

    # Fold column level results
    fold_col_results <- plyr::ldply(unique_fold_cols, function(fcol){

      binomial_classification_results_tibble(roc_curve = roc_curves[[fcol]],
                                             roc_nested = NULL,
                                             confusion_matrix = confusion_matrices[[fcol]],
                                             predictions_nested = NULL,
                                             extra_metrics = extra_metrics[[fcol]],
                                             metrics = metrics,
                                             include_predictions = TRUE) %>%
        dplyr::mutate(`Fold Column` = fcol)
    }) %>%
      dplyr::select(.data$`Fold Column`, dplyr::everything())

    # Nest fold column results
    fold_col_results_nested <- fold_col_results %>%
      dplyr::select(-c(.data$Predictions, .data$ROC)) %>%
      legacy_nest(1 : (ncol(fold_col_results) - 2) ) %>% # -2 as we just removed two cols
      dplyr::rename(fold_col_results = data)

    if (isTRUE(both_keep_and_remove_NAs)){
      # Average fold column results for reporting
      average_metrics_NAs_removed <- fold_col_results %>%
        dplyr::select(-.data$`Fold Column`) %>%
        dplyr::summarise_all(list( ~ mean(., na.rm = FALSE))) %>%
        dplyr::mutate(NAs_removed = FALSE)
      average_metrics_NAs_kept <- fold_col_results %>%
        dplyr::select(-.data$`Fold Column`) %>%
        dplyr::summarise_all(list( ~ mean(., na.rm = TRUE))) %>%
        dplyr::mutate(NAs_removed = TRUE)
      average_metrics <- average_metrics_NAs_removed %>%
        dplyr::bind_rows(average_metrics_NAs_kept)
    } else {
      # Average fold column results for reporting
      average_metrics <- fold_col_results %>%
        dplyr::select(-.data$`Fold Column`) %>%
        dplyr::summarise_all(list( ~ mean(., na.rm = na.rm)))
    }

    # Gather the various results
    results <- average_metrics
    if (!is.null(predictions_nested)){
      results[["Predictions"]] <- ifelse(isTRUE(both_keep_and_remove_NAs),
                                         unlist(rep(predictions_nested$predictions, 2), recursive = FALSE),
                                         predictions_nested$predictions)
    }
    results[["Results"]] <- ifelse(isTRUE(both_keep_and_remove_NAs),
                                   unlist(rep(fold_col_results_nested$fold_col_results, 2), recursive = FALSE),
                                   fold_col_results_nested$fold_col_results)
    results[["ROC"]] <- ifelse(isTRUE(both_keep_and_remove_NAs),
                               rep(roc_nested$roc, 2),
                               roc_nested$roc)

  } else {

    results <- binomial_classification_results_tibble(roc_curve = roc_curves[[1]],
                                                      roc_nested = roc_nested,
                                                      confusion_matrix = confusion_matrices[[1]],
                                                      predictions_nested = predictions_nested,
                                                      extra_metrics = extra_metrics[[1]],
                                                      metrics = metrics,
                                                      include_predictions = include_predictions)
  }

  if (isTRUE(both_keep_and_remove_NAs) && length(unique_fold_cols) > 1){
    results[["Confusion Matrix"]] <- unlist(rep(nested_confusion_matrices$confusion_matrices, 2), recursive = FALSE)
  } else {
    results[["Confusion Matrix"]] <- nested_confusion_matrices$confusion_matrices
  }


  results
}


binomial_classification_NA_results_tibble <- function(metrics, include_predictions = TRUE){

  eval_tibble <- tibble::tibble(
    "Balanced Accuracy" = NA,
    "Accuracy" = NA,
    "F1" = NA, 'Sensitivity' = NA, 'Specificity' = NA,
    'Pos Pred Value' = NA, "Neg Pred Value" = NA,
    "AUC" = NA, "Lower CI" = NA, "Upper CI" = NA,
    "Kappa" = NA,
    "MCC" = NA,
    "Detection Rate" = NA,
    "Detection Prevalence" = NA,
    "Prevalence" = NA,
    "Predictions" = NA,
    "ROC" = NA)

  if (!isTRUE(include_predictions)){
    eval_tibble[["Predictions"]] <- NULL
  }

  eval_tibble %>%
    dplyr::select(dplyr::one_of(intersect(metrics, colnames(eval_tibble))))
}

binomial_classification_results_tibble <- function(roc_curve,
                                                   roc_nested,
                                                   confusion_matrix,
                                                   predictions_nested,
                                                   extra_metrics, # TODO rename arg
                                                   metrics,
                                                   include_predictions){

  eval_tibble <- tibble::tibble(
    'Balanced Accuracy' = unname(confusion_matrix$byClass['Balanced Accuracy']),
    'Accuracy' = extra_metrics[["accuracy"]],
    'F1' = unname(confusion_matrix$byClass['F1']),
    "Sensitivity" = unname(confusion_matrix$byClass['Sensitivity']),
    'Specificity' = unname(confusion_matrix$byClass['Specificity']),
    'Pos Pred Value' = unname(confusion_matrix$byClass['Pos Pred Value']),
    'Neg Pred Value' = unname(confusion_matrix$byClass['Neg Pred Value']),
    "AUC" = ifelse(!is.null(roc_curve), pROC::auc(roc_curve)[1], logical()),
    "Lower CI" = ifelse(!is.null(roc_curve), pROC::ci(roc_curve)[1], logical()),
    "Upper CI" = ifelse(!is.null(roc_curve), pROC::ci(roc_curve)[3], logical()),
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

  eval_tibble <- eval_tibble %>%
    dplyr::select(dplyr::one_of(c(
      intersect(metrics, colnames(eval_tibble)),
      "Predictions", "ROC"
    )))

  if (!isTRUE(include_predictions)){
    eval_tibble[["Predictions"]] <- NULL
  }

  eval_tibble
}

fit_confusion_matrix <- function(predicted_classes, targets, cat_levels, positive){

  if (is.numeric(positive)) positive <- cat_levels[positive]
  else if (is.character(positive) && positive %ni% cat_levels){
    stop(paste0("When 'positive' is a character, it must correspond to a factor level in the dependent variable.",
                "\n'positive' is ", positive, " and levels are ", paste(cat_levels, collapse = " and "),"."))
  }

  if (length(cat_levels) < 2){
    stop(paste0("found less than 2 levels in the target column."))
  }

  # Try to use fit a confusion matrix with the predictions and targets
  conf_mat <- tryCatch({
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
  roc_curve <- tryCatch({
    pROC::roc(
      response = targets,
      predictor = predicted_probabilities,
      direction = direction,
      levels = levels
    )
  }, error = function(e) {
    if (grepl('No control observation', as.character(e), ignore.case = TRUE) ||
        grepl('No case observation', as.character(e), ignore.case = TRUE)){
      return(NULL)
    }

    stop(paste0('Receiver Operator Characteristic (ROC) Curve error: ',e))

  })

  roc_curve
}

nest_confusion_matrices <- function(confusion_matrices, cat_levels=c("0","1"), fold_cols=".folds",
                                    include_fold_columns = TRUE){

  if (length(fold_cols) == 1) {
    fold_cols <- rep(fold_cols, length(confusion_matrices))
  }

  tidy_confusion_matrix <- plyr::ldply(seq_len(length(confusion_matrices)), function(i){
    confusion_matrices[[i]]$table %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(Pos0 = c("TP","FN","FP","TN"),
                    Pos1 = c("TN","FP","FN","TP"),
                    `Fold Column` = fold_cols[[i]])
  }) %>%
    dplyr::rename(N=.data$n,
                  Target = .data$Reference) %>%
    dplyr::select(c(.data$`Fold Column`, .data$Prediction, .data$Target,
                    .data$Pos0, .data$Pos1, .data$N)) %>%
    dplyr::rename_at(dplyr::vars(c("Pos0","Pos1")), ~ c(paste0("Pos_",cat_levels[[1]]),
                                                        paste0("Pos_",cat_levels[[2]])))

  if (!isTRUE(include_fold_columns)){
    tidy_confusion_matrix <- tidy_confusion_matrix %>%
      dplyr::select(-dplyr::one_of("Fold Column"))
  }

  tidy_confusion_matrix %>%
    legacy_nest(seq_len(ncol(tidy_confusion_matrix))) %>%
    dplyr::rename(confusion_matrices = data)

}

# Note, if only one confusion matrix object (from caret::confusionMatrix()),
# pass it in a list
nest_multiclass_confusion_matrices <- function(confusion_matrices,
                                               fold_cols = ".folds",
                                               include_fold_columns = TRUE) {

  if (length(fold_cols) == 1) {
    fold_cols <- rep(fold_cols, length(confusion_matrices))
  }

  tidy_confusion_matrices <- plyr::ldply(seq_len(length(confusion_matrices)), function(i){
    confusion_matrices[[i]]$table %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(`Fold Column` = fold_cols[[i]])
  }) %>%
    dplyr::rename(N=.data$n,
                  Target = .data$Reference) %>%
    dplyr::select(c(.data$`Fold Column`, .data$Prediction,
                    .data$Target, .data$N))

  if (!isTRUE(include_fold_columns)){
    tidy_confusion_matrices <- tidy_confusion_matrices %>%
      dplyr::select(-dplyr::one_of("Fold Column"))
  }

  tidy_confusion_matrices %>%
    legacy_nest(seq_len(ncol(tidy_confusion_matrices))) %>%
    dplyr::rename(confusion_matrices = data)

}


