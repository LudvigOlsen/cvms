
multinomial_classification_eval <- function(data,
                                            predictions_col,
                                            targets_col,
                                            id_col = NULL,
                                            id_method = NULL,
                                            fold_info_cols = list(
                                              rel_fold = "rel_fold",
                                              abs_fold = "abs_fold",
                                              fold_column = "fold_column"),
                                            models = NULL,
                                            metrics,
                                            include_fold_columns = TRUE,
                                            include_predictions = TRUE,
                                            na.rm = FALSE
                                            ){
  # Note: predictions are floats (e.g. 0.7), targets are 0 or 1

  # Check and unnest the probabilities

  if (!is.character(predictions_col)){
    stop("'predictions_col' must be the name of a column in 'data' with nested probabilities.")
  }
  if (predictions_col %ni% colnames(data)){
    stop("Could not find the specified 'predictions_col' in 'data'.")
  }

  predicted_probabilities <- tryCatch({
    dplyr::bind_rows(data[[predictions_col]])
  }, error = function(e){
    stop("Could not bind the specified predictions_col: ", predictions_col, ".")
  })

  # Check if there are NAs in predictions
  na_in_predictions <- sum(is.na(predicted_probabilities)) > 0 # TODO Add test
  na_in_targets <- sum(is.na(data[[targets_col]])) > 0

  # Map of fold column, abs_fold and rel_fold
  fold_and_fold_col <- create_fold_and_fold_column_map(data, fold_info_cols)

  # Unique fold columns
  unique_fold_cols <- unique(fold_and_fold_col[["fold_column"]])

  # Keep both aggregates with and without removing NAs
  both_keep_and_remove_NAs <- is.character(na.rm) && na.rm == "both"

  if (!na_in_targets && !na_in_predictions){

    # Convert target column to type character
    data[[targets_col]] <- as.character(data[[targets_col]])

    # Find the levels in the categorical target variable
    cat_levels_in_targets_col <- levels_as_characters(data[[targets_col]])
    classes <- colnames(predicted_probabilities)
    num_classes <- length(classes)

    if (length(setdiff(cat_levels_in_targets_col, classes)) != 0){
      stop("The column names in the nested predicted probabilities do not match the class names in the dependent column.")
    }

    # Order columns by the class names
    predicted_probabilities <- predicted_probabilities %>%
      dplyr::select(dplyr::one_of(classes))

    # Create a column with the predicted class
    data[["predicted_class_index"]] <- argmax(predicted_probabilities)
    data[["predicted_class"]] <- purrr::map_chr(
      data[["predicted_class_index"]],
      .f = function(x){classes[[x]]})

    # Calculate overall accuracy
    overall_accuracy <- data %>%
      dplyr::summarise(overall_accuracy = mean(.data$predicted_class == !!as.name(targets_col))) %>%
      dplyr::pull(.data$overall_accuracy)

    # Nest predictions

    if (isTRUE(include_predictions)){
      predictions_nested <- nesting_predictions_multinomial(
        data = data,
        predictions_col = predictions_col,
        targets_col = targets_col,
        id_col = id_col,
        id_method = id_method,
        fold_info_cols = fold_info_cols,
        include_fold_columns = include_fold_columns)
    } else {
      predictions_nested <- NULL
    }


    # Create unique temporary variable names
    local_tmp_target_var <- create_tmp_var(data,"one_vs_all_targets")
    local_tmp_predicted_probability_var <- create_tmp_var(data,"one_vs_all_predicted_probability")
    local_tmp_predicted_class_var <- create_tmp_var(data,"one_vs_all_predicted_class")

    # Count how many times each class are in the targets_col
    support <- create_support_object(data[[targets_col]])

    # Perform one vs all evaluations
    one_vs_all_evaluations <- plyr::llply(1:num_classes, function(cl){

      data[[local_tmp_target_var]] <- factor(ifelse(data[[targets_col]] == classes[[cl]], 1, 0))
      data[[local_tmp_predicted_probability_var]] <- predicted_probabilities[[cl]]
      data[[local_tmp_predicted_class_var]] <- factor(ifelse(data[["predicted_class_index"]] == cl, 1, 0))

      binomial_eval(
        data = data,
        targets_col = local_tmp_target_var,
        predictions_col = local_tmp_predicted_probability_var,
        predicted_class_col = local_tmp_predicted_class_var,
        unique_fold_cols = unique_fold_cols,
        cat_levels = levels(factor(c(0, 1))),
        positive = 2,
        fold_info_cols = fold_info_cols,
        fold_and_fold_col = fold_and_fold_col,
        predictions_nested = NULL,
        models = models,
        metrics = metrics,
        include_fold_columns = include_fold_columns,
        include_predictions = FALSE,
        na.rm = na.rm
        ) %>%
        dplyr::mutate(Class = classes[[cl]])

    }) %>% dplyr::bind_rows() %>%
      dplyr::left_join(support, by = "Class")

    # Remove Predictions column if it exists
    if ("Predictions" %in% colnames(one_vs_all_evaluations)){
      one_vs_all_evaluations[["Predictions"]] <- NULL
    }

    # Move Support column
    one_vs_all_evaluations <- reposition_column(one_vs_all_evaluations,
                                                "Support",
                                                .before = "ROC")

    # Place Class column first
    one_vs_all_evaluations <- one_vs_all_evaluations %>%
      dplyr::select(.data$Class, dplyr::everything())

    # Extract the metrics for calculating (weighted) averages
    metrics_only <- one_vs_all_evaluations %>%
      dplyr::mutate(Family = "multinomial") %>%
      select_metrics(include_definitions = FALSE)

    if (isTRUE(both_keep_and_remove_NAs)){

      # Calculate the average metrics
      average_metrics <- plyr::ldply(c(TRUE,FALSE), function(nr){
        metrics_only %>%
          dplyr::summarise_all(list(mean), na.rm = isTRUE(nr)) %>%
          dplyr::mutate(NAs_removed = isTRUE(nr))
      })

      # Calculate the weighted average metrics
      weighted_average_metrics <- plyr::ldply(c(TRUE,FALSE), function(nr){
        metrics_only %>%
          dplyr::summarise_all(list(
            ~ weighted.mean(., w = one_vs_all_evaluations[["Support"]])), na.rm = nr) %>%
          dplyr::rename_all(function(x) paste0("Weighted ", x)) %>%
          dplyr::mutate(NAs_removed = isTRUE(nr))
      })

    } else {
      # Calculate the average metrics
      average_metrics <- metrics_only %>%
        dplyr::summarise_all(list(mean), na.rm = na.rm)

      # Calculate the weighted average metrics
      weighted_average_metrics <- metrics_only %>%
        dplyr::summarise_all(list(
          ~ weighted.mean(., w = one_vs_all_evaluations[["Support"]])), na.rm = na.rm) %>%
        dplyr::rename_all(function(x) paste0("Weighted ", x))
    }

    # Keep only the requested metrics
    weighted_average_metrics_to_keep <- dplyr::intersect(c(metrics,"NAs_removed"),
                                                         c(colnames(weighted_average_metrics)))
    weighted_average_metrics <- weighted_average_metrics %>%
      dplyr::select(dplyr::one_of(weighted_average_metrics_to_keep))

    # Gather summarized metrics and add nested predictions
    overall_results <- average_metrics %>%
      tibble::as_tibble() %>%
      dplyr::bind_cols(weighted_average_metrics) %>%
      dplyr::mutate(`Overall Accuracy` = overall_accuracy)

    # Remove one of the NAs_removed columns
    if (length(intersect(c("NAs_removed","NAs_removed1"), colnames(overall_results)) == 2)){
      if(!isTRUE(all.equal(overall_results[["NAs_removed"]], overall_results[["NAs_removed1"]]))){
        stop("'NAs_removed' and 'NAs_removed1' were not identical.")
      }
      overall_results[["NAs_removed1"]] <- NULL
    }

    if (isTRUE(include_predictions) && !is.null(predictions_nested)){
      if (isTRUE(both_keep_and_remove_NAs)){
        overall_results[["Predictions"]] <- rep(predictions_nested$predictions, 2)
      } else {
        overall_results[["Predictions"]] <- predictions_nested$predictions
      }
    }

    # If we don't want the Overall Accuracy metric, remove it
    if ("Overall Accuracy" %ni% metrics){
      overall_results <- overall_results %>%
        dplyr::select(-dplyr::one_of("Overall Accuracy"))
    }

    # Add total counts confusion matrix
    # Try to fit a confusion matrix with the predictions and targets
    overall_confusion_matrix = tryCatch({
      caret::confusionMatrix(factor(data[["predicted_class"]], levels = classes),
                             factor(data[[targets_col]], levels = classes))
    }, error = function(e) {
      stop(paste0('Confusion matrix error: ',e))
    })


    nested_multiclass_confusion_matrices <- nest_multiclass_confusion_matrices(
      list(overall_confusion_matrix),
      include_fold_columns = include_fold_columns)[["confusion_matrices"]]

    if (isTRUE(both_keep_and_remove_NAs)){
      overall_results[["Confusion Matrix"]] <- rep(nested_multiclass_confusion_matrices, 2)
    } else {
      overall_results[["Confusion Matrix"]] <- nested_multiclass_confusion_matrices
    }

    # Rearrange columns in overall results
    all_cols <- colnames(overall_results)
    non_metric_cols <- setdiff(all_cols, metrics)
    new_order <- c(metrics, non_metric_cols)
    overall_results <- overall_results %>%
      dplyr::select(dplyr::one_of(new_order))

    results <- list("Results" = overall_results,
                    "Class Level Results" = one_vs_all_evaluations)

  } else {

    # TODO replace with multinomial NA result
    stop("NOT YET IMPLEMENTED!")
    results <- binomial_classification_NA_results_tibble()

    if (!is.null(models))
      results[["Coefficients"]] <- get_nested_model_coefficients(NULL)

    if (length(unique_fold_cols) > 1){
      results[["Results"]] <- NA
    }
  }

  return(results)

}


argmax_row <- function(...){
  x <- unname(c(...))
  which.max(x)
}

argmax <- function(data){
  purrr::pmap_dbl(data, .f = argmax_row)
}

# Counts how many times each class are in the targets_col
create_support_object <- function(targets){
  support <- data.frame(table(targets), stringsAsFactors = F)
  colnames(support) <- c("Class", "Support")
  support[["Class"]] <- as.character(support[["Class"]])
  support
}

#
# multinomial_classification_NA_results_tibble <- function(){
#
#   return(tibble::tibble("Balanced Accuracy" = NA,
#                         "F1" = NA, 'Sensitivity' = NA, 'Specificity' = NA,
#                         'Pos Pred Value' = NA, "Neg Pred Value"=NA,
#                         "AUC" = NA, "Lower CI" = NA, "Upper CI" = NA,
#                         "Kappa" = NA,
#                         "MCC"=NA,
#                         "Detection Rate" = NA,
#                         "Detection Prevalence" = NA,
#                         "Prevalence" = NA,
#                         "Predictions" = NA,
#                         "ROC" = NA))
# }
#
# multinomial_classification_results_tibble <- function(roc_curve, roc_nested, conf_mat, predictions_nested){
#
#   tibble::tibble('Balanced Accuracy' = unname(conf_mat$byClass['Balanced Accuracy']),
#                  'F1' = unname(conf_mat$byClass['F1']),
#                  "Sensitivity" = unname(conf_mat$byClass['Sensitivity']),
#                  'Specificity' = unname(conf_mat$byClass['Specificity']),
#                  'Pos Pred Value' = unname(conf_mat$byClass['Pos Pred Value']),
#                  'Neg Pred Value' = unname(conf_mat$byClass['Neg Pred Value']),
#                  "AUC" = pROC::auc(roc_curve)[1],
#                  "Lower CI" = pROC::ci(roc_curve)[1],
#                  "Upper CI" = pROC::ci(roc_curve)[3],
#                  "Kappa" = unname(conf_mat$overall['Kappa']),
#                  'MCC' = mltools::mcc(TP=conf_mat$table[1], FP=conf_mat$table[3],
#                                       TN=conf_mat$table[4], FN=conf_mat$table[2]),
#                  'Detection Rate' = unname(conf_mat$byClass['Detection Rate']),
#                  'Detection Prevalence' = unname(conf_mat$byClass['Detection Prevalence']),
#                  'Prevalence' = unname(conf_mat$byClass['Prevalence']),
#                  "Predictions" = ifelse(!is.null(predictions_nested), predictions_nested$predictions, logical()),
#                  "ROC" = ifelse(!is.null(roc_nested), roc_nested$roc, logical()))
# }
