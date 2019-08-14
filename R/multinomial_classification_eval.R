# Note - predictions should be softmaxed rowwise and nested - TODO create tool

multinomial_classification_eval <- function(data,
                                            predictions_col,
                                            targets_col,
                                            id_col = NULL,
                                            id_method = NULL,
                                            fold_info_cols = list(
                                              rel_fold = "rel_fold",
                                              abs_fold = "abs_fold",
                                              fold_column = "fold_column"),
                                            models = NULL){
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

  if (!na_in_targets && !na_in_predictions){

    # Find the levels in the categorical target variable
    cat_levels <- levels_as_characters(data[[targets_col]])
    classes <- cat_levels # TODO FIX COPYING
    num_classes <- length(cat_levels)

    if (ncol(predicted_probabilities) != num_classes ||
        length(setdiff(classes, colnames(predicted_probabilities))) != 0){
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

    # Nest predictions and targets
    # TODO This should not be included in class level results, only for average metrics
    predictions_nested <- tibble::as_tibble(data) %>%
      dplyr::select(!! as.name(fold_info_cols[["fold_column"]]),
                    !! as.name(fold_info_cols[["rel_fold"]]),
                    !! as.name(targets_col),
                    !! as.name(predictions_col),
                    .data$predicted_class
      )

    # If ID evaluation, add ID and method to nested predictions
    if (!is.null(id_col)){
      if (is.null(id_method))
        stop("when 'id_col' is specified, 'id_method' must be specified as well.")

      predictions_nested[[id_col]] <- data[[id_col]]
      predictions_nested[["id_method"]] <- id_method

    }

    # Rename some columns and nest, nest, nest
    predictions_nested <- predictions_nested %>%
      dplyr::rename(Fold = fold_info_cols[["rel_fold"]],
                    `Fold Column` = fold_info_cols[["fold_column"]],
                    Target = !! as.name(targets_col),
                    Prediction = !! as.name(predictions_col),
                    `Predicted Class` = .data$predicted_class
      ) %>%
      legacy_nest(1:ncol(predictions_nested)) %>%
      dplyr::rename(predictions = data)

    # print(predictions_nested$predictions)#[[1]]$Prediction)

    local_tmp_target_var <- create_tmp_var(data,"one_vs_all_targets")
    local_tmp_predicted_probability_var <- create_tmp_var(data,"one_vs_all_predicted_probability")
    local_tmp_predicted_class_var <- create_tmp_var(data,"one_vs_all_predicted_class")

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
        models = models
        ) %>%
        dplyr::mutate(Class = classes[[cl]])

    }) %>% dplyr::bind_rows()

    # Place Class column first
    one_vs_all_evaluations <- one_vs_all_evaluations %>%
      dplyr::select(.data$Class, dplyr::everything())

    # Calculate the average metrics and add overall metrics
    average_metrics <- one_vs_all_evaluations %>%
      dplyr::mutate(Family = "multinomial") %>%
      select_metrics(include_definitions = FALSE) %>%
      dplyr::summarise_all(list(mean), na.rm = TRUE)

    overall_results <- average_metrics %>%
      dplyr::mutate(`Overall Accuracy` = overall_accuracy,
                    Predictions = predictions_nested$predictions)

      overall_results <- overall_results %>%
      dplyr::select(dplyr::one_of("Overall Accuracy"), dplyr::everything())

    # Add total counts confusion matrix
    # Try to use fit a confusion matrix with the predictions and targets
    overall_confusion_matrix = tryCatch({
      caret::confusionMatrix(factor(data[["predicted_class"]], levels = cat_levels),
                             factor(data[[targets_col]], levels = cat_levels))

    }, error = function(e) {
      stop(paste0('Confusion matrix error: ',e))
    })

    overall_results[["Confusion Matrix"]] <- nest_multiclass_confusion_matrices(
      list(overall_confusion_matrix))[["confusion_matrices"]]

    results <- list("Results" = overall_results,
                    "Class_level_results" = one_vs_all_evaluations)

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
