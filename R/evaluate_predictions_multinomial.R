evaluate_predictions_multinomial <- function(data,
                                             predictions_col,
                                             targets_col,
                                             model_was_null_col,
                                             id_col,
                                             id_method,
                                             fold_info_cols = list(rel_fold = "rel_fold",
                                                                   abs_fold = "abs_fold",
                                                                   fold_column = "fold_column"),
                                             model_specifics,
                                             metrics,
                                             include_fold_columns,
                                             include_predictions,
                                             na.rm = TRUE) {

  if (!is.character(predictions_col)){
    stop("'predictions_col' must be the name of a column in 'data' with nested probabilities.")
  }
  if (predictions_col %ni% colnames(data)){
    stop("Could not find the specified 'predictions_col' in 'data'.")
  }

  if (model_specifics[["positive"]] != 2){
    stop("'positive' must be 2 for multinomial evaluation.")
  }

  predicted_probabilities <- tryCatch({
    dplyr::bind_rows(data[[predictions_col]])
  }, error = function(e){
    stop("Could not bind the specified predictions_col: ", predictions_col, ".")
  })

  # Check if there are NAs in predictions or targets
  na_in_predictions <- contains_na(predicted_probabilities)
  na_in_targets <- contains_na(data[[targets_col]])

  # Warn if NA in predictions
  if (isTRUE(na_in_predictions)){
    warning(paste0(
      model_specifics[["caller"]], ": ",
      "predictions contained NA."
    ))
  }

  # Warn if NA in targets
  if (isTRUE(na_in_targets)){
    warning(paste0(
      model_specifics[["caller"]], ": ",
      "targets contained NA."
    ))
  }

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

    # You may not have all the classes in the target (e.g. bad partitioning)
    # And you may not have all the classes in the probability columns.
    # The following errors if there are classes in target
    # that does not have a probability column

    if (length(setdiff(cat_levels_in_targets_col, classes)) != 0){
      stop(paste0("The column names in the nested predicted probabilities ",
                  "do not match the class names in the dependent column."))
    }

    # Order columns by the class names
    predicted_probabilities <- predicted_probabilities %>%
      dplyr::select(dplyr::one_of(classes))

    # Create a column with the predicted class
    data[["predicted_class_index"]] <- argmax(predicted_probabilities)
    data[["predicted_class"]] <- purrr::map_chr(
      data[["predicted_class_index"]],
      .f = function(x){classes[[x]]})

    # Compute multiclass ROC curves and AUC scores
    multiclass_ROC_AUC <- plyr::llply(unique_fold_cols, function(fcol){

      # Extract current fold column
      fcol_data <- data %>%
        dplyr::filter(!!as.name(fold_info_cols[["fold_column"]]) == fcol)

      # Extract and prepare probabilities
      fcol_probabilities <- as.data.frame(
        dplyr::bind_rows(fcol_data[[predictions_col]]))

      # Extract targets
      fcol_targets <- fcol_data[[targets_col]]

      # Calculate multiclass ROC
      roc <- pROC::multiclass.roc(response = fcol_targets,
                                  predictor = fcol_probabilities,
                                  levels = cat_levels_in_targets_col,
                                  direction = ">") # TODO CHECK THIS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      list("ROC" = list(roc),
           "AUC" = as.numeric(pROC::auc(roc)))

    })

    roc_curves <- multiclass_ROC_AUC %c% "ROC"
    auc_scores <- unlist(multiclass_ROC_AUC %c% "AUC")

    # Calculate overall accuracy per fold column
    # Add the AUC scores
    overall_metrics <- data %>%
      dplyr::group_by(!!as.name(fold_info_cols[["fold_column"]])) %>%
      dplyr::summarise(`Overall Accuracy` = mean(.data$predicted_class == !!as.name(targets_col))) %>%
      dplyr::rename(`Fold Column` = !!as.name(fold_info_cols[["fold_column"]])) %>%
      dplyr::mutate(AUC = auc_scores,
                    ROC = roc_curves)

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
      type = "multinomial",
      id_col = id_col,
      id_method = id_method,
      fold_info_cols = fold_info_cols,
      include_fold_columns = include_fold_columns,
      include_predictions = include_predictions)

    # Create unique temporary variable names
    local_tmp_target_var <- create_tmp_var(data,"one_vs_all_targets")
    local_tmp_predicted_probability_var <- create_tmp_var(data,"one_vs_all_predicted_probability")
    local_tmp_predicted_class_var <- create_tmp_var(data,"one_vs_all_predicted_class")

    # Count how many times each class are in the targets_col
    support <- create_support_object(data[[targets_col]])

    # Perform one vs all evaluations
    one_vs_all_evaluations <- plyr::ldply(1:num_classes, function(cl){

      # Create temporary columns with
      # one-vs-all targets and predictions
      data[[local_tmp_target_var]] <- factor(
        ifelse(data[[targets_col]] == classes[[cl]], 1, 0))
      data[[local_tmp_predicted_probability_var]] <- predicted_probabilities[[cl]]
      data[[local_tmp_predicted_class_var]] <- factor(
        ifelse(data[["predicted_class_index"]] == cl, 1, 0))

      # Perform binomial evaluation
      evaluate_predictions_binomial(
        data = data,
        predictions_col = local_tmp_predicted_probability_var,
        predicted_class_col = local_tmp_predicted_class_var,
        targets_col = local_tmp_target_var,
        model_was_null_col = model_was_null_col,
        cat_levels = levels(factor(c(0, 1))),
        fold_info_cols = fold_info_cols,
        fold_and_fold_col = fold_and_fold_col,
        model_specifics = model_specifics,
        metrics = setdiff(metrics, c("AUC","Lower CI","Upper CI")),
        include_fold_columns = include_fold_columns,
        include_predictions = FALSE,
        calculate_roc = FALSE,
        na.rm = ifelse(isTRUE(both_keep_and_remove_NAs), FALSE, na.rm)
      ) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(Class = classes[[cl]])

    }) %>%
      dplyr::left_join(support, by = "Class") %>%
      dplyr::as_tibble()

    # Remove Predictions column if it exists
    if ("Predictions" %in% colnames(one_vs_all_evaluations)){
      one_vs_all_evaluations[["Predictions"]] <- NULL
    }

    # Move Support column
    one_vs_all_evaluations <- reposition_column(one_vs_all_evaluations,
                                                "Support",
                                                .after = tail(
                                                  intersect(metrics, colnames(one_vs_all_evaluations)),
                                                  1)
                                                )

    # Place Class column first
    one_vs_all_evaluations <- one_vs_all_evaluations %>%
      dplyr::select(.data$Class, dplyr::everything())

    # Calculate overall metrics per fold column and average them:
    # 1. Extract the fold column results from each one-vs-all
    # 2. Use those to calculate the overall metrics per fold column (Becomes 'Results' tibble in overall results)
    # 3. Average those.
    # TODO (IMPORTANT THAT THIS IS ADDED TO NEWS.md)

    # Extract the metrics for calculating (weighted) averages
    metrics_only <- one_vs_all_evaluations %>%
      dplyr::pull(.data$Results) %>%
      dplyr::bind_rows()

    # Values to use in na.rm
    if (isTRUE(both_keep_and_remove_NAs))
      na.rm_values <- c(TRUE, FALSE)
    else
      na.rm_values <- na.rm

    # Calculate the average metrics
    average_metrics <- plyr::ldply(na.rm_values, function(nr){
      metrics_only %>%
        dplyr::group_by(!!as.name("Fold Column")) %>%
        dplyr::summarise_all(list(mean), na.rm = nr) %>%
        dplyr::mutate(NAs_removed = nr)
    })

    # Extract Support counts
    support <- one_vs_all_evaluations[["Support"]]

    # Calculate the weighted average metrics
    weighted_average_metrics <- plyr::ldply(na.rm_values, function(nr){
      metrics_only %>%
        dplyr::group_by(!!as.name("Fold Column")) %>%
        dplyr::summarise_all(list(
          ~ weighted.mean(., w = support, na.rm = nr))) %>%
        dplyr::rename_at(.vars = dplyr::vars(-dplyr::one_of("Fold Column")),
                          function(x) paste0("Weighted ", x)) %>%
        dplyr::mutate(NAs_removed = nr)
    })

    # Keep only the requested metrics
    weighted_average_metrics_to_keep <- intersect(
      c("Fold Column", metrics, "NAs_removed"),
      c(colnames(weighted_average_metrics)))
    weighted_average_metrics <- weighted_average_metrics %>%
      dplyr::select(dplyr::one_of(weighted_average_metrics_to_keep))

    # Gather summarized metrics
    fold_column_results <- dplyr::left_join(
      overall_metrics,
        dplyr::left_join(
          average_metrics,
          weighted_average_metrics,
          by = c("Fold Column", "NAs_removed")
        ),
      by = "Fold Column")

    overall_results <- fold_column_results %>%
      dplyr::select(-dplyr::one_of("Fold Column", "ROC"))

    # Average results
    overall_results <- plyr::ldply(na.rm_values, function(nr){ # TODO Document this behavior
      overall_results %>%
        dplyr::filter(!! as.name("NAs_removed") == nr) %>%
        dplyr::group_by(!! as.name("NAs_removed")) %>% # Ensures NAs_removed is kept and not summarized
        dplyr::summarise_all(~mean(., na.rm = nr))
    }) %>% dplyr::as_tibble()

    # Add nested predictions
    if (isTRUE(include_predictions) && !is.null(predictions_nested)){
      overall_results[["Predictions"]] <- repeat_list_if(predictions_nested$predictions, 2,
                                                         both_keep_and_remove_NAs)
    }

    # If we don't want the Overall Accuracy metric, remove it
    if ("Overall Accuracy" %ni% metrics){
      overall_results <- overall_results %>%
        dplyr::select(-dplyr::one_of("Overall Accuracy"))
      fold_column_results <- fold_column_results %>%
        dplyr::select(-dplyr::one_of("Overall Accuracy"))
    }

    # Add total counts confusion matrix
    # Try to fit a confusion matrix with the predictions and targets
    overall_confusion_matrix <- tryCatch({
      caret::confusionMatrix(factor(data[["predicted_class"]], levels = classes),
                             factor(data[[targets_col]], levels = classes))
    }, error = function(e) {
      stop(paste0('Confusion matrix error: ',e))
    })

    # Nest the confusion matrix
    nested_multiclass_confusion_matrix <- nest_multiclass_confusion_matrices(
      list(overall_confusion_matrix),
      include_fold_columns = include_fold_columns)[["confusion_matrices"]]

    # Add confusion matrix to overall results
    overall_results[["Confusion Matrix"]] <- repeat_list_if(
      nested_multiclass_confusion_matrix, 2,
      condition = both_keep_and_remove_NAs)

    fcr_all_cols <- colnames(fold_column_results)
    fcr_prediction_metrics <- intersect(metrics, fcr_all_cols)
    fcr_non_metric_cols <- setdiff(fcr_all_cols, c(fcr_prediction_metrics, "Fold Column"))
    fcr_new_order <- c("Fold Column", fcr_prediction_metrics, fcr_non_metric_cols)

    # Add the nested fold column results
    overall_results <- overall_results %>%
      dplyr::left_join(
        fold_column_results %>%
          dplyr::select(dplyr::one_of(fcr_new_order)) %>%
          dplyr::group_nest(!!as.name("NAs_removed"), .key = "Results") %>%
          tibble::as_tibble(),
        by = "NAs_removed")

    # Add the nested class level results
    overall_results[["Class Level Results"]] <- one_vs_all_evaluations %>%
      legacy_nest(seq_len(ncol(one_vs_all_evaluations))) %>%
      tibble::as_tibble() %>%
      dplyr::pull(.data$data) %>%
      repeat_list_if(2, condition = both_keep_and_remove_NAs)

    # Rearrange columns in results
    all_cols <- colnames(overall_results)
    prediction_metrics <- intersect(metrics, all_cols)
    non_metric_cols <- setdiff(all_cols, c(prediction_metrics, "Class Level Results", "Results"))
    new_order <- c(prediction_metrics, "Results", "Class Level Results", non_metric_cols)
    results <- overall_results %>%
      dplyr::select(dplyr::one_of(new_order))

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

