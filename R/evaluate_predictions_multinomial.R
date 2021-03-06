evaluate_predictions_multinomial <- function(data,
                                             prediction_col,
                                             target_col,
                                             model_was_null_col,
                                             id_col,
                                             id_method,
                                             fold_info_cols = list(
                                               rel_fold = "rel_fold",
                                               abs_fold = "abs_fold",
                                               fold_column = "fold_column"
                                             ),
                                             fold_and_fold_col = NULL,
                                             group_info = NULL,
                                             stds_col = NULL,
                                             model_specifics,
                                             metrics,
                                             include_fold_columns,
                                             include_predictions,
                                             na.rm = TRUE) {

  # TODO Consider if adding more tests here is worth it?
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, min.cols = 2, add = assert_collection)
  checkmate::assert_string(x = prediction_col, add = assert_collection)
  checkmate::assert_string(x = stds_col, null.ok = TRUE, add = assert_collection)
  if (model_specifics[["positive"]] != 2) {
    assert_collection$push("'positive' must be 2 for multinomial evaluation.")
  }
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(x = names(data),
                          must.include = c(prediction_col, target_col),
                          type = "unique",
                          add = assert_collection)
  checkmate::assert_list(x = data[[prediction_col]], add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  predicted_probabilities <- tryCatch({
    dplyr::bind_rows(data[[prediction_col]])
    }, error = function(e) {
      stop(
        paste0(
          "Could not bind the specified 'prediction_col': ",
          prediction_col,
          ". Got error: ",
          e
        )
      )
    }
  )

  # Make sure probabilities sum to 1 row-wise
  probability_row_sums <- predicted_probabilities %>%
    dplyr::mutate(
      row_sum = round(rowSums(.) * 1e+5) / 1e+5,
      sums_to_one = .data$row_sum == 1
    )
  if (any(!probability_row_sums[["sums_to_one"]])) {
    stop("'multinomial' evaluate(): Not all probabilities added up to 1 row-wise (tolerance of 5 decimals).")
  }

  # Check if there are NAs in predictions or targets
  na_in_predictions <- contains_na(predicted_probabilities)
  na_in_targets <- contains_na(data[[target_col]])

  # Warn if NA in predictions
  if (isTRUE(na_in_predictions)) {
    warning(paste0(
      model_specifics[["caller"]], ": ",
      "predictions contained NA."
    ))
  }

  # Warn if NA in targets
  if (isTRUE(na_in_targets)) {
    warning(paste0(
      model_specifics[["caller"]], ": ",
      "targets contained NA."
    ))
  }

  if (is.null(fold_and_fold_col)) {
    # Map of fold column, abs_fold and rel_fold
    fold_and_fold_col <- create_fold_and_fold_column_map(data, fold_info_cols)
  }

  # Unique fold columns
  unique_fold_cols <- unique(fold_and_fold_col[["fold_column"]])

  # Keep both aggregates with and without removing NAs
  both_keep_and_remove_NAs <- is.character(na.rm) && na.rm == "both"

  if (!na_in_targets && !na_in_predictions) {

    # Convert target column to type character
    data[[target_col]] <- as.character(data[[target_col]])

    # Find the levels in the categorical target variable
    cat_levels_in_target_col <- levels_as_characters(data[[target_col]], sort_levels=TRUE)
    pred_col_classes <- colnames(predicted_probabilities)

    # Find classes that are not in targets and were never predicted (always 0.0 probability)
    classes_not_in_target_col <- setdiff(pred_col_classes, cat_levels_in_target_col)
    missing_classes <- purrr::map(.x = classes_not_in_target_col, .f = ~{
      if (all(predicted_probabilities[[.x]] == 0.0))
        .x
    }) %>% unlist(recursive = TRUE)

    # Remove those 'missing' classes
    classes <- sort(setdiff(pred_col_classes, missing_classes))
    if (length(missing_classes) > 0){
      data[[prediction_col]] <- purrr::map(.x = data[[prediction_col]], .f = ~{
        base_deselect(.x, cols = missing_classes)
      })
    }

    num_classes <- length(classes)

    # You may not have all the classes in the target (e.g. bad partitioning)
    # And you may not have all the classes in the probability columns.
    # The following errors if there are classes in target
    # that does not have a probability column

    if (length(setdiff(cat_levels_in_target_col, classes)) != 0) {
      stop(paste0(
        "The column names in the nested predicted probabilities ",
        "do not match the class names in the dependent column."
      ))
    }

    # Order columns by the class names
    predicted_probabilities <- base_select(predicted_probabilities,
      cols = classes
    )

    # Create a column with the predicted class
    data[["predicted_class_index"]] <- argmax(predicted_probabilities)
    data[["predicted_class"]] <- purrr::map_chr(
      data[["predicted_class_index"]],
      .f = function(x) {
        classes[[x]]
      }
    )

    # Compute multiclass metrics and create confusion matrix
    overall_metrics <- plyr::ldply(unique_fold_cols, function(fcol) {

      # Extract current fold column
      fcol_data <- data[data[[fold_info_cols[["fold_column"]]]] == fcol, ]

      # Create multiclass confusion matrix
      # Also calculates MCC and Overall Accuracy
      fcol_confusion_matrix <- tryCatch({
        call_confusion_matrix(
          targets = fcol_data[[target_col]],
          predictions = fcol_data[["predicted_class"]],
          c_levels = classes,
          metrics = metrics,
          do_one_vs_all = FALSE,
          force_multiclass = TRUE
        )
      },
      error = function(e) {
        stop(paste0("Confusion matrix error: ", e))
      })

      if ("AUC" %in% metrics){

        # Extract and prepare probabilities
        fcol_probabilities <- as.data.frame(
          dplyr::bind_rows(fcol_data[[prediction_col]])
        )

        # Calculate multiclass ROC
        roc <- pROC::multiclass.roc(
          response = fcol_data[[target_col]],
          predictor = fcol_probabilities,
          levels = cat_levels_in_target_col
        )
        fcol_confusion_matrix[["ROC"]] <- list(roc)
        fcol_confusion_matrix[["AUC"]] <- as.numeric(pROC::auc(roc))
      }

      fcol_confusion_matrix %>%
        tibble::add_column(`Fold Column` = fcol,
                           .before = colnames(fcol_confusion_matrix)[[1]])
    }) %>% dplyr::as_tibble()

    # Nest predictions and targets
    # Will be NA if any model_was_null is TRUE and
    # include_predictions is TRUE
    # If include_predictions is FALSE,
    # will always return NULL
    predictions_nested <- nest_predictions(
      data = data,
      prediction_col = prediction_col,
      predicted_class_col = "predicted_class",
      target_col = target_col,
      model_was_null_col = model_was_null_col,
      type = "multinomial",
      id_col = id_col,
      id_method = id_method,
      stds_col = stds_col,
      fold_info_cols = fold_info_cols,
      group_info = group_info,
      include_fold_columns = include_fold_columns,
      include_predictions = include_predictions
    )

    # Create unique temporary variable names
    local_tmp_target_var <- create_tmp_name(data, "one_vs_all_targets")
    local_tmp_predicted_probability_var <- create_tmp_name(data, "one_vs_all_predicted_probability")
    local_tmp_predicted_class_var <- create_tmp_name(data, "one_vs_all_predicted_class")

    # Count how many times each class are in the target_col
    support <- create_support_object(data[[target_col]])

    # Find the metrics to calculate in one-vs-all
    one_vs_all_metrics <- setdiff(metrics, c("AUC", "Lower CI", "Upper CI", "MCC"))
    one_vs_all_metrics <- unique(gsub("Weighted ", "", one_vs_all_metrics))

    # Perform one vs all evaluations
    one_vs_all_evaluations <- plyr::ldply(seq_len(num_classes), function(cl) {

      # Create temporary columns with
      # one-vs-all targets and predictions
      data[[local_tmp_target_var]] <- factor(
        ifelse(data[[target_col]] == classes[[cl]], 1, 0)
      )
      data[[local_tmp_predicted_probability_var]] <- predicted_probabilities[[cl]]
      data[[local_tmp_predicted_class_var]] <- factor(
        ifelse(data[["predicted_class_index"]] == cl, 1, 0)
      )

      # Create temporary group info
      if (is.null(group_info)) {
        tmp_group_info <- tibble::tibble("Class" = cl)
      } else {
        tmp_group_info <- group_info
        tmp_group_info[["Class"]] <- cl
      }

      # Perform binomial evaluation
      evaluate_predictions_binomial(
        data = data,
        prediction_col = local_tmp_predicted_probability_var,
        predicted_class_col = local_tmp_predicted_class_var,
        target_col = local_tmp_target_var,
        id_col=NULL,
        model_was_null_col = model_was_null_col,
        cat_levels = levels(factor(c(0, 1))),
        fold_info_cols = fold_info_cols,
        fold_and_fold_col = fold_and_fold_col,
        group_info = tmp_group_info,
        model_specifics = model_specifics,
        metrics = one_vs_all_metrics,
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

    one_vs_all_evaluations[["Predictions"]] <- NULL
    one_vs_all_evaluations[["Positive Class"]] <- NULL
    one_vs_all_evaluations[["Process"]] <- NULL

    # Move Support column
    one_vs_all_evaluations <- reposition_column(
      one_vs_all_evaluations,
      "Support",
      .after = tail(
        intersect(metrics, colnames(one_vs_all_evaluations)),
        1
      )
    )

    # Place Class column first
    one_vs_all_evaluations <- one_vs_all_evaluations[
      , c("Class", setdiff(colnames(one_vs_all_evaluations), "Class"))
    ]

    # Set names for results within the one_vs_all_evaluations
    # to their class for easier `bind_rows()` extraction
    names(one_vs_all_evaluations[["Results"]]) <- one_vs_all_evaluations[["Class"]]
    names(one_vs_all_evaluations[["Confusion Matrix"]]) <- one_vs_all_evaluations[["Class"]]

    # Add Class column to class level fold results
    one_vs_all_evaluations[["Results"]] <- purrr::map2(
      .x = one_vs_all_evaluations[["Results"]],
      .y = one_vs_all_evaluations[["Class"]],
      .f = ~{tibble::add_column(.x, "Class"=.y, .before = names(.x)[[1]])}
    )

    # Set correct Class in class level confusion matrices
    one_vs_all_evaluations[["Confusion Matrix"]] <- purrr::map2(
      .x = one_vs_all_evaluations[["Confusion Matrix"]],
      .y = one_vs_all_evaluations[["Class"]],
      .f = ~{.x[["Class"]] <- .y; .x}
    )

    # Calculate overall metrics per fold column and average them:
    # 1. Extract the fold column results from each one-vs-all
    # 2. Use those to calculate the overall metrics per fold column (Becomes 'Results' tibble in overall results)
    # 3. Average those.

    # Extract the metrics for calculating (weighted) averages
    metrics_only <- one_vs_all_evaluations[["Results"]] %>%
      dplyr::bind_rows() %>%
      base_deselect("Class")

    # Values to use in na.rm
    if (isTRUE(both_keep_and_remove_NAs)) {
      na.rm_values <- c(TRUE, FALSE)
    } else {
      na.rm_values <- na.rm
    }

    # Calculate the average metrics
    average_metrics <- plyr::ldply(na.rm_values, function(nr) {
      metrics_only %>%
        dplyr::group_by(!!as.name("Fold Column")) %>%
        dplyr::summarise_all(list(mean), na.rm = nr) %>%
        dplyr::mutate(NAs_removed = nr)
    })

    # Extract Support counts
    support <- one_vs_all_evaluations[["Support"]]

    # Calculate the weighted average metrics
    weighted_average_metrics <- plyr::ldply(na.rm_values, function(nr) {
      metrics_only %>%
        dplyr::group_by(!!as.name("Fold Column")) %>%
        dplyr::summarise_all(list(
          ~ weighted.mean(., w = support, na.rm = nr)
        )) %>%
        dplyr::rename_at(
          .vars = dplyr::vars(-dplyr::one_of("Fold Column")),
          function(x) paste0("Weighted ", x)
        ) %>%
        dplyr::mutate(NAs_removed = nr)
    })

    # Keep only the requested metrics
    weighted_average_metrics_to_keep <- intersect(
      c("Fold Column", metrics, "NAs_removed"),
      c(colnames(weighted_average_metrics))
    )
    weighted_average_metrics <- base_select(
      weighted_average_metrics,
      cols = weighted_average_metrics_to_keep
    )

    # Gather summarized metrics
    fold_column_results <- dplyr::left_join(
      overall_metrics,
      dplyr::left_join(
        average_metrics,
        weighted_average_metrics,
        by = c("Fold Column", "NAs_removed")
      ),
      by = "Fold Column"
    )

    # Remove non-numeric columns and unwanted metrics
    overall_results <- base_select(
      fold_column_results, cols = c(
        "NAs_removed",
        intersect(colnames(fold_column_results), metrics))
      )

    # Average results # TODO Is this necessary?
    overall_results <- plyr::ldply(na.rm_values, function(nr) { # TODO Document this behavior
      overall_results[overall_results[["NAs_removed"]] == nr, ] %>%
        dplyr::group_by(!!as.name("NAs_removed")) %>% # Ensures NAs_removed is kept and not summarized
        dplyr::summarise_all(~ mean(., na.rm = nr))
    }) %>% dplyr::as_tibble()

    # Add nested predictions
    if (isTRUE(include_predictions) && !is.null(predictions_nested)) {
      overall_results[["Predictions"]] <- repeat_list_if(
        predictions_nested$predictions, 2,
        both_keep_and_remove_NAs
      )
    }

    overall_confusion_matrix <- fold_column_results %>%
      base_select(cols = c("Fold Column", "Confusion Matrix")) %>%
      legacy_unnest(.data$`Confusion Matrix`)

    # Add group info
    if (!is.null(group_info)) {
      overall_confusion_matrix <- group_info %>%
        dplyr::slice(rep(1, nrow(overall_confusion_matrix))) %>%
        dplyr::bind_cols(overall_confusion_matrix)
    }

    # Nest the confusion matrix
    nested_mc_confusion_matrix <- overall_confusion_matrix
    if (!isTRUE(include_fold_columns))
      nested_mc_confusion_matrix[["Fold Column"]] <- NULL
    nested_mc_confusion_matrix <- nested_mc_confusion_matrix %>%
      dplyr::group_nest(.key = "Confusion Matrix") %>%
      dplyr::pull(.data$`Confusion Matrix`)

    # Add confusion matrix to overall results
    overall_results[["Confusion Matrix"]] <- repeat_list_if(
      nested_mc_confusion_matrix, 2,
      condition = both_keep_and_remove_NAs
    )

    # Fold column results
    fcr_all_cols <- colnames(fold_column_results)
    fcr_prediction_metrics <- intersect(metrics, fcr_all_cols)
    fcr_non_metric_cols <- setdiff(fcr_all_cols, c(fcr_prediction_metrics, "Fold Column",
                                                   "Confusion Matrix", "Table"))
    fcr_new_order <- c("Fold Column", fcr_prediction_metrics, fcr_non_metric_cols)

    # Add the nested fold column results
    overall_results <- overall_results %>%
      dplyr::left_join(
        fold_column_results %>%
          base_select(cols = fcr_new_order) %>%
          dplyr::group_nest(!!as.name("NAs_removed"),
                            .key = "Results") %>%
          tibble::as_tibble(),
        by = "NAs_removed"
      )

    # Add the nested class level results
    overall_results[["Class Level Results"]] <- one_vs_all_evaluations %>%
      dplyr::group_nest() %>%
      tibble::as_tibble() %>%
      dplyr::pull(.data$data) %>%
      repeat_list_if(2, condition = both_keep_and_remove_NAs)

    # Add process information
    overall_results[["Process"]] <- list(
      process_info_multinomial(
        data = data,
        target_col = target_col,
        prediction_cols = classes,
        pred_class_col = "predicted_class",
        id_col = id_col,
        apply_softmax = model_specifics[["for_process"]][["apply_softmax"]],
        cat_levels = classes
      )
    )

    # Rearrange columns in results
    all_cols <- colnames(overall_results)
    prediction_metrics <- intersect(metrics, all_cols)
    non_metric_cols <- setdiff(all_cols, c(prediction_metrics, "Class Level Results", "Results"))
    new_order <- c(prediction_metrics, "Results", "Class Level Results", non_metric_cols)
    results <- overall_results %>%
      base_select(cols = new_order)

  } else {

    # TODO replace with multinomial NA result
    stop("NOT YET IMPLEMENTED!")
    results <- binomial_classification_NA_results_tibble()

    if (length(unique_fold_cols) > 1) {
      results[["Results"]] <- NA
    }
  }

  results
}


argmax_row <- function(...) {
  x <- unname(c(...))
  which.max(x)
}

argmax <- function(data) {
  purrr::pmap_dbl(data, .f = argmax_row)
}

# Counts how many times each class are in the target_col
create_support_object <- function(targets) {
  support <- data.frame(table(targets), stringsAsFactors = F)
  colnames(support) <- c("Class", "Support")
  support[["Class"]] <- as.character(support[["Class"]])
  support
}
