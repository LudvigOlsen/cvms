evaluate_predictions_binomial <- function(data,
                                          predictions_col,
                                          predicted_class_col = NULL,
                                          targets_col,
                                          model_was_null_col,
                                          id_col,
                                          id_method,
                                          cat_levels = NULL,
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
                                          calculate_roc = TRUE,
                                          na.rm = TRUE) {
  if (is.null(fold_and_fold_col)) {
    # Map of fold column, abs_fold and rel_fold
    fold_and_fold_col <- create_fold_and_fold_column_map(data, fold_info_cols)
  }

  # multinomial allows for na.rm = "both", but that is not useful here!
  if (is.null(na.rm) || is.character(na.rm) || !is.logical(na.rm)) {
    stop("'na.rm' must be logical")
  }

  # Unique fold columns
  unique_fold_cols <- unique(fold_and_fold_col[["fold_column"]])

  # Check if there are NAs in predictions or targets
  na_in_predictions <- contains_na(data[[predictions_col]])
  na_in_targets <- contains_na(data[[targets_col]])

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

  if (!na_in_targets && !na_in_predictions) {
    if (is.null(cat_levels)) {
      # Find the levels in the categorical target variable
      cat_levels <- sort(levels_as_characters(data[[targets_col]]))
    }

    if (length(cat_levels) > 2) {
      stop("The target column must maximally contain 2 levels.")
    }

    # Create a column with the predicted class based on the chosen cutoff
    # If it wasn't passed by parent function
    if (is.null(predicted_class_col)) {
      predicted_class_col <- create_tmp_name(data, "predicted_class")
      data[[predicted_class_col]] <- ifelse(data[[predictions_col]] < model_specifics[["cutoff"]],
        cat_levels[1], cat_levels[2]
      )
    }

    # Nest predictions and targets
    # Will be NA if any model_was_null is TRUE and
    # include_predictions is TRUE
    # If include_predictions is FALSE,
    # will always return NULL
    predictions_nested <- nest_predictions(
      data = data,
      predictions_col = predictions_col,
      predicted_class_col = predicted_class_col,
      targets_col = targets_col,
      model_was_null_col = model_was_null_col,
      type = "binomial",
      id_col = id_col,
      id_method = id_method,
      stds_col = stds_col,
      fold_info_cols = fold_info_cols,
      group_info = group_info,
      include_fold_columns = include_fold_columns,
      include_predictions = include_predictions
    )

    # Create confusion matrices
    confusion_matrices_list <- binomial_eval_confusion_matrices(
      data = data,
      targets_col = targets_col,
      predicted_class_col = predicted_class_col,
      unique_fold_cols = unique_fold_cols,
      cat_levels = cat_levels,
      positive = model_specifics[["positive"]],
      fold_info_cols = fold_info_cols,
      group_info = group_info,
      include_fold_columns = include_fold_columns,
      metrics = metrics
    )

    if (isTRUE(calculate_roc) && "AUC" %in% metrics) {
      # Create ROC curves
      roc_curves_list <- binomial_eval_roc_curves(
        data = data,
        targets_col = targets_col,
        predictions_col = predictions_col,
        unique_fold_cols = unique_fold_cols,
        cat_levels = cat_levels,
        positive = model_specifics[["positive"]],
        fold_info_cols = fold_info_cols,
        include_fold_columns = include_fold_columns
      )
    } else {
      roc_curves_list <- NULL
    }

    # Combine the different metrics and nested tibbles
    results <- binomial_eval_collect(
      unique_fold_cols = unique_fold_cols,
      roc_curves_list = roc_curves_list,
      confusion_matrices_list = confusion_matrices_list,
      predictions_nested = predictions_nested,
      # extra_metrics = extra_metrics,
      metrics = metrics,
      include_fold_columns = include_fold_columns,
      include_predictions = include_predictions,
      na.rm = na.rm,
      caller = model_specifics[["caller"]]
    )

    if (!isTRUE(calculate_roc)) {
      results[["ROC"]] <- NULL
    }
  } else {
    results <- binomial_classification_NA_results_tibble(
      metrics = metrics, include_predictions = include_predictions
    )
  }

  results
}


binomial_eval_confusion_matrices <- function(data,
                                             targets_col,
                                             predicted_class_col,
                                             unique_fold_cols,
                                             cat_levels,
                                             positive,
                                             fold_info_cols,
                                             group_info,
                                             include_fold_columns,
                                             metrics) {

  # Confusion matrices

  confusion_matrices <- plyr::llply(unique_fold_cols, function(fcol) {

    # Subset data
    fcol_data <- data[data[[fold_info_cols[["fold_column"]]]] == fcol, ]

    if (isTRUE(include_fold_columns)) {
      fold_col <- fcol
    } else {
      fold_col <- NULL
    }

    # Create confusion matrix and add to list
    fcol_conf_mat <- list("x" = fit_confusion_matrix(
      predicted_classes = fcol_data[[predicted_class_col]],
      targets = fcol_data[[targets_col]],
      cat_levels = cat_levels,
      positive = positive,
      fold_col = fold_col,
      group_info = group_info,
      metrics = metrics
    ))

    # Rename list element to the fold column name
    names(fcol_conf_mat) <- fcol

    fcol_conf_mat
  }) %>% unlist(recursive = FALSE)

  nested_confusion_matrices <- nest_confusion_matrices(confusion_matrices,
    fold_cols = unique_fold_cols,
    include_fold_columns = include_fold_columns
  )

  list(
    "nested_confusion_matrices" = nested_confusion_matrices,
    "confusion_matrices" = confusion_matrices
  )
}

binomial_eval_roc_curves <- function(data, targets_col, predictions_col,
                                     unique_fold_cols, cat_levels,
                                     positive, fold_info_cols,
                                     include_fold_columns) {

  # ROC curves

  # Prepare roc_cat_levels order first
  # Note that if this order is reverse of cat_levels,
  # the positive class will be correct when the probability is
  # smaller than a threshold (closer to 0).
  roc_cat_levels <- c(
    cat_levels[cat_levels != positive],
    cat_levels[cat_levels == positive]
  )
  roc_direction <- ifelse(all(roc_cat_levels == rev(cat_levels)), ">", "<")

  roc_curves <- plyr::llply(unique_fold_cols, function(fcol) {

    # Subset data
    fcol_data <- data[data[[fold_info_cols[["fold_column"]]]] == fcol, ]

    # Create ROC curve and add to list
    fcol_roc_curve <- list("x" = fit_roc_curve(
      predicted_probabilities = fcol_data[[predictions_col]],
      targets = fcol_data[[targets_col]],
      levels = roc_cat_levels,
      direction = roc_direction
    ))
    # Rename list element to the fold column name
    names(fcol_roc_curve) <- fcol

    fcol_roc_curve
  }) %>% unlist(recursive = FALSE)

  list("roc_curves" = roc_curves)
}

binomial_eval_collect <- function(unique_fold_cols,
                                  roc_curves_list,
                                  confusion_matrices_list,
                                  predictions_nested,
                                  metrics,
                                  include_fold_columns = TRUE,
                                  include_predictions = TRUE,
                                  na.rm = FALSE,
                                  caller) {
  if (!is.null(roc_curves_list)) {
    # Unpack args
    roc_curves <- roc_curves_list[["roc_curves"]]
  }
  confusion_matrices <- confusion_matrices_list[["confusion_matrices"]]
  nested_confusion_matrices <- confusion_matrices_list[["nested_confusion_matrices"]]

  # Fold column level results
  fold_col_results <- plyr::ldply(unique_fold_cols, function(fcol) {
    if (!is.null(roc_curves_list)) {
      current_roc_curve <- roc_curves[[fcol]]
    } else {
      current_roc_curve <- NULL
    }

    binomial_classification_results_tibble(
      roc_curve = current_roc_curve,
      roc_nested = NULL,
      confusion_matrix = confusion_matrices[[fcol]],
      predictions_nested = NULL,
      metrics = metrics,
      include_predictions = FALSE
    ) %>%
      dplyr::mutate(`Fold Column` = fcol)
  }) %>%
    dplyr::as_tibble()

  # Move Fold Column column to first in data frame
  # NOTE: Faster than select()
  fold_col_results <- fold_col_results[
    , c(
      "Fold Column",
      colnames(fold_col_results)[
        colnames(fold_col_results) != "Fold Column"
      ]
    )
  ]

  # Nest fold column results
  fold_col_results_nested <- fold_col_results
  fold_col_results_nested[["ROC"]] <- NULL
  fold_col_results_nested[["Positive Class"]] <- NULL
  fold_col_results_nested <- dplyr::group_nest(
    fold_col_results_nested,
    .key = "fold_col_results"
  )

  # Average fold column results for reporting
  average_metrics <- fold_col_results
  average_metrics[["Fold Column"]] <- NULL
  average_metrics[["ROC"]] <- NULL
  average_metrics[["Positive Class"]] <- NULL
  average_metrics <- dplyr::summarise_all(
    average_metrics, list(~ mean(., na.rm = na.rm))
  )

  # Gather the various results
  results <- average_metrics

  # Add predictions
  if (!is.null(predictions_nested) && isTRUE(include_predictions)) {
    results[["Predictions"]] <- predictions_nested$predictions
  }

  # Add results
  results[["Results"]] <- fold_col_results_nested$fold_col_results

  if (!is.null(roc_curves_list)) {
    # Add ROC curve info
    if (caller %in% c("evaluate()", "validate()")) {
      results[["ROC"]] <- roc_curves
    } else {
      results[["ROC"]] <- list(roc_curves)
    }
  } else {
    results[["ROC"]] <- NA
  }

  # Add confusion matrix
  results[["Confusion Matrix"]] <- nested_confusion_matrices$confusion_matrices

  # Add positive class (Same for all folds)
  results[["Positive Class"]] <- fold_col_results[["Positive Class"]][[1]]

  results
}

repeat_list_if <- function(l, n, condition) {
  if (isTRUE(condition)) {
    l <- l %>%
      rep(n)
  }

  l
}

repeat_data_frame_if <- function(data, n, condition) {
  if (isTRUE(condition)) {
    data <- data %>%
      dplyr::slice(
        rep(1:dplyr::n(), n)
      )
  }

  data
}

binomial_classification_NA_results_tibble <- function(metrics, include_predictions = TRUE) {
  eval_tibble <- tibble::tibble(
    "Balanced Accuracy" = NA,
    "Accuracy" = NA,
    "F1" = NA, "Sensitivity" = NA, "Specificity" = NA,
    "Pos Pred Value" = NA, "Neg Pred Value" = NA,
    "AUC" = NA, "Lower CI" = NA, "Upper CI" = NA,
    "Kappa" = NA,
    "MCC" = NA,
    "Detection Rate" = NA,
    "Detection Prevalence" = NA,
    "Prevalence" = NA,
    "False Neg Rate" = NA,
    "False Pos Rate" = NA,
    "False Discovery Rate" = NA,
    "False Omission Rate" = NA,
    "Threat Score" = NA,
    "Predictions" = NA,
    "Results" = list(NA),
    "ROC" = NA,
    "Positive Class" = NA
  )

  if (!isTRUE(include_predictions)) {
    eval_tibble[["Predictions"]] <- NULL
  }

  # eval_tibble <- eval_tibble[, c(intersect(metrics, colnames(eval_tibble)),
  #                                "Predictions", "ROC")]
  #
  # if (!isTRUE(include_predictions)){
  #   eval_tibble[["Predictions"]] <- NULL
  # }

  eval_tibble
}

binomial_classification_results_tibble <- function(roc_curve,
                                                   roc_nested,
                                                   confusion_matrix,
                                                   predictions_nested,
                                                   metrics,
                                                   include_predictions) {
  eval_tibble <- tibble::tibble(
    "AUC" = ifelse(!is.null(roc_curve), pROC::auc(roc_curve)[1], logical()),
    "Lower CI" = ifelse(!is.null(roc_curve), pROC::ci(roc_curve)[1], logical()),
    "Upper CI" = ifelse(!is.null(roc_curve), pROC::ci(roc_curve)[3], logical()),
    "Predictions" = ifelse(!is.null(predictions_nested),
      predictions_nested$predictions,
      logical()
    ),
    "ROC" = ifelse(!is.null(roc_nested), roc_nested$roc, logical())
  ) %>%
    dplyr::bind_cols(confusion_matrix %>%
      base_deselect(cols = c("Confusion Matrix", "Table")))

  eval_tibble <- eval_tibble[, c(
    intersect(metrics, colnames(eval_tibble)),
    "Predictions", "ROC", "Positive Class"
  )]

  if (!isTRUE(include_predictions)) {
    eval_tibble[["Predictions"]] <- NULL
  }

  eval_tibble
}

fit_confusion_matrix <- function(predicted_classes, targets,
                                 cat_levels, positive,
                                 fold_col = NULL,
                                 group_info = NULL,
                                 metrics = list()) {
  if (is.numeric(positive)) {
    positive <- cat_levels[positive]
  } else if (is.character(positive) && positive %ni% cat_levels) {
    stop(paste0(
      "When 'positive' is a character, it must correspond to a factor level in the dependent variable.",
      "\n'positive' is ", positive, " and levels are ", paste(cat_levels, collapse = " and "), "."
    ))
  }

  if (length(cat_levels) < 2) {
    stop(paste0("found less than 2 levels in the target column."))
  }

  # Try to use fit a confusion matrix with the predictions and targets
  conf_mat <- tryCatch(
    {
      call_confusion_matrix(
        targets = targets,
        predictions = predicted_classes,
        c_levels = cat_levels,
        metrics = metrics,
        positive = positive,
        do_one_vs_all = TRUE,
        parallel = FALSE,
        fold_col = fold_col,
        group_info = group_info
      )
    },
    error = function(e) {
      stop(paste0("Confusion matrix error: ", e))
    }
  )

  conf_mat
}

# levels must be ordered such that the positive class is last c(neg, pos)
fit_roc_curve <- function(predicted_probabilities, targets, levels = c(0, 1), direction = "<") {

  # Try to fit a ROC curve on the data
  roc_curve <- tryCatch(
    {
      pROC::roc(
        response = targets,
        predictor = predicted_probabilities,
        direction = direction,
        levels = levels
      )
    },
    error = function(e) {
      if (grepl("No control observation", as.character(e), ignore.case = TRUE) ||
        grepl("No case observation", as.character(e), ignore.case = TRUE)) {
        return(NULL)
      }

      stop(paste0("Receiver Operator Characteristic (ROC) Curve error: ", e))
    }
  )

  roc_curve
}

nest_confusion_matrices <- function(confusion_matrices,
                                    fold_cols = ".folds",
                                    include_fold_columns = TRUE) {
  tidy_confusion_matrices <- confusion_matrices %>%
    dplyr::bind_rows() %>%
    dplyr::pull(.data$`Confusion Matrix`) %>%
    dplyr::bind_rows()

  tidy_confusion_matrices <- dplyr::group_nest(tidy_confusion_matrices,
    .key = "confusion_matrices"
  )
  tidy_confusion_matrices
}

# Note, if only one confusion matrix object
# pass it in a list
nest_multiclass_confusion_matrices <- function(confusion_matrices,
                                               fold_cols = ".folds",
                                               include_fold_columns = TRUE) {


  # TODO Test this works here as well
  tidy_confusion_matrices <- confusion_matrices %>%
    dplyr::bind_rows() %>%
    dplyr::pull(.data$`Confusion Matrix`) %>%
    dplyr::bind_rows()

  tidy_confusion_matrices <- dplyr::group_nest(tidy_confusion_matrices,
    .key = "confusion_matrices"
  )
  tidy_confusion_matrices
}
