
#' @title Create a confusion matrix
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a confusion matrix from targets and predictions.
#'  Calculates associated metrics.
#'
#'  Multiclass results are based on one-vs-all evaluations.
#'  Both regular averaging and weighted averaging are available. Also calculates the \code{Overall Accuracy}.
#'
#'  \strong{Note}: In most cases you should use \code{\link[cvms:evaluate]{evaluate()}} instead. It has additional metrics and
#'  works in \code{magrittr} pipes (e.g. \code{\%>\%}) and with \code{\link[dplyr:group_by]{dplyr::group_by()}}.
#'  \code{confusion_matrix()} is more lightweight and may be preferred in programming when you don't need the extra stuff
#'  in \code{\link[cvms:evaluate]{evaluate()}}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family evaluation functions
#' @param targets \code{vector} with true classes. Either \code{numeric} or \code{character}.
#' @param predictions \code{vector} with predicted classes. Either \code{numeric} or \code{character}.
#' @param metrics \code{list} for enabling/disabling metrics.
#'
#'   E.g. \code{list("Accuracy" = TRUE)} would add the regular accuracy metric,
#'   whie \code{list("F1" = FALSE)} would remove the \code{F1} metric.
#'   Default values (TRUE/FALSE) will be used for the remaining available metrics.
#'
#'   You can enable/disable all metrics at once by including
#'   \code{"all" = TRUE/FALSE} in the \code{list}. This is done prior to enabling/disabling
#'   individual metrics, why for instance \code{list("all" = FALSE, "Accuracy" = TRUE)}
#'   would return only the \code{Accuracy} metric.
#'
#'   The \code{list} can be created with
#'   \code{\link[cvms:binomial_metrics]{binomial_metrics()}} or
#'   \code{\link[cvms:multinomial_metrics]{multinomial_metrics()}}.
#'
#'   Also accepts the string \code{"all"}.
#' @param positive Level from \code{`targets`} to predict.
#'  Either as character (\emph{preferable}) or level index (\code{1} or \code{2} - alphabetically). (\strong{Two-class only})
#'
#'  E.g. if we have the levels \code{"cat"} and \code{"dog"} and we want \code{"dog"} to be the positive class,
#'  we can either provide \code{"dog"} or \code{2}, as alphabetically, \code{"dog"} comes after \code{"cat"}.
#'
#'  \strong{Note:} For \emph{reproducibility}, it's preferable to \strong{specify the name directly}, as
#'  different \code{\link[base:locales]{locales}} may sort the levels differently.
#' @param c_levels \code{vector} with categorical levels in the targets. Should have same type as \code{`targets`}.
#'  If \code{NULL}, they are inferred from \code{`targets`}.
#'
#'  N.B. the levels are sorted alphabetically. When \code{`positive`} is numeric (i.e. an index),
#'  it therefore still refers to the index of the alphabetically sorted levels.
#' @param do_one_vs_all Whether to perform \emph{one-vs-all} evaluations
#'  when working with more than 2 classes (multiclass).
#'
#'  If you are only interested in the confusion matrix,
#'  this allows you to skip most of the metric calculations.
#' @param parallel Whether to perform the one-vs-all evaluations in parallel. (Logical)
#'
#'  N.B. This only makes sense when you have a lot of classes or a very large dataset.
#'
#'  Remember to register a parallel backend first.
#'  E.g. with \code{doParallel::registerDoParallel}.
#' @details
#'  The following formulas are used for calculating the metrics:
#'
#'  \code{Sensitivity = TP / (TP + FN)}
#'
#'  \code{Specificity = TN / (TN + FP)}
#'
#'  \code{Pos Pred Value = TP / (TP + FP)}
#'
#'  \code{Neg Pred Value = TN / (TN + FN)}
#'
#'  \code{Balanced Accuracy = (Sensitivity + Specificity) / 2}
#'
#'  \code{Accuracy = (TP + TN) / (TP + TN + FP + FN)}
#'
#'  \code{Overall Accuracy = Correct / (Correct + Incorrect)}
#'
#'  \code{F1 = 2 * Pos Pred Value * Sensitivity / (Pos Pred Value + Sensitivity)}
#'
#'  \code{MCC = ((TP * TN) - (FP * FN)) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))}
#'
#'  Note for \code{MCC}: Formula is for the \emph{binary} case. When the denominator is \code{0},
#'  we set it to \code{1} to avoid \code{NaN}.
#'  See the \code{metrics} vignette for the multiclass version.
#'
#'  \code{Detection Rate = TP / (TP + FN + TN + FP)}
#'
#'  \code{Detection Prevalence = (TP + FP) / (TP + FN + TN + FP)}
#'
#'  \code{Threat Score = TP / (TP + FN + FP)}
#'
#'  \code{False Neg Rate = 1 - Sensitivity}
#'
#'  \code{False Pos Rate = 1 - Specificity}
#'
#'  \code{False Discovery Rate = 1 - Pos Pred Value}
#'
#'  \code{False Omission Rate = 1 - Neg Pred Value}
#'
#'  For \strong{Kappa} the counts (\code{TP}, \code{TN}, \code{FP}, \code{FN}) are normalized to percentages (summing to 1).
#'  Then the following is calculated:
#'
#'  \code{p_observed = TP + TN}
#'
#'  \code{p_expected = (TN + FP) * (TN + FN) + (FN + TP) * (FP + TP)}
#'
#'  \code{Kappa = (p_observed - p_expected) / (1 - p_expected)}
#' @return
#'  \code{tibble} with:
#'
#'  Nested \strong{confusion matrix} (tidied version)
#'
#'  Nested confusion matrix (\strong{table})
#'
#'  The \strong{Positive Class}.
#'
#'  Multiclass only: Nested \strong{Class Level Results} with the two-class metrics,
#'  the nested confusion matrices, and the \strong{Support} metric, which is a
#'  count of the class in the target column and is used for the weighted average metrics.
#'
#'  The following metrics are available (see \code{`metrics`}):
#'
#'  \subsection{Two classes or more}{
#'
#'  \tabular{rrr}{
#'   \strong{Metric} \tab \strong{Name} \tab \strong{Default} \cr
#'   Balanced Accuracy \tab "Balanced Accuracy" \tab Enabled \cr
#'   Accuracy \tab "Accuracy" \tab Disabled \cr
#'   F1 \tab "F1" \tab Enabled \cr
#'   Sensitivity \tab "Sensitivity" \tab Enabled \cr
#'   Specificity \tab "Specificity" \tab Enabled \cr
#'   Positive Predictive Value \tab "Pos Pred Value" \tab Enabled \cr
#'   Negative Predictive Value \tab "Neg Pred Value" \tab Enabled \cr
#'   Kappa \tab "Kappa" \tab Enabled \cr
#'   Matthews Correlation Coefficient \tab "MCC" \tab Enabled \cr
#'   Detection Rate \tab "Detection Rate" \tab Enabled \cr
#'   Detection Prevalence \tab "Detection Prevalence" \tab Enabled \cr
#'   Prevalence \tab "Prevalence" \tab Enabled \cr
#'   False Negative Rate \tab "False Neg Rate" \tab Disabled \cr
#'   False Positive Rate \tab "False Pos Rate" \tab Disabled \cr
#'   False Discovery Rate \tab "False Discovery Rate" \tab Disabled \cr
#'   False Omission Rate \tab "False Omission Rate" \tab Disabled \cr
#'   Threat Score \tab "Threat Score" \tab Disabled \cr
#'  }
#'
#'  The \strong{Name} column refers to the name used in the package.
#'  This is the name in the output and when enabling/disabling in \code{`metrics`}.
#'  }
#'
#'  \subsection{Three classes or more}{
#'
#'  The metrics mentioned above (excluding \code{MCC})
#'  has a weighted average version (disabled by default; weighted by the \strong{Support}).
#'
#'  In order to enable a weighted metric, prefix the metric name with \code{"Weighted "} when specifying \code{`metrics`}.
#'
#'  E.g. \code{metrics = list("Weighted Accuracy" = TRUE)}.
#'
#'  \tabular{rrr}{
#'   \strong{Metric} \tab \strong{Name} \tab \strong{Default} \cr
#'   Overall Accuracy \tab "Overall Accuracy" \tab Enabled \cr
#'   Weighted * \tab "Weighted *" \tab Disabled \cr
#'   Multiclass MCC \tab "MCC" \tab Enabled \cr
#'  }
#'  }
#' @examples
#' \donttest{
#' # Attach cvms
#' library(cvms)
#'
#' # Two classes
#'
#' # Create targets and predictions
#' targets <- c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
#' predictions <- c(1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0)
#'
#' # Create confusion matrix with default metrics
#' cm <- confusion_matrix(targets, predictions)
#' cm
#' cm[["Confusion Matrix"]]
#' cm[["Table"]]
#'
#' # Three classes
#'
#' # Create targets and predictions
#' targets <- c(0, 1, 2, 1, 0, 1, 2, 1, 0, 1, 2, 1, 0)
#' predictions <- c(2, 1, 0, 2, 0, 1, 1, 2, 0, 1, 2, 0, 2)
#'
#' # Create confusion matrix with default metrics
#' cm <- confusion_matrix(targets, predictions)
#' cm
#' cm[["Confusion Matrix"]]
#' cm[["Table"]]
#'
#' # Enabling weighted accuracy
#'
#' # Create confusion matrix with Weighted Accuracy enabled
#' cm <- confusion_matrix(targets, predictions,
#'   metrics = list("Weighted Accuracy" = TRUE)
#' )
#' cm
#' }
confusion_matrix <- function(targets,
                             predictions,
                             metrics = list(),
                             positive = 2,
                             c_levels = NULL,
                             do_one_vs_all = TRUE,
                             parallel = FALSE) {
  call_confusion_matrix(
    targets = targets,
    predictions = predictions,
    metrics = metrics,
    positive = positive,
    c_levels = c_levels,
    do_one_vs_all = do_one_vs_all,
    parallel = parallel
  )
}

call_confusion_matrix <- function(targets,
                                  predictions,
                                  metrics = list(),
                                  positive = 2,
                                  c_levels = NULL,
                                  do_one_vs_all = TRUE,
                                  parallel = FALSE,
                                  fold_col = NULL,
                                  group_info = NULL,
                                  force_multiclass = FALSE) {
  if (checkmate::test_string(x = metrics, pattern = "^all$")) {
    metrics <- list("all" = TRUE)
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(
    x = group_info, null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_flag(x = do_one_vs_all, add = assert_collection)
  checkmate::assert_flag(x = parallel, add = assert_collection)
  checkmate::assert_flag(x = force_multiclass, add = assert_collection)
  checkmate::assert_vector(
    x = targets,
    any.missing = FALSE, add = assert_collection
  )
  checkmate::assert_vector(
    x = predictions,
    any.missing = FALSE, add = assert_collection
  )
  checkmate::assert_vector(
    x = fold_col, null.ok = TRUE,
    any.missing = FALSE, add = assert_collection
  )
  checkmate::assert_vector(
    x = c_levels, null.ok = TRUE,
    any.missing = FALSE, add = assert_collection
  )

  checkmate::assert(
    checkmate::check_choice(
      x = positive,
      choices = c(1, 2)
    ),
    checkmate::check_string(
      x = positive,
      min.chars = 1
    )
  )

  checkmate::reportAssertions(assert_collection)
  if (length(targets) != length(predictions)){
    assert_collection$push("'targets' and 'predictions' must have same length.")
  }

  checkmate::assert(
    checkmate::check_list(
      x = metrics,
      types = "logical",
      any.missing = FALSE,
      names = "named"
    ),
    checkmate::check_character(
      x = metrics,
      any.missing = FALSE,
    )
  )

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Not all of these are necessarily used in confusion_matrix()
  # but we don't want to throw an error for the their presence
  allowed_metrics <- c(
    set_metrics(family = "binomial", metrics_list = "all", include_model_object_metrics = TRUE),
    set_metrics(family = "multinomial", metrics_list = "all", include_model_object_metrics = TRUE)
  )

  if (checkmate::test_character(metrics) &&
      !checkmate::test_string(metrics, fixed = "all")) {
    non_allowed_metrics <- setdiff(metrics, allowed_metrics)
    if (length(non_allowed_metrics) > 0){
      assert_collection$push(
        paste0("'metrics' contained unknown metric name: ",
               paste0(non_allowed_metrics, collapse = ", "),"."))
      checkmate::reportAssertions(assert_collection)
    }
  }

  predictions_chr <- as.character(predictions)
  targets_chr <- as.character(targets)
  unique_levels_in_data <- sort(unique(c(predictions_chr, targets_chr)))

  if (is.null(c_levels)) {
    c_levels <- unique_levels_in_data
  } else {
    c_levels <- as.character(sort(c_levels))
  }

  if (length(setdiff(unique_levels_in_data,
                     as.character(c_levels))) > 0){
    assert_collection$push("'c_levels' does not contain all the levels in 'predictions' and 'targets'.")
    checkmate::reportAssertions(assert_collection)
  }

  if (length(c_levels) == 2 && !isTRUE(force_multiclass)) {

    # If not already a list of metric names
    if (typeof(metrics) != "character" ||
        (length(metrics) == 1 &&
         metrics == "all")) {
      metrics <- set_metrics(
        family = "binomial", metrics_list = metrics,
        include_model_object_metrics = FALSE
      )
    }

    cfm <- create_binomial_confusion_matrix(
      targets_chr = targets_chr,
      predictions_chr = predictions_chr,
      metrics = metrics,
      positive = positive,
      c_levels = c_levels
    )
  } else {
    if (positive != 2) {
      warning("'positive' is ignored in multiclass confusion matrices.")
    }

    # If not already a list of metric names
    if (typeof(metrics) != "character" ||
      (length(metrics) == 1 && metrics == "all")) {
      metrics <- set_metrics(
        family = "multinomial",
        metrics_list = metrics,
        include_model_object_metrics = FALSE
      )
    }

    cfm <- create_multinomial_confusion_matrix(
      targets = targets_chr,
      predictions = predictions_chr,
      metrics = metrics,
      c_levels = c_levels,
      do_one_vs_all = do_one_vs_all,
      parallel = parallel
    )
  }

  if (!is.null(fold_col)) {

    # Add fold column (Only happens in internal use)
    cfm[["Confusion Matrix"]][[1]] <- cfm[["Confusion Matrix"]][[1]] %>%
      tibble::add_column(
        "Fold Column" = fold_col,
        .before = "Prediction"
      )
  }

  if (!is.null(group_info)) {

    # Add group columns
    cfm[["Confusion Matrix"]][[1]] <- group_info %>%
      dplyr::slice(rep(1, nrow(cfm[["Confusion Matrix"]][[1]]))) %>%
      dplyr::bind_cols(cfm[["Confusion Matrix"]][[1]])
  }

  # Reorder metrics
  new_order <- c(setdiff(colnames(cfm), metrics),
                 intersect(metrics, colnames(cfm)))
  cfm <- cfm %>%
    base_select(cols = new_order)

  # Set extra classes
  class(cfm) <- c("cfm_results", class(cfm))
  cfm
}


create_binomial_confusion_matrix <- function(targets_chr,
                                             predictions_chr,
                                             metrics,
                                             positive,
                                             c_levels
                                             ) {

  conf_mat <- create_confusion_matrix(
    targets = factor(targets_chr, levels = c_levels),
    predictions = factor(predictions_chr, levels = c_levels)
  )

  tidy_conf_mat <- tidy_confusion_matrix(conf_mat, c_levels = c_levels)

  positive_level <- extract_positive_level(positive = positive, c_levels = c_levels)

  label_counts <- confusion_label_counts(
    tidy_conf_mat,
    pos_level = positive_level,
    c_levels = c_levels
  )

  # Get metric functions
  metric_fns <- confusion_matrix_metric_fns()

  # Calculate specified metrics

  # Extract metrics to compute
  metrics_to_compute <- intersect(names(metric_fns), metrics)

  # Compute metrics
  computed_metrics <- plyr::llply(metrics_to_compute, function(m) {
    metric_fns[[m]](label_counts)
  }) %>%
    setNames(metrics_to_compute) %>%
    dplyr::bind_cols() %>%
    dplyr::as_tibble()

  overall <- tibble::tibble(
    "Confusion Matrix" = list(tidy_conf_mat),
    "Table" = list(conf_mat)
  ) %>%
    dplyr::bind_cols(computed_metrics) %>%
    dplyr::mutate(`Positive Class` = as.character(positive_level))

  # Set extra classes
  class(overall) <- c("cfm_binomial", class(overall))
  overall
}


create_multinomial_confusion_matrix <- function(targets,
                                                predictions,
                                                metrics,
                                                c_levels,
                                                do_one_vs_all = TRUE,
                                                na.rm = FALSE,
                                                parallel = FALSE) {

  conf_mat <- create_confusion_matrix(
    targets = factor(targets, levels = c_levels),
    predictions = factor(predictions, levels = c_levels)
  )

  tidy_conf_mat <- tidy_confusion_matrix(conf_mat, c_levels = c_levels)

  overall <- tibble::tibble(
    "Confusion Matrix" = list(tidy_conf_mat),
    "Table" = list(conf_mat)
  )

  if ("Overall Accuracy" %in% metrics) {
    overall[["Overall Accuracy"]] <- overall_accuracy(
      targets = targets,
      predictions = predictions
    )
  }

  if ("MCC" %in% metrics) {
    overall[["MCC"]] <- multiclass_mcc(
      conf_mat
    )
  }

  if (isTRUE(do_one_vs_all)) {

    # If a metric is only included in its weighted version
    # we still need to calculate it in the one-vs-all evaluations
    metrics_to_compute <- unique(gsub("Weighted ", "", metrics))
    metrics_to_compute <- metrics_to_compute[metrics_to_compute != "MCC"]

    # Get metric functions
    metric_fns <- confusion_matrix_metric_fns()
    # Remove binary MCC
    metric_fns[["MCC"]] <- NULL

    # Extract the metrics we want weighted averages for
    metrics_for_weighted_avg <- gsub("Weighted ", "", metrics[grep("Weighted ", metrics)])
    metrics_for_weighted_avg <- intersect(names(metric_fns), metrics_for_weighted_avg)

    # Extract the metrics we want regular averages for
    metrics_for_avg <- metrics[!grepl("Weighted ", metrics)]
    metrics_for_avg <- intersect(names(metric_fns), metrics_for_avg)

    # Perform One-vs-All evaluations
    one_vs_all_evaluations <- plyr::ldply(c_levels,
      .parallel = parallel,
      function(cl) {

        # Create temporary columns with
        # one-vs-all targets and predictions
        binomial_tmp_targets <- ifelse(targets == cl, "1", "0")
        binomial_tmp_predictions <- ifelse(predictions == cl, "1", "0")

        create_binomial_confusion_matrix(
          targets_chr = binomial_tmp_targets,
          predictions_chr = binomial_tmp_predictions,
          metrics = metrics_to_compute,
          positive = "1",
          c_levels = c("0", "1")
        ) %>%
          tibble::add_column(
            "Class" = as.character(cl),
            "Support" = sum(targets == cl),
            .before = "Confusion Matrix"
          )
      }
    ) %>%
      dplyr::as_tibble()

    support <- one_vs_all_evaluations[["Support"]]
    one_vs_all_evaluations[["Positive Class"]] <- NULL

    # Set names for results within the one_vs_all_evaluations
    # to their class for easier `bind_rows()` extraction
    names(one_vs_all_evaluations[["Confusion Matrix"]]) <- one_vs_all_evaluations[["Class"]]

    metric_columns <- one_vs_all_evaluations %>%
      base_deselect(cols = c("Class", "Confusion Matrix", "Table", "Support"))

    if (length(metrics_for_avg) > 0) {
      avg_metrics <- metric_columns %>%
        base_select(metrics_for_avg) %>%
        dplyr::summarise_all(.funs = list(~ mean(., na.rm = na.rm)))

      overall <- overall %>%
        dplyr::bind_cols(avg_metrics)
    }

    if (length(metrics_for_weighted_avg) > 0) {
      weighted_avg_metrics <- metric_columns %>%
        base_select(metrics_for_weighted_avg) %>%
        dplyr::summarise_all(.funs = list(
          ~ weighted.mean(., w = support, na.rm = na.rm)
        ))
      colnames(weighted_avg_metrics) <- paste0(
        "Weighted ", colnames(weighted_avg_metrics)
      )

      overall <- overall %>%
        dplyr::bind_cols(weighted_avg_metrics)
    }

    overall[["Class Level Results"]] <- list(one_vs_all_evaluations)

    metrics_order <- intersect(metrics, colnames(overall))
    overall <- overall %>%
      base_select(cols = c(
        "Confusion Matrix", "Table",
        metrics_order, "Class Level Results"
      ))
  }

  # Set extra classes
  class(overall) <- c("cfm_multinomial", class(overall))
  overall
}

confusion_matrix_metric_fns <- function() {
  metric_fns <- list(
    "Balanced Accuracy" = balanced_accuracy,
    "Accuracy" = accuracy,
    "Sensitivity" = sensitivity,
    "Specificity" = specificity,
    "Pos Pred Value" = pos_pred_value,
    "Neg Pred Value" = neg_pred_value,
    "F1" = f1,
    "MCC" = mcc,
    "Kappa" = kappa,
    "Prevalence" = prevalence,
    "Detection Rate" = detection_rate,
    "Detection Prevalence" = detection_prevalence,
    "False Neg Rate" = false_neg_rate,
    "False Pos Rate" = false_pos_rate,
    "False Discovery Rate" = false_discovery_rate,
    "False Omission Rate" = false_omission_rate,
    "Threat Score" = threat_score
  )
}

# Convert a tidied binomial confusion matrix to the "table" format
# NOTE: Probably misses some attributes and class stuff to be an actual table
confusion_matrix_to_table <- function(conf_mat) {
  if (nrow(conf_mat) != 4) {
    stop("only works with 2-class confusion matrix.")
  }

  ns <- conf_mat[["N"]]
  c_levels <- conf_mat[["Prediction"]][1:2]
  m <- matrix(ns, ncol = 2, nrow = 2)
  colnames(m) <- c_levels
  rownames(m) <- c_levels
  names(dimnames(m)) <- c("Prediction", "Target")
  m
}

# example:
# create_confusion_matrix(factor(c(1,2,3), levels = c(1,2,3)),
#                         factor(c(1,1,1), levels = c(1,2,3)))
create_confusion_matrix <- function(targets, predictions) {
  if (!is.factor(targets)) {
    stop("'targets' must be a factor")
  }
  if (!is.factor(predictions)) {
    stop("'predictions' must be a factor")
  }
  if (!all(levels(targets) %in% levels(predictions)) ||
    !all(levels(predictions) %in% levels(targets))) {
    stop("'targets' and 'predictions' must have the same levels. Consider setting levels explicitly in the factors.")
  }

  if (length(targets) != length(predictions)){
    stop("'targets' and 'predictions' must have same length.")
  }

  Prediction <- predictions
  Target <- targets
  table(Prediction, Target)
}

tidy_confusion_matrix <- function(conf_mat, c_levels = NULL) {
  if (!tibble::is_tibble(conf_mat)) {
    conf_mat_df <- dplyr::as_tibble(conf_mat)
  }

  # If conf_mat was 2x2
  if (nrow(conf_mat_df) == 4) {
    if (is.null(c_levels)) {
      c_levels <- c("0", "1")
    }

    # Add Pos_* columns
    conf_mat_df[[paste0("Pos_", c_levels[[1]])]] <- c("TP", "FN", "FP", "TN")
    conf_mat_df[[paste0("Pos_", c_levels[[2]])]] <- c("TN", "FP", "FN", "TP")

    # Move and rename "n" column
    N <- conf_mat_df[["n"]]
    conf_mat_df[["n"]] <- NULL
    conf_mat_df[["N"]] <- N
  } else {
    conf_mat_df <- base_rename(conf_mat_df,
      before = "n",
      after = "N"
    )
  }

  conf_mat_df
}

extract_positive_level <- function(positive = 2, c_levels = NULL){

  if (is.null(c_levels)) {
    if (is.character(positive)) {
      stop("when 'positive' has type character, 'c_levels' must be passed")
    }

    c_levels <- c("0", "1")
  } else {
    c_levels <- sort(c_levels)
  }

  if (is.character(positive)) {
    if (!is.character(c_levels)) {
      stop("when 'positive' has type character, 'c_levels' must have type character as well.")
    }
    if (positive %ni% c_levels) {
      stop("'positive' was not found in 'c_levels'.")
    }

    pos_level <- positive
  } else {
    if (positive %ni% 1:2) {
      stop("when 'positive' is numeric, it must be either 1 or 2.")
    }

    pos_level <- c_levels[[positive]]
  }

  pos_level
}

confusion_label_counts <- function(tidy_conf_mat, pos_level, c_levels = NULL) {
  if (nrow(tidy_conf_mat) != 4) {
    stop("'confusion_label_counts' only works with 2x2 confusion matrices.")
  }

  pos_labels <- paste0("Pos_", pos_level)
  label_cols <- tidy_conf_mat %>%
    base_select(cols = c(pos_labels, "N")) %>%
    dplyr::mutate(N = as.numeric(.data$N)) %>%
    tidyr::spread(key = pos_labels, value = "N") %>%
    unlist()

  label_cols
}

check_label_counts <- function(label_counts) {
  if (length(setdiff(names(label_counts), c("FN", "FP", "TN", "TP"))) > 0 ||
    length(setdiff(c("FN", "FP", "TN", "TP"), names(label_counts))) > 0) {
    stop("'label_counts' must contain exactly the columns 'FN', 'FP', 'TN', and 'TP'.")
  }
}

total_label_count <- function(label_counts) {
  check_label_counts(label_counts)

  label_counts[["TP"]] + label_counts[["FN"]] +
    label_counts[["TN"]] + label_counts[["FP"]]
}

sensitivity <- function(label_counts) {

  # TP / (TP + FN)
  label_counts[["TP"]] / (label_counts[["TP"]] + label_counts[["FN"]])
}

specificity <- function(label_counts) {
  check_label_counts(label_counts)

  # TN / (TN + FP)
  label_counts[["TN"]] / (label_counts[["TN"]] + label_counts[["FP"]])
}

prevalence <- function(label_counts) {
  check_label_counts(label_counts)

  # (TP + FN) / (TP + FN + TN + FP)
  (label_counts[["TP"]] + label_counts[["FN"]]) / total_label_count(label_counts)
}

pos_pred_value <- function(label_counts) {

  # TODO Find out why caret uses such a weird formula for this?

  check_label_counts(label_counts)

  # TP / (TP + FP)
  label_counts[["TP"]] / (label_counts[["TP"]] + label_counts[["FP"]])
}

neg_pred_value <- function(label_counts) {

  # TODO Find out why caret uses such a weird formula for this?

  check_label_counts(label_counts)

  # TN / (TN + FN)
  label_counts[["TN"]] / (label_counts[["TN"]] + label_counts[["FN"]])
}

detection_rate <- function(label_counts) {
  check_label_counts(label_counts)

  # TP / (TP + FN + TN + FP)
  label_counts[["TP"]] / total_label_count(label_counts)
}

detection_prevalence <- function(label_counts) {
  check_label_counts(label_counts)

  # (TP + FP) / (TP + FN + TN + FP)
  (label_counts[["TP"]] + label_counts[["FP"]]) / total_label_count(label_counts)
}

false_neg_rate <- function(label_counts) {

  # FN / (FN + TP)
  1 - sensitivity(label_counts)
}

false_pos_rate <- function(label_counts) {

  # FP / (FP + TN)
  1 - specificity(label_counts)
}

false_discovery_rate <- function(label_counts) {

  # FP / (FP + TP)
  1 - pos_pred_value(label_counts)
}

false_omission_rate <- function(label_counts) {

  # FN / (FN + TN)
  1 - neg_pred_value(label_counts)
}

threat_score <- function(label_counts) {
  check_label_counts(label_counts)

  # TP / (TP + FN + FP)
  label_counts[["TP"]] / (label_counts[["TP"]] + label_counts[["FN"]] + label_counts[["FP"]])
}

balanced_accuracy <- function(label_counts) {
  check_label_counts(label_counts)

  # (sensitivity + specificity) / 2
  (sensitivity(label_counts) + specificity(label_counts)) / 2
}

accuracy <- function(label_counts) {
  check_label_counts(label_counts)

  # (TP + TN) / (TP + TN + FP + FN)
  (label_counts[["TP"]] + label_counts[["TN"]]) / total_label_count(label_counts)
}

f_score <- function(label_counts, beta = 1) {
  check_label_counts(label_counts)

  prec <- pos_pred_value(label_counts)
  reca <- sensitivity(label_counts)

  # (1 + beta^2) * precision * recall / ((beta^2 * precision) + recall)
  (1 + beta^2) * prec * reca / ((beta^2 * prec) + reca)
}

f1 <- function(label_counts) {
  f_score(label_counts, beta = 1)
}

kappa <- function(label_counts) {
  check_label_counts(label_counts)

  # Normalize counts (i.e. sum to 1)
  total <- total_label_count(label_counts)
  tp <- label_counts[["TP"]] / total
  tn <- label_counts[["TN"]] / total
  fp <- label_counts[["FP"]] / total
  fn <- label_counts[["FN"]] / total

  # Percentage observed agreement (correct predictions)
  p_observed <- tp + tn

  # Percentage expected agreement (correct by chance)
  p_expected <- (tn + fp) * (tn + fn) + (fn + tp) * (fp + tp)

  # Kappa
  (p_observed - p_expected) / (1 - p_expected)
}

mcc <- function(label_counts) {
  check_label_counts(label_counts)

  # ( (TP * TN) - (FP * FN) ) / sqrt( (TP + FP)(TP + FN)(TN + FP)(TN + FN) )
  # As is the case in other packages, if the denominator is 0,
  # we set it to 1 to avoid NaN.

  tp <- label_counts[["TP"]]
  tn <- label_counts[["TN"]]
  fp <- label_counts[["FP"]]
  fn <- label_counts[["FN"]]

  num <- (tp * tn) - (fp * fn)
  denom <- sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  if (denom == 0) {
    denom <- 1
  }
  num / denom
}

multiclass_mcc <- function(conf_mat_table){
  # Ported from sklearn:
  # https://github.com/scikit-learn/scikit-learn/blob/95d4f0841/sklearn/metrics/_classification.py
  cm <- as.matrix(conf_mat_table)
  t_sum <- rowSums(t(cm))
  p_sum <- rowSums(cm)
  n_correct <- sum(diag(cm))
  n_samples <- sum(cm)
  # %*% is base matrix multiplication
  cov_ytyp <- n_correct * n_samples - (t_sum %*% p_sum)
  cov_ypyp <- n_samples^2 - (p_sum %*% p_sum)
  cov_ytyt <- n_samples^2 - (t_sum %*% t_sum)
  mcc <- sum(cov_ytyp / sqrt(cov_ytyt * cov_ypyp))
  if (is.na(mcc)) mcc <- 0
  mcc
}


overall_accuracy <- function(targets, predictions, na.rm = FALSE) {
  if (length(targets) != length(predictions)) {
    stop("'predictions' and 'targets' must have the same number of elements.")
  }
  mean(targets == predictions, na.rm = na.rm)
}
