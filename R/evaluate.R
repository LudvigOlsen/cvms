

#   __________________ #< 1ee2f435e0cd344bcd4c561d5eb5542d ># __________________
#   Evaluate                                                                ####


#' @title Evaluate your model's performance
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Evaluate your model's predictions
#'  on a set of evaluation metrics.
#'
#'  Create ID-aggregated evaluations by multiple methods.
#'
#'  Currently supports regression and classification
#'  (binary and multiclass). See \code{type}.
#' @param data Data frame with predictions, targets and (optionally) an ID column.
#'  Can be grouped with \code{\link[dplyr]{group_by}}.
#'
#'  \subsection{Multinomial}{
#'  When \code{type} is \code{"multinomial"}, the predictions can be passed in one of two formats.
#'
#'  \subsection{Probabilities (Preferable)}{
#'
#'  One column per class with the probability of that class.
#'  The columns should have the name of their class,
#'  as they are named in the target column. E.g.:
#'
#'  \tabular{rrrrr}{
#'   \strong{class_1} \tab \strong{class_2} \tab
#'   \strong{class_3} \tab \strong{target}\cr
#'   0.269 \tab 0.528 \tab 0.203 \tab class_2\cr
#'   0.368 \tab 0.322 \tab 0.310 \tab class_3\cr
#'   0.375 \tab 0.371 \tab 0.254 \tab class_2\cr
#'   ... \tab ... \tab ... \tab ...}
#'  }
#'  \subsection{Classes}{
#'
#'  A single column with the predicted classes. E.g.:
#'
#'  \tabular{rrrrr}{
#'   \strong{prediction} \tab \strong{target}\cr
#'   class_2 \tab class_2\cr
#'   class_1 \tab class_3\cr
#'   class_1 \tab class_2\cr
#'   ... \tab ...}
#'
#'  }
#'  }
#'  \subsection{Binomial}{
#'  When \code{type} is \code{"binomial"}, the predictions should be passed as
#'  one column with the probability of class being
#'  the second class alphabetically
#'  (1 if classes are 0 and 1). E.g.:
#'
#'  \tabular{rrrrr}{
#'   \strong{prediction} \tab \strong{target}\cr
#'   0.769 \tab 1\cr
#'   0.368 \tab 1\cr
#'   0.375 \tab 0\cr
#'   ... \tab ...}
#'  }
#'  \subsection{Gaussian}{
#'  When \code{type} is \code{"gaussian"}, the predictions should be passed as
#'  one column with the predicted values. E.g.:
#'
#'  \tabular{rrrrr}{
#'   \strong{prediction} \tab \strong{target}\cr
#'   28.9 \tab 30.2\cr
#'   33.2 \tab 27.1\cr
#'   23.4 \tab 21.3\cr
#'   ... \tab ...}
#'  }
#' @param target_col Name of the column with the true classes/values in \code{data}.
#'
#'  When \code{type} is \code{"multinomial"}, this column should contain the class names,
#'  not their indices.
#' @param prediction_cols Name(s) of column(s) with the predictions.
#'
#'  When evaluating a classification task,
#'  the column(s) should contain the predicted probabilities.
#' @param id_col Name of ID column to aggregate predictions by.
#'
#'  N.B. Current methods assume that the target class/value is constant within the IDs.
#'
#'  N.B. When aggregating by ID, some metrics (such as those from model objects) are excluded.
#' @param id_method Method to use when aggregating predictions by ID. Either \code{"mean"} or \code{"majority"}.
#'
#'  When \code{type} is \code{gaussian}, only the \code{"mean"} method is available.
#'
#'  \subsection{mean}{
#'  The average prediction (value or probability) is calculated per ID and evaluated.
#'  This method assumes that the target class/value is constant within the IDs.
#'  }
#'  \subsection{majority}{
#'  The most predicted class per ID is found and evaluated. In case of a tie,
#'  the winning classes share the probability (e.g. \code{P = 0.5} each when two majority classes).
#'  This method assumes that the target class/value is constant within the IDs.
#'  }
#' @param apply_softmax Whether to apply the softmax function to the
#'  prediction columns when \code{type} is \code{"multinomial"}.
#'
#'  N.B. \strong{Multinomial models only}.
#' @param cutoff Threshold for predicted classes. (Numeric)
#'
#' N.B. \strong{Binomial models only}.
#' @param positive Level from dependent variable to predict.
#'  Either as character or level index (1 or 2 - alphabetically).
#'
#'  E.g. if we have the levels \code{"cat"} and \code{"dog"} and we want \code{"dog"} to be the positive class,
#'  we can either provide \code{"dog"} or \code{2}, as alphabetically, \code{"dog"} comes after \code{"cat"}.
#'
#'  Used when calculating confusion matrix metrics and creating ROC curves.
#'
#'  N.B. Only affects the evaluation metrics.
#'
#'  N.B. \strong{Binomial models only}.
#' @param parallel Whether to run evaluations in parallel,
#'  when \code{data} is grouped with \code{\link[dplyr]{group_by}}.
#' @param metrics List for enabling/disabling metrics.
#'
#'   E.g. \code{list("RMSE" = FALSE)} would remove RMSE from the regression results,
#'   and \code{list("Accuracy" = TRUE)} would add the regular accuracy metric
#'   to the classification results.
#'   Default values (TRUE/FALSE) will be used for the remaining available metrics.
#'
#'   You can enable/disable all metrics at once by including
#'   \code{"all" = TRUE/FALSE} in the list. This is done prior to enabling/disabling
#'   individual metrics, why f.i. \code{list("all" = FALSE, "RMSE" = TRUE)} would return only the RMSE metric.
#'
#'   The list can be created with
#'   \code{\link[cvms:gaussian_metrics]{gaussian_metrics()}},
#'   \code{\link[cvms:binomial_metrics]{binomial_metrics()}}, or
#'   \code{\link[cvms:multinomial_metrics]{multinomial_metrics()}}.
#'
#'   Also accepts the string \code{"all"}.
#' @param type Type of evaluation to perform:
#'
#'  \code{"gaussian"} for regression (like linear regression).
#'
#'  \code{"binomial"} for binary classification.
#'
#'  \code{"multinomial"} for multiclass classification.
#' @param include_predictions Whether to include the predictions
#'  in the output as a nested tibble. (Logical)
#' @param models Deprecated.
#' @details
#'
#'  Packages used:
#'
#'  \strong{Binomial} and \strong{Multinomial}:
#'
#'  ROC and related metrics:
#'
#'  Binomial: \code{\link[pROC:roc]{pROC::roc}}
#'
#'  Multinomial: \code{\link[pROC:multiclass.roc]{pROC::multiclass.roc}}
#' @return
#'  ----------------------------------------------------------------
#'
#'  \subsection{Gaussian Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  Tibble containing the following metrics by default:
#'
#'  Average \strong{RMSE}, \strong{MAE}, \strong{NRMSE}, \strong{RMSEIQR}.
#'
#'  Also includes:
#'
#'  A nested tibble with the \strong{Predictions} and targets.
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Binomial Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  Tibble with the following evaluation metrics, based on a
#'  confusion matrix and a ROC curve fitted to the predictions:
#'
#'  Confusion Matrix:
#'
#'  \strong{Balanced Accuracy},
#'  \strong{F1},
#'  \strong{Sensitivity},
#'  \strong{Specificity},
#'  \strong{Positive Prediction Value},
#'  \strong{Negative Prediction Value},
#'  \strong{Kappa},
#'  \strong{Detection Rate},
#'  \strong{Detection Prevalence},
#'  \strong{Prevalence}, and
#'  \strong{MCC} (Matthews correlation coefficient).
#'
#'  ROC:
#'
#'  \strong{AUC}, \strong{Lower CI}, and \strong{Upper CI}
#'
#'  Note, that the ROC curve is only computed if AUC is enabled. See \code{metrics}.
#'
#'  Other available metrics (disabled by default, see \code{metrics}):
#'  \strong{Accuracy}.
#'
#'  Also includes:
#'
#'  A nested tibble with the \strong{predictions} and targets.
#'
#'  A list of \strong{ROC} curve objects (if computed).
#'
#'  A nested tibble with the \strong{confusion matrix}.
#'  The \code{Pos_} columns tells you whether a row is a
#'  True Positive (TP), True Negative (TN), False Positive (FP), or False Negative (FN),
#'  depending on which level is the "\code{positive}" class. I.e. the level you wish to predict.
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Multinomial Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  For each class, a \emph{one-vs-all} binomial evaluation is performed. This creates
#'  a \strong{Class Level Results} tibble containing the same metrics as the binomial results
#'  described above (excluding AUC, Lower CI and Upper CI), along with the \strong{Support} metric, which is simply a
#'  count of the class in the target column. These metrics are used to calculate the macro metrics
#'  in the output tibble. The nested class level results tibble is also included in the output tibble,
#'  and would usually be reported along with the macro and overall metrics.
#'
#'  The output tibble contains the macro and overall metrics.
#'  The metrics that share their name with the metrics in the nested
#'  class level results tibble are averages of those metrics
#'  (note: does not remove \code{NA}s before averaging).
#'  In addition to these, it also includes the \strong{Overall Accuracy}.
#'
#'  Other available metrics (disabled by default, see \code{metrics}):
#'  \strong{Accuracy}, multiclass \strong{AUC},
#'  \strong{Weighted Balanced Accuracy}, \strong{Weighted Accuracy},
#'  \strong{Weighted F1}, \strong{Weighted Sensitivity}, \strong{Weighted Sensitivity},
#'  \strong{Weighted Specificity}, \strong{Weighted Pos Pred Value},
#'  \strong{Weighted Neg Pred Value}, \strong{Weighted Kappa}, \strong{Weighted MCC},
#'  \strong{Weighted Detection Rate}, \strong{Weighted Detection Prevalence}, and
#'  \strong{Weighted Prevalence}.
#'
#'  Note that the "Weighted" average metrics are weighted by the \code{Support}.
#'
#'  When having a large set of classes, we recommend keeping \code{AUC} disabled.
#'
#'  Also includes:
#'
#'  A nested tibble with the \strong{Predictions} and targets.
#'
#'  A list of \strong{ROC} curve objects when \code{AUC} is enabled.
#'
#'  A nested tibble with the multiclass \strong{Confusion Matrix}.
#'
#'  \subsection{Class Level Results}{
#'
#'  Besides the binomial evaluation metrics and the \code{Support} metric,
#'  the nested class level results tibble also contains:
#'
#'  A nested tibble with the \strong{Confusion Matrix} from the one-vs-all evaluation.
#'  The \code{Pos_} columns tells you whether a row is a
#'  True Positive (TP), True Negative (TN), False Positive (FP), or False Negative (FN),
#'  depending on which level is the "positive" class. In our case, \code{1} is the current class
#'  and \code{0} represents all the other classes together.
#'  }
#'  }
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family evaluation functions
#' @examples
#' \donttest{
#' # Attach packages
#' library(cvms)
#' library(dplyr)
#'
#' # Load data
#' data <- participant.scores
#'
#' # Fit models
#' gaussian_model <- lm(age ~ diagnosis, data = data)
#' binomial_model <- glm(diagnosis ~ score, data = data)
#'
#' # Add predictions
#' data[["gaussian_predictions"]] <- predict(gaussian_model, data,
#'   type = "response",
#'   allow.new.levels = TRUE
#' )
#' data[["binomial_predictions"]] <- predict(binomial_model, data,
#'   allow.new.levels = TRUE
#' )
#'
#' # Gaussian evaluation
#' evaluate(
#'   data = data, target_col = "age",
#'   prediction_cols = "gaussian_predictions",
#'   type = "gaussian"
#' )
#'
#' # Binomial evaluation
#' evaluate(
#'   data = data, target_col = "diagnosis",
#'   prediction_cols = "binomial_predictions",
#'   type = "binomial"
#' )
#'
#' # Multinomial
#'
#' # Create a tibble with predicted probabilities
#' data_mc <- multiclass_probability_tibble(
#'   num_classes = 3, num_observations = 30,
#'   apply_softmax = TRUE, FUN = runif,
#'   class_name = "class_"
#' )
#'
#' # Add targets
#' class_names <- paste0("class_", c(1, 2, 3))
#' data_mc[["target"]] <- sample(
#'   x = class_names,
#'   size = 30, replace = TRUE
#' )
#'
#' # Multinomial evaluation
#' evaluate(
#'   data = data_mc, target_col = "target",
#'   prediction_cols = class_names,
#'   type = "multinomial"
#' )
#'
#' # ID evaluation
#'
#' # Gaussian ID evaluation
#' # Note that 'age' is the same for all observations
#' # of a participant
#' evaluate(
#'   data = data, target_col = "age",
#'   prediction_cols = "gaussian_predictions",
#'   id_col = "participant",
#'   type = "gaussian"
#' )
#'
#' # Binomial ID evaluation
#' evaluate(
#'   data = data, target_col = "diagnosis",
#'   prediction_cols = "binomial_predictions",
#'   id_col = "participant",
#'   id_method = "mean", # alternatively: "majority"
#'   type = "binomial"
#' )
#'
#' # Multinomial ID evaluation
#'
#' # Add IDs and new targets (must be constant within IDs)
#' data_mc[["target"]] <- NULL
#' data_mc[["id"]] <- rep(1:6, each = 5)
#' id_classes <- tibble::tibble(
#'   "id" = 1:6,
#'   target = sample(x = class_names, size = 6, replace = TRUE)
#' )
#' data_mc <- data_mc %>%
#'   dplyr::left_join(id_classes, by = "id")
#'
#' # Perform ID evaluation
#' evaluate(
#'   data = data_mc, target_col = "target",
#'   prediction_cols = class_names,
#'   id_col = "id",
#'   id_method = "mean", # alternatively: "majority"
#'   type = "multinomial"
#' )
#'
#' # Training and evaluating a multinomial model with nnet
#'
#' # Create a data frame with some predictors and a target column
#' class_names <- paste0("class_", 1:4)
#' data_for_nnet <- multiclass_probability_tibble(
#'   num_classes = 3, # Here, number of predictors
#'   num_observations = 30,
#'   apply_softmax = FALSE,
#'   FUN = rnorm,
#'   class_name = "predictor_"
#' ) %>%
#'   dplyr::mutate(class = sample(
#'     class_names,
#'     size = 30,
#'     replace = TRUE
#'   ))
#'
#' # Train multinomial model using the nnet package
#' mn_model <- nnet::multinom(
#'   "class ~ predictor_1 + predictor_2 + predictor_3",
#'   data = data_for_nnet
#' )
#'
#' # Predict the targets in the dataset
#' # (we would usually use a test set instead)
#' predictions <- predict(mn_model, data_for_nnet,
#'   type = "probs"
#' ) %>%
#'   dplyr::as_tibble()
#'
#' # Add the targets
#' predictions[["target"]] <- data_for_nnet[["class"]]
#'
#' # Evaluate predictions
#' evaluate(
#'   data = predictions, target_col = "target",
#'   prediction_cols = class_names,
#'   type = "multinomial"
#' )
#' }
evaluate <- function(data,
                     target_col,
                     prediction_cols,
                     type,
                     id_col = NULL,
                     id_method = "mean",
                     apply_softmax = FALSE,
                     cutoff = 0.5,
                     positive = 2,
                     metrics = list(),
                     include_predictions = TRUE,
                     parallel = FALSE,
                     models = deprecated()) {
  if (!rlang::is_missing(models)) {
    deprecate_stop("1.0.0", "cvms::evaluate(models = )",
      details = "Now only evaluates predictions."
    )
  }

  # Remove unnecessary columns
  data <- base_select(
    data,
    c(
      colnames(dplyr::group_keys(data)),
      target_col,
      prediction_cols,
      id_col
    )
  ) %>%
    dplyr::as_tibble() %>% # removes grouping
    dplyr::group_by_at(colnames(dplyr::group_keys(data)))

  run_evaluate(
    data = data,
    target_col = target_col,
    prediction_cols = prediction_cols,
    type = type,
    id_col = id_col,
    id_method = id_method,
    models = NULL,
    apply_softmax = apply_softmax,
    cutoff = cutoff,
    positive = positive,
    metrics = metrics,
    include_predictions = include_predictions,
    parallel = parallel,
    caller = "evaluate()"
  )
}

run_evaluate <- function(data,
                         target_col,
                         prediction_cols,
                         type = "gaussian",
                         id_col = NULL,
                         id_method = "mean",
                         models = NULL,
                         apply_softmax = FALSE,
                         cutoff = 0.5,
                         positive = 2,
                         metrics = list(),
                         fold_info_cols = NULL,
                         fold_and_fold_col = NULL,
                         include_predictions = TRUE,
                         include_fold_columns = TRUE,
                         parallel = FALSE,
                         na.rm = NULL,
                         caller = "evaluate()") {
  if (checkmate::test_string(x = metrics, pattern = "^all$")) {
    metrics <- list("all" = TRUE)
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  # Data frame
  checkmate::assert_data_frame(
    x = data, min.rows = 1,
    min.cols = 2,
    col.names = "named",
    add = assert_collection
  )

  # String
  checkmate::assert_string(
    x = target_col, min.chars = 1,
    add = assert_collection
  )
  checkmate::assert_character(
    x = prediction_cols,
    min.len = 1,
    min.chars = 1,
    add = assert_collection
  )
  checkmate::assert_string(
    x = id_col, min.chars = 1, null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_string(x = caller, add = assert_collection)

  # Names
  checkmate::reportAssertions(assert_collection) # before names check
  checkmate::assert_names(
    x = colnames(data),
    must.include = c(target_col, prediction_cols, id_col),
    what = "colnames",
    add = assert_collection
  )

  # Choice
  checkmate::assert_choice(
    x = type,
    choices = c("gaussian", "binomial", "multinomial"),
    add = assert_collection
  )
  checkmate::assert_choice(
    x = id_method,
    choices = c("mean", "majority"),
    add = assert_collection
  )

  # Flag
  checkmate::assert_flag(x = apply_softmax, add = assert_collection)
  checkmate::assert_flag(x = include_predictions, add = assert_collection)
  checkmate::assert_flag(x = include_fold_columns, add = assert_collection)
  checkmate::assert_flag(x = parallel, add = assert_collection)
  checkmate::assert_flag(x = na.rm, null.ok = TRUE, add = assert_collection)

  # List
  checkmate::assert_list(
    x = models, null.ok = TRUE,
    min.len = 1, names = "unnamed",
    add = assert_collection
  )
  checkmate::assert_list(
    x = metrics,
    types = "logical",
    any.missing = FALSE,
    names = "named",
    add = assert_collection
  )
  checkmate::assert_list(
    x = fold_info_cols,
    types = "character",
    any.missing = FALSE,
    names = "named",
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_data_frame(
    x = fold_and_fold_col,
    col.names = "named",
    any.missing = FALSE,
    null.ok = TRUE,
    add = assert_collection
  )

  # Number
  checkmate::assert_number(
    x = cutoff,
    lower = 0,
    upper = 1,
    add = assert_collection
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
  # End of argument checks ####

  # Convert families to the internally used
  family <- type

  # Create basic model_specifics object
  model_specifics <- list(
    model_formula = "",
    family = family,
    REML = NULL,
    link = NULL,
    control = NULL,
    cutoff = cutoff,
    positive = positive,
    model_verbose = FALSE,
    model_fn = NULL,
    predict_fn = NULL,
    preprocess_fn = NULL,
    preprocess_once = NULL,
    hparams = NULL,
    caller = caller
  ) %>%
    update_model_specifics()

  info_cols <- list("Results" = FALSE)

  # One-hot encode predicted classes, if multinomial
  # and prediction_cols is one column with classes
  if (type == "multinomial" && length(prediction_cols) == 1) {
    # Extract the categorical levels in both target and prediction cols
    c_levels <- union(data[[target_col]], data[[prediction_cols]])
    data <- one_hot_encode(data, prediction_cols,
      c_levels = c_levels,
      use_epsilon = FALSE
    )
    prediction_cols <- sort(c_levels)

    if (isTRUE(apply_softmax)) {
      stop(paste0(
        "When passing 'prediction_cols' as single column with multiple classes, ",
        "'apply_softmax' should be FALSE."
      ))
    }
  }

  # Find number of classes if classification
  if (type == "binomial") {
    num_classes <- 2
  } else if (type == "multinomial") {
    # We might not have every available class
    # in the targets, so we rely on the number
    # of prediction cols instead
    num_classes <- length(prediction_cols)
  } else {
    num_classes <- NULL
  }

  # If the dataset is grouped, we need the indices and keys for the groups
  # so we can evaluate group wise

  # Get grouping keys
  grouping_keys <- dplyr::group_keys(data)
  # Make sure, the grouping_keys and the dataset are in the same order
  # As we otherwise risk adding them in the wrong order later
  data <- dplyr::arrange(data, !!!rlang::syms(colnames(grouping_keys)))
  # Get group indices
  grouping_factor <- dplyr::group_indices(data)

  # Map for joining the grouping keys to the indices
  grouping_keys_with_indices <- grouping_keys %>%
    dplyr::mutate(.group = 1:dplyr::n())

  if (!is.null(models) && length(unique(grouping_factor)) != length(models)) {
    stop(paste0(
      "When the dataframe is grouped, ",
      "please provide a fitted model object per group or set models to NULL."
    ))
  }

  # Add grouping factor with a unique tmp var
  local_tmp_grouping_factor_var <- create_tmp_name(data, ".group")
  data[[local_tmp_grouping_factor_var]] <- grouping_factor

  # Now that we've saved the groups
  # we can ungroup the dataset
  data <- data %>% dplyr::ungroup()

  # Create temporary prediction column name
  local_tmp_predictions_col_var <- create_tmp_name(data, "tmp_predictions_col")

  if (!is.null(id_col)) {

    # ID level evaluation

    # Currently don't support model object metrics
    # in ID aggregation mode
    if (!is.null(models)){
      stop("When aggregating by ID, 'models' should be NULL.")
    }

    # Prepare data for ID level evaluation
    data_for_id_evaluation <- prepare_id_level_evaluation(
      data = data,
      target_col = target_col,
      prediction_cols = prediction_cols,
      family = family,
      cutoff = cutoff,
      id_col = id_col,
      id_method = id_method,
      groups_col = local_tmp_grouping_factor_var,
      apply_softmax = FALSE,
      new_prediction_col_name = local_tmp_predictions_col_var
    ) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(grouping_keys_with_indices, by = ".group")

    if (family == "multinomial") {
      prediction_cols <- local_tmp_predictions_col_var
    }
    # TODO Test that prediction_cols is correct for the other families?

    # Run ID level evaluation
    evaluations <- run_internal_evaluate_wrapper(
      data = data_for_id_evaluation,
      type = family,
      predictions_col = prediction_cols,
      targets_col = target_col,
      id_col = id_col,
      id_method = id_method,
      fold_info_cols = fold_info_cols,
      fold_and_fold_col = fold_and_fold_col,
      groups_col = local_tmp_grouping_factor_var,
      grouping_keys = grouping_keys,
      models = models,
      model_specifics = model_specifics,
      metrics = metrics,
      info_cols = info_cols,
      num_classes = num_classes,
      parallel = parallel,
      include_predictions = include_predictions,
      include_fold_columns = include_fold_columns,
      na.rm = na.rm
    )
  } else {

    # Regular evaluation

    if (family == "multinomial") {

      # Prepare data for multinomial evaluation
      data <- prepare_multinomial_evaluation(
        data = data,
        target_col = target_col,
        prediction_cols = prediction_cols,
        apply_softmax = apply_softmax,
        new_prediction_col_name = local_tmp_predictions_col_var
      )

      prediction_cols <- local_tmp_predictions_col_var
    } else {
      if (length(prediction_cols) > 1) {
        stop(paste0("'prediction_cols' must have length 1 when type is '", type, "'."))
      }
    }

    # Run evaluation
    evaluations <- run_internal_evaluate_wrapper(
      data = data,
      type = family,
      predictions_col = prediction_cols,
      targets_col = target_col,
      models = models,
      fold_info_cols = fold_info_cols,
      fold_and_fold_col = fold_and_fold_col,
      groups_col = local_tmp_grouping_factor_var,
      grouping_keys = grouping_keys,
      model_specifics = model_specifics,
      metrics = metrics,
      info_cols = info_cols,
      num_classes = num_classes,
      parallel = parallel,
      include_predictions = include_predictions,
      include_fold_columns = include_fold_columns,
      na.rm = na.rm
    )
  }

  evaluations
}


run_internal_evaluate_wrapper <- function(
                                          data,
                                          type,
                                          predictions_col,
                                          targets_col,
                                          models,
                                          groups_col,
                                          grouping_keys,
                                          id_col = NULL,
                                          id_method = NULL,
                                          fold_info_cols = NULL,
                                          fold_and_fold_col = NULL,
                                          model_specifics,
                                          metrics = list(),
                                          info_cols = list(),
                                          include_predictions = TRUE,
                                          include_fold_columns = TRUE,
                                          num_classes = NULL,
                                          na.rm = NULL,
                                          parallel = FALSE) {
  if (type != "gaussian") {
    if (is.null(num_classes)) {
      num_classes <- length(unique(data[[targets_col]]))
    }
  }

  if (is.null(fold_info_cols)) {
    tmp_fold_cols_obj <- create_tmp_fold_cols(data)
    data <- tmp_fold_cols_obj[["data"]]
    fold_info_cols <- tmp_fold_cols_obj[["fold_info_cols"]]
    include_fold_columns <- FALSE
  } else {
    include_fold_columns <- include_fold_columns
  }

  # Extract unique group identifiers
  unique_group_levels <- unique(data[[groups_col]])

  evaluations <- plyr::llply(seq_along(unique_group_levels),
    .parallel = parallel, function(gr_ind) {
      gr <- unique_group_levels[[gr_ind]]

      data_for_current_group <- data[data[[groups_col]] == gr, ]


      # Assign current model
      if (is.null(models)) {
        model <- NULL
      } else {
        model <- list(models[[gr_ind]])
      }

      internal_evaluate(
        data = data_for_current_group,
        type = type,
        predictions_col = predictions_col,
        targets_col = targets_col,
        models = model,
        id_col = id_col,
        id_method = id_method,
        fold_info_cols = fold_info_cols,
        fold_and_fold_col = fold_and_fold_col,
        grouping_key_names = colnames(grouping_keys),
        model_specifics = model_specifics,
        metrics = metrics,
        info_cols = info_cols,
        include_fold_columns = include_fold_columns,
        include_predictions = include_predictions,
        na.rm = na.rm
      )
    }
  ) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  # Add group key to class level results
  if (type == "multinomial") {
    grouping_keys <- repeat_data_frame_if(grouping_keys, 2,
      condition = na.rm == "both"
    )

    # Extract all the class level results tibbles
    # Remove the Results column
    # And add the grouping keys
    class_level_results <- evaluations[["Class Level Results"]] %>%
      # ".__grouping__" is used when na.rm = "both"!
      dplyr::bind_rows(.id = ".__grouping__") %>%
      tibble::as_tibble() %>%
      base_deselect(cols = "Results")
    evaluations[["Class Level Results"]] <- NULL
    grouping_keys[[".__grouping__"]] <- as.character(seq_len(nrow(grouping_keys)))
    evaluations[[".__grouping__"]] <- as.character(seq_len(nrow(evaluations)))
    class_level_results <- grouping_keys %>%
      # TODO This part might not work with na.rm = "both"
      dplyr::right_join(class_level_results, by = ".__grouping__")

    # Nest class level results
    class_level_results <- class_level_results %>%
      dplyr::group_by_at(c(".__grouping__", colnames(grouping_keys))) %>%
      dplyr::group_nest(keep = TRUE) %>%
      # Remove ".__grouping__" again
      dplyr::mutate(data = purrr::map(.data$data,
        .f = ~ .x %>%
          base_deselect(cols = ".__grouping__")
      )) %>%
      base_rename(before = "data", after = "Class Level Results") %>%
      base_select(c(".__grouping__", "Class Level Results"))

    evaluations <- evaluations %>%
      dplyr::left_join(class_level_results, by = ".__grouping__") %>%
      base_deselect(".__grouping__")
  }

  # Add grouping keys
  results <- grouping_keys %>%
    dplyr::bind_cols(evaluations) %>%
    base_deselect(".__grouping__")

  # If na.rm != "both" and it contains the NAs_removed column
  if ((!is.character(na.rm) || na.rm != "both") &&
    "NAs_removed" %in% names(results)) {
    results[["NAs_removed"]] <- NULL
  }

  return(results)
}

internal_evaluate <- function(data,
                              type = "gaussian",
                              predictions_col = "prediction",
                              targets_col = "target",
                              model_was_null_col = NULL,
                              fold_info_cols = list(
                                rel_fold = "rel_fold",
                                abs_fold = "abs_fold",
                                fold_column = "fold_column"
                              ),
                              fold_and_fold_col = NULL,
                              grouping_key_names = NULL,
                              models = NULL,
                              id_col = NULL,
                              id_method = NULL,
                              model_specifics = list(),
                              metrics = list(),
                              info_cols = list(),
                              include_fold_columns = TRUE,
                              include_predictions = TRUE,
                              na.rm = NULL) {
  stopifnot(type %in% c("gaussian", "binomial", "multinomial")) # , "multiclass", "multilabel"))

  # Fill metrics with default values for non-specified metrics
  # and get the names of the metrics
  metrics <- set_metrics(
    family = type, metrics_list = metrics,
    include_model_object_metrics = !is.null(models)
  )
  info_cols <- set_info_cols(family = type, info_cols_list = info_cols)
  if (!is.null(na.rm) && na.rm == "both") {
    info_cols <- c(info_cols, "NAs_removed")
  }

  # Set default na.rm if NULL
  if (is.null(na.rm)) {
    na.rm <- dplyr::case_when(
      type == "gaussian" ~ TRUE,
      type == "binomial" ~ FALSE,
      type == "multinomial" ~ FALSE
    )
  }

  # data is a table with predictions, targets and folds
  # predictions can be values, logits, or classes, depending on evaluation type

  # Unless specified otherwise, we don't care about non-converged models etc. here
  # so we tell the eval that the models were not NULL. (TODO Would we even have predictions otherwise?)
  if (is.null(model_was_null_col)) {
    model_was_null_col <- "model_was_null"
    data[[model_was_null_col]] <- FALSE
  }

  # Extract grouping key info
  if (!is.null(grouping_key_names)) {
    group_info <- data %>%
      base_select(grouping_key_names)
  } else {
    group_info <- NULL
  }


  # Evaluate the predictions
  prediction_evaluation <- internal_evaluate_predictions(
    data = data,
    predictions_col = predictions_col,
    targets_col = targets_col,
    model_was_null_col = model_was_null_col,
    id_col = id_col,
    id_method = id_method,
    type = type,
    fold_info_cols = fold_info_cols,
    fold_and_fold_col = fold_and_fold_col,
    group_info = group_info,
    model_specifics = model_specifics,
    metrics = metrics,
    include_fold_columns = include_fold_columns,
    include_predictions = include_predictions,
    na.rm = na.rm
  )

  if (!is.null(models)) {
    model_evaluations <- plyr::ldply(seq_len(length(models)), function(i) {
      internal_evaluate_model(
        model = models[[i]],
        train_data = NULL,
        test_data = NULL,
        type = type,
        fold_info = NULL,
        fold_info_cols = fold_info_cols,
        model_specifics = model_specifics,
        metrics = metrics,
        include_fold_columns = include_fold_columns
      )
    })

    output <- dplyr::bind_cols(model_evaluations, prediction_evaluation)
  } else {
    output <- prediction_evaluation
  }

  # Extract ROC from Results col
  if (type == "multinomial") {
    ROCs <- output[["Results"]] %c% "ROC" %>%
      unlist(recursive = FALSE) %>%
      unlist(recursive = FALSE) # TODO Remove for repeated cv?
    output[["ROC"]] <- ROCs
  }

  new_col_order <- c(metrics, intersect(info_cols, colnames(output)))
  base_select(output, cols = new_col_order)
}

create_tmp_fold_cols <- function(data) {
  # Create fold columns
  local_tmp_fold_col_var <- create_tmp_name(data, "fold_column")
  local_tmp_rel_fold_col_var <- create_tmp_name(data, "rel_fold")
  local_tmp_abs_fold_col_var <- create_tmp_name(data, "abs_fold")

  data[[local_tmp_fold_col_var]] <- as.character(1)
  data[[local_tmp_rel_fold_col_var]] <- as.character(1)
  data[[local_tmp_abs_fold_col_var]] <- as.character(1)

  fold_info_cols <- list(
    rel_fold = local_tmp_rel_fold_col_var,
    abs_fold = local_tmp_abs_fold_col_var,
    fold_column = local_tmp_fold_col_var
  )

  list(
    "data" = data,
    "fold_info_cols" = fold_info_cols
  )
}
