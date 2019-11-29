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
#'  When \code{type} is \code{"multinomial"}, the predictions should be passed as
#'  one column per class with the probability of that class. The columns should
#'  have the name of their class, as they are named in the target column. E.g.:
#'
#'  \tabular{rrrrr}{
#'   \strong{class_1} \tab \strong{class_2} \tab
#'   \strong{class_3} \tab \strong{target}\cr
#'   0.269 \tab 0.528 \tab 0.203 \tab class_2\cr
#'   0.368 \tab 0.322 \tab 0.310 \tab class_3\cr
#'   0.375 \tab 0.371 \tab 0.254 \tab class_2\cr
#'   ... \tab ... \tab ... \tab ...}
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
#' @param models Unnamed list of fitted model(s) for calculating R^2 metrics and information criterion metrics.
#'  May only work for some types of models.
#'
#'  When only passing one model, remember to pass it in a list (e.g. \code{list(m)}).
#'
#'  N.B. When \code{data} is grouped, provide one model per group in the same order as the groups.
#'
#'  N.B. When aggregating by ID (i.e. when \code{id_col} is not \code{NULL}),
#'  it's not currently possible to pass model objects,
#'  as these would not be aggregated by the IDs.
#'
#'  N.B. Currently, \strong{Gaussian only}.
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
#'   Default values (TRUE/FALSE) will be used for the remaining metrics available.
#'
#'   Also accepts the string \code{"all"}.
#'
#'   N.B. Currently, disabled metrics are still computed.
#' @param type Type of evaluation to perform:
#'
#'  \code{"gaussian"} for regression (like linear regression).
#'
#'  \code{"binomial"} for binary classification.
#'
#'  \code{"multinomial"} for multiclass classification.
#' @param include_predictions Whether to include the predictions
#'  in the output as a nested tibble. (Logical)
#' @details
#'
#'  Packages used:
#'
#'  \strong{Gaussian}:
#'
#'  r2m : \code{\link[MuMIn:r.squaredGLMM]{MuMIn::r.squaredGLMM}}
#'
#'  r2c : \code{\link[MuMIn:r.squaredGLMM]{MuMIn::r.squaredGLMM}}
#'
#'  AIC : \code{\link[stats:AIC]{stats::AIC}}
#'
#'  AICc : \code{\link[MuMIn:AICc]{MuMIn::AICc}}
#'
#'  BIC : \code{\link[stats:BIC]{stats::BIC}}
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
#'  Average \strong{RMSE}, \strong{MAE}, \strong{r2m},
#'  \strong{r2c}, \strong{AIC}, \strong{AICc}, and \strong{BIC}.
#'
#'  N.B. Some of the metrics will only be returned if model
#'  objects were passed, and will be \code{NA} if they could not be
#'  extracted from the passed model objects.
#'
#'  Also includes:
#'
#'  A nested tibble with the \strong{Predictions} and targets.
#'
#'  A nested tibble with the model \strong{Coefficients}. The coefficients
#'  are extracted from the model object with \code{\link[broom:tidy]{broom::tidy()}} or
#'  \code{\link[stats:coef]{coef()}} (with some restrictions on the output).
#'  If these attempts fail, a default coefficients tibble filled with \code{NA}s is returned.
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
#'  ROC:
#'
#'  \strong{AUC}, \strong{Lower CI}, and \strong{Upper CI}
#'
#'  Note, that the ROC curve is only computed if AUC is enabled. See \code{metrics}.
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
#'  Other available metrics (disabled by default, see \code{metrics}):
#'  \strong{Accuracy}, \strong{AIC}, \strong{AICc}, \strong{BIC}.
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
#'  \strong{AIC}, \strong{AICc}, \strong{BIC},
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
#'  \strong{Class Level Results}
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
#'                                           type = "response",
#'                                           allow.new.levels = TRUE)
#' data[["binomial_predictions"]] <- predict(binomial_model, data,
#'                                           allow.new.levels = TRUE)
#'
#' # Gaussian evaluation
#' evaluate(data = data, target_col = "age",
#'          prediction_cols = "gaussian_predictions",
#'          models = list(gaussian_model),
#'          type = "gaussian")
#'
#' # Binomial evaluation
#' evaluate(data = data, target_col = "diagnosis",
#'          prediction_cols = "binomial_predictions",
#'          type = "binomial")
#'
#' # Multinomial
#'
#' # Create a tibble with predicted probabilities
#' data_mc <- multiclass_probability_tibble(
#'     num_classes = 3, num_observations = 30,
#'     apply_softmax = TRUE, FUN = runif,
#'     class_name = "class_")
#'
#' # Add targets
#' class_names <- paste0("class_", c(1,2,3))
#' data_mc[["target"]] <- sample(x = class_names,
#'                               size = 30, replace = TRUE)
#'
#' # Multinomial evaluation
#' evaluate(data = data_mc, target_col = "target",
#'          prediction_cols = class_names,
#'          type = "multinomial")
#'
#' # ID evaluation
#'
#' # Gaussian ID evaluation
#' # Note that 'age' is the same for all observations
#' # of a participant
#' evaluate(data = data, target_col = "age",
#'          prediction_cols = "gaussian_predictions",
#'          id_col = "participant",
#'          type = "gaussian")
#'
#' # Binomial ID evaluation
#' evaluate(data = data, target_col = "diagnosis",
#'          prediction_cols = "binomial_predictions",
#'          id_col = "participant",
#'          id_method = "mean", # alternatively: "majority"
#'          type = "binomial")
#'
#' # Multinomial ID evaluation
#'
#' # Add IDs and new targets (must be constant within IDs)
#' data_mc[["target"]] <- NULL
#' data_mc[["id"]] <- rep(1:6, each = 5)
#' id_classes <- tibble::tibble(
#'     "id" = 1:6,
#'     target = sample(x = class_names, size = 6, replace = TRUE)
#' )
#' data_mc <- data_mc \%>\%
#'     dplyr::left_join(id_classes, by = "id")
#'
#' # Perform ID evaluation
#' evaluate(data = data_mc, target_col = "target",
#'          prediction_cols = class_names,
#'          id_col = "id",
#'          id_method = "mean", # alternatively: "majority"
#'          type = "multinomial")
#'
#' # Training and evaluating a multinomial model with nnet
#'
#' # Create a data frame with some predictors and a target column
#' class_names <- paste0("class_", 1:4)
#' data_for_nnet <- multiclass_probability_tibble(
#'     num_classes = 3, # Here, number of predictors
#'     num_observations = 30,
#'     apply_softmax = FALSE,
#'     FUN = rnorm,
#'     class_name = "predictor_") \%>\%
#'     dplyr::mutate(class = sample(
#'         class_names,
#'         size = 30,
#'         replace = TRUE))
#'
#' # Train multinomial model using the nnet package
#' mn_model <- nnet::multinom(
#'     "class ~ predictor_1 + predictor_2 + predictor_3",
#'     data = data_for_nnet)
#'
#' # Predict the targets in the dataset
#' # (we would usually use a test set instead)
#' predictions <- predict(mn_model, data_for_nnet,
#'                        type = "probs") \%>\%
#'     dplyr::as_tibble()
#'
#' # Add the targets
#' predictions[["target"]] <- data_for_nnet[["class"]]
#'
#' # Evaluate predictions
#' evaluate(data = predictions, target_col = "target",
#'          prediction_cols = class_names,
#'          type = "multinomial")
#' }
evaluate <- function(data,
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
                     include_predictions = TRUE,
                     parallel = FALSE
){

  run_evaluate(
    data = data,
    target_col = target_col,
    prediction_cols = prediction_cols,
    type = type,
    id_col = id_col,
    id_method = id_method,
    models = models,
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
                         caller = "evaluate()"
){

  # Test if type is allowed
  stopifnot(type %in% c("gaussian",
                        "binomial",
                        "multinomial"))

  # Convert families to the internally used
  family <- type

  # Check the passed arguments TODO Add more checks
  check_args_evaluate(data = data,
                      target_col = target_col,
                      prediction_cols = prediction_cols,
                      id_col = id_col,
                      models = models,
                      type = type,
                      apply_softmax = apply_softmax,
                      cutoff = cutoff,
                      positive = positive,
                      parallel = parallel,
                      include_predictions = include_predictions,
                      metrics = metrics)

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

  # Find number of classes if classification
  if (type == "binomial"){
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
  grouping_factor <- dplyr::group_indices(data)
  grouping_keys <- dplyr::group_keys(data)

  if (!is.null(models) && length(unique(grouping_factor)) != length(models)){
    stop(paste0("When the dataframe is grouped, ",
                "please provide a fitted model object per group or set models to NULL."))
  }

  # Add grouping factor with a unique tmp var
  local_tmp_grouping_factor_var <- create_tmp_var(data, ".group")
  data[[local_tmp_grouping_factor_var]] <- grouping_factor

  # Now that we've saved the groups
  # we can ungroup the dataset
  data <- data %>% dplyr::ungroup()

  # Create temporary prediction column name
  local_tmp_predictions_col_var <- create_tmp_var(data, "tmp_predictions_col")

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
    ) %>% dplyr::ungroup()

    if (family == "multinomial")
      prediction_cols <- local_tmp_predictions_col_var
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

    if (family == "multinomial"){

      # Prepare data for multinomial evaluation
      data <- prepare_multinomial_evaluation(data = data,
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
  parallel = FALSE
) {

  if (type != "gaussian"){
    if (is.null(num_classes)){
      num_classes <- length(unique(data[[targets_col]]))
    }
  }

  if (is.null(fold_info_cols)){

    tmp_fold_cols_obj <- create_tmp_fold_cols(data)
    data <- tmp_fold_cols_obj[["data"]]
    fold_info_cols <- tmp_fold_cols_obj[["fold_info_cols"]]
    include_fold_columns <- FALSE
  } else{
    include_fold_columns <- include_fold_columns
  }

  # Extract unique group identifiers
  unique_group_levels <- unique(data[[groups_col]])

  evaluations <- plyr::llply(seq_along(unique_group_levels),
                             .parallel = parallel, function(gr_ind){

    gr <- unique_group_levels[[gr_ind]]

    data_for_current_group <- data[data[[groups_col]] == gr ,]

    # Assign current model
    if (is.null(models)){
      model <- NULL
    } else {
      model <- list(models[[gr_ind]])
    }

    internal_evaluate(data = data_for_current_group,
                      type = type,
                      predictions_col = predictions_col,
                      targets_col = targets_col,
                      models = model,
                      id_col = id_col,
                      id_method = id_method,
                      fold_info_cols = fold_info_cols,
                      fold_and_fold_col = fold_and_fold_col,
                      model_specifics = model_specifics,
                      metrics = metrics,
                      info_cols = info_cols,
                      include_fold_columns = include_fold_columns,
                      include_predictions = include_predictions,
                      na.rm = na.rm)
  }) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble()

  # Add group key to class level results
  if (type == "multinomial"){

    grouping_keys <- repeat_data_frame_if(grouping_keys, 2,
                                          condition = na.rm == "both")

    # Extract all the class level results tibbles
    # Remove the Results column
    # And add the grouping keys
    class_level_results <- evaluations[["Class Level Results"]] %>%
      # ".__grouping__" is used when na.rm = "both"!
      dplyr::bind_rows(.id = ".__grouping__") %>%
      tibble::as_tibble() %>%
      base_deselect(cols = "Results")
    evaluations[["Class Level Results"]] <- NULL
    class_level_results <- grouping_keys %>%
      dplyr::slice(rep(1:dplyr::n(), each = num_classes)) %>%
      dplyr::bind_cols(class_level_results)

    # Nest class level results
    evaluations[["Class Level Results"]] <- class_level_results %>%
      dplyr::group_by_at(c(".__grouping__", colnames(grouping_keys))) %>%
      dplyr::group_nest(keep = TRUE) %>%
      # Remove ".__grouping__" again
      dplyr::mutate(data = purrr::map(.data$data,
                                      .f = ~ .x %>%
                                        base_deselect(cols = ".__grouping__"))) %>%
      dplyr::pull(.data$data)

  }

  # Add grouping keys
  results <- grouping_keys %>%
    dplyr::bind_cols(evaluations)

  # If na.rm != "both" and it contains the NAs_removed column
  if ((!is.character(na.rm) || na.rm != "both") &&
      "NAs_removed" %in% names(results))
    results[["NAs_removed"]] <- NULL

  return(results)

}

internal_evaluate <- function(data,
                              type = "gaussian",
                              predictions_col = "prediction",
                              targets_col = "target",
                              model_was_null_col = NULL,
                              fold_info_cols = list(rel_fold = "rel_fold",
                                                    abs_fold = "abs_fold",
                                                    fold_column = "fold_column"),
                              fold_and_fold_col = NULL,
                              models = NULL,
                              id_col = NULL,
                              id_method = NULL,
                              model_specifics = list(),
                              metrics = list(),
                              info_cols = list(),
                              include_fold_columns = TRUE,
                              include_predictions = TRUE,
                              na.rm = NULL) {

  stopifnot(type %in% c("gaussian", "binomial", "multinomial")) #, "multiclass", "multilabel"))

  # Fill metrics with default values for non-specified metrics
  # and get the names of the metrics
  metrics <- set_metrics(family = type, metrics_list = metrics,
                         include_model_object_metrics = !is.null(models))
  info_cols <- set_info_cols(family = type, info_cols_list = info_cols)
  if (!is.null(na.rm) && na.rm == "both")
    info_cols <- c(info_cols, "NAs_removed")

  # Set default na.rm if NULL
  if (is.null(na.rm)){
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
  if (is.null(model_was_null_col)){
    model_was_null_col <- "model_was_null"
    data[[model_was_null_col]] <- FALSE
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
    model_specifics = model_specifics,
    metrics = metrics,
    include_fold_columns = include_fold_columns,
    include_predictions = include_predictions,
    na.rm = na.rm
  )

  # TODO Remove "Results" from class level results when in evaluate()

  if (!is.null(models)){
    model_evaluations <- plyr::ldply(seq_len(length(models)), function(i){
      internal_evaluate_model(model = models[[i]],
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
  if (type == "multinomial"){
    ROCs <- output[["Results"]] %c% "ROC" %>%
      unlist(recursive = FALSE) %>%
      unlist(recursive = FALSE) # TODO Remove for repeated cv?
    output[["ROC"]] <- ROCs
  }

  new_col_order <- c(metrics, intersect(info_cols, colnames(output)))
  base_select(output, cols = new_col_order)
}

check_args_evaluate <- function(data,
                                target_col,
                                prediction_cols,
                                id_col,
                                models,
                                type,
                                apply_softmax,
                                cutoff,
                                positive,
                                parallel,
                                include_predictions,
                                metrics){

  # TODO Add more checks !!

  # Check columns
  # target_col
  if (!is.character(target_col)) {
    stop("'target_col' must be name of the dependent column in 'data'.")
  }
  if (target_col %ni% colnames(data)){
    stop(paste0("the 'target_col', ",target_col,", was not found in 'data'"))
  }

  # prediction_cols
  if (!is.character(prediction_cols)) {
    stop("'prediction_cols' must be name(s) of the prediction column(s) in 'data'.")
  }
  if (length(setdiff(prediction_cols, colnames(data))) > 0) {
    stop("not all names in 'prediction_cols' was found in 'data'.")
  }

  # id_col
  if (!is.null(id_col)){
    if (!is.character(id_col)) {
      stop("'id_col' must be either a column name or NULL.")
    }
    # if (type == "gaussian") {
    #   warning(paste0("'id_col' is ignored when type is '", type, "'."))
    # }
    if (id_col %ni% colnames(data)) {
      stop(paste0("the 'id_col', ", id_col, ", was not found in 'data'."))
    }
  }

  # softmax
  if (!is_logical_scalar_not_na(apply_softmax)){
    stop("'apply_softmax' must be logical scalar (TRUE/FALSE).")
  }

  # parallel
  if (!is_logical_scalar_not_na(parallel)){
    stop("'parallel' must be a logical scalar (TRUE/FALSE).")
  }

  # parallel
  if (!is_logical_scalar_not_na(include_predictions)){
    stop("'include_predictions' must be a logical scalar (TRUE/FALSE).")
  }

  # metrics
  if (!(is.list(metrics) || metrics == "all")){
    stop("'metrics' must be either a list or the string 'all'.")
  }

  if (is.list(metrics) && length(metrics)>0){
    if (!rlang::is_named(metrics)){
      stop("when 'metrics' is a non-empty list, it must be a named list.")
    }
  }

  # models
  if (!is.null(models)){
    if (!is.list(models)){
      stop("'models' must be provided as an unnamed list with fitted model object(s) or be set to NULL.")
    }
    if (length(models) == 0){
      stop(paste0(
        "'models' must be either NULL or an unnamed list with fitted model object(s). ",
        "'models' had length 0."))
    }
    if (!is.null(names(models))){
      if (length(intersect(names(models), c(
        "coefficients", "residuals", "effects", "call",
        "terms", "formula", "contrasts", "converged", "xlevels"
      ))) > 0){
        stop(paste0("'models' must be provided as an unnamed list with fitted model object(s). ",
                    "Did you pass the model object without putting it in a list?"))
      }
      stop("'models' must be provided as an *unnamed* list with fitted model objects.")
    }
  }

  if (type != "gaussian" && length(models) > 0){
    stop("Currently, 'models' can only be passed when 'type' is 'gaussian'.")
  }


  # TODO add for rest of args

}


create_tmp_fold_cols <- function(data){
  # Create fold columns
  local_tmp_fold_col_var <- create_tmp_var(data, "fold_column")
  local_tmp_rel_fold_col_var <- create_tmp_var(data, "rel_fold")
  local_tmp_abs_fold_col_var <- create_tmp_var(data, "abs_fold")

  data[[local_tmp_fold_col_var]] <- as.character(1)
  data[[local_tmp_rel_fold_col_var]] <- as.character(1)
  data[[local_tmp_abs_fold_col_var]] <- as.character(1)

  fold_info_cols <- list(rel_fold = local_tmp_rel_fold_col_var,
                         abs_fold = local_tmp_abs_fold_col_var,
                         fold_column = local_tmp_fold_col_var)

  list("data" = data,
       "fold_info_cols" = fold_info_cols)
}
