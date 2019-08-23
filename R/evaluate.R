# TODO Find another name, as evaluate is already in generics::evaluate
# Isn't there an appropriate synonym?

#' @title Evaluate your model's performance
#' @description Evaluate your model's predictions
#'  on a set of evaluation metrics.
#'
#'  Create ID-aggregated evaluations by multiple methods.
#'
#'  Currently supports linear regression, binary classification
#'  and multiclass classification (see \code{type}).
#'
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
#'  When \code{type} is \code{"multinomial"}, this column should contain the names of the classes,
#'  not their indices.
#' @param prediction_cols Name(s) of column(s) with the predictions.
#'
#'  When evaluating a classification task,
#'  the column(s) should contain the predicted probabilities.
#' @param id_col Name of ID column to aggregate predictions by.
#'
#'  N.B. Current methods assume that the target class/value is constant within the IDs.
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
#'   E.g. \code{list("RMSE" = FALSE)} would remove RMSE from the results,
#'   and \code{list("Accuracy" = TRUE)} would add the regular accuracy metric
#'   to the classification results.
#'   Default values (TRUE/FALSE) will be used for the remaining metrics available.
#'
#'   Also accepts the string \code{"all"}.
#'
#'   N.B. Currently, disabled metrics are still computed.
#' @param type Type of evaluation to perform:
#'
#'  \code{"gaussian"} for linear regression.
#'
#'  \code{"binomial"} for binary classification.
#'
#'  \code{"multinomial"} for multiclass classification.
#' @details
#'  NB.: When type is \code{"multinomial"}, macro-averaging of metrics returns NaN,
#'  if any of the class level results are NaN.
#'
#'  NB.: When type is \code{"multinomial"}, you can enable weighted averaged metrics
#'  in addition to the regularly averaged metrics. You do this in the \code{metrics} list,
#'  e.g. by \code{metrics = list("Weighted Accuracy" = TRUE)}.
#'
#' @examples
#'  # Attach libraries
#'  library(cvms)
evaluate <- function(data,
                     target_col,
                     prediction_cols,
                     type = "gaussian",
                     id_col = NULL,
                     id_method = "mean",
                     models = NULL,
                     apply_softmax = TRUE,
                     cutoff = 0.5,
                     positive = 2,
                     metrics = list(),
                     parallel = FALSE
){

  # Test if type is allowed
  stopifnot(type %in% c("gaussian", "gaussian_regression", "linear_regression",
                        "binomial", "binomial_classification", "binary_classification",
                        "multinomial", "multinomial_classification", "multiclass_classification")) #"multilabel"))

  # Convert families to the internally used
  if (type %in% c("gaussian", "gaussian_regression", "linear_regression")){
    family <- "gaussian"}
  if (type %in% c("binomial", "binomial_classification", "binary_classification")){
    family <- "binomial"}
  if (type %in% c("multinomial", "multinomial_classification", "multiclass_classification")){
    family <- "multinomial"}

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
                      metrics = metrics)

  # Create basic model_specifics object
  model_specifics <- list(
    model_formula = "",
    family = family,
    REML = FALSE,
    link = NULL,
    cutoff = cutoff,
    positive = positive,
    model_verbose = FALSE
  ) %>%
    basics_update_model_specifics()

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

    # Run ID level evaluation
    evaluations <- run_evaluate_wrapper(
      data = data_for_id_evaluation,
      type = family,
      predictions_col = prediction_cols,
      targets_col = target_col,
      id_col = id_col,
      id_method = id_method,
      groups_col = local_tmp_grouping_factor_var,
      grouping_keys = grouping_keys,
      models = models,
      model_specifics = model_specifics,
      metrics = metrics,
      parallel = parallel
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
        stop(paste0("'prediction_cols' must have length 1 when family is '", family, "'."))
      }
    }

    # Run evaluation
    evaluations <- run_evaluate_wrapper(
      data = data,
      type = family,
      predictions_col = prediction_cols,
      targets_col = target_col,
      models = models,
      groups_col = local_tmp_grouping_factor_var,
      grouping_keys = grouping_keys,
      model_specifics = model_specifics,
      metrics = metrics,
      parallel = parallel
    )
  }

  evaluations

}


run_evaluate_wrapper <- function(data,
                                 type,
                                 predictions_col,
                                 targets_col,
                                 models,
                                 groups_col,
                                 grouping_keys,
                                 id_col = NULL,
                                 id_method = NULL,
                                 fold_info_cols = NULL,
                                 model_specifics,
                                 metrics = list(),
                                 parallel = FALSE) {

  num_classes <- length(unique(data[[targets_col]]))

  if (is.null(fold_info_cols)){

    # Create fold columns
    local_tmp_fold_col_var <- create_tmp_var(data, "fold_column")
    local_tmp_rel_fold_col_var <- create_tmp_var(data, "rel_fold")
    local_tmp_abs_fold_col_var <- create_tmp_var(data, "abs_fold")

    data[[local_tmp_fold_col_var]] <- 1
    data[[local_tmp_rel_fold_col_var]] <- 1
    data[[local_tmp_abs_fold_col_var]] <- 1

    fold_info_cols = list(rel_fold = local_tmp_rel_fold_col_var,
                          abs_fold = local_tmp_abs_fold_col_var,
                          fold_column = local_tmp_fold_col_var)
    include_fold_columns <- FALSE
  } else{
    include_fold_columns <- TRUE
  }

  # Extract unique group identifiers
  unique_group_levels <- unique(data[[groups_col]])

  evaluations <- plyr::llply(seq_along(unique_group_levels), .parallel = parallel, function(gr_ind){

    gr <- unique_group_levels[[gr_ind]]

    data_for_current_group <- data %>%
      dplyr::filter(!!as.name(groups_col) == gr)

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
                      model_specifics = model_specifics,
                      metrics = metrics,
                      include_fold_columns = include_fold_columns)
  })

  if (type == "multinomial"){

    # Extract all the Results tibbles
    # And add the grouping keys
    results <- evaluations %c% "Results" %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()
    results <- grouping_keys %>%
      dplyr::bind_cols(results)

    # Extract all the class level results tibbles
    # And add the grouping keys
    class_level_results <- evaluations %c% "Class Level Results" %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()
    class_level_results <- grouping_keys %>%
      dplyr::slice(rep(1:dplyr::n(), each = num_classes)) %>%
      dplyr::bind_cols(class_level_results)

    return(
      list("Results" = results,
         "Class Level Results" = class_level_results)
    )
  } else {

    # Bind evaluations
    evaluations <- evaluations %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble()

    # Add grouping keys
    results <- grouping_keys %>%
      dplyr::bind_cols(evaluations)

    if (type == "gaussian"){
      results[["Results"]] <- NULL
    }

    return(results)
  }

}

prepare_id_level_evaluation <- function(data,
                                        target_col,
                                        prediction_cols,
                                        family,
                                        id_col,
                                        id_method,
                                        groups_col,
                                        cutoff = NULL,
                                        apply_softmax = length(prediction_cols) > 1,
                                        new_prediction_col_name = "prediction") {

  # Prepare data for id evaluation

  if (is.null(id_col)){
    stop("'id_col' was NULL.")
  }

  if (!is.character(id_col)) {
    stop("'id_col' must be either the name of a column in 'data' or NULL.")
  }
  if (id_col %ni% colnames(data)) {
    stop(paste0("could not find 'id_col', ", id_col, ", in 'data'."))
  }

  if (isTRUE(apply_softmax)) {
    if (length(prediction_cols) < 2) {
      stop("can only apply softmax when there are more than one 'prediction_cols'.")
    }
    data <- softmax(data, cols = prediction_cols)
  }

  if (family == "binomial"){
    if (is.null(cutoff)){
      stop("when 'family' is 'binomial', 'cutoff' must be numeric between 0 and 1.")
    }
    if (is.null(cutoff)){
      stop("when 'family' is 'binomial', 'cutoff' must be numeric between 0 and 1.")
    }
  }

  num_groups <- length(unique(data[[groups_col]]))

  # Add actual class
  id_classes <- data %>%
    dplyr::select(!!as.name(groups_col), !!as.name(id_col), !!as.name(target_col)) %>%
    dplyr::distinct()

  if (id_method == "mean") {

    data_for_id_evaluation <- data %>%
      dplyr::group_by(!!as.name(groups_col), !!as.name(id_col)) %>%
      dplyr::summarise_at(dplyr::vars(prediction_cols), .funs = mean) %>%
      dplyr::left_join(id_classes, by = c(id_col, groups_col)) %>%
      dplyr::ungroup()

  } else if (id_method == "majority"){

    ## By majority vote
    # If multiple classes share the majority, they also share the probability! #mustShare!

    if (family == "multinomial"){

      data[["predicted_class_index"]] <- data %>%
        dplyr::select(dplyr::one_of(prediction_cols)) %>%
        argmax()
      data[["predicted_class"]] <- purrr::map_chr(
        data[["predicted_class_index"]],
        .f = function(x){prediction_cols[[x]]})

      data_majority_count_by_id <- data %>%
        dplyr::group_by(!!as.name(groups_col), !!as.name(id_col)) %>%
        dplyr::count(.data$predicted_class) %>%
        dplyr::left_join(id_classes, by = c(id_col, groups_col)) %>%
        dplyr::ungroup()

      # In case not all classes were predicted
      # We add them with NA values
      # TODO Add unit test to make sure this part works !!!! !!!! !!!!
      classes_to_add <- setdiff(prediction_cols, # use for testing: c(prediction_cols, "cl_7"),
                                data_majority_count_by_id[["predicted_class"]])

      if (length(classes_to_add) > 0){
        na_cols <- dplyr::bind_rows(setNames(rep(
          list(rep(NA, num_groups * nlevels(factor(data_majority_count_by_id[[id_col]])))),
          length(classes_to_add)), classes_to_add))
      }

      # Spread to probability tibble with NAs where NO majority was
      majority_vote_probabilities <- data_majority_count_by_id %>%
        tidyr::spread(key = "predicted_class", value = "n")


      if (length(classes_to_add) > 0){
        majority_vote_probabilities <- majority_vote_probabilities %>%
          dplyr::bind_cols(na_cols) %>%
          dplyr::select(dplyr::one_of(
            groups_col, id_col, target_col, prediction_cols))
      }

      # Set NAs to 0
      majority_vote_probabilities[is.na(majority_vote_probabilities)] <- 0

      # Make all the majority votes ~10000
      # so they will be extremely close to 1 when applying softmax
      # then we will apply softmax in a later step
      majority_vote_probabilities <- majority_vote_probabilities %>%
        dplyr::mutate_at(prediction_cols, function(x){(x/(x+1e-30))*1e+4})

      data_for_id_evaluation <- majority_vote_probabilities %>%
        dplyr::ungroup()

    } else if (family == "binomial"){

      if (length(prediction_cols)>1){
        stop("when 'family' is 'binomial', length of 'prediction_cols' should be 1.")
      }

      data[["predicted_class"]] <- ifelse(data[[prediction_cols]] > cutoff, 1, 0)

      # Create majority count by id
      data_for_id_evaluation <- data %>%
        dplyr::group_by(!!as.name(groups_col), !!as.name(id_col)) %>%
        dplyr::summarise(mean_prediction = mean(.data$predicted_class)) %>%
        dplyr::mutate(mean_prediction = dplyr::case_when(
          mean_prediction > cutoff ~ 1-1e-40, # Almost 1
          mean_prediction == cutoff ~ cutoff, # split decision (unlikely to happen)
          mean_prediction < cutoff ~ 1e-40 # Almost 0
        )) %>%
        dplyr::rename_at("mean_prediction", ~prediction_cols) %>%
        dplyr::left_join(id_classes, by = c(id_col, groups_col)) %>%
        dplyr::ungroup()

    } else {
      stop(paste0("family ", family, " not currently supported for majority vote aggregated ID evaluation."))
    }
  }

  if (family == "multinomial") {

    data_for_id_evaluation <- prepare_multinomial_evaluation(
      data_for_id_evaluation,
      target_col = target_col,
      prediction_cols = prediction_cols,
      apply_softmax = TRUE,
      new_prediction_col_name = new_prediction_col_name
    )

  }

  data_for_id_evaluation

}

prepare_multinomial_evaluation <- function(data,
                                           target_col,
                                           prediction_cols,
                                           apply_softmax,
                                           new_prediction_col_name){

  # Split data into probabilities and the rest
  col_split <- extract_and_remove_probability_cols(data = data,
                                                   prediction_cols = prediction_cols)
  predicted_probabilities <- col_split[["predicted_probabilities"]]
  data <- col_split[["data"]]

  if (isTRUE(apply_softmax)){
    predicted_probabilities <- softmax(predicted_probabilities)
  }

  data[[new_prediction_col_name]] <- predicted_probabilities %>%
    nest_probabilities_rowwise()

  # TODO Do we need to do anything to the dependent column? E.g. make numeric or check against names in prediction_cols?
  # TODO What if the classes are numeric, then what will prediction_cols contain?

  if (length(setdiff(levels_as_characters(data[[target_col]]), prediction_cols)) > 0){
    stop("Not all levels in 'target_col' was found in 'prediction_cols'.")
  }

  data

}

internal_evaluate <- function(data,
                              type = "gaussian",
                              predictions_col = "prediction",
                              targets_col = "target",
                              fold_info_cols = list(rel_fold = "rel_fold",
                                                    abs_fold = "abs_fold",
                                                    fold_column = "fold_column"),
                              models = NULL,
                              id_col = NULL,
                              id_method = NULL,
                              model_specifics = list(),
                              metrics = list(),
                              include_fold_columns = TRUE,
                              include_predictions = TRUE) {

  stopifnot(type %in% c("linear_regression", "gaussian", "binomial", "multinomial")) #, "multiclass", "multilabel"))
  if (type == "linear_regression") type <- "gaussian"

  # Fill metrics with default values for non-specified metrics
  # and get the names of the metrics
  metrics <- set_metrics(family = type, metrics_list = metrics)

  # data is a table with predictions, targets and folds
  # predictions can be values, logits, or classes, depending on evaluation type

  if (type == "gaussian") {
    results <- linear_regression_eval(
      data,
      models = models,
      predictions_col = predictions_col,
      targets_col = targets_col,
      fold_info_cols = fold_info_cols,
      model_specifics = model_specifics,
      metrics = metrics,
      include_fold_columns = include_fold_columns,
      include_predictions = include_predictions)

  } else if (type == "binomial"){

    results <- binomial_classification_eval(
      data,
      predictions_col = predictions_col,
      targets_col = targets_col,
      fold_info_cols = fold_info_cols,
      models = models,
      cutoff = model_specifics[["cutoff"]],
      positive = model_specifics[["positive"]],
      metrics = metrics,
      include_fold_columns = include_fold_columns,
      include_predictions = include_predictions)

  } else if (type == "multinomial"){

    results <- multinomial_classification_eval(
      data,
      predictions_col = predictions_col,
      targets_col = targets_col,
      id_col = id_col,
      id_method = id_method,
      fold_info_cols = fold_info_cols,
      models = models,
      metrics = metrics,
      include_fold_columns = include_fold_columns,
      include_predictions = include_predictions)

  }


  return(results)
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
  if (!is.character(target_col)) {
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
    if (type == "gaussian") {
      warning(paste0("'id_col' is ignored when type is '", type, "'."))
    }
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


  # TODO add for rest of args

}

extract_and_remove_probability_cols <- function(data, prediction_cols) {
  # Split data into probabilities and the rest
  predicted_probabilities <- data %>%
    dplyr::select(dplyr::one_of(prediction_cols))
  data <- data %>%
    dplyr::select(-dplyr::one_of(prediction_cols))

  # Test that the probability columns are numeric
  if (any(!sapply(predicted_probabilities, is.numeric))) {
    stop("the prediction columns must be numeric.")
  }

  list("data" = data,
       "predicted_probabilities" = predicted_probabilities)
}
