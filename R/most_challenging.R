#' @title Find the data points that were hardest to predict
#' @aliases hardest
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'  Finds the data points that, overall, were the most challenging to predict,
#'  based on a prediction metric.
#' @param data Data frame with predictions, targets and observation IDs.
#'  Can be grouped by \code{\link[dplyr:group_by]{dplyr::group_by()}}.
#'
#'  Predictions can be passed as values, predicted classes or predicted probabilities:
#'
#'  \strong{N.B.} Adds \code{\link[base:.Machine]{.Machine$double.eps}} to all probabilities to avoid \code{log(0)}.
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
#'  A single column of type \code{character} with the predicted classes. E.g.:
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
#'  When \code{type} is \code{"binomial"}, the predictions can be passed in one of two formats.
#'
#'  \subsection{Probabilities (Preferable)}{
#'  One column with the \strong{probability of class being
#'  the second class alphabetically}
#'  ("dog" if classes are "cat" and "dog"). E.g.:
#'
#'  \tabular{rrrrr}{
#'   \strong{prediction} \tab \strong{target}\cr
#'   0.769 \tab "dog"\cr
#'   0.368 \tab "dog"\cr
#'   0.375 \tab "cat"\cr
#'   ... \tab ...}
#'  }
#'
#'  \subsection{Classes}{
#'
#'  A single column of type \code{character} with the predicted classes. E.g.:
#'
#'  \tabular{rrrrr}{
#'   \strong{prediction} \tab \strong{target}\cr
#'   class_0 \tab class_1\cr
#'   class_1 \tab class_1\cr
#'   class_1 \tab class_0\cr
#'   ... \tab ...}
#'  }
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
#'
#' @param obs_id_col Name of column with observation IDs. This will be used to aggregate
#'  the performance of each observation.
#' @param target_col Name of column with the true classes/values in \code{data}.
#' @param prediction_col Name of column with the predictions.
#'
#'  When evaluating a classification task, the column should contain the predicted classes.
#' @param type Type of task used to get the predictions:
#'
#'  \code{"gaussian"} for regression (like linear regression).
#'
#'  \code{"binomial"} for binary classification.
#'
#'  \code{"multinomial"} for multiclass classification.
#' @param threshold Threshold to filter observations by. Depends on \code{type} and \code{threshold_is}.
#'
#'  The \code{threshold} can either be a \strong{percentage} or a \strong{score}.
#'  For percentages, a lower \code{threshold}
#'  returns fewer observations. For scores, this depends on \code{type}.
#'
#'  \subsection{Gaussian}{
#'  \subsection{threshold_is "percentage"}{
#'  (Approximate) percentage of the observations with the largest root mean square errors
#'  to return.
#'  }
#'  \subsection{threshold_is "score"}{
#'  Observations with a root mean square error larger than or equal to the \code{threshold} will be returned.
#'  }
#'  }
#'  \subsection{Binomial, Multinomial}{
#'  \subsection{threshold_is "percentage"}{
#'  (Approximate) percentage of the observations to return with:
#'
#'  \code{MAE}, \code{Cross Entropy}: Highest error scores.
#'
#'  \code{Accuracy}: Lowest accuracies
#'  }
#'  \subsection{threshold_is "score"}{
#'  \code{MAE}, \code{Cross Entropy}: Observations with an error score above or equal to the threshold will be returned.
#'
#'  \code{Accuracy}: Observations with an accuracy below or equal to the threshold will be returned.
#'
#'  }
#'  }
#' @param threshold_is Either \code{"score"} or \code{"percentage"}. See \code{threshold}.
#' @param metric The metric to use. If \code{NULL},
#'  the default metric depends on the format of the prediction columns.
#'
#'  \subsection{Binomial, Multinomial}{
#'  \code{"Accuracy"}, \code{"MAE"} or \code{"Cross Entropy"}.
#'
#'  When \emph{one} prediction column with predicted \emph{classes} is passed,
#'  the default is \code{"Accuracy"}.
#'  In this configuration, the other metrics are not calculated.
#'
#'  When \emph{one or more} prediction columns with predicted \emph{probabilities} are passed,
#'  the default is \code{"MAE"}. This is the Mean Absolute Error of the
#'  probability of the target class.
#'  }
#'
#'  \subsection{Gaussian}{
#'  Ignored. Always uses \code{"RMSE"}.
#'  }
#' @param cutoff Threshold for predicted classes. (Numeric)
#'
#'  \strong{N.B. Binomial only}.
#' @return Data frame with the most challenging observations and their metrics.
#'
#'  \code{`>=` / `<=`} denotes the threshold as score.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @examples
#' \donttest{
#' # Attach packages
#' library(cvms)
#' library(dplyr)
#'
#' ##
#' ## Multinomial
#' ##
#'
#' # Find the most challenging data points (per classifier)
#' # in the predicted.musicians dataset
#' # which resembles the "Predictions" tibble from the evaluation results
#'
#' # Passing predicted probabilities
#' # Observations with 30% highest MAE scores
#' most_challenging(
#'   predicted.musicians,
#'   obs_id_col = "ID",
#'   prediction_cols = c("A", "B", "C", "D"),
#'   type = "multinomial",
#'   threshold = 0.30
#' )
#'
#' # Observations with 25% highest Cross Entropy scores
#' most_challenging(
#'   predicted.musicians,
#'   obs_id_col = "ID",
#'   prediction_cols = c("A", "B", "C", "D"),
#'   type = "multinomial",
#'   threshold = 0.25,
#'   metric = "Cross Entropy"
#' )
#'
#' # Passing predicted classes
#' # Observations with 30% lowest Accuracy scores
#' most_challenging(
#'   predicted.musicians,
#'   obs_id_col = "ID",
#'   prediction_cols = "Predicted Class",
#'   type = "multinomial",
#'   threshold = 0.30
#' )
#'
#' # The 40% lowest-scoring on accuracy per classifier
#' predicted.musicians %>%
#'   dplyr::group_by(Classifier) %>%
#'   most_challenging(
#'     obs_id_col = "ID",
#'     prediction_cols = "Predicted Class",
#'     type = "multinomial",
#'     threshold = 0.40
#'   )
#'
#' # Accuracy scores below 0.05
#' most_challenging(
#'   predicted.musicians,
#'   obs_id_col = "ID",
#'   type = "multinomial",
#'   threshold = 0.05,
#'   threshold_is = "score"
#' )
#'
#' ##
#' ## Binomial
#' ##
#'
#' # Subset the predicted.musicians
#' binom_data <- predicted.musicians %>%
#'   dplyr::filter(Target %in% c("A","B")) %>%
#'   dplyr::rename(Prediction = B)
#'
#' # Passing probabilities
#' # Observations with 30% highest MAE
#' most_challenging(
#'   binom_data,
#'   obs_id_col = "ID",
#'   type = "binomial",
#'   prediction_cols = "Prediction",
#'   threshold = 0.30
#' )
#'
#' # Observations with 30% highest Cross Entropy
#' most_challenging(
#'   binom_data,
#'   obs_id_col = "ID",
#'   type = "binomial",
#'   prediction_cols = "Prediction",
#'   threshold = 0.30,
#'   metric = "Cross Entropy"
#' )
#'
#' # Passing predicted classes
#' # Observations with 30% lowest Accuracy scores
#' most_challenging(
#'   binom_data,
#'   obs_id_col = "ID",
#'   type = "binomial",
#'   prediction_cols = "Predicted Class",
#'   threshold = 0.30
#' )
#'
#' ##
#' ## Gaussian
#' ##
#'
#' set.seed(1)
#'
#' df <- data.frame(
#'   "Observation" = rep(1:10, n = 3),
#'   "Target" = rnorm(n = 30, mean = 25, sd = 5),
#'   "Prediction" = rnorm(n = 30, mean = 27, sd = 7)
#' )
#'
#' # The 20% highest RMSE scores
#' most_challenging(
#'   df,
#'   type = "gaussian",
#'   threshold = 0.2
#' )
#'
#' # RMSE scores above 9
#' most_challenging(
#'   df,
#'   type = "gaussian",
#'   threshold = 9,
#'   threshold_is = "score"
#' )
#' }
most_challenging <- function(data,
                             type,
                             obs_id_col = "Observation",
                             target_col = "Target",
                             prediction_cols = ifelse(
                               type == "gaussian",
                               "Prediction",
                               "Predicted Class"
                             ),
                             threshold = 0.15,
                             threshold_is = "percentage",
                             metric = NULL,
                             cutoff = 0.5) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(
    x = data, min.rows = 1,
    min.cols = 3,
    add = assert_collection
  )

  checkmate::assert_string(x = obs_id_col, min.chars = 1, add = assert_collection)
  checkmate::assert_string(x = target_col, min.chars = 1, add = assert_collection)
  checkmate::assert_character(x = prediction_cols,  min.chars = 1,
                              min.len = 1, any.missing = FALSE,
                              add = assert_collection)
  checkmate::assert_choice(
    x = type, choices = c("gaussian", "binomial", "multinomial"),
    add = assert_collection
  )
  if (type != "gaussian"){
    checkmate::assert_choice(
      x = metric, choices = c("Accuracy", "Cross Entropy", "MAE"),
      null.ok = TRUE,
      add = assert_collection
    )
  } else {
    checkmate::assert_choice(
      x = metric, choices = c("RMSE"),
      null.ok = TRUE,
      add = assert_collection
    )
  }

  checkmate::assert_choice(
    x = threshold_is, choices = c("score", "percentage"),
    add = assert_collection
  )
  checkmate::assert_number(
    x = cutoff,
    lower = 0,
    upper = 1,
    add = assert_collection
  )
  checkmate::assert_number(x = threshold, add = assert_collection)
  if (threshold_is == "percentage" &&
      !is_between_(threshold, 0.0, 1.0)) {
    assert_collection$push(
      "when 'threshold_is' a percentage, 'threshold' must be between 0 and 1."
    )
  }
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(
    x = names(data),
    must.include = c(obs_id_col,
                     target_col,
                     prediction_cols),
    add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # If the dataset is grouped, we need the indices and keys for the groups
  # so we can evaluate group wise
  # grouping_factor <- dplyr::group_indices(data)
  grouping_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)

  if (obs_id_col %in% colnames(grouping_keys)){
    assert_collection$push(
      "'data' cannot be grouped by the 'obs_id_col'. This is done internally."
    )
    checkmate::reportAssertions(assert_collection)
  }

  # Select relevant columns
  data <- data[, c(
    colnames(grouping_keys),
    obs_id_col,
    target_col,
    prediction_cols
  )]

  # Possibly multiple prediction cols
  # that should be transformed to probability of target class
  prepped_predictions <-
    prepare_predictions(
      data = data,
      prediction_cols = prediction_cols,
      target_col = target_col,
      cutoff = cutoff,
      type = type
    )

  data <- prepped_predictions[["data"]]
  prediction_col <- prepped_predictions[["prediction_col"]]
  predicted_class_col <- prepped_predictions[["predicted_class_col"]]
  probability_of_target_col <- prepped_predictions[["probability_of_target_col"]]

  if (type %in% c("binomial", "multinomial")) {
    most_challenging_classification(
      data = data,
      obs_id_col = obs_id_col,
      target_col = target_col,
      predicted_class_col = predicted_class_col,
      probability_of_target_col = probability_of_target_col,
      threshold = threshold,
      threshold_is = threshold_is,
      metric = metric,
      grouping_keys = grouping_keys
    )
  } else if (type == "gaussian") {
    most_challenging_gaussian(
      data = data,
      obs_id_col = obs_id_col,
      target_col = target_col,
      prediction_col = prediction_col,
      threshold = threshold,
      threshold_is = threshold_is,
      grouping_keys = grouping_keys
    )
  }
}

# Check prediction columns
check_prediction_cols <- function(data,
                                  prediction_cols,
                                  target_col,
                                  type,
                                  cutoff){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, min.cols = 2, add = assert_collection)
  checkmate::assert_character(x = prediction_cols, any.missing = FALSE,
                              min.len = 1, add = assert_collection)
  checkmate::assert_string(x = target_col, min.chars = 1, add = assert_collection)
  checkmate::assert_string(x = type, min.chars = 1, add = assert_collection)
  checkmate::assert_number(x = cutoff, lower = 0, upper = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (!checkmate::test_names(x = colnames(data),
                           must.include = prediction_cols)){
    assert_collection$push("'data' does not include all the prediction columns.")
  }
  if (!checkmate::test_names(x = colnames(data),
                            must.include = target_col)){
    assert_collection$push("'data' does not include the target column.")
  }

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (type == "gaussian" &&
      !is.numeric(data[[target_col]])){
    assert_collection$push(paste0(
      "When 'type' is 'gaussian', 'data[[target_col]]' must be numeric."
    ))
    checkmate::reportAssertions(assert_collection)
  }

  if (type != "gaussian" &&
      !(is.character(data[[target_col]]) ||
        is.factor(data[[target_col]]))) {
    assert_collection$push(paste0(
      "When 'type' is '",
      type,
      "', 'data[[target_col]]' must either have type 'character' or 'factor'."
    ))
    checkmate::reportAssertions(assert_collection)
  }

  # Checks
  if (length(prediction_cols) == 1){

    if (type == "gaussian"){
      if (!is.double(data[[prediction_cols]])){
        assert_collection$push(
          "when 'type' is 'gaussian', 'data[[prediction_cols]]' must have type 'double'.")
        checkmate::reportAssertions(assert_collection)
      }

    } else if (type == "binomial") {
      if (!(is.double(data[[prediction_cols]]) ||
            is.character(data[[prediction_cols]]))){
        assert_collection$push(
          paste0("when 'type' is 'binomial', 'data[[prediction_cols]]' must b",
                 "e either of type 'double' (probability of positive class) or",
                 " 'character' (predicted class)."))
        checkmate::reportAssertions(assert_collection)
      }
      if (is.numeric(data[[prediction_cols]]) &&
          !checkmate::test_numeric(data[[prediction_cols]],
                                  lower = 0, upper = 1,
                                  any.missing = FALSE)){
        assert_collection$push(
          "when 'type' is 'binomial' and 'data[[prediction_cols]]' is 'numeric', it must have values between 0 and 1."
        )
        checkmate::reportAssertions(assert_collection)
      }
      if (is.character(data[[target_col]]) &&
          length(levels_as_characters(data[[target_col]])) > 2) {
        assert_collection$push("found more than two target classes.")
        checkmate::reportAssertions(assert_collection)
      }

    } else if (type == "multinomial"){
      if (!is.character(data[[prediction_cols]])){
        assert_collection$push(
          "when 'type' is 'multinomial' and prediction_cols has length 1, 'data[[prediction_cols]]' must have type 'character'."
        )
        checkmate::reportAssertions(assert_collection)
      }
    }

  } else {

    if (type != "multinomial"){
      assert_collection$push(
        "'prediction_cols' can only have length > 1 when 'type' is 'multinomial'.")
      checkmate::reportAssertions(assert_collection)
    }

    data_preds <- data[, prediction_cols]
    if (any(!unlist(lapply(data_preds, is.numeric)))) {
      assert_collection$push(
        "when 'prediction_cols' has length > 1, all prediction columns must be numeric.")
      checkmate::reportAssertions(assert_collection)
    }
    if (contains_na(data_preds)) {
      assert_collection$push("prediction columns contained NAs (missing data).")
      checkmate::reportAssertions(assert_collection)
    }
    if (any(round(rowSums(data_preds), digits = 5) != 1)) {
      assert_collection$push(
        paste0("when 'prediction_cols' has length > 1, prediction columns m",
               "ust sum to 1 row-wise."))
      checkmate::reportAssertions(assert_collection)
    }

    if (length(setdiff(levels_as_characters(data[[target_col]]),
                       prediction_cols)) > 0){
      assert_collection$push(
        paste0("when 'prediction_cols' has length > 1, all classes in 'data",
               "[[target_col]]' must have a prediction column."))
      checkmate::reportAssertions(assert_collection)
    }

  }

}

# Extract
prepare_predictions <- function(data,
                                prediction_cols,
                                target_col,
                                cutoff,
                                type) {

  check_prediction_cols(
    data = data,
    prediction_cols = prediction_cols,
    target_col = target_col,
    type = type,
    cutoff
  )

  if (type == "binomial"){
    cat_levels <- levels_as_characters(data[[target_col]])
    if (length(cat_levels) < 2) {
      stop(paste0("found less than 2 levels in the target column."))
    }
    positive <- cat_levels[2]
  }

  # For gaussian
  if (type %ni% c("binomial", "multinomial")) {
    return(list(
      "data" = data,
      "prediction_col" = prediction_cols,
      "predicted_class_col" = NULL,
      "probability_of_target_col" = NULL
    ))
  }

  if (length(prediction_cols) > 1) {

    # Extract predicted class
    data[["predicted_class_index"]] <-
      argmax(data[, prediction_cols])
    data[["predicted_class"]] <- purrr::map_chr(
      data[["predicted_class_index"]],
      .f = function(x) {
        prediction_cols[[x]]
      }
    )

    # Extract probability of target class
    target_index_map <-
      as.list(setNames(seq_along(prediction_cols), prediction_cols))
    target_indices <- purrr::map_int(
      data[[target_col]],
      .f = function(x) {
        target_index_map[[x]]
      }
    )

    # Add + .Machine$double.eps to probabilities
    data[, prediction_cols] <- data[, prediction_cols] + .Machine$double.eps
    # Find probability of targets
    data[["probability_of_target"]] <- as.data.frame(data[, prediction_cols])[
      cbind(seq_along(target_indices), target_indices)]

    prediction_col <- NULL
    probability_of_target_col <- "probability_of_target"
    predicted_class_col <- "predicted_class"

  } else {

    # Extract probability of target
    if (is.numeric(data[[prediction_cols]])) {

      if (type == "binomial"){
        negative <- cat_levels[cat_levels != positive]
        # Find predicted classes
        data[["predicted_class"]] <-
          ifelse(data[[prediction_cols]] > cutoff,
                 positive, negative)
        # Add + .Machine$double.eps to probabilities
        data[[prediction_cols]] <- data[[prediction_cols]] + .Machine$double.eps
        # Find probability of targets
        data[["probability_of_target"]] <-
          ifelse(data[[target_col]] == positive,
                 data[[prediction_cols]],
                 1 - data[[prediction_cols]])

        prediction_col <- NULL
        probability_of_target_col <- "probability_of_target"
        predicted_class_col <- "predicted_class"
      }

    } else {
      probability_of_target_col <- NULL
      prediction_col <- NULL
      predicted_class_col <- prediction_cols
    }

  }

  list("data" = data,
       "prediction_col" = prediction_col,
       "predicted_class_col" = predicted_class_col,
       "probability_of_target_col" = probability_of_target_col)
}


most_challenging_classification <- function(data,
                                            obs_id_col,
                                            target_col,
                                            predicted_class_col,
                                            probability_of_target_col,
                                            threshold,
                                            threshold_is,
                                            metric,
                                            grouping_keys) {
  # Ensure same type
  data[[target_col]] <- as.character(data[[target_col]])
  data[[predicted_class_col]] <- as.character(data[[predicted_class_col]])

  # Count correct and incorrect predictions
  data[["Correct"]] <- data[[predicted_class_col]] == data[[target_col]]
  data[["Incorrect"]] <- data[[predicted_class_col]] != data[[target_col]]
  if (!is.null(probability_of_target_col)){
    # Make sure we don't do log(0)
    data[[probability_of_target_col]] <- ifelse(
      data[[probability_of_target_col]] < 1e-20, 1e-20,
      data[[probability_of_target_col]])
    data[["LogProbabilityOfTarget"]] <- log(data[[probability_of_target_col]])
    data[["ProbabilityOfTarget"]] <- data[[probability_of_target_col]]
    if (is.null(metric))
      metric <- "MAE"
  } else {
    data[["LogProbabilityOfTarget"]] <- 1
    data[["ProbabilityOfTarget"]] <- 1
    if (is.null(metric))
      metric <- "Accuracy"
  }

  # Calculate metrics per group
  by_observation <- data %>%
    dplyr::mutate(corr = .data$Correct) %>%
    dplyr::group_by_at(c(colnames(grouping_keys), obs_id_col)) %>%
    dplyr::summarise(
      Correct = sum(.data$Correct),
      Incorrect = sum(.data$Incorrect),
      Accuracy = mean(.data$corr),
      MAE = mean(1 - .data$ProbabilityOfTarget),
      `Cross Entropy` = -mean(.data$LogProbabilityOfTarget) # TODO Remember to test this
    ) %>% dplyr::ungroup()

  if (is.null(probability_of_target_col)){
    by_observation[["Cross Entropy"]] <- NULL
    by_observation[["MAE"]] <- NULL
  }

  # Find the observations that were the most difficult to predict
  to_return <- exceeds_threshold(data = by_observation,
                                 threshold = threshold,
                                 threshold_is = threshold_is,
                                 metric_name = metric,
                                 maximize = metric == "Accuracy",
                                 grouping_keys = grouping_keys)

  to_return <- to_return %>%
    dplyr::rename(`<=` = .data$Threshold)

  if (metric == "Accuracy"){
    to_return <- to_return %>%
      dplyr::arrange(
        !!!rlang::syms(colnames(grouping_keys)),
        .data$Accuracy
      )
  } else if (metric == "MAE"){
    to_return <- to_return %>%
      dplyr::arrange(
        !!!rlang::syms(colnames(grouping_keys)),
        dplyr::desc(.data$`MAE`),
        dplyr::desc(.data$`Cross Entropy`),
        .data$Accuracy
      )
  } else if (metric == "Cross Entropy"){
    to_return <- to_return %>%
      dplyr::arrange(
        !!!rlang::syms(colnames(grouping_keys)),
        dplyr::desc(.data$`Cross Entropy`),
        dplyr::desc(.data$`MAE`),
        .data$Accuracy
      )
  }

  to_return

}


most_challenging_gaussian <- function(data,
                                      obs_id_col,
                                      target_col,
                                      prediction_col,
                                      threshold,
                                      threshold_is,
                                      grouping_keys) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_numeric(
    x = data[[target_col]],
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_numeric(
    x = data[[prediction_col]],
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Calculate residuals
  tmp_residual_var <- create_tmp_name(data, ".__residuals__")
  data[[tmp_residual_var]] <- data[[target_col]] - data[[prediction_col]]

  # RMS() (x is the residuals)
  root_mean_square <- function(x) sqrt(mean(x^2))

  # Calculate metrics per group
  by_observation <- data %>%
    dplyr::group_by_at(c(colnames(grouping_keys), obs_id_col)) %>%
    dplyr::summarise(
      `MAE` = mean(abs(!!as.name(tmp_residual_var))),
      `RMSE` = root_mean_square(!!as.name(tmp_residual_var))
    ) %>% dplyr::ungroup()

  # Find the observations that were the most difficult to predict
  to_return <- exceeds_threshold(data = by_observation,
                                 threshold = threshold,
                                 threshold_is = threshold_is,
                                 metric_name = "RMSE",
                                 maximize = FALSE,
                                 grouping_keys = grouping_keys)

  # Reorder and return
  to_return %>%
    dplyr::arrange(
      !!!rlang::syms(colnames(grouping_keys)),
      dplyr::desc(.data$RMSE), dplyr::desc(.data$MAE)
    ) %>%
    dplyr::rename(`>=` = .data$Threshold)
}

# TODO add tests
exceeds_threshold <- function(data,
                              threshold,
                              threshold_is,
                              metric_name,
                              maximize,
                              grouping_keys) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, add = assert_collection)
  if (dplyr::is_grouped_df(data))
    assert_collection$push("'data' cannot be grouped at this stage.")
  checkmate::assert_data_frame(x = grouping_keys, add = assert_collection)
  checkmate::assert_choice(x = threshold_is, choices = c("percentage", "score"),
                           add = assert_collection)
  checkmate::assert_flag(x = maximize, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (threshold_is == "percentage")
    checkmate::assert_number(x = threshold, lower = 0, upper = 1, add = assert_collection)
  else {
    checkmate::assert_number(x = threshold, add = assert_collection)
  }
  checkmate::assert_names(x = colnames(data), must.include = c(colnames(grouping_keys), metric_name),
                          what = "colnames", add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (threshold_is == "percentage"){
    # Calculate thresholds per grouping
    probs <- ifelse(isTRUE(maximize), threshold, 1 - threshold)
    thresholds <- data %>%
      dplyr::group_by_at(colnames(grouping_keys)) %>%
      dplyr::summarise(Threshold = stats::quantile(
        !!as.name(metric_name), probs = probs)) %>%
      dplyr::mutate(Threshold = unname(.data$Threshold))
    # Add thresholds to data
    if (ncol(grouping_keys) > 0){
      data <- data %>%
        dplyr::left_join(thresholds, by = colnames(grouping_keys))
    } else {
      data[["Threshold"]] <- thresholds[["Threshold"]]
    }
  } else if (threshold_is == "score") {
    # Add threshold to data
    data[["Threshold"]] <- threshold
  }

  # Find the most difficult observations
  if (isTRUE(maximize)){
    to_return <- data[data[[metric_name]] <= data[["Threshold"]],]
  } else {
    to_return <- data[data[[metric_name]] >= data[["Threshold"]],]
  }

  to_return
}

