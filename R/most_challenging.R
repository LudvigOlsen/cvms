#' @title Find the data points that were hardest to predict
#' @aliases hardest
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'  Finds the data points that, overall, were the most challenging to predict,
#'  based on a prediction score (\code{RMSE} for regression and
#'  \code{Accuracy} for classification).
#' @param data Data frame with predictions, targets and observation IDs.
#'
#'  Can be grouped by \code{\link[dplyr:group_by]{dplyr::group_by()}}.
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
#'  \subsection{"gaussian"}{
#'  \subsection{threshold_is "percentage"}{
#'  (Approximate) percentage of the observations with the largest root mean square errors
#'  to return.
#'  }
#'  \subsection{threshold_is "score"}{
#'  Observations with a root mean square error larger than or equal to the \code{threshold} will be returned.
#'  }
#'  }
#'  \subsection{"binomial", "multinomial"}{
#'  \subsection{threshold_is "percentage"}{
#'  (Approximate) percentage of the observations with the lowest accuracies to return.
#'  }
#'  \subsection{threshold_is "score"}{
#'  Observations with an accuracy below or equal to the threshold will be returned.
#'  }
#'  }
#' @param threshold_is Either \code{"score"} or \code{"percentage"}. See \code{threshold}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @examples
#' # Attach packages
#' library(cvms)
#' library(dplyr)
#'
#' \donttest{
#'
#' ## Multinomial
#'
#' # Find the most challenging data points (per classifier)
#' # in the predicted.musicians dataset
#' # which resembles the "Predictions" tibble from the evaluation results
#'
#' # The 20% lowest scoring on accuracy
#' most_challenging(predicted.musicians,
#'                  obs_id_col = "ID",
#'                  type = "multinomial",
#'                  threshold = 0.30,
#'                  threshold_is = "percentage")
#'
#' # The 20% lowest scoring on accuracy per classifier
#' predicted.musicians %>%
#'   dplyr::group_by(Classifier) %>%
#'   most_challenging(obs_id_col = "ID",
#'                    type = "multinomial",
#'                    threshold = 0.40,
#'                    threshold_is = "percentage")
#'
#' # Accuracy scores below 0.05
#' most_challenging(predicted.musicians,
#'                  obs_id_col = "ID",
#'                  type = "multinomial",
#'                  threshold = 0.05,
#'                  threshold_is = "score")
#'
#' # Accuracy scores below 0.05 per classifier
#' predicted.musicians %>%
#'   dplyr::group_by(Classifier) %>%
#'   most_challenging(obs_id_col = "ID",
#'                    type = "multinomial",
#'                    threshold = 0.05,
#'                    threshold_is = "score")
#'
#' ## Gaussian
#'
#' set.seed(1)
#'
#' df <- data.frame("Observation" = rep(1:10, n = 3),
#'                  "Target" = rnorm(n = 30, mean = 25, sd = 5),
#'                  "Prediction" = rnorm(n = 30, mean = 27, sd = 7))
#'
#' # The 20% highest RMSE scores
#' most_challenging(df,
#'                  type = "gaussian",
#'                  threshold = 0.2,
#'                  threshold_is = "percentage")
#'
#' # RMSE scores above 9
#' most_challenging(df,
#'                  type = "gaussian",
#'                  threshold = 9,
#'                  threshold_is = "score")
#' }
most_challenging <- function(data,
                             type,
                             obs_id_col = "Observation",
                             target_col = "Target",
                             prediction_col = ifelse(
                               type == "gaussian",
                               "Prediction",
                               "Predicted Class"
                             ),
                             threshold = 0.15,
                             threshold_is = "percentage") {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(
    x = data, min.rows = 1,
    min.cols = 3,
    add = assert_collection
  )
  checkmate::assert_string(x = obs_id_col, min.chars = 1, add = assert_collection)
  checkmate::assert_string(x = target_col, min.chars = 1, add = assert_collection)
  checkmate::assert_string(x = prediction_col, min.chars = 1, add = assert_collection)
  checkmate::assert_choice(
    x = type, choices = c("gaussian", "binomial", "multinomial"),
    add = assert_collection
  )
  checkmate::assert_choice(
    x = threshold_is, choices = c("score", "percentage"),
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
  # End of argument checks ####

  # If the dataset is grouped, we need the indices and keys for the groups
  # so we can evaluate group wise
  # grouping_factor <- dplyr::group_indices(data)
  grouping_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)

  if (type %in% c("binomial", "multinomial")) {
    most_challenging_classification(
      data = data,
      obs_id_col = obs_id_col,
      target_col = target_col,
      prediction_col = prediction_col,
      threshold = threshold,
      threshold_is = threshold_is,
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

most_challenging_classification <- function(data,
                                            obs_id_col,
                                            target_col,
                                            prediction_col,
                                            threshold,
                                            threshold_is,
                                            grouping_keys) {
  # Ensure same type
  data[[target_col]] <- as.character(data[[target_col]])
  data[[prediction_col]] <- as.character(data[[prediction_col]])

  # Count correct and incorrect predictions
  data[["Correct"]] <- data[[prediction_col]] == data[[target_col]]
  data[["Incorrect"]] <- data[[prediction_col]] != data[[target_col]]

  # Calculate metrics per group
  by_observation <- data %>%
    dplyr::group_by_at(c(colnames(grouping_keys), obs_id_col)) %>%
    dplyr::summarise(
      Accuracy = mean(.data$Correct),
      Correct = sum(.data$Correct),
      Incorrect = sum(.data$Incorrect)
    ) %>% dplyr::ungroup()
  by_observation <- position_last(by_observation, "Accuracy")

  # Find the observations that were the most difficult to predict
  to_return <- exceeds_threshold(data = by_observation,
                                 threshold = threshold,
                                 threshold_is = threshold_is,
                                 metric_name = "Accuracy",
                                 maximize = TRUE,
                                 grouping_keys = grouping_keys)

  to_return %>%
    dplyr::arrange(
      !!!rlang::syms(colnames(grouping_keys)),
      .data$Accuracy
    ) %>%
    dplyr::rename(`<=` = .data$Threshold)
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

