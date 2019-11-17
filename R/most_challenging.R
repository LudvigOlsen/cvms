#' @title Find the data points that were hardest to predict
#' @aliases hardest
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'  Finds the data points that, overall, were the most challenging to predict,
#'  based on a prediction score (\code{RMSE} for regression and
#'  \code{Accuracy} for classification).
#'
#'
#' @param data Data frame with predictions, targets and observation IDs.
#'
#'  Can be grouped by \code{\link[dplyr:group_by()]{dplyr::group_by()}}.
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
#'  Percentage of the observations with the largest root mean square errors
#'  to return.
#'  }
#'  \subsection{threshold_is "score"}{
#'  Observations with a root mean square error larger than the \code{threshold} will be returned.
#'  }
#'  }
#'  \subsection{"binomial", "multinomial"}{
#'  \subsection{threshold_is "percentage"}{
#'  Percentage of the observations with the lowest accuracies to return.
#'  }
#'  \subsection{threshold_is "score"}{
#'  Observations with an accuracy below the threshold will be returned.
#'  }
#'  }
#' @param threshold_is Either \code{"score"} or \code{"percentage"}. See \code{threshold}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @examples
#' \donttest{
#' # Attach packages
#' library(cvms)
#' library(groupdata2) # partition()
#' library(dplyr) # %>% arrange()
#' library(tibble)
#'
#' # Data is part of cvms
#' data <- participant.scores
#'
#' # Set seed for reproducibility
#' set.seed(1)
#' }
most_challenging <- function(data,
                             obs_id_col = "Observation",
                             target_col = "Target",
                             prediction_col = dplyr::case_when(
                               type == "gaussian" ~ "Prediction",
                               TRUE ~ "Predicted Class"
                             ),
                             type = "binomial",
                             threshold = 0.15,
                             threshold_is = "percentage"){

  # If the dataset is grouped, we need the indices and keys for the groups
  # so we can evaluate group wise
  # grouping_factor <- dplyr::group_indices(data)
  grouping_keys <- dplyr::group_keys(data)
  data <- dplyr::ungroup(data)

  if (threshold_is == "percentage" && !is_between_(threshold, 0.0, 1.0))
    stop("when 'threshold_is' a percentage, 'threshold' must be between 0 and 1.")

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

  data[["Correct"]] <- data[[prediction_col]] == data[[target_col]]
  data[["Incorrect"]] <- data[[prediction_col]] != data[[target_col]]

  by_observation <- data %>%
    dplyr::group_by_at(c(colnames(grouping_keys), obs_id_col)) %>%
    dplyr::summarise(Correct = sum(.data$Correct),
                     Incorrect = sum(.data$Incorrect),
                     Accuracy = .data$Correct/dplyr::n())

  if (threshold_is == "score"){

    to_return <- by_observation[by_observation[["Accuracy"]] < threshold,]

  } else if (threshold_is == "percentage"){

    quantiles <- list("Accuracy" = quantile(by_observation[["Accuracy"]], probs = threshold))
    to_return <- by_observation[by_observation[["Accuracy"]] < quantiles[["Accuracy"]],]

  }

  to_return %>%
    dplyr::arrange(!!!rlang::syms(colnames(grouping_keys)),
                   .data$Accuracy)

}


most_challenging_gaussian <- function(data,
                                      obs_id_col,
                                      target_col,
                                      prediction_col,
                                      threshold,
                                      threshold_is,
                                      grouping_keys){

  if (!is.numeric(data[[target_col]]))
    stop("target_col must be numeric.")
  if (!is.numeric(data[[prediction_col]]))
    stop("target_col must be numeric.")

  tmp_residual_var <- create_tmp_var(data, ".__residuals__")
  data[tmp_residual_var] <- data[[target_col]] - data[[prediction_col]]

  root_mean_square <- function(x) sqrt(mean(x^2))

  by_observation <- data %>%
    dplyr::group_by_at(c(colnames(grouping_keys), obs_id_col)) %>%
    dplyr::summarise(`MAE` = mean(abs(!!as.name(tmp_residual_var))),
                     `RMSE` = root_mean_square(!!as.name(tmp_residual_var)))

  if (threshold_is == "score"){

    to_return <- by_observation[by_observation[["RMSE"]] > threshold,]

  } else if (threshold_is == "percentage"){

    quantiles <- list("RMSE" = quantile(by_observation[["RMSE"]], probs = (1-threshold)))

    to_return <- by_observation[by_observation[["RMSE"]] > quantiles[["RMSE"]],]

  }

  to_return %>%
    dplyr::arrange(!!!rlang::syms(colnames(grouping_keys)),
                   desc(.data$RMSE), desc(.data$MAE))

}
