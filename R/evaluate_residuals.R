
#   __________________ #< f29ca547300fc1bd04428e6410ff2da3 ># __________________
#   Evaluate residuals                                                      ####


#' @title Evaluate residuals from a regression task
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Calculates a large set of error metrics from regression residuals.
#'
#'  \strong{Note}: Used in \code{\link[cvms:evaluate]{evaluate()}}.
#'  In most contexts, that is the function you want.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family evaluation functions
#' @param data Data frame with predictions and targets.
#' @param targets_col Name of the column with the true values in \code{data}.
#' @param predictions_col Name of column with the predicted values in \code{data}.
#' @param metrics List for enabling/disabling metrics.
#'
#'   E.g. \code{list("RMSE" = FALSE)} would disable \code{RMSE}.
#'   Default values (\code{TRUE}/\code{FALSE}) will be used for the remaining available metrics.
#'
#'   You can enable/disable all metrics at once by including
#'   \code{"all" = TRUE/FALSE} in the list. This is done prior to enabling/disabling
#'   individual metrics, why f.i. \code{list("all" = FALSE, "RMSE" = TRUE)}
#'   would return only the RMSE metric.
#'
#'   The list can be created with
#'   \code{\link[cvms:gaussian_metrics]{gaussian_metrics()}}.
#'
#'   Also accepts the string \code{"all"}.
#' @details
#'  The metric formulas are listed in the \emph{Metrics} vignette.
#' @return
#'  Tbl (tibble) with the calculated metrics.
#'
#'  The following metrics are available (see \code{metrics}):
#'
#'  \tabular{rrr}{
#'   \strong{Metric} \tab \strong{Name} \tab \strong{Default} \cr
#'   Mean Absolute Error \tab "MAE" \tab Enabled \cr
#'   Root Mean Square Error \tab "RMSE" \tab Enabled \cr
#'   Normalized RMSE (by target range) \tab "NRMSE" \tab Disabled \cr
#'   Normalized RMSE (by target IQR) \tab "RMSEIQR" \tab Disabled \cr
#'   Normalized RMSE (by target STD) \tab "RMSESTD" \tab Disabled \cr
#'   Root Mean Squared Log Error \tab "RMSLE" \tab Enabled \cr
#'   Mean Absolute Log Error \tab "MALE" \tab Disabled \cr
#'   Relative Absolute Error \tab "RAE" \tab Disabled \cr
#'   Relative Squared Error \tab "RSE" \tab Disabled \cr
#'   Root Relative Squared Error \tab "RRSE" \tab Disabled \cr
#'   Mean Absolute Percentage Error \tab "MAPE" \tab Disabled \cr
#'   Mean Squared Error \tab "MSE" \tab Disabled \cr
#'   Total Absolute Error \tab "TAE" \tab Disabled \cr
#'   Total Squared Error \tab "TSE" \tab Disabled \cr
#'  }
#'
#'  The \strong{Name} column refers to the name used in the package.
#'  This is the name in the output and when enabling/disabling in \code{metrics}.
#' @examples
#' # Attach packages
#' library(cvms)
#'
#' data <- data.frame(
#'   "targets" = rnorm(100, 14.7, 3.6),
#'   "predictions" = rnorm(100, 13.2, 4.6)
#' )
#'
#' evaluate_residuals(data = data,
#'                    predictions_col = "predictions",
#'                    targets_col = "targets"
#' )
evaluate_residuals <- function(data,
                               predictions_col,
                               targets_col,
                               metrics = list()) {
  if (checkmate::test_string(x = metrics, pattern = "^all$")) {
    metrics <- list("all" = TRUE)
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, min.rows = 1, min.cols = 2, add = assert_collection)
  checkmate::assert_string(x = predictions_col, add = assert_collection)
  checkmate::assert_string(x = targets_col, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(
    x = colnames(data),
    must.include = c(predictions_col, targets_col),
    what = "colnames"
  )
  checkmate::assert_list(
    x = metrics,
    types = "logical",
    any.missing = FALSE,
    names = "named",
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  metrics <- set_metrics(
    family = "gaussian", metrics_list = metrics,
    include_model_object_metrics = FALSE
  )

  call_evaluate_residuals(
    data = data,
    predictions_col = predictions_col,
    targets_col = targets_col,
    metrics = metrics,
    return_nas = FALSE
  )
}

call_evaluate_residuals <- function(data,
                                    predictions_col,
                                    targets_col,
                                    metrics,
                                    return_nas = FALSE) {

  # If the dataset is grouped, we need the indices and keys for the groups
  # so we can evaluate group wise

  # Get grouping keys
  grouping_keys <- dplyr::group_keys(data)
  # Make sure, the grouping_keys and the dataset are in the same order
  # As we otherwise risk adding them in the wrong order later
  data <- dplyr::arrange(data, !!!rlang::syms(colnames(grouping_keys)))
  # Get group indices
  grouping_factor <- dplyr::group_indices(data)

  # Calculate metric per group
  metrics_per_group <- data %>%
    dplyr::group_by_at(colnames(grouping_keys)) %>%
    dplyr::summarize(m = list(residual_metrics(
      !!as.name(predictions_col),
      !!as.name(targets_col),
      return_nas = return_nas
    ))) %>%
    legacy_unnest()

  metrics_per_group %>%
    base_select(c(
      colnames(grouping_keys),
      intersect(metrics, colnames(metrics_per_group))
    ))
}

residual_metrics <- function(predictions, targets, na.rm = TRUE, return_nas = FALSE) {
  if (isTRUE(return_nas)) {
    rmse <- NA
    mae <- NA
    nrmse <- NA
    rmseiqr <- NA
    rmsestd <- NA
    rmsle <- NA
    male <- NA
    rae <- NA
    rse <- NA
    rrse <- NA
    mape <- NA
    mse <- NA
    tae <- NA
    tse <- NA
  } else {
    if (!(is.numeric(predictions) || is.integer(predictions))) {
      stop("'predictions' must be numeric")
    }
    if (!(is.numeric(targets) || is.integer(targets))) {
      stop("'predictions' must be numeric")
    }
    if (length(predictions) != length(targets)) {
      stop("predictions and targets must have same length")
    }

    # Target descriptors
    targets_mean <- mean(targets, na.rm = na.rm)
    targets_range <- max(targets, na.rm = na.rm) - min(targets, na.rm = na.rm)
    targets_iqr <- IQR(targets, na.rm = na.rm)
    targets_std <- sd(targets, na.rm = na.rm)

    # Residuals
    residuals__ <- targets - predictions
    squared_residuals <- residuals__^2
    abs_residuals <- abs(residuals__)

    # Centered targets
    targets_centered <- targets - targets_mean
    abs_targets_centered <- abs(targets_centered)
    square_targets_centered <- targets_centered^2

    # total absolute error
    tae <- sum(abs_residuals, na.rm = na.rm)
    # total square error
    tse <- sum(squared_residuals, na.rm = na.rm)
    # mean absolute error
    mae <- mean(abs_residuals, na.rm = na.rm)
    # mean square error
    mse <- mean(squared_residuals, na.rm = na.rm)
    # root mean square error
    rmse <- sqrt(mse)

    # Normalized RMSE scores https://en.wikipedia.org/wiki/Root-mean-square_deviation
    rmseiqr <- rmse / targets_iqr
    nrmse <- rmse / targets_range
    rmsestd <- rmse / targets_std

    # relative absolute error
    rae <- tae / sum(abs_targets_centered)
    # relative squared error
    rse <- tse / sum(square_targets_centered)
    # root relative squared error
    rrse <- sqrt(rse)
    # absolute percentage errors
    # Note: Wiki has percentage error be ((y-p)/y) but we have ((p-y)/y)
    ape <- abs(residuals__ / targets)
    # mean absolute percentage error
    mape <- mean(ape)

    # https://www.sciencedirect.com/science/article/pii/S0169207016000121
    # TODO Can be included if meaningful
    # arctan absolute percentage error
    # aape <- atan(ape)
    # mean arctan absolute percentage error
    # maape <- mean(aape)

    # symmetric mean absolute percentage error (special version, 0-100% bounds)
    # https://en.wikipedia.org/wiki/Symmetric_mean_absolute_percentage_error
    # Removed based on https://robjhyndman.com/hyndsight/smape/
    # smape <- mean(abs_residuals / (abs(targets) + abs(predictions)))

    # log error
    le <- tryCatch(log(1 + predictions) - log(1 + targets),
      warning = function(w) {
        return(NaN)
      }
    )
    # mean squared log error
    msle <- mean(le^2)
    # root mean squared log error
    rmsle <- sqrt(msle)
    # mean absolute log error
    male <- mean(abs(le))
  }

  tibble::tibble(
    "RMSE" = rmse,
    "MAE" = mae,
    "NRMSE" = nrmse,
    "RMSEIQR" = rmseiqr,
    "RMSESTD" = rmsestd,
    "RMSLE" = rmsle,
    "MALE" = male,
    "RAE" = rae,
    "RSE" = rse,
    "RRSE" = rrse,
    "MAPE" = mape,
    "MSE" = mse,
    "TAE" = tae,
    "TSE" = tse
  )
}
