#' Calculate regression error metrics
regression_errors <- function(data,
                              predictions_col,
                              targets_col,
                              metrics = list()){

  metrics <- set_metrics(family = "gaussian", metrics_list = metrics,
                         include_model_object_metrics = FALSE)

  call_regression_errors(
    data = data,
    predictions_col = predictions_col,
    targets_col = targets_col,
    metrics = metrics,
    return_nas = FALSE
  )

}

call_regression_errors <- function(data,
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
  data <- dplyr::arrange(data, !!! rlang::syms(colnames(grouping_keys)))
  # Get group indices
  grouping_factor <- dplyr::group_indices(data)

  # Calculate metric per group
  metrics_per_group <- data %>%
    dplyr::group_by_at(colnames(grouping_keys)) %>%
    dplyr::summarize(m = list(residual_metrics(
      !!as.name(predictions_col),
      !!as.name(targets_col),
      return_nas = return_nas))) %>%
    legacy_unnest()

  metrics_per_group %>%
    base_select(c(colnames(grouping_keys),
                  intersect(metrics, colnames(metrics_per_group))))

}

residual_metrics <- function(predictions, targets, na.rm = TRUE, return_nas = FALSE){

  if (isTRUE(return_nas)){

    rmse <- NA
    mae <- NA
    nrmse <- NA
    rmseiqr <- NA
    rmsestd <- NA
    rae <- NA
    rrse <- NA
    mape <- NA
    mse <- NA
    tae <- NA
    tse <- NA

  } else {

    if (!(is.numeric(predictions) || is.integer(predictions))){
      stop("'predictions' must be numeric")
    }
    if (!(is.numeric(targets) || is.integer(targets))){
      stop("'predictions' must be numeric")
    }
    if (length(predictions) != length(targets)){
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
    # root relative squared error
    rrse <- sqrt(tse / sum(square_targets_centered))
    # mean absolute percentage error
    mape <- mean(abs(residuals__/targets))

  }

  tibble::tibble("RMSE" = rmse,
                 "MAE" = mae,
                 "NRMSE" = nrmse,
                 "RMSEIQR" = rmseiqr,
                 "RMSESTD" = rmsestd,
                 "RAE" = rae,
                 "RRSE" = rrse,
                 "MAPE" = mape,
                 "MSE" = mse,
                 "TAE" = tae,
                 "TSE" = tse
                 )

}
