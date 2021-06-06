# Evaluates single model object
# and extracts information like coefficients

# Note: We could perhaps skip this function,
# as it is almost just passing on to evaluate_model_object()
# but will leave for now, in case it is useful to have an extra layer

internal_evaluate_model <- function(model,
                                    train_data,
                                    test_data,
                                    type,
                                    fold_info,
                                    fold_info_cols = list(
                                      "rel_fold" = "rel_fold",
                                      "abs_fold" = "abs_fold",
                                      "fold_column" = "fold_column"
                                    ),
                                    model_specifics = list(),
                                    metrics,
                                    include_fold_columns = TRUE) {
  evaluate_model_object(
    model = model,
    train_data = train_data,
    test_data = test_data,
    type = type,
    fold_info = fold_info,
    fold_info_cols = fold_info_cols,
    model_specifics = model_specifics,
    metrics = metrics,
    include_fold_columns = include_fold_columns
  )
}


evaluate_model_object <- function(model,
                                  train_data,
                                  test_data,
                                  type,
                                  fold_info,
                                  fold_info_cols,
                                  model_specifics,
                                  metrics,
                                  include_fold_columns) {
  if (type == "gaussian") {
    REML <- tryCatch(
      {
        model_specifics[["REML"]]
      },
      error = function(e) {
        stop("model_specifics must contain the REML argument.")
      }
    )
  } else {
    REML <- NULL
  }

  if (!is.null(model)) {

    # Calculate model metrics
    model_metrics <- calculate_model_metrics(
      model = model, REML = REML, metrics = metrics
    )

    # Get model coefficients
    nested_coefficients <- tryCatch(
      {
        get_nested_model_coefficients(
          model = model,
          train_data = train_data,
          test_data = test_data,
          fold_info = fold_info,
          include_fold_columns = include_fold_columns,
          caller = model_specifics[["caller"]]
        )
      },
      error = function(e) {
        get_nested_model_coefficients(
          model = NULL, include_fold_columns = include_fold_columns
        )
      }
    )
  } else {
    model_metrics <- calculate_model_metrics(NULL, NULL, metrics = metrics)

    nested_coefficients <- get_nested_model_coefficients(
      model = NULL, include_fold_columns = include_fold_columns
    )
  }

  # Add fold info if required
  if (isTRUE(include_fold_columns)) {
    model_metrics <- model_metrics %>%
      dplyr::mutate(
        abs_fold = fold_info[["abs_fold"]],
        rel_fold = fold_info[["rel_fold"]],
        fold_column = fold_info[["fold_column"]]
      )
  }


  model_metrics %>%
    dplyr::mutate(Coefficients = nested_coefficients)
}


# Calculate model object metrics if required
calculate_model_metrics <- function(model, REML, metrics) {
  if (is.null(model)) {
    r2m_ <- NA
    r2c_ <- NA
    AIC_ <- NA
    AICc_ <- NA
    BIC_ <- NA
  } else {
    r2m_ <- run_if_in(
      function() {
        calculate_r2m(model)
      },
      metric_name = "r2m", metrics = metrics
    )

    r2c_ <- run_if_in(
      function() {
        calculate_r2c(model)
      },
      metric_name = "r2c", metrics = metrics
    )

    AIC_ <- run_if_in(
      function() {
        calculate_AIC(model)
      },
      metric_name = "AIC", metrics = metrics
    )

    AICc_ <- run_if_in(
      function() {
        calculate_AICc(model, REML)
      },
      metric_name = "AICc", metrics = metrics
    )

    BIC_ <- run_if_in(
      function() {
        calculate_BIC(model)
      },
      metric_name = "BIC", metrics = metrics
    )
  }

  # Create tibble with the calculated metrics
  # A metric will be NA if not in 'metrics'
  model_metrics <- tibble::tibble(
    "r2m" = r2m_, "r2c" = r2c_,
    "AIC" = AIC_, "AICc" = AICc_,
    "BIC" = BIC_
  )

  # Return the metrics we want
  model_metrics %>%
    base_select(cols = intersect(metrics, colnames(model_metrics)))
}

run_if_in <- function(calc_fn, metric_name, metrics) {
  if (metric_name %in% metrics) {
    calc_fn()
  } else {
    NA
  }
}
