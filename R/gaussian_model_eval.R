gaussian_model_eval <- function(model,
                                train_data,
                                test_data,
                                type,
                                fold_info,
                                model_specifics,
                                metrics,
                                include_fold_columns) {
  REML <- tryCatch({
    model_specifics[["REML"]]
  }, error = function(e) {
    stop("model_specifics must contain the REML argument.")
  })

  if (!is.null(model)) {
    # Get model metrics
    model_metrics <- calculate_gaussian_model_metrics(
      model = model, REML = REML, metrics = metrics)

    # Deselect disabled metrics
    model_metrics <- model_metrics %>%
      dplyr::select(dplyr::one_of(intersect(metrics, colnames(model_metrics)))) %>%
      mutate(
        abs_fold = fold_info[["abs_fold"]],
        rel_fold = fold_info[["rel_fold"]],
        fold_column = fold_info[["fold_column"]]
      )

    # Get model coefficients
    nested_coefficients <- tryCatch({
      get_nested_model_coefficients(
        model = model,
        train_data = train_data,
        test_data  = test_data,
        fold_info = fold_info,
        include_fold_columns = include_fold_columns
      )

    }, error = function(e) {
      get_nested_model_coefficients(
        model = NULL, include_fold_columns = include_fold_columns)

    })

  } else {

    model_metrics <- calculate_gaussian_model_metrics(NULL, NULL, metrics = metrics)

    # TODO Create test of this
    model_metrics[[fold_info_cols[["fold_column"]]]] <-
      fold_and_fold_col[[fold_info_cols[["fold_column"]]]]
    model_metrics[[fold_info_cols[["abs_fold"]]]] <-
      fold_and_fold_col[[fold_info_cols[["abs_fold"]]]]
    model_metrics[[fold_info_cols[["rel_fold"]]]] <-
      fold_and_fold_col[[fold_info_cols[["rel_fold"]]]]

    nested_coefficients <- get_nested_model_coefficients(
      model = NULL, include_fold_columns = include_fold_columns)

  }

  model_metrics %>%
    dplyr::mutate(Coefficients = nested_coefficients)
}


# calculate model object metrics if required
calculate_gaussian_model_metrics <- function(model, REML, metrics){

  if (is.null(model)){

    r2m_ <- NA
    r2c_ <- NA
    AIC_ <- NA
    AICc_ <- NA
    BIC_ <- NA

  } else {

    r2m_ <- run_if_in(
      function(){ calculate_r2m(model) },
      metric_name = "r2m", metrics = metrics)

    r2c_ <- run_if_in(
      function(){ calculate_r2c(model) },
      metric_name = "r2c", metrics = metrics)

    AIC_ <- run_if_in(
      function(){ calculate_AIC(model) },
      metric_name = "AIC", metrics = metrics)

    AICc_ <- run_if_in(
      function(){ calculate_AICc(model, REML) },
      metric_name = "AICc", metrics = metrics)

    BIC_ <- run_if_in(
      function(){ calculate_BIC(model) },
      metric_name = "BIC", metrics = metrics)

  }

  tibble::tibble('r2m' = r2m_, 'r2c' = r2c_,
                 'AIC' = AIC_, 'AICc' = AICc_,
                 'BIC' = BIC_)
}

run_if_in <- function(calc_fn, metric_name, metrics){
  if (metric_name %in% metrics){
    calc_fn()
  } else {
    NA
  }
}
