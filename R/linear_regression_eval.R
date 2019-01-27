linear_regression_eval <- function(data,
                                   models,
                                   predictions_col = "predictions",
                                   targets_col = "targets",
                                   folds_col = "fold",
                                   model_specifics=list()){

  REML <- tryCatch({
    model_specifics[["REML"]]
    }, error = function(e){
      stop("model_specifics must contain the REML argument.")
    })

  num_folds <- length(unique(data[[folds_col]]))

  # When adding NULL to a list, it isn't actually added
  # so if a model object is NULL (didn't converge),
  # the list will be shorter than the number of folds
  if(length(models) == num_folds){

    # Calculate RMSE
    rmse_per_fold <- data %>%
      dplyr::group_by(!! as.name(folds_col)) %>%
      dplyr::summarize(RMSE = calculate_RMSE(!! as.name(predictions_col),
                                    !! as.name(targets_col)))

    avg_rmse <- rmse_per_fold %>%
      dplyr::pull(RMSE) %>%
      mean() %>%
      tibble::as_tibble() %>%
      dplyr::rename(RMSE = value)

    # Get model metrics
    model_metrics_per_fold <- plyr::ldply(models, function(m){
      linear_regression_model_eval(m, REML)
    })

    avg_model_metrics <- model_metrics_per_fold %>%
      dplyr::summarise_all(dplyr::funs(mean))

    # Get model coefficients
    nested_coefficients <- tryCatch({
      get_nested_model_coefficients(models)
    }, error = function(e){
      get_nested_model_coefficients(NULL)
    })


  } else {
    rmse_per_fold <- tibble::tibble("RMSE"=rep(NA, num_folds))
    avg_rmse <- tibble::tibble("RMSE"=NA)
    model_metrics_per_fold <- linear_regression_model_eval(NULL, NULL) %>%
      rep(num_folds) %>%
      dplyr::bind_rows()
    avg_model_metrics <- linear_regression_model_eval(NULL, FALSE)
    nested_coefficients <- get_nested_model_coefficients(NULL)
  }

  # Combine
  avg_results <- avg_rmse %>%
    dplyr::bind_cols(avg_model_metrics)

  results_per_fold <- rmse_per_fold %>%
    dplyr::bind_cols(model_metrics_per_fold)

  # nest fold results and add to result tibble
  avg_results[["Results"]] <- nest_results(results_per_fold)[["results"]]

  avg_results[["Coefficients"]] <- nested_coefficients

  return(avg_results)

}


linear_regression_model_eval <- function(model, REML){

  if (is.null(model)){
    r2m_ = NA
    r2c_ = NA
    AIC_ = NA
    AICc_ = NA
    BIC_ = NA
  } else {
    r2m_ <- calculate_r2m(model)
    r2c_ <- calculate_r2c(model)
    AIC_ <- calculate_AIC(model)
    AICc_ <- calculate_AICc(model, REML)
    BIC_ <- calculate_BIC(model)
  }

  tibble::tibble('r2m' = r2m_, 'r2c' = r2c_,
                 'AIC' = AIC_, 'AICc' = AICc_,
                 'BIC' = BIC_)

}

# Try to retrieve
get_nested_model_coefficients <- function(models){
  # Note: models should be ordered by the fold they were fitted in

  if(is.null(models)){

      tibble::tibble('term'=NA, 'estimate'=NA,
                     'std.error'=NA, 'statistic'=NA,
                     'fold'=NA) %>%
      nest_models() %>%
      dplyr::pull(coefficients) %>%
      return()
  }

  tryCatch({
    fold = 0
    plyr::llply(models, function(model){
      fold <<- fold + 1
      broom::tidy(model, effects = c("fixed")) %>%
        dplyr::mutate(fold = fold)
    }) %>%
      dplyr::bind_rows() %>%
      nest_models() %>%
      dplyr::pull(coefficients)
  }, error = function(e){
    stop(paste0("Error when extracting model coefficients: ", e))
  })
}
