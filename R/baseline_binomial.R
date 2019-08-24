create_binomial_baseline_evaluations <- function(test_data,
                                                 dependent_col,
                                                 reps = 100,
                                                 positive = 2,
                                                 cutoff = 0.5,
                                                 na.rm = TRUE,
                                                 parallel_ = FALSE){

  # Check positive
  if (is.null(positive)){
    stop("'positive' was NULL. Must be either whole number or character.")
  }
  if (length(positive) != 1){
    stop("'positive' must have length 1.")
  }
  if (!(is.character(positive) || arg_is_wholenumber_(positive))){
    stop("'positive' must be either a whole number or character.")
  }
  if (arg_is_wholenumber_(positive) && positive %ni% c(1,2)){
    stop("When 'positive' is numeric, it must be either 1 or 2.")
  }

  # Check cutoff
  if (is.null(cutoff)){
    stop("'cutoff' was NULL. Must be numeric between 0.0 and 1.0.")
  }
  if (length(cutoff) != 1){
    stop("'cutoff' must have length 1.")
  }
  if (!is.numeric(cutoff)){
    stop("'cutoff' must be numeric.")
  }
  if (!is_between_(cutoff, 0.0,1.0)){
    stop("'cutoff' must be between 0.0 and 1.0.")
  }

  # Get targets
  targets <- test_data[[dependent_col]]
  n_targets <- length(targets)

  # Check dependent variable
  if (length(unique(targets)) > 2){
    if (is.numeric(targets) || is.integer(targets)){
      stop("The dependent column must maximally contain 2 levels. Did you specify the correct family?")
    }
    else {
      stop("The dependent column must maximally contain 2 levels.")
    }
  }

  # Check na.rm
  if(!is_logical_scalar_not_na(na.rm)){
    stop("'na.rm' must be logical scalar (TRUE/FALSE).")
  }

  # Add fold info columns
  test_data[["rel_fold"]] <- 1
  test_data[["abs_fold"]] <- 1

  # Create model_specifics object
  # Update to get default values when an argument was not specified
  model_specifics <- list(
    model_formula = "",
    family = "binomial",
    REML = FALSE,
    link = NULL,
    cutoff = cutoff,
    positive = positive,
    model_verbose = FALSE
  ) %>%
    basics_update_model_specifics()

  # Sample random probabilities
  random_probabilities <- split(runif(n_targets*reps),
                                f = factor(rep(1:reps, each = n_targets)))

  # Create all 0 and all 1 probabilities
  all_0_probabilities <- rep(0.000001, n_targets)
  all_1_probabilities <- rep(0.999999, n_targets)

  # Evaluate random probabilities

  evaluations_random <- plyr::llply(1:reps, .parallel = parallel_, function(evaluation){

    test_data[["prediction"]] <- random_probabilities[[evaluation]]

    # This will be changed to evaluation repetition later on
    test_data[["fold_column"]] <- evaluation

    internal_evaluate(
      test_data,
      type = "binomial",
      predictions_col = "prediction",
      targets_col = dependent_col,
      fold_info_cols = list(
        rel_fold = "rel_fold",
        abs_fold = "abs_fold",
        fold_column = "fold_column"
      ),
      # models=NULL,
      model_specifics = model_specifics
    )
  }) %>% dplyr::bind_rows() %>% # Works with nested tibbles (ldply doesn't seem to)
    dplyr::mutate(
      Family = "binomial",
      Dependent = dependent_col
    )

  # TODO Rename Fold Column to Repetition or similar in evaluations$Predictions

  metric_cols <- select_metrics(evaluations_random, include_definitions = FALSE)
  summarized_metrics <- summarize_metric_cols(metric_cols, na.rm = na.rm)

  # Evaluate all 0s

  test_data[["prediction"]] <- all_0_probabilities

  # This will be changed to evaluation repetition later on
  test_data[["fold_column"]] <- reps + 1

  evaluations_all_0 <- internal_evaluate(
    test_data,
    type = "binomial",
    predictions_col = "prediction",
    targets_col = dependent_col,
    fold_info_cols = list(
      rel_fold = "rel_fold",
      abs_fold = "abs_fold",
      fold_column = "fold_column"
    ),
    # models = NULL,
    model_specifics = model_specifics
  ) %>%
    dplyr::mutate(Family = "binomial",
                  Dependent = dependent_col) %>%
    select_metrics(include_definitions = FALSE) %>%
    dplyr::mutate(Measure = paste0("All_",
                                  ifelse(
                                    is.character(test_data[[dependent_col]]),
                                    sort(unique(test_data[[dependent_col]]))[[1]],
                                    0)))

  # Evaluate all 1s

  test_data[["prediction"]] <- all_1_probabilities

  # This will be changed to evaluation repetition later on
  test_data[["fold_column"]] <- reps + 2

  evaluations_all_1 <- internal_evaluate(
    test_data,
    type = "binomial",
    predictions_col = "prediction",
    targets_col = dependent_col,
    fold_info_cols = list(
      rel_fold = "rel_fold",
      abs_fold = "abs_fold",
      fold_column = "fold_column"
    ),
    # models=NULL,
    model_specifics = model_specifics) %>%
    dplyr::mutate(Family = "binomial",
                  Dependent = dependent_col) %>%
    select_metrics(include_definitions = FALSE) %>%
    dplyr::mutate( Measure = paste0("All_",
                                    ifelse(
                                      is.character(test_data[[dependent_col]]),
                                      sort(unique(test_data[[dependent_col]]))[[2]],
                                      1)))

  # Collect the summarized metrics

  overall_evaluations <- summarized_metrics %>%
    dplyr::bind_rows(evaluations_all_0,
                     evaluations_all_1)

  # Return summarized metrics and the random evaluations in a list

  return(list("summarized_metrics" = overall_evaluations,
              "random_evaluations" = evaluations_random))
}


replace_inf_with_na <- function(metric_cols){
  # Get rows with INFs
  metric_cols_with_infs <- metric_cols[is.infinite(rowSums(metric_cols)),]

  # Replace infs with NA
  if(nrow(metric_cols_with_infs) > 0){
    metric_cols <- do.call(data.frame,c(lapply(metric_cols, function(x) replace(x, is.infinite(x), NA)),
                                         check.names=FALSE,fix.empty.names = FALSE, stringsAsFactors=FALSE))
  }

  list("metric_cols" = metric_cols,
       "metric_cols_with_infs" = metric_cols_with_infs)
}

summarize_metric_cols <- function(metric_cols, na.rm = TRUE){

  # Start by replacing INF with NA
  # We keep the infs as well, as we wish to distinguish between them in the summary
  inf_replacement <- replace_inf_with_na(metric_cols)
  metric_cols <- inf_replacement[["metric_cols"]]
  metric_cols_with_infs <- inf_replacement[["metric_cols_with_infs"]]

  # Summarize the metrics with a range of functions
  # Note: This may be better solveable with pivot_* from tidyr, when it is on CRAN
  # This isn't exactly pretty.

  summarized_metrics <- dplyr::bind_rows(
    metric_cols %>% dplyr::summarize_all(.funs = list(~mean(., na.rm = na.rm))) %>% dplyr::mutate(f = "Mean"),
    metric_cols %>% dplyr::summarize_all(.funs = list(~median(., na.rm = na.rm))) %>% dplyr::mutate(f = "Median"),
    metric_cols %>% dplyr::summarize_all(.funs = list(~sd(., na.rm = na.rm))) %>% dplyr::mutate(f = "SD"),
    metric_cols %>% dplyr::summarize_all(.funs = list(~IQR(., na.rm = na.rm))) %>% dplyr::mutate(f = "IQR"),
    metric_cols %>% dplyr::summarize_all(.funs = list(~max(., na.rm = na.rm))) %>% dplyr::mutate(f = "Max"),
    metric_cols %>% dplyr::summarize_all(.funs = list(~min(., na.rm = na.rm))) %>% dplyr::mutate(f = "Min"),
    metric_cols %>% dplyr::summarize_all(.funs = list(~sum(is.na(.)))) %>% dplyr::mutate(f = "NAs"),
    metric_cols_with_infs %>% dplyr::summarize_all(.funs = list(~sum(is.infinite(.)))) %>% dplyr::mutate(f = "INFs")
  ) %>%
    dplyr::select(.data$f, dplyr::everything()) %>%
    dplyr::rename(Measure = .data$f)

  # Remove the INFs from the NAs count
  if(nrow(metric_cols_with_infs) > 0){
    NAs_row_number <- which(summarized_metrics$Measure == "NAs")
    INFs_row_number <- which(summarized_metrics$Measure == "INFs")
    summarized_metrics[NAs_row_number,-1] <- summarized_metrics[NAs_row_number,-1] - summarized_metrics[INFs_row_number,-1]
  }

  summarized_metrics
}
