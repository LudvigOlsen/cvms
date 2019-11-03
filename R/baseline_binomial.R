create_binomial_baseline_evaluations <- function(test_data,
                                                 dependent_col,
                                                 reps = 100,
                                                 positive = 2,
                                                 cutoff = 0.5,
                                                 metrics = list(),
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
      stop(paste0("The dependent column must maximally contain 2 levels.",
                  " Did you specify the correct family?"))
    }
    else {
      stop("The dependent column must maximally contain 2 levels.")
    }
  }

  # Check na.rm
  if(!is_logical_scalar_not_na(na.rm)){
    stop("'na.rm' must be logical scalar (TRUE/FALSE).")
  }

  # Create all 0 and all 1 probabilities
  all_0_probabilities <- rep(0.000001, n_targets)
  all_1_probabilities <- rep(0.999999, n_targets)

  # Sample random probabilities
  random_probabilities <- runif(n_targets * reps)

  # Collect the probability sets
  probability_sets <- split(c(
    random_probabilities,
    all_0_probabilities,
    all_1_probabilities
  ),
  f = factor(rep(1:(reps + 2), each = n_targets)))

  # Create temporary fold columns
  tmp_fold_cols_obj <- create_tmp_fold_cols(test_data)
  test_data <- tmp_fold_cols_obj[["data"]]
  fold_info_cols <- tmp_fold_cols_obj[["fold_info_cols"]]
  fold_and_fold_col <- create_fold_and_fold_column_map(
    data = test_data, fold_info_cols = fold_info_cols)

  # Evaluate probability sets
  evaluations <- plyr::llply(seq_len(length(probability_sets)), .parallel = parallel_,
                             function(evaluation){

    test_data[["prediction"]] <- probability_sets[[evaluation]]

    run_evaluate(
      data = test_data,
      target_col = dependent_col,
      prediction_cols = "prediction",
      type = "binomial",
      id_col = NULL,
      id_method = "mean",
      fold_info_cols = fold_info_cols,
      fold_and_fold_col = fold_and_fold_col,
      cutoff = cutoff,
      positive = positive,
      metrics = metrics,
      include_predictions = TRUE,
      include_fold_columns = FALSE, # We're not providing any fold info so won't make sense
      caller = "baseline()"
    )
  }) %>% dplyr::bind_rows() %>% # Works with nested tibbles (ldply doesn't seem to)
    dplyr::mutate(
      Family = "binomial",
      Dependent = dependent_col
    )

  # Add Repetition column to predictions
  evaluations[["Predictions"]] <-
    add_repetition_col_to_nested_tibbles(evaluations[["Predictions"]])
  evaluations[["Confusion Matrix"]] <-
    add_repetition_col_to_nested_tibbles(evaluations[["Confusion Matrix"]])

  # Create the "all_0", etc., labels
  if (is.character(test_data[[dependent_col]]))
    all_x_labels <- sort(unique(test_data[[dependent_col]]))
  else
    all_x_labels <- c(0, 1)

  # Extract the all_0 and all_1 evaluations
  evaluations_all_0_1 <- evaluations[c(reps + 1, reps + 2) ,] %>%
    select_metrics(include_definitions = FALSE) %>%
    dplyr::mutate(Measure = paste0("All_", all_x_labels))

  # Extract random evaluations
  evaluations_random <- evaluations[-c(reps + 1, reps + 2) ,]

  # Remove collected evaluations from memory
  evaluations <- NULL

  # Extract and summarize metrics from random evaluations
  metric_cols <- select_metrics(evaluations_random, include_definitions = FALSE)
  summarized_metrics <- summarize_metrics(metric_cols, na.rm = na.rm, inf.rm = TRUE)

  # Collect the summarized metrics
  overall_evaluations <- summarized_metrics %>%
    dplyr::bind_rows(evaluations_all_0_1) %>%
    dplyr::as_tibble()

  # Return summarized metrics and the random evaluations in a list

  return(list("summarized_metrics" = overall_evaluations,
              "random_evaluations" = evaluations_random))
}


add_repetition_col_to_nested_tibbles <- function(l){
  # Add Repetition column to nested tibble
  l %>%
    dplyr::bind_rows(.id = "Repetition") %>%
    dplyr::mutate(Repetition = as.integer(.data$Repetition)) %>%
    dplyr::group_nest(.data$Repetition, keep = TRUE) %>%
    dplyr::pull(.data$data)
}

