
create_gaussian_baseline_evaluations <- function(train_data,
                                                 test_data,
                                                 dependent_col,
                                                 random_effects = NULL,
                                                 n_samplings = 100,
                                                 min_training_rows = 5,
                                                 min_training_rows_left_out = 3,
                                                 REML = FALSE,
                                                 metrics = list(),
                                                 na.rm = TRUE,
                                                 parallel_ = FALSE) {


  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_flag(x = na.rm, add = assert_collection)

  # Minimum requirement
  # Is there a better heuristic including n_samplings?
  # We want at least 3 rows in a sampled training set. (This is arbitrary)
  checkmate::assert_data_frame(x = train_data, min.rows = 11, add = assert_collection)
  checkmate::assert_number(x = min_training_rows, lower = 3, add = assert_collection)
  checkmate::assert_number(x = min_training_rows_left_out, lower = 2, add = assert_collection)

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (!is.null(random_effects)) {
    model_formula <- formula(paste0(dependent_col, " ~ 1 + ", random_effects))
  } else {
    model_formula <- formula(paste0(dependent_col, " ~ 1"))
  }

  # Extract Dependent, Fixed, Random
  model_effects <- extract_model_effects(list(model_formula))

  # Extract all model variables
  model_variables <- all.vars(terms(model_formula))

  # Test that all model variables are in the data frames

  if (length(setdiff(model_variables, colnames(train_data)))) {
    stop(paste0(
      "could not find these variables in the training data: ",
      paste0(setdiff(model_variables, colnames(train_data)),
        collapse = ", "
      )
    ))
  }
  if (length(setdiff(model_variables, colnames(test_data)))) {
    stop(paste0(
      "could not find these variables in the test data: ",
      paste0(setdiff(model_variables, colnames(test_data)),
        collapse = ", "
      )
    ))
  }

  # Subset data frames to only contain the relevant columns

  train_data <- train_data %>%
    base_select(cols = model_variables)

  test_data <- test_data %>%
    base_select(cols = model_variables)

  # Get targets
  test_targets <- test_data[[dependent_col]]
  train_targets <- train_data[[dependent_col]]
  n_test_targets <- length(test_targets)
  n_train_targets <- length(train_targets)

  # Sample probability of a row being included
  train_set_inclusion_vals <-
    data.frame(
      "inclusion_probability" = runif(n_train_targets * n_samplings),
      "split_factor" = factor(rep(1:n_samplings, each = n_train_targets))
    ) %>%
    dplyr::group_by(.data$split_factor) %>%
    dplyr::mutate(indices = 1:dplyr::n()) %>%
    dplyr::ungroup()

  # Find the boundaries for sampling a threshold
  # such that at least min_training_rows are included
  # and at least min_training_rows_left_out are not included

  sampling_boundaries <- train_set_inclusion_vals
  sampling_boundaries <- sampling_boundaries[order(sampling_boundaries$split_factor,
                            -sampling_boundaries$inclusion_probability,
                            method = "radix"), ]
  sampling_boundaries <- sampling_boundaries %>%
    dplyr::group_by(.data$split_factor) %>%
    dplyr::slice(min_training_rows,
                 n_train_targets - min_training_rows_left_out + 1) %>%
    dplyr::mutate(
      to_add = c(-0.001, 0.001),
      limits = .data$inclusion_probability + .data$to_add,
      min_max = c("max_", "min_")
    ) %>%
    base_select(cols = c("split_factor", "min_max", "limits")) %>%
    tidyr::spread(key = "min_max", value = "limits") %>%
    dplyr::mutate(
      inclusion_probability_threshold = runif(
        1, min = .data$min_, max = .data$max_)) %>%
    base_deselect(cols = c("max_", "min_"))

  # Filter rows to get training set indices
  # for the n_samplings evaluations
  train_sets_indices <- train_set_inclusion_vals %>%
    dplyr::inner_join(sampling_boundaries, by = "split_factor")
  train_sets_indices <- train_sets_indices[
    train_sets_indices[["inclusion_probability"]] >
      train_sets_indices[["inclusion_probability_threshold"]],
  ] %>%
    base_select(cols = c("split_factor", "indices"))

  # Get the lists of indices
  train_sets_indices <- split(train_sets_indices[["indices"]],
    f = train_sets_indices[["split_factor"]]
  )
  train_sets_indices[[as.character(n_samplings + 1)]] <- seq_len(n_train_targets)

  # Fit baseline model
  if (is.null(random_effects)) {
    lm_fn <- lm
  } else {
    lm_fn <- function(...) {
      lme4::lmer(..., REML = REML)
    }
  }

  # Create temporary fold columns
  tmp_fold_cols_obj <- create_tmp_fold_cols(test_data)
  test_data <- tmp_fold_cols_obj[["data"]]
  fold_info_cols <- tmp_fold_cols_obj[["fold_info_cols"]]
  fold_and_fold_col <- create_fold_and_fold_column_map(
    data = test_data, fold_info_cols = fold_info_cols
  )

  # Evaluate randomly sampled train set with model "y~1" (potentially plus random effects)

  evaluations <- plyr::llply(1:(n_samplings + 1),
    .parallel = parallel_,
    function(evaluation) {

      # Get indices for this evaluation
      inds <- train_sets_indices[[evaluation]]

      # Subset training data for this evaluation
      sampled_train_set <- train_data[inds, ]

      # Fit baseline model
      baseline_linear_model <- lm_fn(
        formula = model_formula,
        data = sampled_train_set
      )

      # Predict test set with baseline model
      test_data[["prediction"]] <- stats::predict(baseline_linear_model,
        test_data,
        allow.new.levels = TRUE
      )

      run_evaluate(
        data = test_data,
        target_col = dependent_col,
        prediction_cols = "prediction",
        models = list(baseline_linear_model),
        type = "gaussian",
        id_col = NULL,
        id_method = "mean",
        fold_info_cols = fold_info_cols,
        fold_and_fold_col = fold_and_fold_col,
        metrics = metrics,
        include_predictions = TRUE,
        include_fold_columns = FALSE, # We're not providing any fold info so won't make sense
        caller = "baseline()"
      ) %>%
        dplyr::mutate(`Training Rows` = nrow(sampled_train_set))
    }
  ) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(
      Family = "gaussian"
    ) %>%
    tibble::add_column(!!!model_effects) # bind_cols for recycling 1-row tibble

  # Extract the "all_rows" evaluation
  evaluation_all_rows <- evaluations[c(n_samplings + 1), ] %>%
    select_metrics(
      include_definitions = FALSE,
      additional_includes = "Training Rows"
    ) %>%
    dplyr::mutate(Measure = "All_rows")

  # Extract the random evaluations
  evaluations_random <- evaluations[-c(n_samplings + 1), ]

  # Remove collected evaluations from memory
  evaluations <- NULL

  # Add Repetition column to predictions
  evaluations_random[["Predictions"]] <-
    add_repetition_col_to_nested_tibbles(evaluations_random[["Predictions"]])
  # Add Repetition column to coefficients
  evaluations_random[["Coefficients"]] <-
    add_repetition_col_to_nested_tibbles(evaluations_random[["Coefficients"]])

  # Summarize metric cols
  metric_cols <- select_metrics(evaluations_random,
    include_definitions = FALSE,
    additional_includes = "Training Rows"
  )
  summarized_metrics <- summarize_metrics(metric_cols, na.rm = na.rm, inf.rm = TRUE)

  # Collect the summarized metrics
  overall_evaluations <- summarized_metrics %>%
    dplyr::bind_rows(evaluation_all_rows) %>%
    dplyr::as_tibble()

  return(list(
    "summarized_metrics" = overall_evaluations,
    "random_evaluations" = evaluations_random
  ))
}

# Remove the INFs from the NAs count
subtract_inf_count_from_na_count <- function(summarized_metrics) {

  # Find the row indices of the NA and INF counts
  NAs_row_number <- which(summarized_metrics$Measure == "NAs")
  INFs_row_number <- which(summarized_metrics$Measure == "INFs")

  measure_col <- summarized_metrics[,"Measure"]
  summarized_metrics[["Measure"]] <- NULL

  # Subtract the INF counts from the NA counts
  summarized_metrics[NAs_row_number,] <- summarized_metrics[NAs_row_number,] -
    summarized_metrics[INFs_row_number,]

  summarized_metrics <- summarized_metrics %>%
    tibble::add_column(Measure = measure_col[["Measure"]],
                       .before = colnames(summarized_metrics)[[1]])
  summarized_metrics
}
