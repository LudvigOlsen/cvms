
create_gaussian_baseline_evaluations <- function(train_data,
                                                 test_data,
                                                 dependent_col,
                                                 random_effects = NULL,
                                                 n_samplings = 100,
                                                 min_training_rows = 5,
                                                 min_training_rows_left_out = 3,
                                                 na.rm = TRUE,
                                                 parallel_ = FALSE){


  # Minimum requirement
  # Is there a better heuristic including n_samplings?
  # We want at least 3 rows in a sampled training set. (This is arbitrary)
  stopifnot(nrow(train_data) > 10,
            min_training_rows >= 3,
            min_training_rows_left_out >= 2)

  # Check na.rm
  if(!is_logical_scalar_not_na(na.rm)){
    stop("'na.rm' must be logical scalar (TRUE/FALSE).")
  }

  if (!is.null(random_effects)){
    model_formula <- formula(paste0(dependent_col, " ~ 1 + ", random_effects))
  } else {
    model_formula <- formula(paste0(dependent_col, " ~ 1"))
  }

  # Extract Dependent, Fixed, Random
  model_effects <- extract_model_effects(list(model_formula))

  # Extract all model variables
  model_variables <- all.vars(terms(model_formula))

  # Test that all model variables are in the data frames

  if (length(setdiff(model_variables, colnames(train_data)))){
    stop(paste0(
      "could not find these variables in the training data: ",
      paste0(setdiff(model_variables, colnames(train_data)),
             collapse = ", ")
    ))
  }
  if (length(setdiff(model_variables, colnames(test_data)))){
    stop(paste0(
      "could not find these variables in the test data: ",
      paste0(setdiff(model_variables, colnames(test_data)),
             collapse = ", ")
    ))
  }

  # Subset data frames to only contain the relevant columns

  train_data <- train_data %>%
    dplyr::select(dplyr::one_of(model_variables))

  test_data <- test_data %>%
    dplyr::select(dplyr::one_of(model_variables))

  # Get targets
  test_targets <- test_data[[dependent_col]]
  train_targets <- train_data[[dependent_col]]
  n_test_targets <- length(test_targets)
  n_train_targets <- length(train_targets)

  # Add fold info columns
  test_data[["rel_fold"]] <- 1
  test_data[["abs_fold"]] <- 1

  # Create model_specifics object
  # Update to get default values when an argument was not specified
  model_specifics <- list(
    model_formula = model_formula,
    family = "gaussian",
    REML = FALSE,
    link = NULL,
    cutoff = 0.5,
    positive = 2,
    model_verbose = FALSE,
    caller = "baseline()") %>%
    basics_update_model_specifics()

  # Sample probability of a row being included
  train_set_inclusion_vals <-
    data.frame(
      "inclusion_probability" = runif(n_train_targets * n_samplings),
      "split_factor" = factor(rep(1:n_samplings, each = n_train_targets))
    ) %>%
    dplyr::group_by(.data$split_factor) %>%
    dplyr::mutate(indices = 1:dplyr::n())

  # Find the boundaries for sampling a threshold
  # such that at least min_training_rows are included
  # and at least min_training_rows_left_out are not included
  sampling_boundaries <- train_set_inclusion_vals %>%
    dplyr::arrange(.data$split_factor, dplyr::desc(.data$inclusion_probability)) %>%
    dplyr::filter(dplyr::row_number() == min_training_rows |
                  dplyr::row_number() == n_train_targets - min_training_rows_left_out + 1) %>%
    dplyr::mutate(to_add = c(-0.001, 0.001),
                  limits = .data$inclusion_probability + .data$to_add,
                  min_max = c("max_","min_")) %>%
    dplyr::select(.data$split_factor, .data$min_max, .data$limits) %>%
    tidyr::spread(key = "min_max", value="limits") %>%
    dplyr::mutate(inclusion_probability_threshold = runif(1, min = .data$min_, max = .data$max_)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(.data$max_, .data$min_))

  # Filter rows to get training set indices
  # for the n_samplings evaluations
  train_sets_indices <- train_set_inclusion_vals %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(sampling_boundaries, by = "split_factor") %>%
    dplyr::filter(.data$inclusion_probability > .data$inclusion_probability_threshold) %>%
    dplyr::select(c(.data$split_factor, .data$indices))

  # Get the lists of indices
  train_sets_indices <- split(train_sets_indices[["indices"]],
                              f = train_sets_indices[["split_factor"]])

  # Fit baseline model
  if (is.null(random_effects)){
    lm_fn <- lm
  } else {
    lm_fn <- lme4::lmer
  }

  # Evaluate randomly sampled train set with model "y~1" (potentially plus random effects)

  evaluations_random <- plyr::llply(1:n_samplings, .parallel = parallel_, function(evaluation){

    # Get indices for this evaluation
    inds <- train_sets_indices[[evaluation]]

    # Subset training data for this evaluation
    sampled_train_set <- train_data[inds,]

    # Fit baseline model
    baseline_linear_model <- lm_fn(model_specifics[["model_formula"]],
                                data = sampled_train_set)

    # Predict test set with baseline model
    test_data[["prediction"]] <- stats::predict(baseline_linear_model,
                                                test_data, allow.new.levels = TRUE)

    # This will be changed to evaluation repetition later on
    test_data[["fold_column"]] <- evaluation

    internal_evaluate(
      test_data,
      type = "gaussian",
      predictions_col = "prediction",
      targets_col = dependent_col,
      fold_info_cols = list(
        rel_fold = "rel_fold",
        abs_fold = "abs_fold",
        fold_column = "fold_column"
      ),
      models = list(baseline_linear_model),
      model_specifics = model_specifics
    ) %>%
      dplyr::select(-.data$Results) %>%
      dplyr::mutate(`Training Rows` = nrow(sampled_train_set))
  }) %>%  dplyr::bind_rows() %>%
    dplyr::mutate(
      Family = "gaussian"
    ) %>%
    tibble::add_column(!!! model_effects) # bind_cols for recycling 1-row tibble

  # TODO Rename Fold Column to Repetition or similar in evaluations$Predictions

  metrics_cols <- select_metrics(evaluations_random, include_definitions = FALSE)
  metrics_cols[["Training Rows"]] <- evaluations_random[["Training Rows"]]

  # Get rows with INFs
  metrics_cols_with_infs <- metrics_cols[is.infinite(rowSums(
    metrics_cols %>% dplyr::mutate(Family="gaussian") %>% select_metrics(include_definitions = FALSE)
    )),]

  # Replace infs with NA
  if(nrow(metrics_cols_with_infs) > 0){
    metrics_cols <- do.call(data.frame,c(lapply(metrics_cols, function(x) replace(x, is.infinite(x), NA)),
                                         check.names=FALSE,fix.empty.names = FALSE, stringsAsFactors=FALSE))
  }

  # This may be better solveable with pivot_* from tidyr, when it is on CRAN
  # This isn't exactly pretty.
  summarized_metrics <- dplyr::bind_rows(
    metrics_cols %>% dplyr::summarize_all(.funs = list(~mean(., na.rm = na.rm))) %>% dplyr::mutate(f = "Mean"),
    metrics_cols %>% dplyr::summarize_all(.funs = list(~median(., na.rm = na.rm))) %>% dplyr::mutate(f = "Median"),
    metrics_cols %>% dplyr::summarize_all(.funs = list(~sd(., na.rm = na.rm))) %>% dplyr::mutate(f = "SD"),
    metrics_cols %>% dplyr::summarize_all(.funs = list(~IQR(., na.rm = na.rm))) %>% dplyr::mutate(f = "IQR"),
    metrics_cols %>% dplyr::summarize_all(.funs = list(~max(., na.rm = na.rm))) %>% dplyr::mutate(f = "Max"),
    metrics_cols %>% dplyr::summarize_all(.funs = list(~min(., na.rm = na.rm))) %>% dplyr::mutate(f = "Min"),
    metrics_cols %>% dplyr::summarize_all(.funs = list(~sum(is.na(.)))) %>% dplyr::mutate(f = "NAs"),
    metrics_cols_with_infs %>% dplyr::summarize_all(.funs = list(~sum(is.infinite(.)))) %>% dplyr::mutate(f = "INFs")
  ) %>%
    dplyr::select(.data$f, dplyr::everything()) %>%
    dplyr::rename(Measure = .data$f)

  # Remove the INFs from the NAs count
  if(nrow(metrics_cols_with_infs) > 0){
    NAs_row_number <- which(summarized_metrics$Measure == "NAs")
    INFs_row_number <- which(summarized_metrics$Measure == "INFs")
    summarized_metrics[NAs_row_number,-1] <- summarized_metrics[NAs_row_number,-1] -
      summarized_metrics[INFs_row_number,-1]
  }


  # Fitting on all rows

  # Fit baseline model
  baseline_linear_model_all_rows <- lm_fn(model_specifics[["model_formula"]], data = train_data)

  # Predict test set with baseline model
  test_data[["prediction"]] <- stats::predict(baseline_linear_model_all_rows,
                                              test_data, allow.new.levels=TRUE)

  # This will be changed to evaluation repetition later on
  test_data[["fold_column"]] <- n_samplings + 1

  evaluation_all_rows <- internal_evaluate(
    test_data,
    type = "gaussian",
    predictions_col = "prediction",
    targets_col = dependent_col,
    fold_info_cols = list(rel_fold = "rel_fold",
                          abs_fold = "abs_fold",
                          fold_column = "fold_column"),
    models = list(baseline_linear_model_all_rows),
    model_specifics = model_specifics) %>%
    dplyr::mutate(Family = "gaussian") %>%
    select_metrics(include_definitions = FALSE) %>%
    dplyr::mutate(
      `Training Rows` = nrow(train_data),
      Measure = "All_rows"
    )

  # Collect the summarized metrics
  overall_evaluations <- summarized_metrics %>%
    dplyr::bind_rows(evaluation_all_rows)

  return(list("summarized_metrics" = overall_evaluations,
              "random_evaluations" = evaluations_random))


}
