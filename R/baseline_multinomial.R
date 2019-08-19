baseline_multinomial <- function(test_data,
                                 dependent_col,
                                 reps = 100,
                                 na.rm = TRUE,
                                 parallel_ = FALSE){

  # Check na.rm
  if(!is_logical_scalar_not_na(na.rm)){
    stop("'na.rm' must be logical scalar (TRUE/FALSE).")
  }

  # Extract the dependent column from the test set
  targets <- as.character(test_data[[dependent_col]])
  num_targets <- length(targets)
  # Extract the unique class labels
  classes <- unique(targets)
  # Count number of classes
  num_classes <- length(classes)

  # Add fold info columns
  test_data[["rel_fold"]] <- 1
  test_data[["abs_fold"]] <- 1

  # Create model_specifics object
  # Update to get default values when an argument was not specified
  model_specifics <- list(
    model_formula = "",
    family = "multinomial",
    REML = FALSE,
    link = NULL,
    model_verbose = FALSE
  ) %>%
    basics_update_model_specifics()

  # TODO Test num_classes etc.

  random_probabilities <- create_multinomial_probability_tibble(num_classes = num_classes,
                                                                num_observations = num_targets * reps,
                                                                apply_softmax = TRUE) %>%
    dplyr::rename_all(~classes) %>%
    nest_probabilities_rowwise() %>%
    split(f = factor(rep(1:reps, each = num_targets)))

  evaluations_random <- plyr::llply(1:reps, .parallel = parallel_, function(evaluation){

    test_data[["prediction"]] <- random_probabilities[[evaluation]]

    # This will be changed to evaluation repetition later on
    test_data[["fold_column"]] <- evaluation

    results <- internal_evaluate(
      test_data,
      type = "multinomial",
      predictions_col = "prediction",
      targets_col = dependent_col,
      fold_info_cols = list(
        rel_fold = "rel_fold",
        abs_fold = "abs_fold",
        fold_column = "fold_column"
      ),
      # models=NULL,
      model_specifics = model_specifics)

    results[["results"]][["Class"]] <- "Avg"

    results %>%
      dplyr::bind_rows() %>%  # bind the dataframes
      dplyr::mutate(Repetition = evaluation)


  }) %>% dplyr::bind_rows() %>% # Works with nested tibbles (ldply doesn't seem to)
    dplyr::mutate(
      Family = "multinomial",
      Dependent = dependent_col
    ) %>%
    dplyr::select(.data$Class, dplyr::everything()) %>%
    dplyr::select(.data$Repetition, dplyr::everything())

  # Extract the metrics
  metric_cols_all <- select_metrics(evaluations_random, include_definitions = FALSE,
                                    additional_includes = "Class")

  # Extract the "Avg result" rows and select the metric columns
  metric_cols_avg <- metric_cols_all %>%
    dplyr::filter(.data$Class == "Avg") %>%
    dplyr::select(-.data$Class)

  # Summarize the metrics
  summarized_avg_metrics <- summarize_metric_cols(metric_cols_avg, na.rm = na.rm)

  # Find the class level summaries
  summarized_metrics_class_level <- plyr::ldply(classes, function(cl){
    # Extract the class level (non averaged) results and select the metric columns
    metric_cols_current_class <- metric_cols_all %>%
      dplyr::filter(.data$Class == cl) %>%
      dplyr::select(-c(.data$Class, .data$`Overall Accuracy`))

    summarized_class_level_result <- summarize_metric_cols(
      metric_cols_current_class,
      na.rm = na.rm) %>%
      dplyr::bind_rows(
        all_or_nothing_evaluations(test_data = test_data,
                                   targets_col = dependent_col,
                                   current_class = cl,
                                   reps = reps)
      ) %>%
      dplyr::mutate(Class = cl)

    # TODO Make all 0/1 evals per class

  }) %>% dplyr::select(.data$Class, dplyr::everything())

  return(list("summarized_average_metrics" = summarized_avg_metrics,
              "summarized_class_level_metrics" = summarized_metrics_class_level,
              "random_evaluations" = evaluations_random))

}


all_or_nothing_evaluations <- function(test_data, targets_col, current_class, reps){

  num_targets <- nrow(test_data)

  local_tmp_target_var <- create_tmp_var(test_data,"all_or_nothing_targets")
  local_tmp_predicted_probability_all_1_var <- create_tmp_var(test_data,"all_1_predicted_probability")
  local_tmp_predicted_probability_all_0_var <- create_tmp_var(test_data,"all_0_predicted_probability")

  test_data[[local_tmp_target_var]] <- factor(ifelse(test_data[[targets_col]] == current_class, 1, 0))
  test_data[[local_tmp_predicted_probability_all_1_var]] <- rep(0.999999, num_targets)
  test_data[[local_tmp_predicted_probability_all_0_var]] <- rep(0.000001, num_targets)

  # Create model_specifics object
  # Update to get default values when an argument was not specified
  model_specifics <- list(
    model_formula = "",
    family = "multinomial",
    REML = FALSE,
    link = NULL,
    positive = 2,
    cutoff = 0.5,
    model_verbose = FALSE
  ) %>%
    basics_update_model_specifics()

  # This will be changed to evaluation repetition later on
  test_data[["fold_column"]] <- reps + 1 # TODO is this necessary?

  evaluations_all_0 <- internal_evaluate(
    test_data,
    type = "binomial",
    predictions_col = local_tmp_predicted_probability_all_0_var,
    targets_col = local_tmp_target_var,
    fold_info_cols = list(
      rel_fold = "rel_fold",
      abs_fold = "abs_fold",
      fold_column = "fold_column"
    ),
    # models = NULL,
    model_specifics = model_specifics) %>%
    dplyr::mutate(Family = "multinomial",
                  Dependent = targets_col) %>%
    select_metrics(include_definitions = FALSE) %>%
    dplyr::mutate( Measure = "All_0")

  # Evaluate all 1s

  # This will be changed to evaluation repetition later on
  test_data[["fold_column"]] <- reps + 2

  evaluations_all_1 <- internal_evaluate(
    test_data,
    type = "binomial",
    predictions_col = local_tmp_predicted_probability_all_1_var,
    targets_col = local_tmp_target_var,
    fold_info_cols = list(
      rel_fold = "rel_fold",
      abs_fold = "abs_fold",
      fold_column = "fold_column"
    ),
    # models=NULL,
    model_specifics = model_specifics) %>%
    dplyr::mutate(Family = "multinomial",
                  Dependent = targets_col) %>%
    select_metrics(include_definitions = FALSE) %>%
    dplyr::mutate( Measure = "All_1")

  # Collect and return the all or nothing results
  dplyr::bind_rows(evaluations_all_0,
                     evaluations_all_1)

}


nest_row <- function(...){
  col_names <- names(c(...))
  x <- unname(c(...))
  x <- dplyr::as_tibble(t(matrix(x)),
                        .name_repair = ~ col_names)
  # colnames(x) <- col_names
  legacy_nest(x) %>%
    dplyr::rename(probabilities = data)
}

nest_probabilities_rowwise <- function(data){
  purrr::pmap_dfr(data, .f = nest_row) %>%
    dplyr::pull(.data$probabilities)
}
