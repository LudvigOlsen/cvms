create_multinomial_baseline_evaluations <- function(test_data,
                                                    dependent_col,
                                                    reps = 100,
                                                    na.rm = TRUE,
                                                    parallel_ = FALSE) {

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

  # Create predicted probability tibbles

  random_probabilities <- create_multinomial_probability_tibble(num_classes = num_classes,
                                                                num_observations = num_targets * reps,
                                                                apply_softmax = TRUE) %>%
    dplyr::rename_all(~classes) %>%
    nest_probabilities_rowwise() %>%
    split(f = factor(rep(1:reps, each = num_targets)))

  # Create data frame with either 1 or 0 probability for a class for all targets
  # This is done for all classes
  all_or_nothing_probabilities <- plyr::ldply(1:num_classes, function(cl){
    zero_probs <- rep(0.000000001 / num_classes, num_classes)
    zero_probs_df <- do.call(data.frame, as.list(zero_probs))
    colnames(zero_probs_df) <- classes
    zero_probs_df %>%
      dplyr::slice(rep(1:dplyr::n(), each = num_targets)) %>%
      dplyr::mutate_at(dplyr::vars(classes[[cl]]), ~(1-0.000000001))
  }) %>%
    nest_probabilities_rowwise() %>%
    split(f = factor(rep(1:num_classes, each = num_targets)))

  # Evaluate the probability tibbles

  # Evaluate random predictions
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

    results


  })

  # Evaluate all or nothing predictions
  evaluations_all_or_nothing <- plyr::llply(1:num_classes, .parallel = parallel_, function(cl_ind){

    test_data[["prediction"]] <- all_or_nothing_probabilities[[cl_ind]]

    # This will be changed to class later on
    test_data[["fold_column"]] <- cl_ind

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

    results

  })

  # Extract evaluations

  evaluations_random_results <- evaluations_random %c% "Results"
  evaluations_random_class_level_results <- evaluations_random %c% "Class_level_results"
  evaluations_all_or_nothing_results <- evaluations_all_or_nothing %c% "Results"
  evaluations_all_or_nothing_class_level_results <- evaluations_all_or_nothing %c% "Class_level_results"

  evaluations_random_results <- evaluations_random_results %>%
    dplyr::bind_rows(.id = "Repetition") %>%
    dplyr::mutate(
      Family = "multinomial",
      Dependent = dependent_col
    )

  evaluations_random_class_level_results <- evaluations_random_class_level_results %>%
    dplyr::bind_rows(.id = "Repetition") %>%
    dplyr::mutate(
      Family = "binomial", # Note, the one-vs-all evals are binomial
      Dependent = dependent_col
    )

  evaluations_all_or_nothing_results <- setNames(evaluations_all_or_nothing_results, classes) %>%
    dplyr::bind_rows(.id = "All_class") %>%
    dplyr::mutate(
      Family = "multinomial",
      Dependent = dependent_col
    )

  # Gather the evaluations in the correct form

  # Extract the metrics
  metric_cols_results <- select_metrics(evaluations_random_results, include_definitions = FALSE)
  metric_cols_class_level_results <- select_metrics(evaluations_random_class_level_results,
                                                    include_definitions = FALSE,
                                                    additional_includes = "Class")

  # Summarize the metrics
  # and add the all or nothing evaluations
  summarized_avg_metrics <- summarize_metric_cols(metric_cols_results, na.rm = na.rm) %>%
    dplyr::bind_rows(evaluations_all_or_nothing_results %>%
                       dplyr::mutate(Measure = paste0("All_", .data$All_class)) %>%
                       dplyr::select(-dplyr::one_of("All_class")))

  # Find the class level summaries
  summarized_metrics_class_level <- plyr::ldply(classes, function(cl){
    # Extract the class level (non averaged) results and select the metric columns
    metric_cols_current_class <- metric_cols_class_level_results %>%
      dplyr::filter(.data$Class == cl) %>%
      dplyr::select(-.data$Class)

    summarized_class_level_result <- summarize_metric_cols(
      metric_cols_current_class,
      na.rm = na.rm) %>%
      dplyr::bind_rows(all_or_nothing_evaluations(
        test_data = test_data,
        targets_col = dependent_col,
        current_class = cl,
        reps = reps)
      ) %>%
      dplyr::mutate(Class = cl)

  }) %>% dplyr::select(.data$Class, dplyr::everything())

  # TODO Rename to something meaningful.
  # Note, overall accuracy is not an average, so average metrics are not that meaningful!
  list(
    "Summarized" = list(
      "Results" = summarized_avg_metrics,
      "Class Level Results" = summarized_metrics_class_level
      ),
    "Random Evaluations" = list(
      "Results" = evaluations_random_results,
      "Class Level Results" = evaluations_random_class_level_results
      )
  )

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

# Nests all columns rowwise
# Note: I tried with pmap_dfr and a nest_row function
# but it was a lot slower
nest_probabilities_rowwise <- function(data){
  n_cols <- ncol(data)
  tmp_index <- create_tmp_var(data)
  data[[tmp_index]] <- 1:nrow(data)
  data %>%
    legacy_nest(1:n_cols) %>%
    dplyr::pull(.data$data)
}
