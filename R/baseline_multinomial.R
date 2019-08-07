baseline_multinomial <- function(test_data,
                                 dependent_col,
                                 reps=100,
                                 parallel_ = FALSE){

  # Extract the dependent column from the test set
  targets <- as.character(test_data[[dependent_col]])
  n_targets <- length(targets)
  # Extract the unique class labels
  classes <- unique(targets)
  # Count number of classes
  num_classes <- length(classes)

  print(classes)

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

  random_probabilities <- matrix(runif(num_classes * n_targets * reps), ncol=num_classes) %>%
    dplyr::as_tibble() %>%
    softmax() %>%
    dplyr::rename_all( ~ classes) %>%
    nest_probabilities_rowwise() %>%
    split(f = factor(rep(1:reps, each=n_targets)))

  evaluations_random <- plyr::llply(1:reps, .parallel = parallel_, function(evaluation){

    test_data[["prediction"]] <- random_probabilities[[evaluation]]

    # This will be changed to evaluation repetition later on
    test_data[["fold_column"]] <- evaluation

    evaluate(test_data,
             type = "multinomial",
             predictions_col = "prediction",
             targets_col = dependent_col,
             fold_info_cols = list(rel_fold = "rel_fold",
                                   abs_fold = "abs_fold",
                                   fold_column = "fold_column"),
             # models=NULL,
             model_specifics = model_specifics) %>%
      dplyr::mutate(Repetition = evaluation)

  }) %>% dplyr::bind_rows() %>% # Works with nested tibbles (ldply doesn't seem to)
    dplyr::mutate(
      Family = "multinomial",
      Dependent = dependent_col
    ) %>%
    dplyr::select(.data$Repetition, dplyr::everything())

  # Extract the metrics
  metric_cols_all <- select_metrics(evaluations_random, include_definitions = FALSE,
                                additional_includes = "Class")

  # Extract the "Avg result" rows and select the metric columns
  metric_cols_avg <- metric_cols_all %>%
    dplyr::filter(.data$Class == "Avg") %>%
    dplyr::select(-.data$Class)

  # Summarize the metrics
  summarized_avg_metrics <- summarize_metric_cols(metric_cols_avg)

  # Find the class level summaries
  summarized_metrics_class_level <- plyr::ldply(classes, function(cl){
    # Extract the class level (non averaged) results and select the metric columns
    metric_cols_current_class <- metric_cols_all %>%
      dplyr::filter(.data$Class != "Avg") %>%
      dplyr::select(-c(.data$Class, .data$`Overall Accuracy`))

    summarize_metric_cols(metric_cols_current_class) %>%
      dplyr::mutate(Class = cl)

  }) %>% dplyr::select(.data$Class, dplyr::everything())

  View(summarized_metrics_class_level)
  # predicted_class <- argmax(random_probabilities)


}


nest_row <- function(...){
  col_names <- names(c(...))
  x <- unname(c(...))
  x <- dplyr::as_tibble(t(matrix(x)))
  colnames(x) <- col_names
  tidyr::nest(x) %>%
    dplyr::rename(probabilities = data)
}

nest_probabilities_rowwise <- function(data){
  purrr::pmap_dfr(data, .f = nest_row) %>%
    dplyr::pull(.data$probabilities)
}
