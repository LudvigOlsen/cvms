create_multinomial_baseline_evaluations <- function(test_data,
                                                    dependent_col,
                                                    metrics = list(),
                                                    reps = 100,
                                                    na.rm = TRUE,
                                                    random_generator_fn = runif,
                                                    parallel_ = FALSE) {

  # Check na.rm
  if(!is_logical_scalar_not_na(na.rm)){
    stop("'na.rm' must be logical scalar (TRUE/FALSE).")
  }

  # get metric names
  metrics <- set_metrics("multinomial", metrics_list = metrics)

  # Extract the dependent column from the test set
  targets <- as.character(test_data[[dependent_col]])
  num_targets <- length(targets)
  # Extract the unique class labels
  classes <- unique(targets)
  # Count number of classes
  num_classes <- length(classes)

  # TODO Test num_classes etc.

  # Create predicted probability tibbles
  random_probabilities <- multiclass_probability_tibble(
    num_classes = num_classes,
    num_observations = num_targets * reps,
    FUN = random_generator_fn,
    apply_softmax = TRUE) %>%
    dplyr::rename_all(~classes) %>%
    split(f = factor(rep(1:reps, each = num_targets)))

  # We want a probability within 0 and 1, so this is how close we get to those limits
  almost_zero <- 0.000000001

  # Create data frame with either 1 or 0 probability for a class for all targets
  # This is done for all classes
  all_or_nothing_probabilities <-
    plyr::ldply(1:num_classes, function(cl) {
      zero_probs <- rep(almost_zero / num_classes, num_classes)
      zero_probs_df <- do.call(data.frame, as.list(zero_probs))
      colnames(zero_probs_df) <- classes
      zero_probs_df %>%
        dplyr::slice(rep(1:dplyr::n(), each = num_targets)) %>%
        dplyr::mutate_at(dplyr::vars(classes[[cl]]), ~ (1 - almost_zero))
    }) %>%
    split(f = factor(rep(1:num_classes, each = num_targets)))

  # Evaluate the probability tibbles

  # Evaluate random predictions
  evaluations_random <- plyr::ldply(seq_len(reps), .parallel = parallel_,
                                    function(evaluation){
    probabilities <- random_probabilities[[evaluation]]
    test_data <- dplyr::bind_cols(test_data,
                                  probabilities)

    evals <- run_evaluate(
      data = test_data,
      target_col = dependent_col,
      prediction_cols = colnames(probabilities),
      type = "multinomial",
      id_col = NULL,
      id_method = "mean",
      apply_softmax = FALSE,
      metrics = list(),
      include_predictions = TRUE,
      include_fold_columns = FALSE, # We're not providing any fold info so won't make sense
      na.rm = "both",
      caller = "baseline()"
    )

    evals %>%
      tibble::add_column("Repetition" = evaluation,
                         .before = names(evals)[[1]])

  }) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(
      Family = "multinomial",
      Dependent = dependent_col
    )

  # Evaluate all or nothing predictions
  evaluations_all_or_nothing <- plyr::ldply(seq_len(num_classes), .parallel = parallel_, function(cl_ind){

    probabilities <- all_or_nothing_probabilities[[cl_ind]]
    test_data <- dplyr::bind_cols(test_data,
                                  probabilities)

    evals <- run_evaluate(
      data = test_data,
      target_col = dependent_col,
      prediction_cols = colnames(probabilities),
      type = "multinomial",
      id_col = NULL,
      id_method = "mean",
      apply_softmax = FALSE,
      metrics = list(),
      include_predictions = TRUE,
      include_fold_columns = FALSE, # We're not providing any fold info so won't make sense
      caller = "baseline()"
    )

    evals %>%
      tibble::add_column("Class" = classes[[cl_ind]],
                         .before = names(evals)[[1]])

  }) %>%
    dplyr::as_tibble()

  # Extract evaluations

  evaluations_random_results <- base_deselect(
    evaluations_random, cols = "Class Level Results")

  # Keep NAs_removed == FALSE and pull Class Level Results
  evaluations_random_class_level_results <- evaluations_random[
    !evaluations_random[["NAs_removed"]],
    ][["Class Level Results"]]

  evaluations_all_or_nothing_results <- base_deselect(evaluations_all_or_nothing,
                                                      cols = "Class Level Results")

  evaluations_all_or_nothing_class_level_results <-
    evaluations_all_or_nothing[["Class Level Results"]]

  # Subset the version with and without na.rm = TRUE

  evaluations_random_results_NAs_removed <- evaluations_random_results[
    # Keep NAs_removed == TRUE
    evaluations_random_results[["NAs_removed"]] ,
    # Remove the NAs_removed column
    setdiff(colnames(evaluations_random_results), "NAs_removed")
  ]

  evaluations_random_results_NAs_kept <- evaluations_random_results[
    # Keep NAs_removed == FALSE
    !evaluations_random_results[["NAs_removed"]] ,
    # Remove the NAs_removed column
    setdiff(colnames(evaluations_random_results), "NAs_removed")
  ]

  evaluations_random_class_level_results <- evaluations_random_class_level_results %>%
    dplyr::bind_rows(.id = "Repetition") %>%
    dplyr::mutate(
      Family = "binomial", # Note, the one-vs-all evals are binomial
      Dependent = dependent_col,
      Repetition = as.numeric(.data$Repetition)
    )

  evaluations_all_or_nothing_results <- evaluations_all_or_nothing_results %>%
    base_rename(before = "Class", after = "All_class") %>%
    dplyr::mutate(
      Family = "multinomial",
      Dependent = dependent_col
    ) %>%
    dplyr::arrange(.data$All_class)

  # Gather the evaluations in the correct form

  # Extract the metrics
  metric_cols_results_NAs_removed <- select_metrics(evaluations_random_results_NAs_removed,
                                                    include_definitions = FALSE)
  metric_cols_results_NAs_kept <- select_metrics(evaluations_random_results_NAs_kept,
                                                 include_definitions = FALSE)
  metric_cols_class_level_results <- select_metrics(evaluations_random_class_level_results,
                                                    include_definitions = FALSE,
                                                    additional_includes = "Class")

  # Find the class level summaries
  summarized_metrics_class_level <- plyr::ldply(classes, function(cl){
    # Extract the class level (non averaged) results and select the metric columns
    metric_cols_current_class <- metric_cols_class_level_results[
      metric_cols_class_level_results[["Class"]] == cl,]
    metric_cols_current_class[["Class"]] <- NULL

    summarized_class_level_result <- summarize_metrics(
      metric_cols_current_class,
      na.rm = TRUE,
      inf.rm = TRUE) %>%
      dplyr::bind_rows(all_or_nothing_evaluations(
        test_data = test_data,
        targets_col = dependent_col,
        current_class = cl,
        reps = reps,
        metrics = list("AUC" = FALSE)) # AUC will be done along with Overall Acc.
      ) %>%
      dplyr::mutate(Class = cl)

  }) %>%
    dplyr::as_tibble()

  # Move Class to be first column
  summarized_metrics_class_level <- summarized_metrics_class_level[
    , c("Class", setdiff(names(summarized_metrics_class_level), "Class"))
  ]

  # Extract the metrics that we need to get entirely from the repetition results
  # such as Overall Accuracy

  # Extract the metrics that we need to get from the repetition results
  repetition_result_metrics <- setdiff(
    colnames(metric_cols_results_NAs_removed),
    colnames(summarized_metrics_class_level)
  )

  # Summarize the metrics
  summarized_repetitions <- metric_cols_results_NAs_removed %>%
    summarize_metrics(na.rm = FALSE, inf.rm = TRUE)

  # Extract the ones where we want min and max to come from
  # the repetition results
  summarized_repetition_result_metrics <- base_select(
    summarized_repetitions,
    cols = c("Measure", repetition_result_metrics)) %>%

    # As these would have NA and INF counts from the
    # random evaluation results (computed with na.rm = TRUE)
    # it would be confusing to have two types of counts,
    # and we instead set them to NA
    dplyr::mutate_at(dplyr::vars(-.data$Measure),
                     ~ifelse(.data$Measure == "NAs", NA, .)) %>%
    dplyr::mutate_at(dplyr::vars(-.data$Measure),
                     ~ifelse(.data$Measure == "INFs", NA, .))

  # Remove
  summarized_repetitions <- summarized_repetitions %>%
    base_deselect(repetition_result_metrics)
  summarized_repetitions <- summarized_repetitions[
    summarized_repetitions[["Measure"]] %ni% c("Min","Max","NAs","INFs"),
  ]

  # Extract the overall max,min,NAs count, and INFs count
  # from the summarized class level results
  overall_max <- summarize_measure(data = summarized_metrics_class_level,
                                   measure_name = "Max",
                                   FUN = max, na.rm = FALSE)
  overall_min <- summarize_measure(data = summarized_metrics_class_level,
                                   measure_name = "Min",
                                   FUN = min, na.rm = FALSE)
  overall_NAs <- summarize_measure(data = summarized_metrics_class_level,
                                   measure_name = "NAs",
                                   FUN = sum, na.rm = FALSE)
  overall_INFs <- summarize_measure(data = summarized_metrics_class_level,
                                    measure_name = "INFs",
                                    FUN = sum, na.rm = FALSE)

  summarized_avg_metrics <- summarized_repetitions %>%
    dplyr::bind_rows(overall_max, overall_min,
                     overall_NAs, overall_INFs) %>%
    dplyr::left_join(summarized_repetition_result_metrics, by = "Measure") %>%
    dplyr::bind_rows(evaluations_all_or_nothing_results %>%
                       dplyr::mutate(Measure = paste0("All_", .data$All_class)) %>%
                       select_metrics(include_definitions = FALSE,
                                      additional_includes = "Measure")) %>%
    base_select(c("Measure", metrics))

  # Nest the repetition class level results
  # And add to the random evaluations tibble

  evaluations_random_results_NAs_kept <-
    evaluations_random_results_NAs_kept %>%
    tibble::add_column(
      "Class Level Results" = evaluations_random_class_level_results %>%
        dplyr::ungroup() %>%
        dplyr::group_nest(.data$Repetition,
                          .key = "Class Level Results",
                          keep = TRUE) %>%
        dplyr::pull(.data$`Class Level Results`),
      .before = "Family"
    )

  # Add Repetition column to confusion matrix column
  evaluations_random_results_NAs_kept[["Confusion Matrix"]] <-
    add_repetition_col_to_nested_tibbles(evaluations_random_results_NAs_kept[["Confusion Matrix"]])

  # Add Repetition column to predictions column
  evaluations_random_results_NAs_kept[["Predictions"]] <-
    add_repetition_col_to_nested_tibbles(evaluations_random_results_NAs_kept[["Predictions"]])

  # Group the summarized class level results for nesting
  summarized_metrics_class_level <- summarized_metrics_class_level %>%
    dplyr::ungroup() %>% # Just to make sure
    dplyr::group_by(.data$Class)

  # Nest the summarized class level results
  nested_class_level <- tibble::tibble("Class" = unlist(dplyr::group_keys(summarized_metrics_class_level)))
  nested_class_level[["Results"]] <- summarized_metrics_class_level %>%
    dplyr::group_nest(.key = "Results", keep = FALSE) %>%
    dplyr::pull(.data$Results)

  # TODO Rename to something meaningful.
  # Note, overall accuracy is not an average, so average metrics are not that meaningful!
  list(
    "summarized_metrics" = summarized_avg_metrics,
    "summarized_class_level_results" = nested_class_level,
    "random_evaluations" = evaluations_random_results_NAs_kept
  )

}


all_or_nothing_evaluations <- function(test_data, targets_col, current_class, reps, metrics = list()){

  num_targets <- nrow(test_data)

  local_tmp_target_var <- create_tmp_var(test_data, "all_or_nothing_targets")
  local_tmp_predicted_probability_all_1_var <- create_tmp_var(test_data, "all_1_predicted_probability")
  local_tmp_predicted_probability_all_0_var <- create_tmp_var(test_data, "all_0_predicted_probability")

  test_data[[local_tmp_target_var]] <- factor(ifelse(test_data[[targets_col]] == current_class, 1, 0))
  test_data[[local_tmp_predicted_probability_all_1_var]] <- rep(0.999999, num_targets)
  test_data[[local_tmp_predicted_probability_all_0_var]] <- rep(0.000001, num_targets)
  pred_cols <- c(local_tmp_predicted_probability_all_0_var,
                 local_tmp_predicted_probability_all_1_var)

  evaluations <- plyr::ldply(seq_len(2), function(i){

    run_evaluate(
      data = test_data,
      target_col = local_tmp_target_var,
      prediction_cols = pred_cols[[i]],
      type = "binomial",
      id_col = NULL,
      id_method = "mean",
      apply_softmax = FALSE,
      metrics = metrics,
      include_predictions = FALSE,
      include_fold_columns = FALSE, # We're not providing any fold info so won't make sense
      caller = "baseline()"
    ) %>%
      dplyr::mutate(Measure = paste0("All_", i - 1))

  }) %>%
    dplyr::as_tibble() %>%
    dplyr::mutate(Family = "multinomial") %>%
    select_metrics(include_definitions = FALSE,
                   additional_includes = "Measure")

  evaluations

}

# Nests all columns rowwise
# Note: I tried with pmap_dfr and a nest_row function
# but it was a lot slower
nest_rowwise <- function(data){
  n_cols <- ncol(data)
  tmp_index <- create_tmp_var(data)
  data[[tmp_index]] <- seq_len(nrow(data))
  data %>%
    dplyr::group_by(!!as.name(tmp_index)) %>%
    dplyr::group_nest() %>%
    dplyr::pull(.data$data)
}

summarize_measure <- function(data, measure_name, FUN, na.rm = FALSE){
  data[data[["Measure"]] == measure_name,] %>%
    dplyr::mutate(Family = "binomial") %>%
    select_metrics(include_definitions = FALSE) %>%
    dplyr::summarise_all(.funs = list(~FUN(., na.rm = na.rm))) %>%
    dplyr::mutate(Measure = measure_name)
}

