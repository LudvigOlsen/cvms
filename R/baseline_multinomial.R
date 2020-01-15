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
  metric_names <- set_metrics("multinomial", metrics_list = metrics)

  # If metric == "all", we'll convert it to the named list of logicals
  if (!is.list(metrics) &&
      is.character(metrics) &&
      metrics == "all"){
    metrics <- as.list(rep(TRUE, length(metric_names)))
    names(metrics) <- metric_names
  }

  # Extract the dependent column from the test set
  targets <- as.character(test_data[[dependent_col]])
  num_targets <- length(targets)
  # Extract the unique class labels
  classes <- unique(targets)
  # Count number of classes
  num_classes <- length(classes)

  # Remove columns with the name of a class from the dataset
  if (dependent_col %in% classes){
    stop("'dependent_col' cannot be the name of one of the classes.")
  }
  # If we only include the dependent column
  # we won't have a problem with columns named the same as the classes
  test_data <- test_data[, dependent_col]

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

  # Create temporary fold columns
  tmp_fold_cols_obj <- create_tmp_fold_cols(test_data)
  test_data <- tmp_fold_cols_obj[["data"]]
  fold_info_cols <- tmp_fold_cols_obj[["fold_info_cols"]]
  fold_and_fold_col <- create_fold_and_fold_column_map(
    data = test_data, fold_info_cols = fold_info_cols)

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
      fold_info_cols = fold_info_cols,
      fold_and_fold_col = fold_and_fold_col,
      apply_softmax = FALSE,
      metrics = metrics,
      include_predictions = TRUE,
      include_fold_columns = FALSE, # We're not providing any fold info so won't make sense
      na.rm = FALSE,
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
  evaluations_all_or_nothing <- plyr::ldply(seq_len(num_classes),
                                            .parallel = parallel_,
                                            function(cl_ind){

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
      fold_info_cols = fold_info_cols,
      fold_and_fold_col = fold_and_fold_col,
      apply_softmax = FALSE,
      metrics = metrics,
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

  # Pull Class Level Results
  evaluations_random_class_level_results <- evaluations_random[["Class Level Results"]]

  evaluations_all_or_nothing_results <- base_deselect(evaluations_all_or_nothing,
                                                      cols = "Class Level Results")

  evaluations_all_or_nothing_class_level_results <-
    evaluations_all_or_nothing[["Class Level Results"]]

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
  metric_cols_results <- select_metrics(evaluations_random_results,
                                        include_definitions = FALSE)
  metric_cols_class_level_results <- select_metrics(evaluations_random_class_level_results,
                                                    include_definitions = FALSE,
                                                    additional_includes = "Class")

  # Prepare metrics to use in all or nothing evaluations
  all_or_nothing_metrics <- set_all_or_nothing_metrics(metrics = metrics)

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
        metrics = all_or_nothing_metrics)
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
    colnames(metric_cols_results),
    colnames(summarized_metrics_class_level)
  )

  # Summarize the metrics
  summarized_repetitions <- metric_cols_results %>%
    summarize_metrics(na.rm = TRUE, inf.rm = TRUE)

  # Extract the overall max,min,NAs count, and INFs count
  # from the summarized class level results
  class_level_max <- summarize_measure(data = summarized_metrics_class_level,
                                       measure_name = "Max",
                                       FUN = max, na.rm = FALSE) %>%
    dplyr::mutate(Measure = "CL_Max")
  class_level_min <- summarize_measure(data = summarized_metrics_class_level,
                                       measure_name = "Min",
                                       FUN = min, na.rm = FALSE) %>%
    dplyr::mutate(Measure = "CL_Min")
  class_level_NAs <- summarize_measure(data = summarized_metrics_class_level,
                                       measure_name = "NAs",
                                       FUN = sum, na.rm = FALSE) %>%
    dplyr::mutate(Measure = "CL_NAs")
  class_level_INFs <- summarize_measure(data = summarized_metrics_class_level,
                                        measure_name = "INFs",
                                        FUN = sum, na.rm = FALSE) %>%
    dplyr::mutate(Measure = "CL_INFs")

  summarized_avg_metrics <- summarized_repetitions %>%
    dplyr::bind_rows(class_level_max, class_level_min,
                     class_level_NAs, class_level_INFs) %>%
    dplyr::bind_rows(evaluations_all_or_nothing_results %>%
                       dplyr::mutate(Measure = paste0("All_", .data$All_class)) %>%
                       select_metrics(include_definitions = FALSE,
                                      additional_includes = "Measure")) %>%
    base_select(c("Measure", metric_names))  %>%
    dplyr::as_tibble()

  # Nest the repetition class level results
  # And add to the random evaluations tibble

  evaluations_random_results <-
    evaluations_random_results %>%
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
  evaluations_random_results[["Confusion Matrix"]] <-
    add_repetition_col_to_nested_tibbles(evaluations_random_results[["Confusion Matrix"]])

  # Add Repetition column to predictions column
  evaluations_random_results[["Predictions"]] <-
    add_repetition_col_to_nested_tibbles(evaluations_random_results[["Predictions"]])

  # Group the summarized class level results for nesting
  summarized_metrics_class_level <- summarized_metrics_class_level %>%
    dplyr::ungroup() %>% # Just to make sure
    dplyr::group_by(.data$Class)

  # Nest the summarized class level results
  nested_class_level <- tibble::tibble("Class" = unlist(dplyr::group_keys(summarized_metrics_class_level))) %>%
    dplyr::left_join(
      summarized_metrics_class_level %>%
        dplyr::group_nest(.key = "Results", keep = FALSE),
      by = "Class"
    )

  # TODO Rename to something meaningful.
  # Note, overall accuracy is not an average, so average metrics are not that meaningful!
  list(
    "summarized_metrics" = summarized_avg_metrics,
    "summarized_class_level_results" = nested_class_level,
    "random_evaluations" = evaluations_random_results
  )

}


all_or_nothing_evaluations <- function(test_data, targets_col, current_class, reps, metrics = list()){

  num_targets <- nrow(test_data)

  local_tmp_target_var <- create_tmp_name(test_data, "all_or_nothing_targets")
  local_tmp_predicted_probability_all_1_var <- create_tmp_name(test_data, "all_1_predicted_probability")
  local_tmp_predicted_probability_all_0_var <- create_tmp_name(test_data, "all_0_predicted_probability")

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
  tmp_index <- create_tmp_name(data)
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

set_all_or_nothing_metrics <- function(metrics){
  # Don't calculate AUC in the all_or_nothing_evaluations
  # as it will be done along with Overall Acc.
  all_or_nothing_metrics <- metrics
  all_or_nothing_metrics[["AUC"]] <- FALSE

  # Add the non-weighted version when f.i. "Accuracy" = FALSE but "Weighted Accuracy" = TRUE
  weighted_metrics <- names(all_or_nothing_metrics)[
    grepl("Weighted ", names(all_or_nothing_metrics))]
  metric_names_to_add_as_true <- gsub("Weighted ", "", weighted_metrics)
  metrics_to_add_as_true <- rep(TRUE, length(metric_names_to_add_as_true))
  names(metrics_to_add_as_true) <- metric_names_to_add_as_true
  # Remove from all_or_nothing_metrics
  all_or_nothing_metrics <- all_or_nothing_metrics[
    names(all_or_nothing_metrics) %ni% metric_names_to_add_as_true
  ]
  # Add as TRUE (c() also concatenates lists)
  all_or_nothing_metrics <- c(all_or_nothing_metrics, metrics_to_add_as_true)

  # Also remove multinomial metrics like weighted *
  allowed_binomial_metrics <- set_metrics(
    family = "binomial", metrics_list = "all")
  all_or_nothing_metric_names <- intersect(
    names(all_or_nothing_metrics), allowed_binomial_metrics)
  all_or_nothing_metrics <- all_or_nothing_metrics[
    names(all_or_nothing_metrics) %in% all_or_nothing_metric_names]

  # Sanity check - TODO Add tests to make sure it's never used!
  if (!is.logical(all_or_nothing_metrics[["AUC"]]))
    stop("all_or_nothing_metrics had non-logical element.")

  all_or_nothing_metrics
}
