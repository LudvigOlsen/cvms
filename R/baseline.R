
baseline <- function(test_data,
                     dependent_col,
                     reps=100, # how many times to randomly sample probabilities (bootstrapping?)
                     family = 'binomial',
                     positive = 2,
                     cutoff = 0.5){

  if (family == "binomial"){
    return(
      create_binomial_baseline_evaluations(
        test_data = test_data,
        dependent_col = dependent_col,
        reps=reps,
        positive = positive,
        cutoff = cutoff)
    )

  } else {
    stop("Only the binomial family is currently supported.")
    # lm(dependent ~ 1, ...) ?
  }

}

create_binomial_baseline_evaluations <- function(test_data,
                                        dependent_col,
                                        reps=100,
                                        positive = 2,
                                        cutoff = 0.5){

  # TODO ADD stopifnot for positive and cutoff

  # Get targets
  targets <- test_data[[dependent_col]]
  n_targets <- length(targets)

  # Add fold info columns
  test_data[["rel_fold"]] <- 1
  test_data[["abs_fold"]] <- 1

  # Create model_specifics object
  # Update to get default values when an argument was not specified
  model_specifics <- list(
    model_formula="",
    family="binomial",
    REML=FALSE,
    link=NULL,
    cutoff = cutoff,
    positive = positive,
    model_verbose = FALSE) %>%
    basics_update_model_specifics()

    # Sample random probabilities
    random_probabilities <- split(runif(n_targets*reps), f = factor(rep(1:reps, each=n_targets)))

    # Create all 0 and all 1 probabilities
    all_0_probabilities <- rep(0.000001, n_targets)
    all_1_probabilities <- rep(0.999999, n_targets)

    # Evaluate random probabilities

    evaluations_random <- plyr::llply(1:reps, function(evaluation){

      test_data[["prediction"]] <- random_probabilities[[evaluation]]

      # This will be changed to evaluation repetition later on
      test_data[["fold_column"]] <- evaluation

      evaluate(test_data,
               type="binomial",
               predictions_col = "prediction",
               targets_col = dependent_col,
               fold_info_cols = list(rel_fold="rel_fold",
                                     abs_fold="abs_fold",
                                     fold_column="fold_column"),
               # models=NULL,
               model_specifics=model_specifics)
    }) %>% dplyr::bind_rows() %>%
      dplyr::mutate(
        Family = "binomial",
        Dependent = dependent_col,
        Fixed = "None"
      )

    # TODO Rename Fold Column to Repetition or similar in evaluations$Predictions

    metrics_cols <- select_metrics(evaluations_random, include_model_definitions = FALSE)

    # This may be better solveable with pivot_* from tidyr, when it is on CRAN
    # This isn't exactly pretty.
    summarized_metrics <- dplyr::bind_rows(
      metrics_cols %>% dplyr::summarize_all(.funs = list(~mean(.))) %>% dplyr::mutate(f = "Mean"),
      metrics_cols %>% dplyr::summarize_all(.funs = list(~median(.))) %>% dplyr::mutate(f = "Median"),
      metrics_cols %>% dplyr::summarize_all(.funs = list(~sd(.))) %>% dplyr::mutate(f = "SD"),
      metrics_cols %>% dplyr::summarize_all(.funs = list(~IQR(.))) %>% dplyr::mutate(f = "IQR"),
      metrics_cols %>% dplyr::summarize_all(.funs = list(~max(.))) %>% dplyr::mutate(f = "Max"),
      metrics_cols %>% dplyr::summarize_all(.funs = list(~min(.))) %>% dplyr::mutate(f = "Min")
    ) %>%
      dplyr::select(.data$f, dplyr::everything()) %>%
      dplyr::rename(Measure = .data$f)

    # Evaluate all 0s

    test_data[["prediction"]] <- all_0_probabilities

    # This will be changed to evaluation repetition later on
    test_data[["fold_column"]] <- reps + 1

    evaluations_all_0 <- evaluate(test_data,
             type="binomial",
             predictions_col = "prediction",
             targets_col = dependent_col,
             fold_info_cols = list(rel_fold="rel_fold",
                                   abs_fold="abs_fold",
                                   fold_column="fold_column"),
             # models=NULL,
             model_specifics=model_specifics) %>%
      dplyr::mutate(Family = "binomial",
                    Dependent = dependent_col,
                    Fixed = "None") %>%
      select_metrics(include_model_definitions = FALSE) %>%
      dplyr::mutate( Measure = "All_0")

    # Evaluate all 1s

    test_data[["prediction"]] <- all_1_probabilities

    # This will be changed to evaluation repetition later on
    test_data[["fold_column"]] <- reps + 2

    evaluations_all_1 <- evaluate(test_data,
                                  type="binomial",
                                  predictions_col = "prediction",
                                  targets_col = dependent_col,
                                  fold_info_cols = list(rel_fold="rel_fold",
                                                        abs_fold="abs_fold",
                                                        fold_column="fold_column"),
                                  # models=NULL,
                                  model_specifics=model_specifics) %>%
      dplyr::mutate(Family = "binomial",
                    Dependent = dependent_col,
                    Fixed = "None") %>%
      select_metrics(include_model_definitions = FALSE) %>%
      dplyr::mutate( Measure = "All_1")

    # Collect the summarized metrics

    overall_evaluations <- summarized_metrics %>%
      dplyr::bind_rows(evaluations_all_0,
                       evaluations_all_1)

    # Return summarized metrics and the random evaluations in a list

    return(list("summarized_metrics" = overall_evaluations,
                "random_evaluations" = evaluations_random))
}
