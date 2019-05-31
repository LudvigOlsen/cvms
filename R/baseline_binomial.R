create_binomial_baseline_evaluations <- function(test_data,
                                                 dependent_col,
                                                 reps=100,
                                                 positive = 2,
                                                 cutoff = 0.5,
                                                 parallel_ = FALSE){

  # TODO ADD stopifnot for positive and cutoff

  # Get targets
  targets <- test_data[[dependent_col]]
  n_targets <- length(targets)

  # Check dependent variable
  if (nlevels(targets) > 2){
    if (is.numeric(targets) || is.integer(targets)){
      stop("The dependent column must maximally contain 2 levels. Did you specify the correct family?")
    }
    else {
      stop("The dependent column must maximally contain 2 levels.")
    }
  }

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

  evaluations_random <- plyr::llply(1:reps, .parallel = parallel_, function(evaluation){

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
  }) %>% dplyr::bind_rows() %>% # Works with nested tibbles (ldply doesn't seem to)
    dplyr::mutate(
      Family = "binomial",
      Dependent = dependent_col
    )

  # TODO Rename Fold Column to Repetition or similar in evaluations$Predictions

  metrics_cols <- select_metrics(evaluations_random, include_definitions = FALSE)

  # Get rows with INFs
  metrics_cols_with_infs <- metrics_cols[is.infinite(rowSums(metrics_cols)),]

  # Replace infs with NA
  if(nrow(metrics_cols_with_infs) > 0){
    metrics_cols <- do.call(data.frame,c(lapply(metrics_cols, function(x) replace(x, is.infinite(x), NA)),
                                         check.names=FALSE,fix.empty.names = FALSE, stringsAsFactors=FALSE))
  }

  # This may be better solveable with pivot_* from tidyr, when it is on CRAN
  # This isn't exactly pretty.
  summarized_metrics <- dplyr::bind_rows(
    metrics_cols %>% dplyr::summarize_all(.funs = list(~mean(., na.rm = TRUE))) %>% dplyr::mutate(f = "Mean"),
    metrics_cols %>% dplyr::summarize_all(.funs = list(~median(., na.rm = TRUE))) %>% dplyr::mutate(f = "Median"),
    metrics_cols %>% dplyr::summarize_all(.funs = list(~sd(., na.rm = TRUE))) %>% dplyr::mutate(f = "SD"),
    metrics_cols %>% dplyr::summarize_all(.funs = list(~IQR(., na.rm = TRUE))) %>% dplyr::mutate(f = "IQR"),
    metrics_cols %>% dplyr::summarize_all(.funs = list(~max(., na.rm = TRUE))) %>% dplyr::mutate(f = "Max"),
    metrics_cols %>% dplyr::summarize_all(.funs = list(~min(., na.rm = TRUE))) %>% dplyr::mutate(f = "Min"),
    metrics_cols %>% dplyr::summarize_all(.funs = list(~sum(is.na(.)))) %>% dplyr::mutate(f = "NAs"),
    metrics_cols_with_infs %>% dplyr::summarize_all(.funs = list(~sum(is.infinite(.)))) %>% dplyr::mutate(f = "INFs")
  ) %>%
    dplyr::select(.data$f, dplyr::everything()) %>%
    dplyr::rename(Measure = .data$f)

  # Remove the INFs from the NAs count
  if(nrow(metrics_cols_with_infs) > 0){
    NAs_row_number <- which(summarized_metrics$Measure == "NAs")
    INFs_row_number <- which(summarized_metrics$Measure == "INFs")
    summarized_metrics[NAs_row_number,-1] <- summarized_metrics[NAs_row_number,-1] - summarized_metrics[INFs_row_number,-1]
  }


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
                  Dependent = dependent_col) %>%
    select_metrics(include_definitions = FALSE) %>%
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
                  Dependent = dependent_col) %>%
    select_metrics(include_definitions = FALSE) %>%
    dplyr::mutate( Measure = "All_1")

  # Collect the summarized metrics

  overall_evaluations <- summarized_metrics %>%
    dplyr::bind_rows(evaluations_all_0,
                     evaluations_all_1)

  # Return summarized metrics and the random evaluations in a list

  return(list("summarized_metrics" = overall_evaluations,
              "random_evaluations" = evaluations_random))
}
