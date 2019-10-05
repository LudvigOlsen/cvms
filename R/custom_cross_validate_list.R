custom_cross_validate_list <- function(data,
                                       formulas,
                                       model_fn,
                                       predict_fn,
                                       preprocess_fn = NULL,
                                       preprocess_once = FALSE,
                                       hyperparameters = NULL,
                                       fold_cols = '.folds',
                                       family = 'gaussian',
                                       cutoff = 0.5,
                                       positive = 2,
                                       metrics = list(),
                                       rm_nc = FALSE,
                                       model_verbose = FALSE,
                                       parallel_ = FALSE,
                                       parallelize = "models") {

  # Set errors if input variables aren't what we expect / can handle
  # WORK ON THIS SECTION!
  stopifnot(is.data.frame(data),
            is.character(positive) || positive %in% c(1,2)
  )

  data <- dplyr::as_tibble(data) %>%
    dplyr::ungroup()

  # Check metrics
  check_metrics_list(metrics)

  # Fill metrics with default values for non-specified metrics
  # and get the names of the metrics to use
  metrics <- set_metrics(family = family, metrics_list = metrics,
                         include_model_object_metrics = TRUE)

  # Check that the fold column(s) is/are factor(s)
  check_fold_col_factor(data = data, fold_cols = fold_cols)

  # Get evaluation functions
  if (family == "gaussian"){
    evaluation_type <- "gaussian"
  } else if (family == "binomial"){
    evaluation_type <- "binomial"
  } else if (family == "multinomial"){
    evaluation_type <- "multinomial"
  } else {stop("Only 'gaussian', 'binomial', and 'multinomial' evaluation types are currently allowed.")}

  # Create model_specifics object
  # Update to get default values when an argument was not specified
  model_specifics <- list(
    model_formula = "",
    family = family,
    REML = NULL,
    link = NULL,
    control = NULL,
    cutoff = cutoff,
    positive = positive,
    model_verbose = model_verbose,
    model_fn = model_fn,
    predict_fn = predict_fn,
    preprocess_fn = preprocess_fn,
    preprocess_once = preprocess_once,
    hparams = NULL,
    caller = "cross_validate_fn()"
  ) %>%
    custom_update_model_specifics()

  ## Create computational grid

  computation_grid <- create_computation_grid(data = data,
                                              hparams = hyperparameters,
                                              formulas = formulas,
                                              fold_cols = fold_cols)

  n_models <- length(unique(computation_grid[["model"]]))
  n_model_instances <- nrow(computation_grid)

  # TODO Make better message here. It seems like a good idea
  # to inform the user about the number of models to fit etc.
  # so they know how long it may take
  # Perhaps add a progress bar?
  message(paste0("Will cross-validate ", n_models,
                 " models. This requires fitting ",
                 n_model_instances, " model instances."))

  if(isTRUE(preprocess_once)){

    data <- run_preprocess_once(data = data,
                                computation_grid = computation_grid,
                                model_specifics = model_specifics,
                                fold_cols = fold_cols)
  }

  # cross_validate all the models using ldply()
  validated_folds <- plyr::llply(seq_len(nrow(computation_grid)),
                                 .parallel = all(parallel_, parallelize == "models"), # TODO change to folds
                                 .fun = function(r){

    # Extract current row from computation grid
    to_compute <- computation_grid %>% dplyr::filter(dplyr::row_number() == r)

    model_specifics[["model_formula"]] <- to_compute[["Formula"]]
    model_specifics[["hparams"]] <- to_compute[["hparams"]]

    fold_info <- list(
      "rel_fold" = to_compute[["rel_fold"]],
      "abs_fold" = to_compute[["abs_fold"]],
      "fold_column" = as.character(to_compute[["fold_col_name"]])
    )

    custom_validate_fold(data = data,
                         fold_info = fold_info,
                         evaluation_type = evaluation_type,
                         model_specifics = model_specifics,
                         model_specifics_update_fn = NULL,
                         metrics = metrics,
                         fold_cols = fold_cols)
  })

  # Extract predictions and targets
  predictions_and_targets <- validated_folds %c% "predictions_and_targets"

  # Extract model object metrics
  model_metrics <- validated_folds %c% "model_metrics"

  # Extract whether the models were NULL or not
  model_was_null <- unlist(validated_folds %c% "model_was_null")

  # Add to computation grid
  computation_grid <- computation_grid %>%
    dplyr::mutate(model_eval = model_metrics,
                  Predictions = predictions_and_targets,
                  model_was_null = model_was_null) %>%
    dplyr::arrange(.data$model, .data$abs_fold) # TODO: delete once we know it is working?

  fold_info_cols <- list(rel_fold = "rel_fold",
                        abs_fold = "abs_fold",
                        fold_column = "fold_column")

  # Evaluate predictions
  cross_validations <- plyr::llply(seq_len(n_models), function(m){

    # Extract grid for current model
    current_grid <- computation_grid %>%
      dplyr::filter(.data$model == m)

    # Extract current predictions
    current_predictions <- dplyr::bind_rows(current_grid[["Predictions"]])

    # Extract current model object evaluations
    current_model_evals <- dplyr::bind_rows(current_grid[["model_eval"]])

    # Extract current model metrics + some fold cols
    current_model_metrics <- current_model_evals %>%
      dplyr::select(dplyr::one_of(c(
        fold_info_cols[["fold_column"]],
        fold_info_cols[["rel_fold"]],
        intersect(metrics, colnames(current_model_evals))
      )))

    # Average the model metrics
    # First by fold column
    # Then again
    average_model_metrics <- current_model_metrics %>%
      dplyr::select(-dplyr::one_of(fold_info_cols[["rel_fold"]])) %>%
      dplyr::group_by(!!as.name(fold_info_cols[["fold_column"]])) %>%
      dplyr::summarise_all(.funs = ~mean(.)) %>%
      dplyr::select(-dplyr::one_of(fold_info_cols[["fold_column"]])) %>%
      dplyr::summarise_all(.funs = ~mean(.))

    # Prepare and nest the warnings and messages
    current_warnings_and_messages <- current_model_evals %>%
      dplyr::select(dplyr::one_of("Warnings and Messages")) %>%
      legacy_unnest()
    nested_current_warnings_and_messages <- current_warnings_and_messages %>%
      legacy_nest(seq_len(ncol(current_warnings_and_messages))) %>%
      dplyr::pull(.data$data)

    # Sum the warning and message counts
    current_warnings_and_messages_counts <- current_model_evals %>%
      dplyr::select(
        dplyr::one_of(
          c("Convergence Warnings",
            "Singular Fit Messages",
            "Other Warnings")
          )) %>%
      dplyr::summarise_all(.funs = ~sum(.))

    # Prepare and nest the coefficients
    current_coefficients <- current_model_evals[["Coefficients"]] %>%
      dplyr::bind_rows()
    nested_current_coefficients <- current_coefficients %>%
      legacy_nest(seq_len(ncol(current_coefficients))) %>%
      dplyr::pull(.data$data)

    # Evaluate the predictions
    prediction_evaluation <- internal_evaluate_predictions(
      data = current_predictions,
      predictions_col = "prediction",
      targets_col = "target",
      model_was_null_col = "model_was_null",
      type = family,
      fold_info_cols = fold_info_cols,
      model_specifics = model_specifics,
      metrics = metrics,
      include_fold_columns = TRUE, # TODO Perhaps should be arg in main fn?
      include_predictions = TRUE # TODO Perhaps should be arg in main fn?
    )

    # Extract the prediction fold results tibble
    fold_results <- prediction_evaluation[["Results"]][[1]]
    prediction_evaluation[["Results"]] <- NULL
    # Add the model metric object results
    fold_results <- fold_results %>%
      dplyr::full_join(current_model_metrics,
                       by = c(`Fold Column` = fold_info_cols[["fold_column"]],
                              Fold = fold_info_cols[["rel_fold"]]))
    # Nest fold results
    nested_fold_results <- fold_results %>%
      legacy_nest(seq_len(ncol(fold_results))) %>%
      dplyr::pull(.data$data)

    # Combine the various columns
    prediction_evaluation %>%
      dplyr::bind_cols(average_model_metrics) %>%
      tibble::add_column(Results = nested_fold_results,
                         Coefficients = nested_current_coefficients) %>%
      reposition_column("Predictions", .before = "Results") %>%
      dplyr::bind_cols(current_warnings_and_messages_counts) %>%
      dplyr::mutate(`Warnings and Messages` = nested_current_warnings_and_messages)

  })

  if (family %in% c("binomial", "gaussian")){

    cross_validations_results <- cross_validations %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(Family = model_specifics[["family"]]) %>%
      dplyr::select(-.data$`Singular Fit Messages`)

  } else if (family == "multinomial"){

    # Extract and nest class level results
    cross_validations_class_level_results <-
      plyr::ldply(cross_validations %c% "Class Level Results", function(clr) {
        legacy_nest(clr, seq_len(ncol(clr)))
        }) %>%
      tibble::as_tibble() %>%
      dplyr::pull(.data$data)

    # Extact results and add family and class level results
    cross_validations_results <- dplyr::bind_rows(cross_validations %c% "Results") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(Family = model_specifics[["family"]]) %>%
      dplyr::select(-.data$`Singular Fit Messages`) %>%
      tibble::add_column(`Class Level Results` = cross_validations_class_level_results,
                         .before = "Predictions")
  }

  # Extract the first row for each model in the computation grid
  grid_first_rows <- computation_grid %>%
    dplyr::group_by(.data$model) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::arrange(.data$model)

  # Extract hparams from grid
  hparams <- grid_first_rows %>%
    dplyr::pull(.data$hparams)

  # Extract formulas from grid
  model_formulas <- grid_first_rows %>%
    dplyr::pull(.data$Formula)

  # Now we want to take the formula from the formulas and split it up into
  # fixed effects and random effects
  # Some users might want to mix models with an without random effects,
  # and so we first try to seperate into fixed and random,
  # and if no random effects are found for any of the models,
  # we remove the column "random".
  # Models without random effects will get NA in the random column.

  # Could this be sped up with dynamic programming? (simple lookup thing?)
  # Instead of doing the same thing for a lot of formulas?
  # Probably the lookup will make it similar in performance?
  mixed_effects <- extract_model_effects(model_formulas)

  # We put the two data frames together
  output <- dplyr::bind_cols(cross_validations_results,
                             mixed_effects)

  if (!is.null(hyperparameters)){
    output <- output %>%
      tibble::add_column("HParams" = hparams,
                         .before = "Dependent")
  }


  # If asked to remove non-converged models from output
  if (isTRUE(rm_nc)){

    output <- output %>%
      dplyr::filter(.data$`Convergence Warnings` == 0)

  }

  # and return it
  return(output)

}
