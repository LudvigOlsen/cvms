cross_validate_list <- function(data,
                                formulas,
                                model_fn,
                                predict_fn,
                                preprocess_fn = NULL,
                                preprocess_once = FALSE,
                                hyperparameters = NULL,
                                fold_cols = ".folds",
                                family = "gaussian",
                                cutoff = 0.5,
                                positive = 2,
                                metrics = list(),
                                info_cols = list(), # TODO use in cross_validate to turn on info cols
                                rm_nc = FALSE,
                                verbose = FALSE,
                                parallel_ = FALSE,
                                caller = "cross_validate_fn()") {
  if (checkmate::test_string(x = metrics, pattern = "^all$")) {
    metrics <- list("all" = TRUE)
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(
    x = data,
    min.rows = 2,
    min.cols = 2,
    add = assert_collection
  )
  checkmate::assert_character(
    x = formulas,
    min.len = 1,
    any.missing = FALSE,
    add = assert_collection
  )
  checkmate::assert_character(
    x = fold_cols,
    min.len = 1,
    any.missing = FALSE,
    add = assert_collection
  )
  checkmate::assert_choice(
    x = family,
    choices = c(
      "gaussian",
      "binomial",
      "multinomial"
    ),
    .var.name = "family/type",
    add = assert_collection
  )
  checkmate::assert_number(
    x = cutoff,
    lower = 0,
    upper = 1,
    add = assert_collection
  )

  # Positive
  checkmate::assert(
    checkmate::check_choice(
      x = positive,
      choices = c(1, 2)
    ),
    checkmate::check_string(
      x = positive,
      min.chars = 1
    )
  )

  checkmate::assert_list(
    x = metrics,
    types = "logical",
    any.missing = FALSE,
    names = "named",
    add = assert_collection
  )

  checkmate::assert(
    checkmate::check_data_frame(
      x = hyperparameters,
      col.names = "named",
      min.rows = 1,
      min.cols = 1,
      null.ok = TRUE
    ),
    checkmate::check_list(
      x = hyperparameters,
      null.ok = TRUE,
      any.missing = FALSE,
      min.len = 1,
      names = "named"
    )
  )

  checkmate::assert_list(
    x = info_cols,
    any.missing = FALSE,
    names = "named",
    add = assert_collection
  )
  checkmate::assert_flag(x = verbose, add = assert_collection)
  checkmate::assert_flag(x = rm_nc, add = assert_collection)
  checkmate::assert_flag(x = preprocess_once, add = assert_collection)
  checkmate::assert_flag(
    x = parallel_,
    add = assert_collection,
    .var.name = "parallel"
  )
  checkmate::assert_string(x = caller, add = assert_collection)

  checkmate::assert_function(
    x = model_fn,
    add = assert_collection
  )
  checkmate::assert_function(
    x = predict_fn,
    add = assert_collection
  )
  checkmate::assert_function(
    x = preprocess_fn,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)

  # Argument names

  checkmate::assert_names(
    x = names(formals(model_fn)),
    identical.to = c("train_data", "formula", "hyperparameters"),
    what = "argument names",
    .var.name = "model_fn argument names",
    add = assert_collection)

  checkmate::assert_names(
    x = names(formals(predict_fn)),
    identical.to = c(
      "test_data", "model",
      "formula", "hyperparameters",
      "train_data"
    ),
    what = "argument names",
    .var.name = "predict_fn argument names",
    add = assert_collection)

  if (!is.null(preprocess_fn)){
    checkmate::assert_names(
      x = names(formals(preprocess_fn)),
      identical.to = c(
        "train_data", "test_data",
        "formula", "hyperparameters"
      ),
      what = "argument names",
      .var.name = "preprocess_fn argument names",
      add = assert_collection)
  }

  checkmate::reportAssertions(assert_collection)
  if (length(setdiff(fold_cols, colnames(data))) > 0){
    assert_collection$push(
      paste0("the following 'fold_cols' columns were not in 'data': ", paste0(
        setdiff(fold_cols, colnames(data)), collapse = ", "
      ))
    )
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Convert to tibble
  data <- dplyr::as_tibble(data) %>%
    dplyr::ungroup()

  # Add identifier for each observation so we can find which
  # ones are hard to predict
  tmp_observation_id_col <- create_tmp_name(data, name = ".observation")
  data[[tmp_observation_id_col]] <- seq_len(nrow(data))

  # Get evaluation type
  evaluation_type <- family

  # Check metrics # TODO Is this redundant?
  check_metrics_list(metrics)
  check_metrics_list(info_cols)

  # Fill metrics with default values for non-specified metrics
  # and get the names of the metrics to use
  metrics <- set_metrics(
    family = family, metrics_list = metrics,
    include_model_object_metrics = TRUE
  )
  info_cols <- set_info_cols(
    family = family,
    info_cols_list = info_cols
  )

  # Check that the fold column(s) is/are factor(s)
  check_fold_col_factor(data = data, fold_cols = fold_cols)

  # When using cross_validate() we need to extract a few hparams
  # Hyperparameters for REML, link, control, is_cross_validate
  special_hparams <- extract_special_fn_specific_hparams(
    hyperparameters = hyperparameters
  )
  is_cross_validate <- special_hparams[["is_special_fn"]]
  REML <- special_hparams[["REML"]]
  link <- special_hparams[["link"]]
  control <- special_hparams[["control"]]

  # Create model_specifics object
  # Update to get default values when an argument was not specified
  model_specifics <- list(
    model_formula = "",
    family = family,
    REML = REML, # TODO Anyway to safely remove this and link and control from model_specifics?
    link = link,
    control = control,
    cutoff = cutoff,
    positive = positive,
    model_verbose = FALSE, # TODO Should this be removed or renamed to verbose?
    model_fn = model_fn,
    predict_fn = predict_fn,
    preprocess_fn = preprocess_fn,
    preprocess_once = preprocess_once,
    hparams = NULL,
    observation_id_col = tmp_observation_id_col,
    caller = caller
  ) %>%
    update_model_specifics()

  ## Create computational grid

  computation_grid <- create_computation_grid(
    data = data,
    hparams = hyperparameters,
    formulas = formulas,
    fold_cols = fold_cols
  )

  n_models <- length(unique(computation_grid[["model"]]))
  n_model_instances <- nrow(computation_grid)
  n_folds <- length(unique(computation_grid[["abs_fold"]]))

  # TODO Perhaps add a progress bar?
  if (isTRUE(verbose)){
    message(
      paste0(
        "Will cross-validate ",
        n_models,
        " models. This requires fitting ",
        n_model_instances,
        " model instances."
      )
    )
  }

  if (isTRUE(preprocess_once)) {
    data <- run_preprocess_once(
      data = data,
      computation_grid = computation_grid,
      model_specifics = model_specifics,
      fold_cols = fold_cols
    )
  }

  # Set names of fold info columns
  # Should match those in fold_info below
  fold_info_cols <- list(
    "rel_fold" = "rel_fold",
    "abs_fold" = "abs_fold",
    "fold_column" = "fold_column"
  )

  # cross_validate all the models
  validated_folds <- plyr::llply(seq_len(nrow(computation_grid)),
    .parallel = parallel_,
    .fun = function(r) {

      # Extract current row from computation grid
      to_compute <- computation_grid[r, ]

      model_specifics[["model_formula"]] <- to_compute[["Formula"]]

      # TODO Could maybe be done elsewhere for avoiding redundant checks:
      # Check that formula contains dependent variable
      y_col <- extract_y(model_specifics[["model_formula"]]) # Name of target column
      if (is.null(y_col)) stop("The model formula does not contain a dependent variable.")

      model_specifics[["hparams"]] <- to_compute[["hparams"]]

      fold_info <- list(
        "rel_fold" = to_compute[["rel_fold"]],
        "abs_fold" = to_compute[["abs_fold"]],
        "fold_column" = as.character(to_compute[["fold_col_name"]])
      )

      validate_fold(
        data = data,
        fold_info = fold_info,
        fold_info_cols = fold_info_cols,
        evaluation_type = evaluation_type,
        model_specifics = model_specifics,
        model_specifics_update_fn = NULL,
        metrics = metrics,
        fold_cols = fold_cols,
        err_nc = FALSE,
        return_model = FALSE
      )
    }
  )

  # Extract predictions and targets
  predictions_and_targets <- validated_folds %c% "predictions_and_targets"

  # Extract model object metrics
  model_evaluations <- validated_folds %c% "model_evaluation"

  # Extract preprocessing parameters
  preprocess_params <- validated_folds %c% "preprocess_parameters"

  # Extract whether the models were NULL or not
  model_was_null <- unlist(validated_folds %c% "model_was_null")

  # Add to computation grid
  computation_grid <- computation_grid %>%
    dplyr::mutate(
      model_eval = model_evaluations,
      Predictions = predictions_and_targets,
      model_was_null = model_was_null,
      Preprocess = preprocess_params
    )

  # Evaluate predictions
  cross_validations <- plyr::llply(seq_len(n_models),
    .parallel = parallel_,
    .fun = function(m) {

      # Extract grid for current model
      current_grid <- computation_grid[computation_grid[["model"]] == m, ]

      # Extract current predictions
      current_predictions <- dplyr::bind_rows(current_grid[["Predictions"]])

      # Extract current model object evaluations
      current_model_evals <- dplyr::bind_rows(current_grid[["model_eval"]])

      # Extract current preprocessing parameters
      current_preprocess_params <- dplyr::bind_rows(current_grid[["Preprocess"]])
      nested_current_preprocess_params <- current_preprocess_params %>%
        dplyr::group_nest() %>%
        dplyr::pull(.data$data)

      # Extract current model metrics + some fold cols
      current_model_metrics <- current_model_evals %>%
        base_select(cols = c(
          fold_info_cols[["fold_column"]],
          fold_info_cols[["rel_fold"]],
          intersect(metrics, colnames(current_model_evals))
        ))

      # Average the model metrics
      # First by fold column
      # Then again
      average_model_metrics <- current_model_metrics %>%
        base_deselect(cols = fold_info_cols[["rel_fold"]]) %>%
        dplyr::group_by(!!as.name(fold_info_cols[["fold_column"]])) %>%
        dplyr::summarise_all(.funs = ~ mean(.)) %>%
        base_deselect(cols = fold_info_cols[["fold_column"]]) %>%
        dplyr::summarise_all(.funs = ~ mean(.))

      # Prepare and nest the warnings and messages
      current_warnings_and_messages <- current_model_evals %>%
        base_select(cols = "Warnings and Messages") %>%
        legacy_unnest()
      nested_current_warnings_and_messages <- current_warnings_and_messages %>%
        dplyr::group_nest() %>%
        dplyr::pull(.data$data)

      # Sum the warning and message counts
      current_warnings_and_messages_counts <- current_model_evals %>%
        base_select(
          cols = c(
            "Convergence Warnings",
            "Singular Fit Messages",
            "Other Warnings"
          )
        ) %>%
        dplyr::summarise_all(.funs = ~ sum(.))

      # Prepare and nest the coefficients
      current_coefficients <- current_model_evals[["Coefficients"]] %>%
        dplyr::bind_rows()
      nested_current_coefficients <- current_coefficients %>%
        dplyr::group_nest() %>%
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
        include_fold_columns = TRUE,
        include_predictions = TRUE # TODO Perhaps should be arg in main fn?
      )

      if (family == "gaussian") {

        # Extract the prediction fold results tibble
        fold_results <- prediction_evaluation[["Results"]][[1]]
        prediction_evaluation[["Results"]] <- NULL
        # Add the model metric object results
        fold_results <- fold_results %>%
          dplyr::full_join(current_model_metrics,
            by = c(
              `Fold Column` = fold_info_cols[["fold_column"]],
              Fold = fold_info_cols[["rel_fold"]]
            )
          )
      } else if (family %in% c("binomial", "multinomial")) {

        # In classification, we evaluate the collected (all folds) predictions
        # per fold column. So if the Results column exists,
        # we will join them per
        # TODO: Make sure, Results is always included in prediction_evaluation !!!
        fold_results <- prediction_evaluation[["Results"]][[1]]
        prediction_evaluation[["Results"]] <- NULL

        # Prepare model metrics for joining with the prediction results

        # Extract model metric names
        model_metric_names <- intersect(names(current_model_metrics), metrics)

        if (length(model_metric_names) > 0) {

          # TODO The new tidyr::nest or chop() interface might be able to do this part
          # without the loop and stuff (kind of messy). Requires v1.0.0 though
          # so for now we will do it this way, and change it if profiling
          # marks it as problematic. Note: It seems to be fairly taxing, so
          # perhaps it is worth checking the tidyr version and only using
          # this when necessary?

          if (tidyr_new_interface()) { # Chop is new in tidyr 1.0.0
            # TODO What's the effect of using chop on speed?
            # It's at least a LOT prettier than the below version
            fold_col_model_metrics_nested <- current_model_metrics %>%
              base_deselect(cols = "rel_fold") %>%
              dplyr::group_by(!!as.name(fold_info_cols[["fold_column"]])) %>%
              tidyr::chop(cols = model_metric_names)
          } else {
            fold_col_model_metrics_nested <- plyr::ldply(model_metric_names, function(mn) {
              current_model_metrics %>%
                base_select(cols = c(fold_info_cols[["fold_column"]], mn)) %>%
                dplyr::group_by(!!as.name(fold_info_cols[["fold_column"]])) %>%
                legacy_nest(2, .key = "value") %>%
                dplyr::mutate(metric = mn)
            }) %>%
              dplyr::as_tibble() %>%
              tidyr::spread(
                key = "metric",
                value = "value"
              )
          }

          fold_results <- fold_results %>%
            dplyr::full_join(fold_col_model_metrics_nested,
              by = c(`Fold Column` = fold_info_cols[["fold_column"]])
            )
          fold_results <- fold_results %>%
            base_select(cols = c(
              "Fold Column", metrics,
              setdiff(
                colnames(fold_results),
                c("Fold Column", metrics)
              )
            ))
        }
      }

      if (!is.data.frame(fold_results) && is.na(fold_results)) {
        nested_fold_results <- list(fold_results)
      } else {
        # Nest fold results
        nested_fold_results <- fold_results %>%
          dplyr::group_nest() %>%
          dplyr::pull(.data$data)
      }

      # Combine the various columns
      evaluation <- prediction_evaluation %>%
        dplyr::bind_cols(average_model_metrics) %>%
        tibble::add_column(
          "Results" = nested_fold_results,
          "Coefficients" = nested_current_coefficients,
          "Preprocess" = nested_current_preprocess_params
        ) %>%
        reposition_column("Predictions", .before = "Results") %>%
        tibble::add_column(
          Folds = n_folds,
          `Fold Columns` = length(fold_cols)
        ) %>%
        dplyr::bind_cols(current_warnings_and_messages_counts) %>%
        dplyr::mutate(`Warnings and Messages` = nested_current_warnings_and_messages)

      if (is.null(preprocess_fn) || ncol(current_preprocess_params) == 0) {
        evaluation[["Preprocess"]] <- NULL
      }

      evaluation
    }
  ) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Family = model_specifics[["family"]])

  # Extract the first row for each model in the computation grid
  grid_first_rows <- computation_grid %>%
    dplyr::group_by(.data$model) %>%
    dplyr::slice(1)
  grid_first_rows <- grid_first_rows[
    order(grid_first_rows$model,
          method = "radix"), ]

  # Extract hparams from grid
  hparams <- grid_first_rows[["hparams"]]

  # Extract formulas from grid
  model_formulas <- grid_first_rows[["Formula"]]

  # Now we want to take the formula from the formulas and split it up into
  # fixed effects and random effects
  # Some users might want to mix models with an without random effects,
  # and so we first try to seperate into fixed and random,
  # and if no random effects are found for any of the models,
  # we remove the column "random".
  # Models without random effects will get NA in the random column.

  # Extract model effects from the original formulas
  # (as we need their order in a moment)
  original_formula_order <- extract_model_effects(formulas) %>%
    dplyr::mutate(Formula = formulas)

  # Get model effects for the current output rows
  mixed_effects <- tibble::tibble("Formula" = model_formulas) %>%
    dplyr::left_join(original_formula_order,
      by = "Formula"
    ) %>%
    base_deselect(cols = "Formula")

  # Remove Formula column
  original_formula_order[["Formula"]] <- NULL

  # We put the two data frames together
  output <- dplyr::bind_cols(
    cross_validations,
    mixed_effects
  )

  if (!is.null(hyperparameters)) {
    output <- output %>%
      tibble::add_column(
        "HParams" = hparams,
        .before = "Dependent"
      )
  }

  # Reorder data frame
  # Reorder rows by original formula order
  # This also removes unwanted columns
  new_col_order <- c(metrics, intersect(info_cols, colnames(output)))
  output <-
    original_formula_order %>%
    dplyr::left_join(output,
                     by = names(original_formula_order)) %>%
    base_select(cols = new_col_order) %>%
    position_first("Fixed")

  # If asked to remove non-converged models from output
  if (isTRUE(rm_nc)) {
    output <- output[output[["Convergence Warnings"]] == 0, ]
  }

  # and return it
  output
}


extract_from_hparams_for_cross_validate <- function(hyperparameters, param) {
  if (!is.null(hyperparameters) &&
    param %in% names(hyperparameters)) {
    return(hyperparameters[[param]])
  }
  NULL
}
extract_from_hparams_for_validate <- extract_from_hparams_for_cross_validate
