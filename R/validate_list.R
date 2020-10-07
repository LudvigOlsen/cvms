

#   __________________ #< 9d281822e7dafc7afc466a38db278b1e ># __________________
#   Validate list                                                           ####


validate_list <- function(train_data,
                          test_data = NULL,
                          formulas,
                          model_fn,
                          predict_fn,
                          preprocess_fn = NULL,
                          preprocess_once = FALSE,
                          hyperparameters = NULL,
                          partitions_col = ".partitions",
                          family = "gaussian",
                          cutoff = 0.5,
                          positive = 2,
                          metrics = list(),
                          info_cols = list(),
                          err_nc = FALSE,
                          rm_nc = FALSE,
                          verbose = FALSE,
                          parallel_ = FALSE,
                          return_models = TRUE,
                          caller = "validate_fn()") {
  if (checkmate::test_string(x = metrics, pattern = "^all$")) {
    metrics <- list("all" = TRUE)
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(
    x = train_data,
    min.rows = 1,
    min.cols = 2,
    add = assert_collection
  )
  checkmate::assert_data_frame(
    x = test_data,
    min.rows = 1,
    min.cols = 2,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_character(
    x = formulas,
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
  checkmate::assert_flag(x = err_nc, add = assert_collection)
  checkmate::assert_flag(x = rm_nc, add = assert_collection)
  checkmate::assert_flag(x = preprocess_once, add = assert_collection)
  checkmate::assert_flag(
    x = parallel_,
    add = assert_collection,
    .var.name = "parallel"
  )
  checkmate::assert_flag(x = return_models, add = assert_collection)
  checkmate::assert_string(x = caller, add = assert_collection)
  checkmate::assert_string(x = partitions_col, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (is.null(test_data) &&
      partitions_col %ni% colnames(train_data)){
    assert_collection$push("Could not find 'partition_col' column in 'train_data'.")
  }

  # Functions

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
  # End of argument checks ####

  train_data <- dplyr::as_tibble(train_data) %>%
    dplyr::ungroup()

  tmp_observation_id_col <- create_tmp_name(test_data, name = ".observation")

  if (!is.null(test_data)) {
    test_data <- dplyr::as_tibble(test_data) %>%
      dplyr::ungroup()

    # Add identifier for each observation so we can find which
    # ones are hard to predict
    test_data[[tmp_observation_id_col]] <- as.integer(seq_len(nrow(test_data)))
    train_data[[tmp_observation_id_col]] <- as.integer(-1)

    data <- train_data %>%
      dplyr::bind_rows(test_data, .id = partitions_col) %>%
      dplyr::mutate(.partitions = as.factor(as.integer(.data$.partitions)))
  } else {
    if (length(setdiff(as.character(train_data[[partitions_col]]), c("1", "2"))) > 0 ||
      length(setdiff(c("1", "2"), as.character(train_data[[partitions_col]]))) > 0) {
      stop(paste0(
        "'partitions_col' must contain the values 1 and 2, ",
        "where 1 signals the training set and 2 signals the test set"
      ))
    }

    if (".___observations___" %in% names(train_data)) {
      warning("column '.___observations___' will be overwritten.")
    }

    data <- train_data %>%
      dplyr::mutate(.partitions = as.factor(
        as.integer(as.character(.data$.partitions))
      )) %>%
      dplyr::group_by(.data$.partitions) %>%
      dplyr::mutate(.___observations___ = as.integer(seq_len(dplyr::n()))) %>%
      base_rename(
        before = ".___observations___",
        after = tmp_observation_id_col
      )
    # Set observation ids in training set to -1
    data[data[[".partitions"]] == levels(data[[".partitions"]])[[1]], ][[tmp_observation_id_col]] <- as.integer(-1)
  }

  # Get evaluation type
  if (family %in% c("gaussian", "binomial", "multinomial")) {
    evaluation_type <- family
  } else {
    stop(paste0(
      "Only 'gaussian', 'binomial', and 'multinomial' ",
      "evaluation types are currently allowed."
    ))
  }

  # Check metrics
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

  # When using validate() we need to extract a few hparams
  # Hyperparameters for REML, link, control, is_validate
  special_hparams <- extract_special_fn_specific_hparams(
    hyperparameters = hyperparameters
  )
  is_validate <- special_hparams[["is_special_fn"]]
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
    fold_cols = partitions_col,
    test_fold = 2
  )

  n_models <- length(unique(computation_grid[["model"]]))
  n_model_instances <- nrow(computation_grid)
  n_folds <- length(unique(computation_grid[["abs_fold"]]))

  if (isTRUE(verbose)) {
    # TODO Make better message here. It seems like a good idea
    # to inform the user about the number of models to fit etc.
    # so they know how long it may take
    # Perhaps add a progress bar?
    message(paste0("Will validate ", n_models, " models."))
  }

  if (isTRUE(preprocess_once)) {
    data <- run_preprocess_once(
      data = data,
      computation_grid = computation_grid,
      model_specifics = model_specifics,
      fold_cols = partitions_col
    )
  }

  # Set names of fold info columns
  # Should match those in fold_info below
  fold_info_cols <- list(
    "rel_fold" = "rel_fold",
    "abs_fold" = "abs_fold",
    "fold_column" = "fold_column"
  )

  # cross_validate all the models using ldply()
  validations <- plyr::llply(seq_len(nrow(computation_grid)),
    .parallel = parallel_,
    .fun = function(r) {

      # Extract current row from computation grid
      to_compute <- computation_grid[r, ]

      model_specifics[["model_formula"]] <-
        to_compute[["Formula"]]
      model_specifics[["hparams"]] <-
        to_compute[["hparams"]]

      # Check that formula contains dependent variable
      y_col <- extract_y(model_specifics[["model_formula"]]) # Name of target column
      if (is.null(y_col)) stop("The model formula does not contain a dependent variable.")

      fold_info <- list(
        "rel_fold" = to_compute[["rel_fold"]],
        "abs_fold" = to_compute[["abs_fold"]],
        "fold_column" = as.character(to_compute[["fold_col_name"]])
      )

      validated_folds <- validate_fold(
        data = data,
        fold_info = fold_info,
        fold_info_cols = fold_info_cols,
        evaluation_type = evaluation_type,
        model_specifics = model_specifics,
        model_specifics_update_fn = NULL,
        metrics = metrics,
        fold_cols = partitions_col,
        include_fold_columns = FALSE,
        err_nc = err_nc,
        return_model = TRUE
      )

      # Extract predictions and targets
      predictions_and_targets <- validated_folds[["predictions_and_targets"]]
      nested_predictions_and_targets <- predictions_and_targets %>%
        dplyr::group_nest() %>%
        dplyr::pull(.data$data)

      # Temporary fold info
      predictions_and_targets <- predictions_and_targets
      predictions_and_targets[[
      fold_info_cols[["fold_column"]]]] <- 1
      predictions_and_targets[[
      fold_info_cols[["rel_fold"]]]] <- 1
      predictions_and_targets[[
      fold_info_cols[["abs_fold"]]]] <- 1

      # Extract model object metrics
      model_evaluation <- validated_folds[["model_evaluation"]]

      # Extract model object
      # (they are in a list to allow them being added to tibble)
      model_object <- validated_folds[["model"]]

      # Extract preprocessing parameters
      preprocess_params <- validated_folds[["preprocess_parameters"]]
      nested_preprocess_params <- preprocess_params %>%
        dplyr::group_nest() %>%
        dplyr::pull(.data$data)

      # Extract whether the model was NULL or not
      model_was_null <- validated_folds[["model_was_null"]]

      # Evaluate the predictions
      prediction_evaluation <- internal_evaluate_predictions(
        data = predictions_and_targets,
        predictions_col = "prediction",
        targets_col = "target",
        model_was_null_col = "model_was_null",
        type = family,
        fold_info_cols = fold_info_cols,
        model_specifics = model_specifics,
        metrics = metrics,
        include_fold_columns = FALSE,
        include_predictions = TRUE # TODO Perhaps should be arg in main fn?
      ) %>%
        base_deselect(cols = "Results")

      # Combine the various columns
      evaluation <- prediction_evaluation %>%
        dplyr::bind_cols(model_evaluation) %>%
        tibble::add_column(
          "Preprocess" = nested_preprocess_params,
          "HParams" = list(model_specifics[["hparams"]][[1]]),
          "Formula" = model_specifics[["model_formula"]]
        ) %>%
        dplyr::bind_cols(extract_model_effects(model_specifics[["model_formula"]]))

      if (is.null(preprocess_fn) || ncol(preprocess_params) == 0) {
        evaluation[["Preprocess"]] <- NULL
      }

      if (isTRUE(return_models)) {
        evaluation[["Model"]] <- list(model_object)
      }

      evaluation
    }
  ) %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Family = model_specifics[["family"]])

  # Order by original formulas
  output <- tibble::tibble("Formula" = formulas) %>%
    dplyr::left_join(validations, by = "Formula") %>%
    base_deselect(cols = "Formula")

  if (is.null(hyperparameters)) {
    output[["HParams"]] <- NULL
  }

  # Reorder data frame
  new_col_order <- c(metrics, intersect(info_cols, colnames(output)))
  output <- output %>%
    base_select(cols = new_col_order) %>%
    position_first("Fixed")

  # Remove singular_fit_messages if not cross_validate
  if (!isTRUE(is_validate)) {
    output[["Singular Fit Messages"]] <- NULL
  }

  # If asked to remove non-converged models from output
  if (isTRUE(rm_nc)) {
    output <- output[output[["Convergence Warnings"]] == 0, ]
  }

  output
}
