prepare_train_test <- function(data, fold_info, fold_cols, model_specifics) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data , add = assert_collection)
  checkmate::assert_character(x = fold_cols, add = assert_collection)
  checkmate::assert_list(x = fold_info, types = c("character", "numeric"),
                         add = assert_collection)
  checkmate::assert_list(x = model_specifics,
                         add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(x = colnames(data),
                          must.include = fold_cols,
                          what = "colnames",
                          add = assert_collection)
  checkmate::assert_names(x = names(fold_info),
                          must.include = c("fold_column", "rel_fold"),
                          add = assert_collection)
  checkmate::assert_names(x = names(model_specifics),
                          must.include = c("preprocess_once",
                                           "model_formula",
                                           "preprocess_fn",
                                           "caller"),
                          add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Extract train and test sets
  data_subset <- subset_data(
    data = data,
    fold_info = fold_info,
    fold_cols = fold_cols
  )
  train_data <- data_subset[["train"]]
  test_data <- data_subset[["test"]]

  if ("observation_id_col" %in% names(model_specifics)) {
    # We don't want to preprocess the observation IDs
    test_observation_ids <- test_data[[model_specifics[["observation_id_col"]]]]
    test_data[[model_specifics[["observation_id_col"]]]] <- NULL
    train_data[[model_specifics[["observation_id_col"]]]] <- NULL
  }

  # Preprocess data

  # Extract formula and hparams
  if (isTRUE(model_specifics[["preprocess_once"]])) {
    current_formula <- NULL
    current_hparams <- NULL
  } else {
    current_formula <- as.formula(model_specifics[["model_formula"]])
    current_hparams <- extract_hparams(model_specifics)
  }

  preprocess_fn <- model_specifics[["preprocess_fn"]]

  if (!is.null(preprocess_fn)) {

    # Preprocess
    preprocess_process <- tryCatch(
      {
        purrr::map(.x = 1, .f = purrr::quietly(function(.x) {
          preprocess_fn(
            train_data = train_data,
            test_data = test_data,
            formula = current_formula,
            hyperparameters = current_hparams
          )
        }))
      },
      error = function(e) {
        stop(
          create_message(
            m = e,
            caller = model_specifics[["caller"]],
            formula = current_formula,
            fold_col = fold_info[["fold_column"]],
            fold = fold_info[["rel_fold"]],
            hyperparameters = current_hparams,
            why = ifelse(grepl("Must be a formula, not 'NULL'", e),
                         "Possibly caused by 'preprocessing_once' being TRUE?",
                         "")
          )
        )
      }
    )

    train_test <- preprocess_process[[1]][["result"]]
    warnings <- preprocess_process[[1]][["warnings"]]
    messages <- preprocess_process[[1]][["messages"]]

    train_data <- tryCatch(
      {
        train_test[["train"]]
      },
      error = function(e) {
        stop(
          create_message(
            m = "Could not extract the training data from the output of 'preprocess_fn'.",
            caller = model_specifics[["caller"]],
            why = "Did you name the output list correctly?"
          )
        )
      }
    )

    test_data <- tryCatch(
      {
        train_test[["test"]]
      },
      error = function(e) {
        stop(
          create_message(
            m = "Could not extract the test data from the output of 'preprocess_fn'.",
            caller = model_specifics[["caller"]],
            why = "Did you name the output list correctly?"
          )
        )
      }
    )

    if ("parameters" %in% names(train_test)) {
      preprocess_parameters <- tryCatch(
        {
          params <- train_test[["parameters"]]
          if (!is.data.frame(params)) {
            stop("the returned preprocessing parameters object was not a data frame.")
          }
          params %>%
            dplyr::as_tibble() %>%
            tibble::add_column(
              `Fold Column` = as.character(fold_info[["fold_column"]]),
              Fold = as.integer(as.character(fold_info[["rel_fold"]])),
              .before = names(params)[[1]]
            )
        },
        error = function(e) {
          stop(
            create_message(
              m = "Could not extract the preprocessing parameters properly from the output of 'preprocess_fn'.",
              caller = model_specifics[["caller"]]
            )
          )
        }
      )
    } else {
      # Empty tibble
      preprocess_parameters <- tibble::tibble()
    }
  } else {
    warnings <- character()
    messages <- character()
    # Empty tibble
    preprocess_parameters <- tibble::tibble()
  }

  # Create tibble with warnings and messages
  warnings_and_messages <- create_warnings_and_messages_tibble(
    warnings = warnings,
    messages = messages,
    fn = "preprocess_fn"
  ) %>%
    dplyr::mutate(
      `Fold Column` = as.character(fold_info[["fold_column"]]),
      Fold = fold_info[["rel_fold"]]
    ) %>%
    base_select(cols = c("Fold Column", "Fold", "Function", "Type", "Message"))

  # Message the caught messages to the user
  for (m in messages) {

    # purrr::quietly adds \n to end of messages, which we're not interested in here
    m <- gsub("\\\n$", "", m)

    message(
      create_message(
        m = m,
        caller = model_specifics[["caller"]],
        formula = current_formula,
        fold_col = fold_info[["fold_column"]],
        fold = fold_info[["rel_fold"]],
        hyperparameters = current_hparams
      )
    )
  }

  # Throw the caught warnings
  for (w in warnings) {
    warning(
      create_message(
        m = w,
        caller = model_specifics[["caller"]],
        formula = current_formula,
        fold_col = fold_info[["fold_column"]],
        fold = fold_info[["rel_fold"]],
        hyperparameters = current_hparams
      )
    )
  }

  if ("observation_id_col" %in% names(model_specifics)) {
    # Add back the observation IDs to the test set
    test_data[[model_specifics[["observation_id_col"]]]] <- test_observation_ids
  }

  list(
    "train" = train_data,
    "test" = test_data,
    "preprocess_parameters" = preprocess_parameters,
    "warnings_and_messages" = warnings_and_messages,
    "n_unknown_warnings" = length(warnings)
  )
}


run_preprocess_once <- function(data,
                                computation_grid,
                                model_specifics,
                                fold_cols) {

  # Extract fold grid for model 1
  fold_grid_model_1 <- computation_grid[
    computation_grid[["model"]] == 1,
  ] %>%
    base_select(cols = c("fold_col_name", "rel_fold", "abs_fold"))

  # Subset and preprocess each train/test split
  # Nest each split
  data <- plyr::ldply(fold_grid_model_1[["abs_fold"]], function(a_f) {

    # Extract current fold info
    current_fold_info <- fold_grid_model_1[
      fold_grid_model_1[["abs_fold"]] == a_f,
    ]

    # Subset and preprocess train/test splits
    train_test <- prepare_train_test(
      data = data,
      fold_info = list(
        "rel_fold" = current_fold_info[["rel_fold"]],
        "fold_column" = as.character(current_fold_info[["fold_col_name"]])
      ),
      fold_cols = fold_cols,
      model_specifics = model_specifics
    )

    extract_and_nest <- function(train_test, element) {
      train_test[[element]] %>%
        dplyr::group_nest() %>%
        dplyr::pull(.data$data)
    }

    # Extract train and test sets
    # Nest both and add to tibble
    tibble::tibble(
      "train" = extract_and_nest(train_test, "train"),
      "test" = extract_and_nest(train_test, "test"),
      "preprocess_parameters" = extract_and_nest(train_test, "preprocess_parameters"),
      "warnings_and_messages" = extract_and_nest(train_test, "warnings_and_messages"),
      "n_unknown_warnings" = train_test[["n_unknown_warnings"]],
      "abs_fold" = a_f,
      "rel_fold" = current_fold_info[["rel_fold"]],
      "fold_column" = as.character(current_fold_info[["fold_col_name"]])
    )
  }) %>%
    dplyr::as_tibble()

  data
}


subset_data <- function(data, fold_info, fold_cols) {

  # Important to ensure it is character instead of factor
  # As that will match the rel_fold with the level index column instead
  current_fold_column <- as.character(fold_info[["fold_column"]])

  # Create training set for this iteration
  train_data <- data[data[[current_fold_column]] != fold_info[["rel_fold"]], ]
  # Create test set for this iteration
  test_data <- data[data[[current_fold_column]] == fold_info[["rel_fold"]], ]

  # Remove folds column(s) from subsets, so we can use "y ~ ." method
  # when defining the model formula.
  train_data <- train_data %>%
    dplyr::ungroup() %>%
    base_deselect(cols = fold_cols)

  test_data <- test_data %>%
    dplyr::ungroup() %>%
    base_deselect(cols = fold_cols)

  list(
    "train" = train_data,
    "test" = test_data
  )
}
