prepare_train_test <- function(data, fold_info, fold_cols, model_specifics){

  # Extract train and test sets
  data_subset <- subset_data(data = data,
                             fold_info = fold_info,
                             fold_cols = fold_cols)
  train_data <- data_subset[["train"]]
  test_data <- data_subset[["test"]]

  # Preprocess data
  # TODO Should catch warnings and error here
  # So probably want to do a wrapper for subsetting and preprocessing?
  preprocess_fn <- model_specifics[["preprocess_fn"]]

  if (!is.null(preprocess_fn)){

    # Extract formula and hparams

    if (isTRUE(model_specifics[["preprocess_once"]])){
      current_formula <- NULL
      current_hparams <- NULL
    } else {
      current_formula <- model_specifics[["formula"]]
      current_hparams <- extract_hparams(model_specifics,
                                         as_list = FALSE)
    }

    # Preprocess
    preprocess_process <- tryCatch({
      purrr::map(.x = 1, .f = purrr::quietly(function(.x){
        preprocess_fn(train_data = train_data,
                      test_data = test_data,
                      formula = current_formula,
                      hyperparameters = current_hparams)
      }))
    }, error = function(e){
      stop(paste('',
                 '-------------------------------------', # TODO just calculate the number of hyphens?
                 paste0(model_specifics[["caller"]], ': Error:'),
                 'In formula:',
                 current_formula,
                 'For fold column:',
                 fold_info[["fold_column"]],
                 'In fold:',
                 fold_info[["rel_fold"]],
                 e, sep = "\n"))
    })

    train_test <- preprocess_process[[1]][["result"]]
    warnings <- preprocess_process[[1]][["warnings"]]
    messages <- preprocess_process[[1]][["messages"]]

    train_data <- tryCatch({train_test[["train"]]},
                           error = function(e){
                             stop(paste('',
                                        '-------------------------------------',
                                        paste0(model_specifics[["caller"]], ': Error:'),
                                        "Could not extract the training data from the output of 'preprocess_fn'.",
                                        "Did you name the output list correctly?",
                                        sep = "\n"))
                           })

    test_data <- tryCatch({train_test[["test"]]},
                           error = function(e){
                             stop(paste('',
                                        '-------------------------------------',
                                        paste0(model_specifics[["caller"]], ': Error:'),
                                        "Could not extract the test data from the output of 'preprocess_fn'.",
                                        "Did you name the output list correctly?",
                                        sep = "\n"))
                           })
  } else {
    warnings <- character()
    messages <- character()
  }

  # Create tibble with warnings and messages
  warnings_and_messages <- create_warnings_and_messages_tibble(warnings = warnings,
                                                               messages = messages,
                                                               fn = "preprocess_fn") %>%
    dplyr::mutate(Fold = fold_info[["rel_fold"]],
                  `Fold Column` = fold_info[["fold_column"]]) %>%
    dplyr::select(dplyr::one_of(
      c("Fold Column", "Fold", "Function", "Type", "Message")))

  list("train" = train_data,
       "test" = test_data,
       "warnings_and_messages" = warnings_and_messages,
       "n_unknown_warnings" = length(warnings))
}




subset_data <- function(data, fold_info, fold_cols){

  # Important to ensure it is character instead of factor
  # As that will match the rel_fold with the level index column instead
  current_fold_column <- as.character(fold_info[["fold_column"]])

  # Create training set for this iteration
  train_data <- data[data[[current_fold_column]] != fold_info[["rel_fold"]],]
  # Create test set for this iteration
  test_data <- data[data[[current_fold_column]] == fold_info[["rel_fold"]],]

  # Remove folds column(s) from subsets, so we can use "y ~ ." method
  # when defining the model formula.
  train_data <- train_data %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::one_of(fold_cols))

  test_data <- test_data %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::one_of(fold_cols))

  list("train" = train_data,
       "test" = test_data)
}
