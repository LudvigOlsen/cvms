library(cvms)
context("example functions")

## example_preprocess_functions()

test_that("the expected function definitions are returned in example_preprocess_functions()",{

  testthat::skip("Fails in check - IMPROVE THESE TESTS")
  expect_equal(head(capture.output(example_preprocess_functions("standardize")), 22),
               c("function(train_data, test_data, formula, hyperparameters){",
                 "",
                 "      # Get centering and scaling parameters from the train_data",
                 "      preprocess_params <- caret::preProcess(train_data,",
                 "                                             method = c(\"scale\", \"center\"))",
                 "",
                 "      # Apply standardization to all numeric variables in",
                 "      # train_data and test_data",
                 "      train_data <- stats::predict(preprocess_params, train_data)",
                 "      test_data <- stats::predict(preprocess_params, test_data)",
                 "",
                 "      # Extract parameters and add to tibble",
                 "      tidy_parameters <- tibble::tibble(\"Measure\" = c(\"Mean\", \"SD\")) %>%",
                 "        dplyr::bind_cols(",
                 "          dplyr::bind_rows(preprocess_params$mean,",
                 "                           preprocess_params$std)",
                 "          )",
                 "",
                 "      list(\"train\" = train_data,",
                 "           \"test\" = test_data,",
                 "           \"parameters\" = tidy_parameters)",
                 "    }")
               )

  expect_equal(head(capture.output(example_preprocess_functions("normalize")), 22),
               c("function(train_data, test_data, formula, hyperparameters){",
                 "",
                 "        # Get normalization parameters from the train_data",
                 "        preprocess_params <- caret::preProcess(train_data,",
                 "                                               method = c(\"range\"),",
                 "                                               rangeBounds = c(0,1))",
                 "",
                 "        # Apply normalization to all numeric variables in",
                 "        # train_data and test_data",
                 "        train_data <- stats::predict(preprocess_params, train_data)",
                 "        test_data <- stats::predict(preprocess_params, test_data)",
                 "",
                 "        # Extract parameters and add to tibble",
                 "        tidy_parameters <- tibble::tibble(\"Measure\" = c(\"Min\", \"Max\")) %>%",
                 "          dplyr::bind_cols(",
                 "            dplyr::as_tibble(preprocess_params$ranges)",
                 "          )",
                 "",
                 "        list(\"train\" = train_data,",
                 "             \"test\" = test_data,",
                 "             \"parameters\" = tidy_parameters)", "      }")
  )

})

test_that("the expected output is returned from example_preprocess_functions() functions",{

  # Standardize

  # Load data and fold it
    set_seed_for_R_compatibility(1)
    partitions <- groupdata2::partition(participant.scores, p = 0.75,
                                 cat_col = 'diagnosis',
                                 id_col = 'participant')
    train_set <- partitions[[1]]
    test_set <- partitions[[2]]

    # Standardize with preprocess_fn
    standardize_fn <- example_preprocess_functions("standardize")
    standardized_by_fn <- standardize_fn(train_data = train_set, test_data = test_set,
                                         formula = as.formula("diagnosis ~ ."),
                                         hyperparameters = NULL)

    # Standardize manually
    train_mean_age <- mean(train_set$age)
    train_sd_age <- sd(train_set$age)
    train_mean_score <- mean(train_set$score)
    train_sd_score <- sd(train_set$score)
    train_mean_session <- mean(train_set$session)
    train_sd_session <- sd(train_set$session)
    train_mean_diagnosis <- mean(train_set$diagnosis)
    train_sd_diagnosis <- sd(train_set$diagnosis)

    standardized_train <- train_set
    standardized_test <- test_set
    standardized_train[["age"]] <- (standardized_train[["age"]]-train_mean_age) / train_sd_age
    standardized_test[["age"]] <- (standardized_test[["age"]]-train_mean_age) / train_sd_age
    standardized_train[["score"]] <- (standardized_train[["score"]]-train_mean_score) / train_sd_score
    standardized_test[["score"]] <- (standardized_test[["score"]]-train_mean_score) / train_sd_score
    standardized_train[["session"]] <- (standardized_train[["session"]]-train_mean_session) / train_sd_session
    standardized_test[["session"]] <- (standardized_test[["session"]]-train_mean_session) / train_sd_session
    standardized_train[["diagnosis"]] <- (standardized_train[["diagnosis"]]-train_mean_diagnosis) / train_sd_diagnosis
    standardized_test[["diagnosis"]] <- (standardized_test[["diagnosis"]]-train_mean_diagnosis) / train_sd_diagnosis

    # Test
    expect_identical(standardized_by_fn[["train"]], standardized_train)
    expect_identical(standardized_by_fn[["test"]], standardized_test)
    expect_equal(standardized_by_fn[["parameters"]]$Measure,
                 c("Mean", "SD"))

    expect_equal(standardized_by_fn[["parameters"]]$age,
                 c(train_mean_age, train_sd_age),
                 tolerance = 1e-4)
    expect_equal(standardized_by_fn[["parameters"]]$age,
                 c(29.2857142857143, 7.93185260290972),
                 tolerance = 1e-4)

    expect_equal(standardized_by_fn[["parameters"]]$diagnosis,
                 c(0.571428571428571, 0.50709255283711),
                 tolerance = 1e-4)
    expect_equal(standardized_by_fn[["parameters"]]$diagnosis,
                 c(train_mean_diagnosis, train_sd_diagnosis),
                 tolerance = 1e-4)

    expect_equal(standardized_by_fn[["parameters"]]$score,
                 c(40.2857142857143, 19.4220051929322),
                 tolerance = 1e-4)
    expect_equal(standardized_by_fn[["parameters"]]$score,
                 c(train_mean_score, train_sd_score),
                 tolerance = 1e-4)

    expect_equal(standardized_by_fn[["parameters"]]$session,
                 c(2, 0.836660026534076),
                 tolerance = 1e-4)
    expect_equal(standardized_by_fn[["parameters"]]$session,
                 c(train_mean_session, train_sd_session),
                 tolerance = 1e-4)

    # Normalize

    # Normalize with preprocess_fn
    normalize_fn <- example_preprocess_functions("range")
    normalized_by_fn <- normalize_fn(train_data = train_set, test_data = test_set,
                                     formula = as.formula("diagnosis ~ ."),
                                     hyperparameters = NULL)

    # Normalize manually
    train_min_age <- min(train_set[["age"]])
    train_max_age <- max(train_set[["age"]])
    train_min_diagnosis <- min(train_set[["diagnosis"]])
    train_max_diagnosis <- max(train_set[["diagnosis"]])
    train_min_score <- min(train_set[["score"]])
    train_max_score <- max(train_set[["score"]])
    train_min_session <- min(train_set[["session"]])
    train_max_session <- max(train_set[["session"]])

    minMaxScaler <- function(x, min__, max__){
      (x - min__) / (max__- min__)
    }

    normalized_train_set <- train_set
    normalized_test_set <- test_set
    normalized_train_set[["age"]] <- minMaxScaler(normalized_train_set[["age"]],
                                                  train_min_age, train_max_age)
    normalized_train_set[["diagnosis"]] <- minMaxScaler(normalized_train_set[["diagnosis"]],
                                                        train_min_diagnosis, train_max_diagnosis)
    normalized_train_set[["score"]] <- minMaxScaler(normalized_train_set[["score"]],
                                                    train_min_score, train_max_score)
    normalized_train_set[["session"]] <- minMaxScaler(normalized_train_set[["session"]],
                                                      train_min_session, train_max_session)
    normalized_test_set[["age"]] <- minMaxScaler(normalized_test_set[["age"]],
                                                  train_min_age, train_max_age)
    normalized_test_set[["diagnosis"]] <- minMaxScaler(normalized_test_set[["diagnosis"]],
                                                        train_min_diagnosis, train_max_diagnosis)
    normalized_test_set[["score"]] <- minMaxScaler(normalized_test_set[["score"]],
                                                    train_min_score, train_max_score)
    normalized_test_set[["session"]] <- minMaxScaler(normalized_test_set[["session"]],
                                                      train_min_session, train_max_session)

    # Test
    expect_identical(normalized_by_fn[["train"]], normalized_train_set)
    expect_identical(normalized_by_fn[["test"]], normalized_test_set)
    expect_equal(normalized_by_fn[["parameters"]]$Measure,
                 c("Min", "Max"))
    expect_equal(normalized_by_fn[["parameters"]]$age,
                 c(20, 43),
                 tolerance = 1e-4)
    expect_equal(normalized_by_fn[["parameters"]]$diagnosis,
                 c(0, 1),
                 tolerance = 1e-4)
    expect_equal(normalized_by_fn[["parameters"]]$score,
                 c(10, 81),
                 tolerance = 1e-4)
    expect_equal(normalized_by_fn[["parameters"]]$session,
                 c(1, 3),
                 tolerance = 1e-4)


})

