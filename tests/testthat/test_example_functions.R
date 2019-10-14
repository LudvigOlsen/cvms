library(cvms)
context("example functions")

## example_preprocess_functions()

test_that("the expected function definitions are returned in example_preprocess_functions()",{

  expect_equal(head(capture.output(example_preprocess_functions("standardize")), 22),
               c("function(train_data, test_data, formula, hyperparameters){",
                 "",
                 "      # Get centering and scaling parameters from the train_data",
                 "      preprocess_params <- caret::preProcess(train_data,",
                 "                                             method = c(\"scale\", \"center\"))",
                 "",
                 "      # Apply standardization to all numeric variables in",
                 "      # train_data and test_data",
                 "      train_data <- predict(preprocess_params, train_data)",
                 "      test_data <- predict(preprocess_params, test_data)",
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
                 "        train_data <- predict(preprocess_params, train_data)",
                 "        test_data <- predict(preprocess_params, test_data)",
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
                                         formula = NULL, hyperparameters = NULL)

    # Standardize manually
    standardize_params <- caret::preProcess(train_set, method = c("scale", "center"))
    standardized_train_set <- predict(standardize_params, train_set)
    standardized_test_set <- predict(standardize_params, test_set)

    # Test
    expect_identical(standardized_by_fn[["train"]], standardized_train_set)
    expect_identical(standardized_by_fn[["test"]], standardized_test_set)
    expect_equal(standardized_by_fn[["parameters"]]$Measure,
                 c("Mean", "SD"))
    expect_equal(standardized_by_fn[["parameters"]]$age,
                 c(29.2857142857143, 7.93185260290972),
                 tolerance = 1e-4)
    expect_equal(standardized_by_fn[["parameters"]]$diagnosis,
                 c(0.571428571428571, 0.50709255283711),
                 tolerance = 1e-4)
    expect_equal(standardized_by_fn[["parameters"]]$score,
                 c(40.2857142857143, 19.4220051929322),
                 tolerance = 1e-4)
    expect_equal(standardized_by_fn[["parameters"]]$session,
                 c(2, 0.836660026534076),
                 tolerance = 1e-4)

    # Normalize

    # Normalize with preprocess_fn
    normalize_fn <- example_preprocess_functions("normalize")
    normalized_by_fn <- normalize_fn(train_data = train_set, test_data = test_set,
                                     formula = NULL, hyperparameters = NULL)

    # Normalize manually
    normalize_params <- caret::preProcess(train_set, method = c("range"),
                                          rangeBounds = c(0,1))
    normalized_train_set <- predict(normalize_params, train_set)
    normalized_test_set <- predict(normalize_params, test_set)

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

