library(cvms)
context("most_challenging()")

test_that("binomial model works with most_challenging()", {

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat_ready <- participant.scores %>%
    dplyr::mutate(diagnosis = as.factor(diagnosis))
  dat_list <- groupdata2::fold(dat_ready,
    k = 4,
    num_fold_cols = 8,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  CV_binom <- cross_validate(
    dat_list,
    formulas = c("diagnosis ~ score", "diagnosis ~ score + age"),
    fold_cols = paste0(".folds_", 1:8),
    family = "binomial"
  )

  collected_preds <- dplyr::bind_rows(CV_binom$Predictions, .id = "Model") %>%
    dplyr::group_by(.data$Model)

  hard_to_predict <- most_challenging(collected_preds,
    threshold = 0.25,
    threshold_is = "percentage"
  )

  expect_equal(
    hard_to_predict$Model,
    c(
      "1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2",
      "2", "2"
    )
  )
  expect_equal(
    hard_to_predict$Observation,
    c(1L, 4L, 5L, 7L, 10L, 20L, 21L, 1L, 4L, 7L, 10L, 20L, 21L, 30L)
  )
  expect_equal(
    hard_to_predict$Correct,
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
  )
  expect_equal(
    hard_to_predict$Incorrect,
    c(8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L)
  )
  expect_equal(
    hard_to_predict$Accuracy,
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
  )

  hard_to_predict <- most_challenging(collected_preds, threshold_is = "score")

  expect_equal(
    hard_to_predict$Model,
    c(
      "1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2",
      "2", "2"
    )
  )
  expect_equal(
    hard_to_predict$Observation,
    c(1L, 4L, 5L, 7L, 10L, 20L, 21L, 1L, 4L, 7L, 10L, 20L, 21L, 30L)
  )
  expect_equal(
    hard_to_predict$Correct,
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
  )
  expect_equal(
    hard_to_predict$Incorrect,
    c(8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L)
  )
  expect_equal(
    hard_to_predict$Accuracy,
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
  )
})


test_that("multinomial model works with most_challenging()", {

  # Load data and fold it
  set_seed_for_R_compatibility(1)

  # Create and fold dataset
  data_mc <- multiclass_probability_tibble(
    num_classes = 3, num_observations = 50,
    apply_softmax = TRUE, FUN = runif,
    class_name = "predictor_"
  )
  class_names <- paste0("class_", c(1, 2, 3))
  data_mc[["target"]] <- factor(sample(
    x = class_names,
    size = 50, replace = TRUE
  ))
  dat <- groupdata2::fold(data_mc, k = 4, num_fold_cols = 8)

  multinom_model_fn <- function(train_data, formula, hyperparameters) {
    nnet::multinom(
      formula = formula, # converted to formula object within fit_model()
      data = train_data
    )
  }

  random_predict_fn <- function(test_data, model, formula, hyperparameters) {
    multiclass_probability_tibble(
      num_classes = 3, num_observations = nrow(test_data),
      apply_softmax = TRUE, FUN = runif,
      class_name = "class_"
    )
  }

  CVmultinomlist <- cross_validate_fn(dat,
    model_fn = multinom_model_fn,
    predict_fn = random_predict_fn,
    formulas = c(
      "target ~ predictor_1 + predictor_2 + predictor_3",
      "target ~ predictor_1"
    ),
    fold_cols = paste0(".folds_", 1:8),
    type = "multinomial"
  )


  collected_preds <- dplyr::bind_rows(CVmultinomlist$Predictions, .id = "Model") %>%
    dplyr::group_by(.data$Model)

  hard_to_predict <- most_challenging(collected_preds,
    threshold = 0.15,
    threshold_is = "percentage"
  )

  expect_equal(
    hard_to_predict$Model,
    c("1", "1", "1", "1", "1", "1", "2", "2", "2")
  )
  expect_equal(
    hard_to_predict$Observation,
    c(4L, 10L, 11L, 12L, 17L, 35L, 5L, 33L, 50L)
  )
  expect_equal(
    hard_to_predict$Correct,
    c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)
  )
  expect_equal(
    hard_to_predict$Incorrect,
    c(8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L)
  )
  expect_equal(
    hard_to_predict$Accuracy,
    c(0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  hard_to_predict <- most_challenging(collected_preds, threshold = 0.15, threshold_is = "score")

  expect_equal(
    hard_to_predict$Model,
    c(
      "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1", "1",
      "1", "1", "2", "2", "2"
    )
  )
  expect_equal(
    hard_to_predict$Observation,
    c(
      4L, 10L, 11L, 12L, 17L, 35L, 6L, 8L, 28L, 31L, 33L, 39L, 47L,
      48L, 5L, 33L, 50L
    )
  )
  expect_equal(
    hard_to_predict$Correct,
    c(
      0L, 0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 0L,
      0L, 0L
    )
  )
  expect_equal(
    hard_to_predict$Incorrect,
    c(
      8L, 8L, 8L, 8L, 8L, 8L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 7L, 8L,
      8L, 8L
    )
  )
  expect_equal(
    hard_to_predict$Accuracy,
    c(
      0, 0, 0, 0, 0, 0, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125,
      0.125, 0.125, 0, 0, 0
    )
  )
})



test_that("gaussian model works with most_challenging()", {

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat_ready <- participant.scores %>%
    dplyr::mutate(diagnosis = as.factor(diagnosis))
  dat_list <- groupdata2::fold(dat_ready,
    k = 4,
    num_fold_cols = 8,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  CV_gauss <- cross_validate(
    dat_list,
    formulas = c(
      "score ~ diagnosis + session",
      "score ~ diagnosis + age + session"
    ),
    fold_cols = paste0(".folds_", 1:8),
    family = "gaussian"
  )

  collected_preds <- dplyr::bind_rows(CV_gauss$Predictions, .id = "Model") %>%
    dplyr::group_by(.data$Model)

  hard_to_predict <- most_challenging(collected_preds,
    type = "gaussian",
    threshold = 0.1, threshold_is = "percentage"
  )

  expect_equal(
    hard_to_predict$Model,
    c("1", "1", "1", "2", "2", "2")
  )
  expect_equal(
    hard_to_predict$Observation,
    c(20L, 24L, 21L, 20L, 24L, 21L)
  )
  expect_equal(
    hard_to_predict$MAE,
    c(
      26.6104166666667, 20.9373511904762,
      18.2130952380952, 26.7693082340977,
      21.7422827228115, 18.3719868055263
    )
  )
  expect_equal(
    hard_to_predict$RMSE,
    c(
      26.6125303350383, 20.9515642469683,
      18.2223366019013, 26.7757193125139,
      21.7845630990429, 18.3931551635641
    )
  )

  hard_to_predict <- most_challenging(collected_preds,
    type = "gaussian",
    threshold = 13.9, threshold_is = "score"
  )

  expect_equal(
    hard_to_predict$Model,
    c("1", "1", "1", "1", "2", "2", "2", "2", "2")
  )
  expect_equal(
    hard_to_predict$Observation,
    c(20L, 24L, 21L, 3L, 20L, 24L, 21L, 27L, 3L)
  )
  expect_equal(
    hard_to_predict$MAE,
    c(
      26.6104166666667, 20.9373511904762, 18.2130952380952, 14.7703373015873,
      26.7693082340977, 21.7422827228115, 18.3719868055263, 13.4855783921144,
      13.8922545752059
    )
  )
  expect_equal(
    hard_to_predict$RMSE,
    c(
      26.6125303350383, 20.9515642469683, 18.2223366019013, 14.7823170647799,
      26.7757193125139, 21.7845630990429, 18.3931551635641, 14.7146578087269,
      13.9530043384422
    )
  )
})
