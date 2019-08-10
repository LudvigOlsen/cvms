library(cvms)
context("evaluate()")


test_that("multinomial evaluations are correct in evaluate()",{

  set_seed_for_R_compatibility(1)
  random_probabilities <- create_multinomial_probability_tibble(
    num_classes = 5,
    num_observations = 20,
    apply_softmax = FALSE # Test with as well
  )
  expect_equal(sum(random_probabilities), 51.78471, tolerance = 1e-5)

  data_ <- random_probabilities %>%
    dplyr::mutate(cl = as.factor(rep(1:5, each = 4)),
                  cl_char = paste0("cl_", cl))

  expect_error(
    evaluate(
      data = data_,
      dependent_col = "cl",
      prediction_cols = paste0("class_", 1:5),
      type = "multiclass_classification",
      apply_softmax = TRUE
    ),
    "Not all levels in 'dependent_col' was found in 'prediction_cols'.",
    fixed = T
  )

  data_ <- data_ %>%
    dplyr::rename_at(dplyr::vars(paste0("class_", 1:5)), .funs = ~paste0("cl_", 1:5))

  mn_eval_1 <- evaluate(
    data = data_,
    dependent_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multiclass_classification",
    apply_softmax = TRUE
  )

  # TODO Add more tests
  expect_equal(mn_eval_1$Results$`Overall Accuracy`, 0.2)
  expect_equal(mn_eval_1$Results$`Balanced Accuracy`, 0.5)

  expect_equal(mn_eval_1$Class_level_results$Class, c("cl_1","cl_2","cl_3","cl_4","cl_5"))

  # TODO


  # ID level
  data_ <- data_ %>%
    dplyr::mutate(id = factor(rep(1:10, each=2)))

  mn_eval_2 <- evaluate(
    data = data_,
    dependent_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    id_col = "id",
    id_method = "mean",
    type = "multiclass_classification",
    apply_softmax = TRUE
  )

  data_2 <- data_ %>% dplyr::mutate(fold_ = 1) %>% dplyr::bind_rows(data_ %>% dplyr::mutate(fold_ = 2))

  mn_eval_3 <- evaluate(
    data = data_2 %>% dplyr::group_by(fold_),
    dependent_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    id_col = "id",
    id_method = "majority",
    type = "multiclass_classification",
    apply_softmax = TRUE
  )

  # TODO test that group_by and evaluate work correctly together

})

# TODO Add test that majority vote id_method works when not all classes are predicted most by one of the ids

test_that("softmax works in create_multinomial_probability_tibble()",{

  # Test softmax was applied correctly in create_multinomial_probability_tibble
  set_seed_for_R_compatibility(1)
  random_probabilities_1 <- create_multinomial_probability_tibble(
    num_classes = 3,
    num_observations = 20,
    apply_softmax = TRUE
  )
  set_seed_for_R_compatibility(1)
  random_probabilities_2 <- create_multinomial_probability_tibble(
    num_classes = 3,
    num_observations = 20,
    apply_softmax = FALSE
  ) %>% softmax()

  expect_equal(sum(random_probabilities_1),sum(random_probabilities_2))
  expect_equal(sum(random_probabilities_1), 20) # due to softmax, each row sums to 1

  expect_equal(sum(softmax_row(c(1,2,3,4))), 1)
  expect_equal(as.vector(t(softmax_row(c(1,2,3,4)))),
               c(0.03205860,0.08714432,0.23688282,0.64391426))
  expect_equal(colnames(softmax_row(c(1,2,3,4))), c("V1","V2","V3","V4"))

})
