library(cvms)
context("reconstruct_formulas()")

test_that("formulas without random effects are properly reconstructed from cross_validate results with reconstruct_formulas()",{

  data <- participant.scores

  model_formulas <- c("score~age +diagnosis",
                      "score~age*diagnosis ")


  library(groupdata2)

  data <- fold(data, k=3, cat_col="diagnosis", id_col="participant")
  cv_results <- cross_validate(data, models = model_formulas, family = "gaussian")
  cv_results <- cv_results[order(cv_results$RMSE), ]

  expect_equal(reconstruct_formulas(cv_results), c("score ~ age + diagnosis", "score ~ age * diagnosis"))
  expect_equal(reconstruct_formulas(cv_results, topn = 1), c("score ~ age + diagnosis"))

})


test_that("formulas with random effects are properly reconstructed from cross_validate results with reconstruct_formulas()",{

  data <- participant.scores

  model_formulas <- c("score~age + (1|participant) + (1|diagnosis)",
                      "score~age*diagnosis  +(1|participant)")


  library(groupdata2)

  data <- fold(data, k=2, cat_col="diagnosis", id_col="participant")
  suppressWarnings(suppressMessages({cv_results <- cross_validate(data, models = model_formulas, family = "gaussian")}))
  cv_results <- cv_results[order(cv_results$RMSE), ]
  cv_results$Random

  expect_equal(reconstruct_formulas(cv_results), c("score ~ age + (1|participant) + (1|diagnosis)",
                                                   "score ~ age * diagnosis + (1|participant)"))
  expect_equal(reconstruct_formulas(cv_results, topn = 1), c("score ~ age + (1|participant) + (1|diagnosis)"))

})
