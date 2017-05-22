library(cvms)
context("cross_validate()")

test_that("gaussian models with cross_validate()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Cross-validate the data
  CVed <- cross_validate(dat, "score~diagnosis",
                         folds_col = '.folds',
                         family='gaussian', REML = FALSE,
                         model_verbose=FALSE)

  expect_equal(CVed$RMSE, 17.16817, tolerance=1e-3)
  expect_equal(CVed$r2m, 0.2640793, tolerance=1e-3)
  expect_equal(CVed$r2c, 0.2640793, tolerance=1e-3)
  expect_equal(CVed$AIC, 194.6904, tolerance=1e-3)
  expect_equal(CVed$AICc, 195.9963, tolerance=1e-3)
  expect_equal(CVed$BIC, 198.0243, tolerance=1e-3)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Convergence Warnings`, 0)


})

test_that("binomial models with cross_validate()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Cross-validate the data
  CVbinom <- cross_validate(dat, "diagnosis~score",
                            folds_col = '.folds',
                            family='binomial', REML = FALSE,
                            model_verbose=FALSE)

  expect_equal(CVbinom$AUC, 0.7615741, tolerance=1e-3)
  expect_equal(CVbinom$`Lower CI`, 0.5851154, tolerance=1e-3)
  expect_equal(CVbinom$`Upper CI`, 0.9380328, tolerance=1e-3)
  expect_equal(CVbinom$kappa, 0.4927536, tolerance=1e-3)
  expect_equal(CVbinom$Sensitivity, 0.5833333, tolerance=1e-3)
  expect_equal(CVbinom$Specificity, 0.8888889, tolerance=1e-3)
  expect_equal(CVbinom$`Pos Pred Value`, 0.7777778, tolerance=1e-3)
  expect_equal(CVbinom$`Neg Pred Value`, 0.7619048, tolerance=1e-3)
  expect_equal(CVbinom$F1, 0.6666667, tolerance=1e-3)
  expect_equal(CVbinom$Prevalence, 0.4, tolerance=1e-3)
  expect_equal(CVbinom$`Detection Rate`, 0.2333333, tolerance=1e-3)
  expect_equal(CVbinom$`Detection Prevalence`, 0.3, tolerance=1e-3)
  expect_equal(CVbinom$`Balanced Accuracy`, 0.7361111, tolerance=1e-3)
  expect_equal(CVbinom$Folds, 4)
  expect_equal(CVbinom$`Convergence Warnings`, 0)


})

test_that("convergence errors count works cross_validate()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  expect_warning(cross_validate(dat, "score~diagnosis*age*session + (1+diagnosis|session) + (1+diagnosis|age)",
                         folds_col = '.folds',
                         family='gaussian'), "cross_validate(): Convergence Warning:", fixed = TRUE)

})

