library(cvms)
context("cross_validate() with link functions")

test_that("gaussian models with link functions with cross_validate()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # log link

  # Cross-validate the data
  CV_gauss <- cross_validate(dat, "score~diagnosis",
                        fold_cols = '.folds',
                        family='gaussian', link = 'log',
                        model_verbose=FALSE)

  expect_equal(CV_gauss$RMSE, 39.56006, tolerance=1e-3)
  expect_equal(CV_gauss$AIC, 194.6612, tolerance=1e-3)

  CV_gauss <- cross_validate(dat, "score~diagnosis+(1|session)",
                             fold_cols = '.folds',
                             family='gaussian', link = 'log',
                             model_verbose=FALSE)

  expect_equal(CV_gauss$RMSE, 39.48817, tolerance=1e-3)
  expect_equal(CV_gauss$AIC, 187.5134, tolerance=1e-3)

  # inverse link

  # Cross-validate the data
  CV_gauss <- cross_validate(dat, "score~diagnosis",
                             fold_cols = '.folds',
                             family='gaussian', link = 'inverse',
                             model_verbose=FALSE)

  expect_equal(CV_gauss$RMSE, 42.832, tolerance=1e-3)
  expect_equal(CV_gauss$AIC, 194.6612, tolerance=1e-3)

  # Cross-validate the data
  CV_gauss <- cross_validate(dat, "score~diagnosis",
                             fold_cols = '.folds',
                             family='gaussian',
                             link = 'identity',
                             model_verbose=FALSE)

  expect_equal(CV_gauss$RMSE, 16.5418, tolerance=1e-3)
  expect_equal(CV_gauss$AIC, 194.6612, tolerance=1e-3)


})


test_that("binomial models with link functions with cross_validate()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # logit link

  # Cross-validate the data
  CV_binom <- cross_validate(dat, "diagnosis~score",
                             fold_cols = '.folds',
                             family='binomial', link = 'logit',
                             model_verbose=FALSE,
                             positive=1)

  expect_equal(CV_binom$AUC, 0.7638889, tolerance=1e-3)
  expect_equal(CV_binom$Sensitivity, 0.5833333, tolerance=1e-3)

  # Cross-validate the data
  CV_binom <- cross_validate(dat, "diagnosis~score+(1|session)",
                             fold_cols = '.folds',
                             family='binomial', link = 'logit',
                             model_verbose=FALSE,
                             positive=1)

  expect_equal(CV_binom$AUC, 0.8425926, tolerance=1e-3)
  expect_equal(CV_binom$Sensitivity, 0.8333333, tolerance=1e-3)

  # probit

  # Cross-validate the data
  CV_binom <- cross_validate(dat, "diagnosis~score",
                             fold_cols = '.folds',
                             family='binomial', link = 'probit',
                             model_verbose=FALSE,
                             positive=1)

  expect_equal(CV_binom$AUC, 0.7638889, tolerance=1e-3)
  expect_equal(CV_binom$Sensitivity, 0.5833333, tolerance=1e-3)

  # Cross-validate the data
  # Throws error
  expect_error(cross_validate(dat, "diagnosis~score+(1|session)",
                             fold_cols = '.folds',
                             family='binomial', link = 'probit',
                             model_verbose=FALSE,
                             positive=1),
               regexp="PIRLS step-halvings failed to reduce deviance in pwrssUpdate")
#
#   expect_equal(CV_binom$AUC, 0.8657407, tolerance=1e-3)
#   expect_equal(CV_binom$Sensitivity, 0.8333333, tolerance=1e-3)


  # cauchit

  # Cross-validate the data
  CV_binom <- cross_validate(dat, "diagnosis~score",
                             fold_cols = '.folds',
                             family='binomial', link = 'cauchit',
                             model_verbose=FALSE,
                             positive=1)

  expect_equal(CV_binom$AUC, 0.75, tolerance=1e-3)
  expect_equal(CV_binom$Sensitivity, 0.5, tolerance=1e-3)

  # hierarchical cauchit didn't converge

})
