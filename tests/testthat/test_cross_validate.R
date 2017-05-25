library(cvms)
context("cross_validate()")

# NOTICE:
# Numbers tested are the results I got and not "what should be"
# This will allow me to see if something changes, but it shouldn't give false confidence.


test_that("binomial models work with cross_validate()",{


  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')


  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                folds_col = '.folds', family='binomial',
                                REML = FALSE, model_verbose=FALSE)

  expect_equal(CVbinomlist$AUC, c(0.7615741,0.8333333), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.5851154,0.6841541), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9380328,0.9825126), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4927536, -0.3636364), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8888889,0.6666667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7777778,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7619048,0.5000000), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3,0.2), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111,0.3333333), tolerance=1e-3)
  expect_equal(CVbinomlist$Folds, c(4,4))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlist$Family, c('binomial','binomial'))
  expect_equal(CVbinomlist$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlist$Fixed, c('score','age'))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("prediction","predicted_class","target"))
  expect_equal(colnames(CVbinomlist$ROC[[1]]), c("sensitivities","specificities"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlist$ROC[[1]]),29)


})

test_that("binomial models work with cross_validate()",{


  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')


  CVbinomlistrand <- cross_validate(dat,
                                    models = c("diagnosis~score + (1|session)","diagnosis~age + (1|session)"),
                                    folds_col = '.folds',
                                    family='binomial',
                                    REML = FALSE,
                                    link = NULL,
                                    model_verbose=FALSE)

  expect_equal(CVbinomlistrand$AUC, c(0.8611111,0.8333333), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Lower CI`, c(0.7103334,0.6841541), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Upper CI`, c(1.0000000,0.9825126), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Kappa, c(0.6575342, -0.3636364), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Sensitivity, c(0.8333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Specificity, c(0.8333333,0.6666667), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Pos Pred Value`, c(0.7692308,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Neg Pred Value`, c(0.8823529,0.5000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$F1, c(0.8, NA), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Detection Rate`, c(0.3333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Detection Prevalence`, c(0.4333333,0.2), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Balanced Accuracy`, c(0.8333333,0.3333333), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Folds, c(4,4))
  expect_equal(CVbinomlistrand$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlistrand$Family, c('binomial','binomial'))
  expect_equal(CVbinomlistrand$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlistrand$Fixed, c('score','age'))
  expect_equal(CVbinomlistrand$Random, c('1|session','1|session'))


  # Enter sub tibbles
  expect_is(CVbinomlistrand$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlistrand$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlistrand$Predictions[[1]]), c("prediction","predicted_class","target"))
  expect_equal(colnames(CVbinomlistrand$ROC[[1]]), c("sensitivities","specificities"))
  expect_equal(nrow(CVbinomlistrand$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlistrand$ROC[[1]]),27) # Why?

  expect_is(CVbinomlistrand$Predictions[[2]], "tbl_df")
  expect_is(CVbinomlistrand$ROC[[2]], "tbl_df")
  expect_equal(colnames(CVbinomlistrand$Predictions[[2]]), c("prediction","predicted_class","target"))
  expect_equal(colnames(CVbinomlistrand$ROC[[2]]), c("sensitivities","specificities"))
  expect_equal(nrow(CVbinomlistrand$Predictions[[2]]),30)
  expect_equal(nrow(CVbinomlistrand$ROC[[2]]),11) # Why?

})


