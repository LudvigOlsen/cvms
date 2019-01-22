library(cvms)
context("validate()")

# NOTICE:
# Numbers tested are the results I got and not "what should be"
# This will allow me to see if something changes, but it shouldn't give false confidence.


test_that("binomial model work with validate()", {
  # Load data and partition it
  set.seed(1)
  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = 'diagnosis',
    id_col = 'participant',
    list_out = FALSE
  )


  Vbinom <- validate(
    train_data = dat,
    models = "diagnosis~score",
    test_data = NULL,
    partitions_col = ".partitions",
    family = 'binomial',
    REML = FALSE,
    model_verbose = FALSE
  )

  Vbinom_results <- Vbinom$Results

  expect_equal(Vbinom_results$AUC, 0.9444444, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Lower CI`, 0.7904551, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Upper CI`, 1, tolerance = 1e-3)
  expect_equal(Vbinom_results$Kappa, 0.7272727, tolerance = 1e-3)
  expect_equal(Vbinom_results$Sensitivity, 0.6666667, tolerance = 1e-3)
  expect_equal(Vbinom_results$Specificity, 1, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Pos Pred Value`, 1, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Neg Pred Value`, 0.8571429, tolerance = 1e-3)
  expect_equal(Vbinom_results$F1, 0.8, tolerance = 1e-3)
  expect_equal(Vbinom_results$Prevalence, 0.3333333, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Detection Rate`, 0.2222222, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Detection Prevalence`,
               0.2222222,
               tolerance = 1e-3)
  expect_equal(Vbinom_results$`Balanced Accuracy`, 0.8333333, tolerance =
                 1e-3)
  expect_equal(Vbinom_results$`Convergence Warnings`, 0)
  expect_equal(Vbinom_results$Family, 'binomial')
  expect_equal(Vbinom_results$Dependent, 'diagnosis')
  expect_equal(Vbinom_results$Fixed, 'score')

  # Enter sub tibbles
  expect_is(Vbinom_results$Predictions[[1]], "tbl_df")
  expect_is(Vbinom_results$ROC[[1]], "tbl_df")
  expect_equal(
    colnames(Vbinom_results$Predictions[[1]]),
    c("prediction", "predicted_class", "target")
  )
  expect_equal(colnames(Vbinom_results$ROC[[1]]),
               c("sensitivities", "specificities"))
  expect_equal(nrow(Vbinom_results$Predictions[[1]]), 9)
  expect_equal(nrow(Vbinom_results$ROC[[1]]), 9)

})

test_that("binomial mixed model work with validate()", {
  # Load data and fold it
  set.seed(3)
  dat <- groupdata2::partition(
    participant.scores,
    p = 0.7,
    cat_col = 'diagnosis',
    id_col = 'participant',
    list_out = FALSE
  )

  Vbinom <-
    validate(
      train_data = dat,
      models = "diagnosis~score + (1|session)",
      test_data = NULL,
      partitions_col = ".partitions",
      family = 'binomial',
      REML = FALSE,
      model_verbose = FALSE
    )

  Vbinom_results <- Vbinom$Results

  expect_equal(Vbinom_results$AUC, 0.9444444, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Lower CI`, 0.8156077, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Upper CI`, 1, tolerance = 1e-3)
  expect_equal(Vbinom_results$Kappa, 0.3333333, tolerance = 1e-3)
  expect_equal(Vbinom_results$Sensitivity, 0.3333333, tolerance = 1e-3)
  expect_equal(Vbinom_results$Specificity, 1, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Pos Pred Value`, 1, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Neg Pred Value`, 0.6, tolerance = 1e-3)
  expect_equal(Vbinom_results$F1, 0.5, tolerance = 1e-3)
  expect_equal(Vbinom_results$Prevalence, 0.5, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Detection Rate`, 0.1666667, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Detection Prevalence`,
               0.1666667,
               tolerance = 1e-3)
  expect_equal(Vbinom_results$`Balanced Accuracy`, 0.6666667, tolerance =
                 1e-3)
  expect_equal(Vbinom_results$`Convergence Warnings`, 0)
  expect_equal(Vbinom_results$Family, 'binomial')
  expect_equal(Vbinom_results$Dependent, 'diagnosis')
  expect_equal(Vbinom_results$Fixed, 'score')
  expect_equal(Vbinom_results$Random, '1|session')

  # Enter sub tibbles
  expect_is(Vbinom_results$Predictions[[1]], "tbl_df")
  expect_is(Vbinom_results$ROC[[1]], "tbl_df")
  expect_equal(
    colnames(Vbinom_results$Predictions[[1]]),
    c("prediction", "predicted_class", "target")
  )
  expect_equal(colnames(Vbinom_results$ROC[[1]]),
               c("sensitivities", "specificities"))
  expect_equal(nrow(Vbinom_results$Predictions[[1]]), 12)
  expect_equal(nrow(Vbinom_results$ROC[[1]]), 12)

})


test_that("binomial model work with test_data in validate()", {
  # Load data and partition it
  set.seed(1)
  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = 'diagnosis',
    id_col = 'participant',
    list_out = TRUE
  )

  Vbinom <-
    validate(
      train_data = dat[[1]],
      models = "diagnosis~score",
      test_data = dat[[2]],
      family = 'binomial',
      REML = FALSE,
      model_verbose = FALSE
    )

  Vbinom_results <- Vbinom$Results

  expect_equal(Vbinom_results$AUC, 0.9444444, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Lower CI`, 0.7904551, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Upper CI`, 1, tolerance = 1e-3)
  expect_equal(Vbinom_results$Kappa, 0.7272727, tolerance = 1e-3)
  expect_equal(Vbinom_results$Sensitivity, 0.6666667, tolerance = 1e-3)
  expect_equal(Vbinom_results$Specificity, 1, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Pos Pred Value`, 1, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Neg Pred Value`, 0.8571429, tolerance = 1e-3)
  expect_equal(Vbinom_results$F1, 0.8, tolerance = 1e-3)
  expect_equal(Vbinom_results$Prevalence, 0.3333333, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Detection Rate`, 0.2222222, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Detection Prevalence`,
               0.2222222,
               tolerance = 1e-3)
  expect_equal(Vbinom_results$`Balanced Accuracy`, 0.8333333, tolerance =
                 1e-3)
  expect_equal(Vbinom_results$`Convergence Warnings`, 0)
  expect_equal(Vbinom_results$Family, 'binomial')
  expect_equal(Vbinom_results$Dependent, 'diagnosis')
  expect_equal(Vbinom_results$Fixed, 'score')

  # Enter sub tibbles
  expect_is(Vbinom_results$Predictions[[1]], "tbl_df")
  expect_is(Vbinom_results$ROC[[1]], "tbl_df")
  expect_equal(
    colnames(Vbinom_results$Predictions[[1]]),
    c("prediction", "predicted_class", "target")
  )
  expect_equal(colnames(Vbinom_results$ROC[[1]]),
               c("sensitivities", "specificities"))
  expect_equal(nrow(Vbinom_results$Predictions[[1]]), 9)
  expect_equal(nrow(Vbinom_results$ROC[[1]]), 9)

})



test_that("gaussian model with cross_validate()", {
  # Load data and fold it
  set.seed(1)

  dat <- groupdata2::partition(
    participant.scores,
    p = 0.7,
    cat_col = 'diagnosis',
    id_col = 'participant',
    list_out = FALSE
  )

  Vgauss <-
    validate(
      train_data = dat,
      models = "score~diagnosis+(1|session)",
      test_data = NULL,
      partitions_col = ".partitions",
      link = NULL,
      family = 'gaussian',
      REML = FALSE,
      model_verbose = FALSE
    )

  Vgauss_results <- Vgauss$Results

  expect_equal(Vgauss_results$RMSE, 8.08783, tolerance = 1e-3)
  expect_equal(Vgauss_results$r2m, 0.1658105, tolerance = 1e-3)
  expect_equal(Vgauss_results$r2c, 0.7674236, tolerance = 1e-3)
  expect_equal(Vgauss_results$AIC, 147.728, tolerance = 1e-3)
  expect_equal(Vgauss_results$AICc, 150.8049, tolerance = 1e-3)
  expect_equal(Vgauss_results$BIC, 151.2895, tolerance = 1e-3)
  expect_equal(Vgauss_results$`Convergence Warnings`, 0)
  expect_equal(Vgauss_results$Family, 'gaussian')
  expect_equal(Vgauss_results$Dependent, 'score')
  expect_equal(Vgauss_results$Fixed, 'diagnosis')
  expect_equal(Vgauss_results$Random, '1|session')

})


test_that("Right glm model used in validate()", {
  # Create data that should be easy to model
  set.seed(7)

  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = 'diagnosis',
    id_col = 'participant',
    list_out = FALSE
  )


  validated <-
    validate(
      train_data = dat,
      models = "diagnosis~score",
      partitions_col = '.partitions',
      family = 'binomial'
    )
  same_model <-
    glm(diagnosis ~ score, data = dat[dat$.partitions == 1, ], family = 'binomial')
  expect_equal(validated$Models[[1]]$coefficients,
               same_model$coefficients,
               tolerance = 1e-3)
  expect_equal(validated$Models[[1]]$residuals,
               same_model$residuals,
               tolerance = 1e-3)
  expect_equal(validated$Models[[1]]$aic, same_model$aic, tolerance = 1e-3)
  expect_equal(validated$Models[[1]]$effects, same_model$effects, tolerance =
                 1e-3)

})

test_that("Right glmer model used in validate()", {
  # Create data that should be easy to model
  set.seed(7)

  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = 'diagnosis',
    id_col = 'participant',
    list_out = FALSE
  )


  validated <-
    validate(
      train_data = dat,
      models = "diagnosis~score+(1|session)",
      partitions_col = '.partitions',
      family = 'binomial'
    )
  same_model <-
    lme4::glmer(diagnosis ~ score + (1 | session),
                data = dat[dat$.partitions == 1, ],
                family = 'binomial')
  expect_equal(validated$Models[[1]]@resp, same_model@resp, tolerance = 1e-3)
  # expect_equal(validated$Models[[1]]@call, same_model@call, tolerance = 1e-3) # TODO: not working?
  expect_equal(validated$Models[[1]]@optinfo$val,
               same_model@optinfo$val,
               tolerance = 1e-3)
  expect_equal(validated$Models[[1]]@beta, same_model@beta, tolerance = 1e-3)

})
