library(cvms)
context("validate()")

# NOTICE:
# Numbers tested are the results I got and not "what should be"
# This will allow me to see if something changes, but it shouldn't give false confidence.


test_that("binomial model work with validate()", {

  # skip_test_if_old_R_version()

  # Load data and partition it
  set_seed_for_R_compatibility(2)
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
    model_verbose = FALSE,
    positive = 1
  )

  Vbinom_results <- Vbinom$Results

  expect_equal(Vbinom_results$AUC, 0.833, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Lower CI`, 0.475, tolerance = 1e-3)
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
    c("Fold Column","Fold","Target","Prediction","Predicted Class")
  )
  expect_equal(colnames(Vbinom_results$ROC[[1]]),
               c("Sensitivities", "Specificities"))
  expect_equal(nrow(Vbinom_results$Predictions[[1]]), 9)
  expect_equal(nrow(Vbinom_results$ROC[[1]]), 10)

})

test_that("binomial mixed model work with validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(7)
  dat <- groupdata2::partition(
    participant.scores,
    p = 0.7,
    cat_col = 'diagnosis',
    id_col = 'participant',
    list_out = FALSE
  )

  # Making sure the partitioning is not the error
  expect_equal(dat$.partitions, factor(c(2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,1,1,1,1,1,1,2,2,2)))

  Vbinom <-
    validate(
      train_data = dat,
      models = "diagnosis~score + (1|session)",
      test_data = NULL,
      partitions_col = ".partitions",
      family = 'binomial',
      REML = FALSE,
      model_verbose = FALSE,
      positive=1
    )

  Vbinom_results <- Vbinom$Results

  expect_equal(Vbinom_results$AUC, 0.764, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Lower CI`, 0.475, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Upper CI`, 1, tolerance = 1e-3)
  expect_equal(Vbinom_results$Kappa, 0.167, tolerance = 1e-3)
  expect_equal(Vbinom_results$Sensitivity, 0.5, tolerance = 1e-3)
  expect_equal(Vbinom_results$Specificity, 0.667, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Pos Pred Value`, 0.6, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Neg Pred Value`, 0.571, tolerance = 1e-3)
  expect_equal(Vbinom_results$F1, 0.545, tolerance = 1e-3)
  expect_equal(Vbinom_results$Prevalence, 0.5, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Detection Rate`, 0.25, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Detection Prevalence`,
               0.417,
               tolerance = 1e-3)
  expect_equal(Vbinom_results$`Balanced Accuracy`, 0.583,
               tolerance = 1e-3)
  expect_equal(Vbinom_results$`Convergence Warnings`, 0)
  expect_equal(Vbinom_results$`Singular Fit Messages`, 0)
  expect_equal(Vbinom_results$Family, 'binomial')
  expect_equal(Vbinom_results$Dependent, 'diagnosis')
  expect_equal(Vbinom_results$Fixed, 'score')
  expect_equal(Vbinom_results$Random, '(1|session)')

  # Enter sub tibbles
  expect_is(Vbinom_results$Predictions[[1]], "tbl_df")
  expect_is(Vbinom_results$ROC[[1]], "tbl_df")
  expect_equal(
    colnames(Vbinom_results$Predictions[[1]]),
    c("Fold Column","Fold","Target","Prediction","Predicted Class")
  )
  expect_equal(colnames(Vbinom_results$ROC[[1]]),
               c("Sensitivities", "Specificities"))
  expect_equal(nrow(Vbinom_results$Predictions[[1]]), 12)
  expect_equal(nrow(Vbinom_results$ROC[[1]]), 12)

})


test_that("binomial model work with test_data in validate()", {

  # skip_test_if_old_R_version()

  # Load data and partition it
  set_seed_for_R_compatibility(1)
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
      model_verbose = FALSE,
      positive=1
    )

  Vbinom_results <- Vbinom$Results

  expect_equal(Vbinom_results$AUC, 0.944, tolerance = 1e-3)
  expect_equal(Vbinom_results$`Lower CI`, 0.79, tolerance = 1e-3)
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
    c("Fold Column","Fold","Target","Prediction","Predicted Class")
  )
  expect_equal(colnames(Vbinom_results$ROC[[1]]),
               c("Sensitivities", "Specificities"))
  expect_equal(nrow(Vbinom_results$Predictions[[1]]), 9)
  expect_equal(nrow(Vbinom_results$ROC[[1]]), 9)

})



test_that("gaussian model with validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(4)

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

  expect_equal(Vgauss_results$RMSE, 7.75, tolerance = 1e-3)
  expect_equal(Vgauss_results$r2m, 0.305, tolerance = 1e-3)
  expect_equal(Vgauss_results$r2c, 0.749, tolerance = 1e-3)
  expect_equal(Vgauss_results$AIC, 149, tolerance = 1e-3)
  expect_equal(Vgauss_results$AICc, 152, tolerance = 1e-3)
  expect_equal(Vgauss_results$BIC, 152.5377, tolerance = 1e-3)
  expect_equal(Vgauss_results$`Convergence Warnings`, 0)
  expect_equal(Vgauss_results$`Singular Fit Messages`, 0)
  expect_equal(Vgauss_results$Family, 'gaussian')
  expect_equal(Vgauss_results$Dependent, 'score')
  expect_equal(Vgauss_results$Fixed, 'diagnosis')
  expect_equal(Vgauss_results$Random, '(1|session)')

})


test_that("Right glm model used in validate()", {

  # skip_test_if_old_R_version()

  # Create data that should be easy to model
  set_seed_for_R_compatibility(7)

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
      family = 'binomial',
      positive=1
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

  # skip_test_if_old_R_version()

  # Create data that should be easy to model
  set_seed_for_R_compatibility(7)

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
      family = 'binomial',
      positive=1
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


test_that("model using dot in formula ( y ~ . ) works with validate()",{

  # skip_test_if_old_R_version()

  # We wish to test if using the dot "y~." method in the model formula
  # correctly leaves out .folds column.

  # Create data that should be easy to model
  set_seed_for_R_compatibility(7)

  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = 'diagnosis',
    id_col = 'participant',
    list_out = FALSE
  ) %>%
    dplyr::select(-c(participant, session))


  # Expect no warnings
  # https://stackoverflow.com/questions/22003306/is-there-something-in-testthat-like-expect-no-warnings
  expect_warning(validate(dat, models = c("diagnosis~."),
                          family='binomial',
                          partitions_col = '.partitions',
                          REML = FALSE, model_verbose=FALSE),
                 regexp = NA)

  # Expect no warnings
  # https://stackoverflow.com/questions/22003306/is-there-something-in-testthat-like-expect-no-warnings
  expect_warning(validate(dat, models = c("score~."),
                                partitions_col = '.partitions',
                                family='gaussian',
                                REML = FALSE, model_verbose=FALSE),
                 regexp = NA)


})

test_that("Singular fit messages counted in validate()", {

  # skip_test_if_old_R_version()

  # Create data that should be easy to model
  set_seed_for_R_compatibility(7)

  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = 'diagnosis',
    id_col = 'participant',
    list_out = FALSE
  )

  expect_message(validated <-
    validate(
      train_data = dat,
      models = "diagnosis~score+(1|session)+(1|participant)",
      partitions_col = '.partitions',
      family = 'binomial'
    ), "Boundary \\(Singular\\) Fit Message")

  expect_equal(validated$Results$`Singular Fit Messages`, 1)

})

