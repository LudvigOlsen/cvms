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

test_that("binomial model with metrics list work with validate()", {

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
    metrics = list("Accuracy" = TRUE,
                   "Lower CI" = FALSE),
    model_verbose = FALSE,
    positive = 1
  )

  Vbinom_results <- Vbinom$Results

  expect_equal(Vbinom_results$`Balanced Accuracy`, 0.8333333,
               tolerance = 1e-3)
  expect_equal(Vbinom_results$Accuracy, 0.8888889,
               tolerance = 1e-3)
  expect_equal(colnames(Vbinom_results),
               c("Balanced Accuracy", "Accuracy", "F1", "Sensitivity", "Specificity",
                 "Pos Pred Value", "Neg Pred Value", "AUC", "Upper CI", "Kappa",
                 "MCC", "Detection Rate", "Detection Prevalence", "Prevalence",
                 "Predictions", "ROC", "Confusion Matrix", "Coefficients", "Convergence Warnings",
                 "Singular Fit Messages", "Family", "Link", "Dependent", "Fixed"
               ))
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

test_that("gaussian model with metrics list works with validate()", {

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
      metrics = list("RMSE" = FALSE,
                     "r2m" = TRUE),
      model_verbose = FALSE
    )

  Vgauss_results <- Vgauss$Results

  expect_equal(Vgauss_results$r2m, 0.305, tolerance = 1e-3)
  expect_equal(colnames(Vgauss_results),
               c("MAE", "r2m", "r2c", "AIC", "AICc", "BIC", "Predictions", "Coefficients",
                 "Convergence Warnings", "Singular Fit Messages", "Family", "Link",
                 "Dependent", "Fixed", "Random"))
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

test_that("the expected errors are thrown by validate()",{


  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- participant.scores

  expect_error(validate(dat, dat, models = c("diagnosis~score","diagnosis~age"),
                       family = 'fdsfs',
                       REML = FALSE, model_verbose=FALSE,
                       positive=1),
               "Only 'gaussian' and 'binomial' families are currently allowed.", fixed=TRUE)

})

test_that("model_verbose reports the correct model functions in validate()",{


  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::partition(participant.scores, p = .75,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Test the list of verbose messages
  # glm()
  expect_equal(evaluate_promise(validate(dat[[1]], dat[[2]],
                                         models = c("diagnosis~score"),
                                         family = 'binomial',
                                         REML = FALSE, model_verbose=TRUE,
                                         positive=1))$messages,
               as.character(c(
                 "Updated model_specifics to { model_formula = , family = binomial, link = logit, control = (c(\"bobyqa\", \"Nelder_Mead\"), TRUE, FALSE, FALSE, 1e-05, 1e-07, TRUE, TRUE, list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), list(check.conv.grad = list(action = \"warning\", tol = 0.001, relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), list()), REML = FALSE, positive = 1, cutoff = 0.5, model_verbose = TRUE }. Note: If incorrect, remember to name arguments in model_specific.\n",
                 "Model function: Used glm()\n"
               )))

  # glmer
  expect_equal(evaluate_promise(validate(dat[[1]], dat[[2]],
                                         models = c("diagnosis~score+(1|session)"),
                                         family = 'binomial',
                                         REML = FALSE, model_verbose = TRUE,
                                         positive = 1))$messages,
               as.character(c(
                 "Updated model_specifics to { model_formula = , family = binomial, link = logit, control = (c(\"bobyqa\", \"Nelder_Mead\"), TRUE, FALSE, FALSE, 1e-05, 1e-07, TRUE, TRUE, list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), list(check.conv.grad = list(action = \"warning\", tol = 0.001, relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), list()), REML = FALSE, positive = 1, cutoff = 0.5, model_verbose = TRUE }. Note: If incorrect, remember to name arguments in model_specific.\n",
                 "Model function: Used lme4::glmer()\n"
               )))

  # lm
  expect_equal(evaluate_promise(validate(dat[[1]], dat[[2]],
                                         models = c("score~diagnosis"),
                                         family='gaussian',
                                         REML = FALSE, model_verbose=TRUE,
                                         positive=1))$messages,
               as.character(c(
                 "Updated model_specifics to { model_formula = , family = gaussian, link = identity, control = (nloptwrap, TRUE, 1e-05, TRUE, FALSE, list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), list()), REML = FALSE, positive = 1, cutoff = 0.5, model_verbose = TRUE }. Note: If incorrect, remember to name arguments in model_specific.\n",
                 "Model function: Used lm()\n"
               )))

  # glmer
  expect_equal(evaluate_promise(validate(dat[[1]], dat[[2]],
                                         models = c("score~diagnosis+(1|session)"),
                                         family = 'gaussian',
                                         REML = FALSE, model_verbose = TRUE,
                                         positive = 1))$messages,
               as.character(c(
                 "Updated model_specifics to { model_formula = , family = gaussian, link = identity, control = (nloptwrap, TRUE, 1e-05, TRUE, FALSE, list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), list()), REML = FALSE, positive = 1, cutoff = 0.5, model_verbose = TRUE }. Note: If incorrect, remember to name arguments in model_specific.\n",
                 "Model function: Used lme4::lmer()\n"
               )))


})
