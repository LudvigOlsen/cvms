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
    formulas = "diagnosis~score",
    test_data = NULL,
    partitions_col = ".partitions",
    family = 'binomial',
    REML = FALSE,
    verbose = FALSE,
    positive = 1
  )
  expect_equal(Vbinom$AUC, 0.833, tolerance = 1e-3)
  expect_equal(Vbinom$`Lower CI`, 0.475, tolerance = 1e-3)
  expect_equal(Vbinom$`Upper CI`, 1, tolerance = 1e-3)
  expect_equal(Vbinom$Kappa, 0.7272727, tolerance = 1e-3)
  expect_equal(Vbinom$Sensitivity, 0.6666667, tolerance = 1e-3)
  expect_equal(Vbinom$Specificity, 1, tolerance = 1e-3)
  expect_equal(Vbinom$`Pos Pred Value`, 1, tolerance = 1e-3)
  expect_equal(Vbinom$`Neg Pred Value`, 0.8571429, tolerance = 1e-3)
  expect_equal(Vbinom$F1, 0.8, tolerance = 1e-3)
  expect_equal(Vbinom$Prevalence, 0.3333333, tolerance = 1e-3)
  expect_equal(Vbinom$`Detection Rate`, 0.2222222, tolerance = 1e-3)
  expect_equal(Vbinom$`Detection Prevalence`,
               0.2222222,
               tolerance = 1e-3)
  expect_equal(Vbinom$`Balanced Accuracy`, 0.8333333, tolerance =
                 1e-3)
  expect_equal(Vbinom$`Convergence Warnings`, 0)
  expect_equal(Vbinom$Family, 'binomial')
  expect_equal(Vbinom$Dependent, 'diagnosis')
  expect_equal(Vbinom$Fixed, 'score')

  # Enter sub tibbles
  expect_is(Vbinom$Predictions[[1]], "tbl_df")
  expect_is(Vbinom$ROC[[1]], "roc")
  expect_equal(
    colnames(Vbinom$Predictions[[1]]),
    c("Observation","Target","Prediction","Predicted Class")
  )
  expect_equal(nrow(Vbinom$Predictions[[1]]), 9)

  expect_equal(names(Vbinom$ROC[[1]]),
               c("percent", "sensitivities", "specificities", "thresholds",
                 "direction", "cases", "controls", "fun.sesp", "auc", "call",
                 "original.predictor", "original.response", "predictor", "response",
                 "levels"))

  expect_equal(Vbinom$ROC[[1]]$direction,
               "<")
  expect_equal(Vbinom$ROC[[1]]$thresholds,
               c(-Inf, 0.224265219974917, 0.426633976157444, 0.540457154631025,
                 0.648987905756078, 0.725216199854617, 0.75965587124329, 0.827264825824089,
                 0.882622758109746, Inf), tolerance = 1e-5)
  expect_equal(Vbinom$ROC[[1]]$sensitivities,
               c(1, 1, 1, 0.833333333333333, 0.666666666666667, 0.5, 0.5, 0.333333333333333,
                 0.166666666666667, 0), tolerance = 1e-5)
  expect_equal(Vbinom$ROC[[1]]$specificities,
               c(0, 0.333333333333333, 0.666666666666667, 0.666666666666667,
                 0.666666666666667, 0.666666666666667, 1, 1, 1, 1), tolerance = 1e-5)
  expect_equal(as.numeric(Vbinom$ROC[[1]]$auc),
               0.833333333333333, tolerance = 1e-5)

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
    formulas = "diagnosis~score",
    test_data = NULL,
    partitions_col = ".partitions",
    family = 'binomial',
    REML = FALSE,
    metrics = list("Accuracy" = TRUE,
                   "Lower CI" = FALSE),
    verbose = FALSE,
    positive = 1
  )

  expect_equal(Vbinom$`Balanced Accuracy`, 0.8333333,
               tolerance = 1e-3)
  expect_equal(Vbinom$Accuracy, 0.8888889,
               tolerance = 1e-3)
  expect_equal(colnames(Vbinom),
               c("Balanced Accuracy", "Accuracy", "F1", "Sensitivity", "Specificity",
                 "Pos Pred Value", "Neg Pred Value", "AUC", "Upper CI", "Kappa",
                 "MCC", "Detection Rate", "Detection Prevalence", "Prevalence",
                 "Predictions", "ROC", "Confusion Matrix", "Coefficients", "Convergence Warnings",
                 "Singular Fit Messages", "Other Warnings", "Warnings and Messages",
                 "Family", "Model", "Dependent", "Fixed")
               )
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
      formulas = "diagnosis~score + (1|session)",
      test_data = NULL,
      partitions_col = ".partitions",
      family = 'binomial',
      REML = FALSE,
      verbose = FALSE,
      positive=1
    )

  expect_equal(Vbinom$AUC, 0.764, tolerance = 1e-3)
  expect_equal(Vbinom$`Lower CI`, 0.475, tolerance = 1e-3)
  expect_equal(Vbinom$`Upper CI`, 1, tolerance = 1e-3)
  expect_equal(Vbinom$Kappa, 0.167, tolerance = 1e-3)
  expect_equal(Vbinom$Sensitivity, 0.5, tolerance = 1e-3)
  expect_equal(Vbinom$Specificity, 0.667, tolerance = 1e-3)
  expect_equal(Vbinom$`Pos Pred Value`, 0.6, tolerance = 1e-3)
  expect_equal(Vbinom$`Neg Pred Value`, 0.571, tolerance = 1e-3)
  expect_equal(Vbinom$F1, 0.545, tolerance = 1e-3)
  expect_equal(Vbinom$Prevalence, 0.5, tolerance = 1e-3)
  expect_equal(Vbinom$`Detection Rate`, 0.25, tolerance = 1e-3)
  expect_equal(Vbinom$`Detection Prevalence`,
               0.417,
               tolerance = 1e-3)
  expect_equal(Vbinom$`Balanced Accuracy`, 0.583,
               tolerance = 1e-3)
  expect_equal(Vbinom$`Convergence Warnings`, 0)
  expect_equal(Vbinom$`Singular Fit Messages`, 0)
  expect_equal(Vbinom$Family, 'binomial')
  expect_equal(Vbinom$Dependent, 'diagnosis')
  expect_equal(Vbinom$Fixed, 'score')
  expect_equal(Vbinom$Random, '(1|session)')

  # Enter sub tibbles
  expect_is(Vbinom$Predictions[[1]], "tbl_df")
  expect_is(Vbinom$ROC[[1]], "roc")
  expect_equal(
    colnames(Vbinom$Predictions[[1]]),
    c("Observation","Target","Prediction","Predicted Class")
  )
  expect_equal(nrow(Vbinom$Predictions[[1]]), 12)

  expect_equal(names(Vbinom$ROC[[1]]),
               c("percent", "sensitivities", "specificities", "thresholds",
                 "direction", "cases", "controls", "fun.sesp", "auc", "call",
                 "original.predictor", "original.response", "predictor", "response",
                 "levels"))

  expect_equal(Vbinom$ROC[[1]]$direction,
               "<")
  expect_equal(Vbinom$ROC[[1]]$thresholds,
               c(-Inf, 5.26142227038134e-11, 2.9859423313853e-08, 1.13438806464474e-07,
                 3.80808821466656e-07, 0.349577298215006, 0.833659423893193, 0.983056382137284,
                 0.998594470992238, 0.999619864886364, 0.99999933823515, Inf), tolerance = 1e-5)
  expect_equal(Vbinom$ROC[[1]]$sensitivities,
               c(1, 1, 1, 0.833333333333333, 0.833333333333333, 0.666666666666667,
                 0.666666666666667, 0.666666666666667, 0.5, 0.333333333333333,
                 0.166666666666667, 0), tolerance = 1e-5)
  expect_equal(Vbinom$ROC[[1]]$specificities,
               c(0, 0.166666666666667, 0.333333333333333, 0.333333333333333,
                 0.5, 0.5, 0.666666666666667, 0.833333333333333, 0.833333333333333,
                 1, 1, 1), tolerance = 1e-5)
  expect_equal(as.numeric(Vbinom$ROC[[1]]$auc),
               0.763888888888889, tolerance = 1e-5)

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
      formulas = "diagnosis~score",
      test_data = dat[[2]],
      family = 'binomial',
      REML = FALSE,
      verbose = FALSE,
      positive=1
    )

  expect_equal(Vbinom$AUC, 0.944, tolerance = 1e-3)
  expect_equal(Vbinom$`Lower CI`, 0.79, tolerance = 1e-3)
  expect_equal(Vbinom$`Upper CI`, 1, tolerance = 1e-3)
  expect_equal(Vbinom$Kappa, 0.7272727, tolerance = 1e-3)
  expect_equal(Vbinom$Sensitivity, 0.6666667, tolerance = 1e-3)
  expect_equal(Vbinom$Specificity, 1, tolerance = 1e-3)
  expect_equal(Vbinom$`Pos Pred Value`, 1, tolerance = 1e-3)
  expect_equal(Vbinom$`Neg Pred Value`, 0.8571429, tolerance = 1e-3)
  expect_equal(Vbinom$F1, 0.8, tolerance = 1e-3)
  expect_equal(Vbinom$Prevalence, 0.3333333, tolerance = 1e-3)
  expect_equal(Vbinom$`Detection Rate`, 0.2222222, tolerance = 1e-3)
  expect_equal(Vbinom$`Detection Prevalence`,
               0.2222222,
               tolerance = 1e-3)
  expect_equal(Vbinom$`Balanced Accuracy`, 0.8333333, tolerance =
                 1e-3)
  expect_equal(Vbinom$`Convergence Warnings`, 0)
  expect_equal(Vbinom$Family, 'binomial')
  expect_equal(Vbinom$Dependent, 'diagnosis')
  expect_equal(Vbinom$Fixed, 'score')

  # Enter sub tibbles
  expect_is(Vbinom$Predictions[[1]], "tbl_df")
  expect_is(Vbinom$ROC[[1]], "roc")
  expect_equal(
    colnames(Vbinom$Predictions[[1]]),
    c("Observation","Target","Prediction","Predicted Class")
  )
  expect_equal(nrow(Vbinom$Predictions[[1]]), 9)

  expect_equal(length(Vbinom$ROC), 1)
  expect_equal(length(Vbinom$ROC[[1]]$sensitivities), 9)
  expect_equal(Vbinom$ROC[[1]]$sensitivities,
               c(1, 1, 1, 0.833333333333333, 0.833333333333333, 0.5, 0.333333333333333,
                 0.166666666666667, 0), tolerance = 1e-5)
  expect_equal(Vbinom$ROC[[1]]$specificities,
               c(0, 0.333333333333333, 0.666666666666667, 0.666666666666667,
                 1, 1, 1, 1, 1), tolerance = 1e-5)
  expect_equal(Vbinom$ROC[[1]]$thresholds,
               c(-Inf, 0.305300064306695, 0.520681562211535,
                 0.618752367349243, 0.679450474597164, 0.734648936984941,
                 0.802650625978057, 0.848041386220925, Inf), tolerance = 1e-5)

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
      formulas = "score~diagnosis+(1|session)",
      test_data = NULL,
      partitions_col = ".partitions",
      family = 'gaussian',
      REML = FALSE,
      verbose = FALSE
    )

  expect_equal(Vgauss$RMSE, 7.75, tolerance = 1e-3)
  expect_equal(Vgauss$r2m, 0.305, tolerance = 1e-3)
  expect_equal(Vgauss$r2c, 0.749, tolerance = 1e-3)
  expect_equal(Vgauss$AIC, 149, tolerance = 1e-3)
  expect_equal(Vgauss$AICc, 152, tolerance = 1e-3)
  expect_equal(Vgauss$BIC, 152.5377, tolerance = 1e-3)
  expect_equal(Vgauss$`Convergence Warnings`, 0)
  expect_equal(Vgauss$`Singular Fit Messages`, 0)
  expect_equal(Vgauss$Family, 'gaussian')
  expect_equal(Vgauss$Dependent, 'score')
  expect_equal(Vgauss$Fixed, 'diagnosis')
  expect_equal(Vgauss$Random, '(1|session)')

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
      formulas = "score~diagnosis+(1|session)",
      test_data = NULL,
      partitions_col = ".partitions",
      family = 'gaussian',
      REML = FALSE,
      metrics = list("RMSE" = FALSE,
                     "r2m" = TRUE),
      verbose = FALSE
    )

  expect_equal(Vgauss$r2m, 0.305, tolerance = 1e-3)
  expect_equal(colnames(Vgauss),
               c("MAE","RMSLE", "r2m", "r2c", "AIC", "AICc", "BIC", "Predictions", "Coefficients",
                 "Convergence Warnings", "Singular Fit Messages", "Other Warnings",
                 "Warnings and Messages", "Family", "Model", "Dependent",
                 "Fixed", "Random"))
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
      formulas = "diagnosis~score",
      partitions_col = '.partitions',
      family = 'binomial',
      positive=1
    )
  same_model <-
    glm(diagnosis ~ score, data = dat[dat$.partitions == 1, ], family = 'binomial')
  expect_equal(validated$Model[[1]]$coefficients,
               same_model$coefficients,
               tolerance = 1e-3)
  expect_equal(validated$Model[[1]]$residuals,
               same_model$residuals,
               tolerance = 1e-3)
  expect_equal(validated$Model[[1]]$aic, same_model$aic, tolerance = 1e-3)
  expect_equal(validated$Model[[1]]$effects, same_model$effects, tolerance =
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
      formulas = "diagnosis~score+(1|session)",
      partitions_col = '.partitions',
      family = 'binomial',
      positive=1
    )
  same_model <-
    lme4::glmer(diagnosis ~ score + (1 | session),
                data = dat[dat$.partitions == 1, ],
                family = 'binomial')
  expect_equal(validated$Model[[1]]@resp, same_model@resp, tolerance = 1e-3)
  # expect_equal(validated$Model[[1]]@call, same_model@call, tolerance = 1e-3) # TODO: not working?
  expect_equal(validated$Model[[1]]@optinfo$val,
               same_model@optinfo$val,
               tolerance = 1e-3)
  expect_equal(validated$Model[[1]]@beta, same_model@beta, tolerance = 1e-3)

  expect_equal(validated$Predictions[[1]]$Target,
               c(0, 0, 0, 1, 1, 1, 1, 1, 1))
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
  expect_warning(validate(dat, formulas = c("diagnosis~."),
                          family = 'binomial',
                          partitions_col = '.partitions',
                          REML = FALSE, verbose = FALSE),
                 regexp = NA)

  # Expect no warnings
  # https://stackoverflow.com/questions/22003306/is-there-something-in-testthat-like-expect-no-warnings
  expect_warning(validate(dat, formulas = c("score~."),
                          partitions_col = '.partitions',
                          family = 'gaussian',
                          REML = FALSE, verbose = FALSE),
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
      formulas = "diagnosis~score+(1|session)+(1|participant)",
      partitions_col = '.partitions',
      family = 'binomial'
    ), "Boundary \\(Singular\\) Fit Message")

  expect_equal(validated$`Singular Fit Messages`, 1)

})

test_that("the expected errors are thrown by validate()",{


  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- participant.scores

  expect_error(strip_msg(validate(dat, dat, formulas = c("diagnosis~score", "diagnosis~age"),
                        family = 'fdsfs',
                        REML = FALSE, verbose = FALSE,
                        positive = 1)),
               strip(paste0("1 assertions failed:\n * Variable 'family': Must be element",
                      " of set\n * {'gaussian','binomial','multinomial'}, but is 'f",
                      "dsfs'.")), fixed=TRUE)

  expect_error(suppressWarnings(
    validate(
      train_data = dat,
      test_data = dplyr::sample_frac(dat, 0.2),
      formulas = c("diagnosis~score*age+(1|session)"),
      family = 'gaussian',
      REML = FALSE,
      verbose = FALSE,
      control = lme4::lmerControl(optimizer = "bobyqa",
                                  optCtrl = list(maxfun = 10)),
      err_nc = TRUE
    )
  ),
  "Model did not converge.", fixed = TRUE)

})

test_that("verbose reports the correct model functions in validate()",{


  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::partition(participant.scores,
                               p = .75,
                               cat_col = 'diagnosis',
                               id_col = 'participant')

  # Test the list of verbose messages
  # glm()
  expect_equal(evaluate_promise(validate(dat[[1]], dat[[2]],
                                         formulas = c("diagnosis~score"),
                                         family = 'binomial',
                                         REML = FALSE, verbose = TRUE,
                                         positive = 1))$messages,
               c(
                 "Will validate 1 models.\n",
                 "\n--------------------------\nvalidate(): Message:\nIn model:\ndiagnosis~score\nFor fold column:\n.partitions\nIn fold:\n2\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = c(\"bobyqa\", \"Nelder_Mead\"), calc.derivs = TRUE, use.last.params = FALSE, restart_edge = FALSE, boundary.tol = 1e-05, tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), checkConv = list(\n    check.conv.grad = list(action = \"warning\", tol = 0.001, relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : binomial, is_special_fn : TRUE\ncross_validate(): Used glm() to fit the model.'\n"
               ))

  # glmer
  expect_equal(evaluate_promise(validate(dat[[1]], dat[[2]],
                                         formulas = c("diagnosis~score+(1|session)"),
                                         family = 'binomial',
                                         REML = FALSE, verbose = TRUE,
                                         positive = 1))$messages,
               c(
                 "Will validate 1 models.\n",
                 "\n--------------------------\nvalidate(): Message:\nIn model:\ndiagnosis~score+(1|session)\nFor fold column:\n.partitions\nIn fold:\n2\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = c(\"bobyqa\", \"Nelder_Mead\"), calc.derivs = TRUE, use.last.params = FALSE, restart_edge = FALSE, boundary.tol = 1e-05, tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), checkConv = list(\n    check.conv.grad = list(action = \"warning\", tol = 0.001, relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : binomial, is_special_fn : TRUE\ncross_validate(): Used lme4::glmer() to fit the model.'\n"
               ))

  # lm
  expect_equal(evaluate_promise(validate(dat[[1]], dat[[2]],
                                         formulas = c("score~diagnosis"),
                                         family = 'gaussian',
                                         REML = FALSE, verbose = TRUE,
                                         positive = 1))$messages,
               c(
                 "Will validate 1 models.\n",
                 "\n--------------------------\nvalidate(): Message:\nIn model:\nscore~diagnosis\nFor fold column:\n.partitions\nIn fold:\n2\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"nloptwrap\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : gaussian, is_special_fn : TRUE\ncross_validate(): Used lm() to fit the model.'\n"
               ))

  # lmer
  expect_equal(evaluate_promise(validate(dat[[1]], dat[[2]],
                                         formulas = c("score~diagnosis+(1|session)"),
                                         family = 'gaussian',
                                         REML = FALSE, verbose = TRUE,
                                         positive = 1))$messages,
               c("Will validate 1 models.\n",
                 "\n--------------------------\nvalidate(): Message:\nIn model:\nscore~diagnosis+(1|session)\nFor fold column:\n.partitions\nIn fold:\n2\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"nloptwrap\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : gaussian, is_special_fn : TRUE\ncross_validate(): Used lme4::lmer() to fit the model.'\n"
               ))


})
