library(cvms)
context("validate()")

# NOTICE:
# Numbers tested are the results I got and not "what should be"
# This will allow me to see if something changes, but it shouldn't give false confidence.


test_that("binomial model work with validate()", {

  # skip_test_if_old_R_version()

  # Load data and partition it
  xpectr::set_test_seed(2)
  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  Vbinom <- validate(
    train_data = dat,
    formulas = "diagnosis~score",
    test_data = NULL,
    partitions_col = ".partitions",
    family = "binomial",
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
    tolerance = 1e-3
  )
  expect_equal(Vbinom$`Balanced Accuracy`, 0.8333333,
    tolerance =
      1e-3
  )
  expect_equal(Vbinom$`Convergence Warnings`, 0)
  expect_equal(Vbinom$Dependent, "diagnosis")
  expect_equal(Vbinom$Fixed, "score")

  # Enter sub tibbles
  expect_is(Vbinom$Predictions[[1]], "tbl_df")
  expect_is(Vbinom$ROC[[1]], "roc")
  expect_equal(
    colnames(Vbinom$Predictions[[1]]),
    c("Observation", "Target", "Prediction", "Predicted Class")
  )
  expect_equal(nrow(Vbinom$Predictions[[1]]), 9)

  expect_equal(
    names(Vbinom$ROC[[1]]),
    c(
      "percent", "sensitivities", "specificities", "thresholds",
      "direction", "cases", "controls", "fun.sesp", "auc", "call",
      "original.predictor", "original.response", "predictor", "response",
      "levels"
    )
  )

  expect_equal(
    Vbinom$ROC[[1]]$direction,
    ">"
  )
  expect_equal(
    Vbinom$ROC[[1]]$thresholds,
    c(Inf, 0.882622758109746, 0.827264825824089, 0.75965587124329,
    0.725216199854617, 0.648987905756078, 0.540457154631025, 0.426633976157444,
    0.224265219974917, -Inf),
    tolerance = 1e-5
  )
  expect_equal(
    Vbinom$ROC[[1]]$sensitivities,
    c(1, 1, 1, 1, 0.666666666666667, 0.666666666666667, 0.666666666666667,
    0.666666666666667, 0.333333333333333, 0),
    tolerance = 1e-5
  )
  expect_equal(
    Vbinom$ROC[[1]]$specificities,
    c(0, 0.166666666666667, 0.333333333333333, 0.5, 0.5, 0.666666666666667,
    0.833333333333333, 1, 1, 1),
    tolerance = 1e-5
  )
  expect_equal(as.numeric(Vbinom$ROC[[1]]$auc),
    0.833333333333333,
    tolerance = 1e-5
  )

  # Test Process
  expect_true(
    as.character(Vbinom$Process[[1]]) %in%
    paste0("---\nProcess Information\n---\nTarget column: target\nPredi",
           "ction column: prediction\nFamily / type: Binomial\nClasses: ",
           "0, 1\nPositive class: 0\nCutoff: 0.5\nProbabilities are of c",
           "lass: 1\nProbabilities < 0.5 are considered: 0\nProbabilitie",
           "s >= 0.5 are considered: 1\nLocale used when sorting class l",
           "evels (LC_ALL): \n  ",
           c("en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8",
             "C/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8"),
           "\nTarget counts: total=9, 0=3, 1=6\nPro",
           "bability summary: mean: 0.615, median: 0.719, range: [0.097,",
           " 0.899], SD: 0.262, IQR: 0.286\n---"))

})

test_that("binomial model with metrics list work with validate()", {

  testthat::skip_on_cran()

  # Load data and partition it
  xpectr::set_test_seed(2)
  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  Vbinom <- validate(
    train_data = dat,
    formulas = "diagnosis~score",
    test_data = NULL,
    partitions_col = ".partitions",
    family = "binomial",
    REML = FALSE,
    metrics = list(
      "Accuracy" = TRUE,
      "Lower CI" = FALSE
    ),
    verbose = FALSE,
    positive = 1
  )

  expect_equal(Vbinom$`Balanced Accuracy`, 0.8333333,
    tolerance = 1e-3
  )
  expect_equal(Vbinom$Accuracy, 0.8888889,
    tolerance = 1e-3
  )
  expect_equal(
    colnames(Vbinom),
    c(
      "Fixed", "Balanced Accuracy", "Accuracy", "F1", "Sensitivity", "Specificity",
      "Pos Pred Value", "Neg Pred Value", "AUC", "Upper CI", "Kappa",
      "MCC", "Detection Rate", "Detection Prevalence", "Prevalence",
      "Predictions", "ROC", "Confusion Matrix", "Coefficients", "Convergence Warnings",
      "Singular Fit Messages", "Other Warnings", "Warnings and Messages",
      "Process", "Model", "Dependent"
    )
  )
})


test_that("binomial mixed model work with validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(7)
  dat <- groupdata2::partition(
    participant.scores,
    p = 0.7,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  # Making sure the partitioning is not the error
  expect_equal(
    dat$.partitions,
    factor(c(2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1,
             1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2)))

  Vbinom <-
    validate(
      train_data = dat,
      formulas = "diagnosis~score + (1|session)",
      test_data = NULL,
      partitions_col = ".partitions",
      family = "binomial",
      REML = FALSE,
      verbose = FALSE,
      positive = 1
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
    tolerance = 1e-3
  )
  expect_equal(Vbinom$`Balanced Accuracy`, 0.583,
    tolerance = 1e-3
  )
  expect_equal(Vbinom$`Convergence Warnings`, 0)
  expect_equal(Vbinom$`Singular Fit Messages`, 0)
  expect_equal(Vbinom$Dependent, "diagnosis")
  expect_equal(Vbinom$Fixed, "score")
  expect_equal(Vbinom$Random, "(1|session)")

  # Enter sub tibbles
  expect_is(Vbinom$Predictions[[1]], "tbl_df")
  expect_is(Vbinom$ROC[[1]], "roc")
  expect_equal(
    colnames(Vbinom$Predictions[[1]]),
    c("Observation", "Target", "Prediction", "Predicted Class")
  )
  expect_equal(nrow(Vbinom$Predictions[[1]]), 12)

  expect_equal(
    names(Vbinom$ROC[[1]]),
    c("percent", "sensitivities", "specificities", "thresholds",
      "direction", "cases", "controls", "fun.sesp", "auc", "call",
      "original.predictor", "original.response", "predictor", "response",
      "levels"
    )
  )

  expect_equal(
    Vbinom$ROC[[1]]$direction,
    ">"
  )
  expect_equal(
    Vbinom$ROC[[1]]$thresholds,
    c(Inf, 0.99999933823515, 0.999619864886364, 0.998594470992238,
    0.983056382137284, 0.833659423893193, 0.349577298215006, 3.80808821466656e-07,
    1.13438806464474e-07, 2.9859423313853e-08, 5.26142227038134e-11,
    -Inf),
    tolerance = 1e-5
  )
  expect_equal(
    Vbinom$ROC[[1]]$sensitivities,
    c(1, 1, 1, 0.833333333333333, 0.833333333333333, 0.666666666666667,
    0.5, 0.5, 0.333333333333333, 0.333333333333333, 0.166666666666667,
    0),
    tolerance = 1e-5
  )
  expect_equal(
    Vbinom$ROC[[1]]$specificities,
    c(0, 0.166666666666667, 0.333333333333333, 0.5, 0.666666666666667,
    0.666666666666667, 0.666666666666667, 0.833333333333333, 0.833333333333333,
    1, 1, 1),
    tolerance = 1e-5
  )
  expect_equal(as.numeric(Vbinom$ROC[[1]]$auc),
    0.763888888888889,
    tolerance = 1e-5
  )

  # Test Process
  expect_true(
    as.character(Vbinom$Process[[1]]) %in%
      paste0("---\nProcess Information\n---\nTarget column: target\nPredi",
             "ction column: prediction\nFamily / type: Binomial\nClasses: ",
             "0, 1\nPositive class: 0\nCutoff: 0.5\nProbabilities are of c",
             "lass: 1\nProbabilities < 0.5 are considered: 0\nProbabilitie",
             "s >= 0.5 are considered: 1\nLocale used when sorting class l",
             "evels (LC_ALL): \n  ",
             c("en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8",
               "C/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8"),
             "\nTarget counts: total=12, 0=6, 1=6\nPro",
             "bability summary: mean: 0.555, median: 0.834, range: [0, 1], ",
             "SD: 0.497, IQR: 0.999\n---"))

})


test_that("binomial model work with test_data in validate()", {

  testthat::skip_on_cran()

  # Load data and partition it
  xpectr::set_test_seed(1)
  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = TRUE
  )

  Vbinom <-
    validate(
      train_data = dat[[1]],
      formulas = "diagnosis~score",
      test_data = dat[[2]],
      family = "binomial",
      REML = FALSE,
      verbose = FALSE,
      positive = 1
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
    tolerance = 1e-3
  )
  expect_equal(Vbinom$`Balanced Accuracy`, 0.8333333,
    tolerance =
      1e-3
  )
  expect_equal(Vbinom$`Convergence Warnings`, 0)
  expect_equal(Vbinom$Dependent, "diagnosis")
  expect_equal(Vbinom$Fixed, "score")

  # Enter sub tibbles
  expect_is(Vbinom$Predictions[[1]], "tbl_df")
  expect_is(Vbinom$ROC[[1]], "roc")
  expect_equal(
    colnames(Vbinom$Predictions[[1]]),
    c("Observation", "Target", "Prediction", "Predicted Class")
  )
  expect_equal(nrow(Vbinom$Predictions[[1]]), 9)

  expect_equal(length(Vbinom$ROC), 1)
  expect_equal(length(Vbinom$ROC[[1]]$sensitivities), 9)
  expect_equal(
    Vbinom$ROC[[1]]$sensitivities,
    c(1, 1, 1, 1, 1, 0.666666666666667, 0.666666666666667, 0.333333333333333, 0),
    tolerance = 1e-5
  )
  expect_equal(
    Vbinom$ROC[[1]]$specificities,
    c(0, 0.166666666666667, 0.333333333333333, 0.5, 0.833333333333333,
    0.833333333333333, 1, 1, 1),
    tolerance = 1e-5
  )
  expect_equal(
    Vbinom$ROC[[1]]$thresholds,
    c(Inf, 0.848041386220925, 0.802650625978057, 0.734648936984941,
    0.679450474597164, 0.618752367349243, 0.520681562211535, 0.305300064306695,
    -Inf),
    tolerance = 1e-5
  )
})



test_that("gaussian model with validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(4)

  dat <- groupdata2::partition(
    participant.scores,
    p = 0.7,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  Vgauss <-
    validate(
      train_data = dat,
      formulas = "score~diagnosis+(1|session)",
      test_data = NULL,
      partitions_col = ".partitions",
      family = "gaussian",
      metrics = list("r2m" = TRUE, "r2c" = TRUE),
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
  expect_equal(Vgauss$Dependent, "score")
  expect_equal(Vgauss$Fixed, "diagnosis")
  expect_equal(Vgauss$Random, "(1|session)")

  expect_true(
    as.character(Vgauss$Process[[1]]) %in%
    paste0("---\nProcess Information\n---\nTarget column: target\nPredi",
           "ction column: prediction\nFamily / type: Gaussian\nTarget su",
           "mmary: mean: 37.417, median: 37.5, range: [10, 67], SD: 18.7",
           "01, IQR: 23\nPrediction summary: mean: 43.417, median: 42.80",
           "7, range: [16.173, 69.441], SD: 17.635, IQR: 22.5\nLocale (L",
           "C_ALL): \n  ",
           c("en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8",
             "C/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8"),
           "\n---"))
})

test_that("gaussian model with metrics list works with validate()", {

  testthat::skip_on_cran()

  # Load data and fold it
  xpectr::set_test_seed(4)

  dat <- groupdata2::partition(
    participant.scores,
    p = 0.7,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  Vgauss <-
    validate(
      train_data = dat,
      formulas = "score~diagnosis+(1|session)",
      test_data = NULL,
      partitions_col = ".partitions",
      family = "gaussian",
      REML = FALSE,
      metrics = list(
        "RMSE" = FALSE,
        "r2m" = TRUE
      ),
      verbose = FALSE
    )

  expect_equal(Vgauss$r2m, 0.305, tolerance = 1e-3)
  expect_equal(
    colnames(Vgauss),
    c("Fixed", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE", "r2m",
    "AIC", "AICc", "BIC", "Predictions", "Coefficients", "Convergence Warnings",
    "Singular Fit Messages", "Other Warnings", "Warnings and Messages",
    "Process", "Model", "Dependent", "Random")
  )
})


test_that("Right glm model used in validate()", {

  # skip_test_if_old_R_version()

  # Create data that should be easy to model
  xpectr::set_test_seed(7)

  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  validated <-
    validate(
      train_data = dat,
      formulas = "diagnosis~score",
      partitions_col = ".partitions",
      family = "binomial",
      positive = 1
    )
  same_model <-
    glm(diagnosis ~ score, data = dat[dat$.partitions == 1, ], family = "binomial")
  expect_equal(validated$Model[[1]]$coefficients,
    same_model$coefficients,
    tolerance = 1e-3
  )
  expect_equal(validated$Model[[1]]$residuals,
    same_model$residuals,
    tolerance = 1e-3
  )
  expect_equal(validated$Model[[1]]$aic, same_model$aic, tolerance = 1e-3)
  expect_equal(validated$Model[[1]]$effects, same_model$effects,
    tolerance =
      1e-3
  )
})

test_that("Right glmer model used in validate()", {

  # skip_test_if_old_R_version()

  # Create data that should be easy to model
  xpectr::set_test_seed(7)

  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  validated <-
    validate(
      train_data = dat,
      formulas = "diagnosis~score+(1|session)",
      partitions_col = ".partitions",
      family = "binomial",
      positive = 1
    )
  same_model <-
    lme4::glmer(diagnosis ~ score + (1 | session),
      data = dat[dat$.partitions == 1, ],
      family = "binomial"
    )
  expect_equal(validated$Model[[1]]@resp, same_model@resp, tolerance = 1e-3)
  # expect_equal(validated$Model[[1]]@call, same_model@call, tolerance = 1e-3) # TODO: not working?
  expect_equal(validated$Model[[1]]@optinfo$val,
    same_model@optinfo$val,
    tolerance = 1e-3
  )
  expect_equal(validated$Model[[1]]@beta, same_model@beta, tolerance = 1e-3)

  expect_equal(
    validated$Predictions[[1]]$Target,
    c(0, 0, 0, 1, 1, 1, 1, 1, 1)
  )
})


test_that("model using dot in formula ( y ~ . ) works with validate()", {

  # skip_test_if_old_R_version()

  # We wish to test if using the dot "y~." method in the model formula
  # correctly leaves out .folds column.

  # Create data that should be easy to model
  xpectr::set_test_seed(7)

  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  ) %>%
    dplyr::select(-c(participant, session))


  # Expect no warnings
  # https://stackoverflow.com/questions/22003306/is-there-something-in-testthat-like-expect-no-warnings
  expect_warning(validate(dat,
    formulas = c("diagnosis~."),
    family = "binomial",
    partitions_col = ".partitions",
    REML = FALSE, verbose = FALSE
  ),
  regexp = NA
  )

  # Expect no warnings
  # https://stackoverflow.com/questions/22003306/is-there-something-in-testthat-like-expect-no-warnings
  expect_warning(validate(dat,
    formulas = c("score~."),
    partitions_col = ".partitions",
    family = "gaussian",
    REML = FALSE, verbose = FALSE
  ),
  regexp = NA
  )
})

test_that("Singular fit messages counted in validate()", {

  # skip_test_if_old_R_version()

  # Create data that should be easy to model
  xpectr::set_test_seed(7)

  dat <- groupdata2::partition(
    participant.scores,
    p = 0.8,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  expect_message(validated <-
    validate(
      train_data = dat,
      formulas = "diagnosis~score+(1|session)+(1|participant)",
      partitions_col = ".partitions",
      family = "binomial"
    ), "Boundary \\(Singular\\) Fit Message")

  expect_equal(validated$`Singular Fit Messages`, 1)
})

test_that("the expected errors are thrown by validate()", {


  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- participant.scores

  expect_error(
    xpectr::strip_msg(validate(dat, dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    family = "fdsfs",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )),
    xpectr::strip(paste0(
    "1 assertions failed:\n * Variable 'family': Must be element",
    " of set\n * {'gaussian','binomial','multinomial'}, but is 'f",
    "dsfs'."
  )),
  fixed = TRUE
  )

  expect_error(suppressWarnings(
    validate(
      train_data = dat,
      test_data = dplyr::sample_frac(dat, 0.2),
      formulas = c("diagnosis~score*age+(1|session)"),
      family = "gaussian",
      REML = FALSE,
      verbose = FALSE,
      control = lme4::lmerControl(
        optimizer = "bobyqa",
        optCtrl = list(maxfun = 10)
      ),
      err_nc = TRUE
    )
  ),
  "Model did not converge.",
  fixed = TRUE
  )
})

test_that("verbose reports the correct model functions in validate()", {

  testthat::skip_on_cran()

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::partition(participant.scores,
    p = .75,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  if (!is_tibble_v2() && is_newer_lme4()){
    # Test the list of verbose messages
    # glm()

    ## Testing 'validate(dat[[1]], dat[[2]], formulas = c(...'              ####
    ## Initially generated by xpectr
    xpectr::set_test_seed(42)
    # Testing side effects
    # Assigning side effects
    side_effects_12059 <- xpectr::capture_side_effects(validate(dat[[1]], dat[[2]],
                 formulas = c("diagnosis~score"),
                 family = "binomial",
                 REML = FALSE, verbose = TRUE,
                 positive = 1
        ), reset_seed = TRUE)
    expect_equal(
      xpectr::strip(side_effects_12059[['warnings']]),
      xpectr::strip(character(0)),
      fixed = TRUE)
    expect_equal(
      xpectr::strip(side_effects_12059[['messages']]),
      xpectr::strip(c("Will validate 1 models.\n", "---\nvalidate(): cross_validate(): Used glm() to fit the model.'\nFor:\nFormula: diagnosis~score\nFold column: .partitions\nFold: 2\nHyperparameters: REML : FALSE, control : list(list(optimizer = c(\"bobyqa\", \"Nelder_Mead\"), restart_edge = FALSE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, \n    relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(), tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE)), model_verbose : TRUE, family : binomial, is_special_fn : TRUE\n")),
      fixed = TRUE)
    # Assigning output
    output_12059 <- xpectr::suppress_mw(validate(dat[[1]], dat[[2]],
                 formulas = c("diagnosis~score"),
                 family = "binomial",
                 REML = FALSE, verbose = TRUE,
                 positive = 1
        ))
    # Testing class
    expect_equal(
      class(output_12059),
      c("tbl_df", "tbl", "data.frame"),
      fixed = TRUE)
    # Testing column values
    expect_equal(
      output_12059[["Fixed"]],
      "score",
      fixed = TRUE)
    expect_equal(
      output_12059[["Balanced Accuracy"]],
      0.83333,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["F1"]],
      0.8,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Sensitivity"]],
      0.66667,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Specificity"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Pos Pred Value"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Neg Pred Value"]],
      0.85714,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["AUC"]],
      0.94444,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Lower CI"]],
      0.79046,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Upper CI"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Kappa"]],
      0.72727,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["MCC"]],
      0.75593,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Detection Rate"]],
      0.22222,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Detection Prevalence"]],
      0.22222,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Prevalence"]],
      0.33333,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Convergence Warnings"]],
      0,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Singular Fit Messages"]],
      0,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Other Warnings"]],
      0,
      tolerance = 1e-4)
    expect_equal(
      output_12059[["Process"]][[1]][["Positive Class"]],
      "0",
      fixed = TRUE)
    expect_equal(
      output_12059[["Dependent"]],
      "diagnosis",
      fixed = TRUE)
    # Testing column names
    expect_equal(
      names(output_12059),
      c("Fixed", "Balanced Accuracy", "F1", "Sensitivity", "Specificity",
        "Pos Pred Value", "Neg Pred Value", "AUC", "Lower CI", "Upper CI",
        "Kappa", "MCC", "Detection Rate", "Detection Prevalence", "Prevalence",
        "Predictions", "ROC", "Confusion Matrix", "Coefficients", "Convergence Warnings",
        "Singular Fit Messages", "Other Warnings", "Warnings and Messages",
        "Process", "Model", "Dependent"),
      fixed = TRUE)
    # Testing column classes
    expect_equal(
      xpectr::element_classes(output_12059),
      c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
        "numeric", "numeric", "numeric", "list", "list", "list", "list",
        "integer", "integer", "integer", "list", "list",
        "list", "character"),
      fixed = TRUE)
    # Testing column types
    expect_equal(
      xpectr::element_types(output_12059),
      c("character", "double", "double", "double", "double", "double",
        "double", "double", "double", "double", "double", "double",
        "double", "double", "double", "list", "list", "list", "list",
        "integer", "integer", "integer", "list", "list",
        "list", "character"),
      fixed = TRUE)
    # Testing dimensions
    expect_equal(
      dim(output_12059),
      c(1L, 26L))
    # Testing group keys
    expect_equal(
      colnames(dplyr::group_keys(output_12059)),
      character(0),
      fixed = TRUE)
    ## Finished testing 'validate(dat[[1]], dat[[2]], formulas = c(...'     ####

  }

  if (!is_tibble_v2() && is_newer_lme4()){
    # glmer


    ## Testing 'validate(dat[[1]], dat[[2]], formulas = c(...'              ####
    ## Initially generated by xpectr
    xpectr::set_test_seed(42)
    # Testing side effects
    # Assigning side effects
    side_effects_19148 <- xpectr::capture_side_effects(validate(dat[[1]], dat[[2]],
                 formulas = c("diagnosis~score+(1|session)"),
                 family = "binomial",
                 REML = FALSE, verbose = TRUE,
                 positive = 1
        ), reset_seed = TRUE)
    expect_equal(
      xpectr::strip(side_effects_19148[['warnings']]),
      xpectr::strip(c("ci.auc() of a ROC curve with AUC == 1 is always 1-1 and can be misleading.",
        "ci.auc() of a ROC curve with AUC == 1 is always 1-1 and can be misleading.")),
      fixed = TRUE)
    expect_equal(
      xpectr::strip(side_effects_19148[['messages']]),
      xpectr::strip(c("Will validate 1 models.\n", "---\nvalidate(): cross_validate(): Used lme4::glmer() to fit the model.'\nFor:\nFormula: diagnosis~score+(1|session)\nFold column: .partitions\nFold: 2\nHyperparameters: REML : FALSE, control : list(list(optimizer = c(\"bobyqa\", \"Nelder_Mead\"), restart_edge = FALSE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, \n    relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(), tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE)), model_verbose : TRUE, family : binomial, is_special_fn : TRUE\n")),
      fixed = TRUE)
    # Assigning output
    output_19148 <- xpectr::suppress_mw(validate(dat[[1]], dat[[2]],
                 formulas = c("diagnosis~score+(1|session)"),
                 family = "binomial",
                 REML = FALSE, verbose = TRUE,
                 positive = 1
        ))
    # Testing class
    expect_equal(
      class(output_19148),
      c("tbl_df", "tbl", "data.frame"),
      fixed = TRUE)
    # Testing column values
    expect_equal(
      output_19148[["Fixed"]],
      "score",
      fixed = TRUE)
    expect_equal(
      output_19148[["Balanced Accuracy"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["F1"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Sensitivity"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Specificity"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Pos Pred Value"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Neg Pred Value"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["AUC"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Lower CI"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Upper CI"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Kappa"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["MCC"]],
      1,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Detection Rate"]],
      0.33333,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Detection Prevalence"]],
      0.33333,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Prevalence"]],
      0.33333,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Convergence Warnings"]],
      0,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Singular Fit Messages"]],
      0,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Other Warnings"]],
      0,
      tolerance = 1e-4)
    expect_equal(
      output_19148[["Process"]][[1]][["Positive Class"]],
      "0",
      fixed = TRUE)
    expect_equal(
      output_19148[["Process"]][[1]][["Family"]],
      "Binomial",
      fixed = TRUE)
    expect_equal(
      output_19148[["Dependent"]],
      "diagnosis",
      fixed = TRUE)
    expect_equal(
      output_19148[["Random"]],
      "(1|session)",
      fixed = TRUE)
    # Testing column names
    expect_equal(
      names(output_19148),
      c("Fixed", "Balanced Accuracy", "F1", "Sensitivity", "Specificity",
        "Pos Pred Value", "Neg Pred Value", "AUC", "Lower CI", "Upper CI",
        "Kappa", "MCC", "Detection Rate", "Detection Prevalence", "Prevalence",
        "Predictions", "ROC", "Confusion Matrix", "Coefficients", "Convergence Warnings",
        "Singular Fit Messages", "Other Warnings", "Warnings and Messages",
        "Process", "Model", "Dependent", "Random"),
      fixed = TRUE)
    # Testing column classes
    expect_equal(
      xpectr::element_classes(output_19148),
      c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
        "numeric", "numeric", "numeric", "list", "list", "list", "list",
        "integer", "integer", "integer", "list", "list",
        "list", "character", "character"),
      fixed = TRUE)
    # Testing column types
    expect_equal(
      xpectr::element_types(output_19148),
      c("character", "double", "double", "double", "double", "double",
        "double", "double", "double", "double", "double", "double",
        "double", "double", "double", "list", "list", "list", "list",
        "integer", "integer", "integer", "list", "list",
        "list", "character", "character"),
      fixed = TRUE)
    # Testing dimensions
    expect_equal(
      dim(output_19148),
      c(1L, 27L))
    # Testing group keys
    expect_equal(
      colnames(dplyr::group_keys(output_19148)),
      character(0),
      fixed = TRUE)
    ## Finished testing 'validate(dat[[1]], dat[[2]], formulas = c(...'     ####

  }

  # lm

  ## Testing 'validate(dat[[1]], dat[[2]], formulas = c("s...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(validate(dat[[1]], dat[[2]],
             formulas = c("score~diagnosis"),
             family = "gaussian",
             REML = FALSE, verbose = TRUE,
             positive = 1
    ), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['warnings']]),
    xpectr::strip(character(0)),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['messages']]),
    xpectr::strip(c("Will validate 1 models.\n", "---\nvalidate(): cross_validate(): Used lm() to fit the model.'\nFor:\nFormula: score~diagnosis\nFold column: .partitions\nFold: 2\nHyperparameters: REML : FALSE, control : list(list(optimizer = \"nloptwrap\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : gaussian, is_special_fn : TRUE\n")),
    fixed = TRUE)
  # Assigning output
  output_19148 <- xpectr::suppress_mw(validate(dat[[1]], dat[[2]],
             formulas = c("score~diagnosis"),
             family = "gaussian",
             REML = FALSE, verbose = TRUE,
             positive = 1
    ))
  # Testing class
  expect_equal(
    class(output_19148),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19148[["Fixed"]],
    "diagnosis",
    fixed = TRUE)
  expect_equal(
    output_19148[["RMSE"]],
    14.32077,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["MAE"]],
    11.32099,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["NRMSE(IQR)"]],
    0.95472,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["RRSE"]],
    0.77293,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["RAE"]],
    0.81729,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["RMSLE"]],
    0.4338,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["AIC"]],
    184.78402,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["AICc"]],
    186.19579,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["BIC"]],
    187.91759,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["Convergence Warnings"]],
    0,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["Singular Fit Messages"]],
    0,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["Other Warnings"]],
    0,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["Process"]][[1]][["Family"]],
    "Gaussian",
    fixed = TRUE)
  expect_equal(
    output_19148[["Dependent"]],
    "score",
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(output_19148),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
      "AIC", "AICc", "BIC", "Predictions", "Coefficients", "Convergence Warnings",
      "Singular Fit Messages", "Other Warnings", "Warnings and Messages",
      "Process", "Model", "Dependent"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19148),
    c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "list", "list",
      "integer", "integer", "integer", "list", "list", "list",
      "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19148),
    c("character", "double", "double", "double", "double", "double",
      "double", "double", "double", "double", "list", "list", "integer",
      "integer", "integer", "list", "list", "list", "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19148),
    c(1L, 19L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19148)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'validate(dat[[1]], dat[[2]], formulas = c("s...'     ####


  # lmer

  ## Testing 'validate(dat[[1]], dat[[2]], formulas = c("s...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(validate(dat[[1]], dat[[2]],
             formulas = c("score~diagnosis+(1|session)"),
             family = "gaussian",
             REML = FALSE, verbose = TRUE,
             positive = 1
    ), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['warnings']]),
    xpectr::strip(character(0)),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['messages']]),
    xpectr::strip(c("Will validate 1 models.\n", "---\nvalidate(): cross_validate(): Used lme4::lmer() to fit the model.'\nFor:\nFormula: score~diagnosis+(1|session)\nFold column: .partitions\nFold: 2\nHyperparameters: REML : FALSE, control : list(list(optimizer = \"nloptwrap\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : gaussian, is_special_fn : TRUE\n")),
    fixed = TRUE)
  # Assigning output
  output_19148 <- xpectr::suppress_mw(validate(dat[[1]], dat[[2]],
             formulas = c("score~diagnosis+(1|session)"),
             family = "gaussian",
             REML = FALSE, verbose = TRUE,
             positive = 1
    ))
  # Testing class
  expect_equal(
    class(output_19148),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19148[["Fixed"]],
    "diagnosis",
    fixed = TRUE)
  expect_equal(
    output_19148[["RMSE"]],
    9.20986,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["MAE"]],
    6.85731,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["NRMSE(IQR)"]],
    0.61399,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["RRSE"]],
    0.49708,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["RAE"]],
    0.49505,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["RMSLE"]],
    0.22504,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["AIC"]],
    166.88262,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["AICc"]],
    169.38262,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["BIC"]],
    171.06071,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["Convergence Warnings"]],
    0,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["Singular Fit Messages"]],
    0,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["Other Warnings"]],
    0,
    tolerance = 1e-4)
  expect_equal(
    output_19148[["Process"]][[1]][["Family"]],
    "Gaussian",
    fixed = TRUE)
  expect_equal(
    output_19148[["Dependent"]],
    "score",
    fixed = TRUE)
  expect_equal(
    output_19148[["Random"]],
    "(1|session)",
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(output_19148),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
      "AIC", "AICc", "BIC", "Predictions", "Coefficients", "Convergence Warnings",
      "Singular Fit Messages", "Other Warnings", "Warnings and Messages",
      "Process", "Model", "Dependent", "Random"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19148),
    c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "list", "list",
      "integer", "integer", "integer", "list", "list", "list",
      "character", "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19148),
    c("character", "double", "double", "double", "double", "double",
      "double", "double", "double", "double", "list", "list", "integer",
      "integer", "integer", "list", "list", "list", "character",
      "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19148),
    c(1L, 20L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19148)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'validate(dat[[1]], dat[[2]], formulas = c("s...'     ####

})
