library(cvms)
context("cross_validate_fn_single()")

test_that("gaussian models with cross_validate_single_fn()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  ### LMER
  cv_result <- cross_validate_fn_single(dat, basics_model_fn,
                                        evaluation_type = "gaussian",
                                        model_specifics = list(
                                          model_formula="score~diagnosis+(1|session)",
                                          family="gaussian",
                                          REML=FALSE,
                                          link="identity",
                                          control=lme4::lmerControl(optimizer="nloptwrap"),
                                          model_verbose = FALSE,
                                          caller = "cross_validate()"),
                                        model_specifics_update_fn = basics_update_model_specifics,
                                        fold_cols =".folds")

  # For comparison
  # cross_validate(dat, "score~diagnosis+(1|session)+(1|participant)", fold_cols = ".folds", model_verbose = TRUE)

  expect_equal(cv_result$RMSE, 9.65949, tolerance=1e-3)
  expect_equal(cv_result$r2m, 0.2821929, tolerance=1e-3)
  expect_equal(cv_result$r2c, 0.804314, tolerance=1e-3)
  expect_equal(cv_result$AIC, 175.9497, tolerance=1e-3)
  expect_equal(cv_result$AICc, 178.2523, tolerance=1e-3)
  expect_equal(cv_result$BIC, 180.3948, tolerance=1e-3)
  expect_equal(cv_result$Folds, 4)
  expect_equal(cv_result$`Convergence Warnings`, 0)
  # expect_equal(cv_result$Family, "gaussian")
  # expect_equal(cv_result$Link, "identity")

  ### LM
  cv_result <- cross_validate_fn_single(dat, basics_model_fn,
                                        evaluation_type = "gaussian",
                                        model_specifics = list(
                                          model_formula="score~diagnosis",
                                          family="gaussian",
                                          REML=FALSE,
                                          link="identity",
                                          model_verbose = FALSE,
                                          caller = "cross_validate()"),
                                        model_specifics_update_fn = basics_update_model_specifics,
                                        fold_cols =".folds")

  expect_equal(cv_result$RMSE, 17.16817, tolerance=1e-3)
  expect_equal(cv_result$r2m, 0.2640793, tolerance=1e-3)
  expect_equal(cv_result$r2c, 0.2640793, tolerance=1e-3)
  expect_equal(cv_result$AIC, 194.6904, tolerance=1e-3)
  expect_equal(cv_result$AICc, 195.9963, tolerance=1e-3)
  expect_equal(cv_result$BIC, 198.0243, tolerance=1e-3)
  expect_equal(cv_result$Folds, 4)
  expect_equal(cv_result$`Convergence Warnings`, 0)
  # expect_equal(cv_result$Family, "gaussian")
  # expect_equal(cv_result$Link, "identity")

})



test_that("binomial models with cross_validate_single_fn()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  ### GLMER
  cv_result <- cross_validate_fn_single(dat, basics_model_fn,
                                        evaluation_type = "binomial",
                                        model_specifics = list(
                                          model_formula="diagnosis~score+(1|session)",
                                          family="binomial",
                                          REML=FALSE,
                                          link=NULL,
                                          positive=1,
                                          cutoff=0.5,
                                          model_verbose = FALSE,
                                          caller = "cross_validate()"),
                                        model_specifics_update_fn = basics_update_model_specifics,
                                        fold_cols =".folds")

  expect_equal(cv_result$AUC, 0.8611111, tolerance=1e-3)
  expect_equal(cv_result$`Lower CI`, 0.7103334, tolerance=1e-3)
  expect_equal(cv_result$`Upper CI`, 1, tolerance=1e-3)
  expect_equal(cv_result$Kappa, 0.6575342, tolerance=1e-3)
  expect_equal(cv_result$Sensitivity, 0.833, tolerance=1e-3)
  expect_equal(cv_result$Specificity, 0.833, tolerance=1e-3)
  expect_equal(cv_result$`Pos Pred Value`, 0.769, tolerance=1e-3)
  expect_equal(cv_result$`Neg Pred Value`, 0.882, tolerance=1e-3)
  expect_equal(cv_result$F1, 0.8, tolerance=1e-3)
  expect_equal(cv_result$Prevalence, 0.4, tolerance=1e-3)
  expect_equal(cv_result$`Detection Rate`, 0.333, tolerance=1e-3)
  expect_equal(cv_result$`Detection Prevalence`, 0.433, tolerance=1e-3)
  expect_equal(cv_result$`Balanced Accuracy`, 0.833, tolerance=1e-3)
  expect_equal(cv_result$Folds, 4)
  expect_equal(cv_result$`Convergence Warnings`, 0)
  expect_equal(cv_result$`Singular Fit Messages`, 0)
  # expect_equal(cv_result$Family, "binomial")
  # expect_equal(cv_result$Link, "logit")


  ### GLM
  cv_result <- cross_validate_fn_single(dat, basics_model_fn,
                                        evaluation_type = "binomial",
                                        model_specifics = list(
                                          model_formula="diagnosis~score",
                                          family="binomial",
                                          REML=FALSE,
                                          link=NULL,
                                          positive=1,
                                          cutoff=0.5,
                                          model_verbose = FALSE,
                                          caller = "cross_validate()"),
                                        model_specifics_update_fn = basics_update_model_specifics,
                                        fold_cols =".folds")

  expect_equal(cv_result$AUC, 0.7615741, tolerance=1e-3)
  expect_equal(cv_result$`Lower CI`, 0.5851154, tolerance=1e-3)
  expect_equal(cv_result$`Upper CI`, 0.9380328, tolerance=1e-3)
  expect_equal(cv_result$Kappa, 0.493, tolerance=1e-3)
  expect_equal(cv_result$Sensitivity, 0.583, tolerance=1e-3)
  expect_equal(cv_result$Specificity, 0.889, tolerance=1e-3)
  expect_equal(cv_result$`Pos Pred Value`, 0.778, tolerance=1e-3)
  expect_equal(cv_result$`Neg Pred Value`, 0.762, tolerance=1e-3)
  expect_equal(cv_result$F1, 0.667, tolerance=1e-3)
  expect_equal(cv_result$Prevalence, 0.4, tolerance=1e-3)
  expect_equal(cv_result$`Detection Rate`, 0.233, tolerance=1e-3)
  expect_equal(cv_result$`Detection Prevalence`, 0.3, tolerance=1e-3)
  expect_equal(cv_result$`Balanced Accuracy`, 0.736, tolerance=1e-3)
  expect_equal(cv_result$Folds, 4)
  expect_equal(cv_result$`Convergence Warnings`, 0)
  expect_equal(cv_result$`Singular Fit Messages`, 0)
  # expect_equal(cv_result$Family, "binomial")
  # expect_equal(cv_result$Link, "logit")

})
