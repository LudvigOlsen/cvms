library(cvms)
context("cross_validate_fn_single()")

test_that("gaussian models with cross_validate_single_fn()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  lmer_model_fn <- function(train_set,
                            test_set,
                            fold,
                            model_verbose,
                            model_formula){

    y_col <- extract_y(model_formula) # Name of target column

    model <- run_basic_model(lme4::lmer, model_formula, data = train_set,
                             warn_info=list(model_formula=model_formula,
                                            fold=fold,
                                            model_verbose=model_verbose))

    if (is.null(model)){
      # Create a list of NA predictions the length of y_column
      predictions = rep(NA, length(test_set[[y_col]])) # TODO remove list() ???
    } else {
      predictions = stats::predict(model, test_set, allow.new.levels=TRUE)
    }

    predictions_and_targets <- tibble::tibble("target"=test_set[[y_col]],
                                              "prediction"=predictions,
                                              "fold"=fold)

    return(list(predictions_and_targets=predictions_and_targets,
                model=model))

  }

  ### LMER
  cv_result <- cross_validate_fn_single(dat, lmer_model_fn,
                                       fold_evaluation_lm_lmer,
                                       eval_aggregation_lm_lmer,
                                       eval_model_specifics = list(
                                         family="gaussian", link=default_link(NULL, "gaussian"),
                                         REML=FALSE),
                                       folds_col =".folds",
                                       model_formula="score~diagnosis+(1|session)",
                                       model_verbose = FALSE)

  # cross_validate(dat, "score~diagnosis+(1|session)", folds_col = ".folds", model_verbose = FALSE)

  expect_equal(cv_result$RMSE, 9.63, tolerance=1e-3)
  expect_equal(cv_result$r2m, 0.221, tolerance=1e-3)
  expect_equal(cv_result$r2c, 0.839, tolerance=1e-3)
  expect_equal(cv_result$AIC, 165.33, tolerance=1e-3)
  expect_equal(cv_result$BIC, 169.775, tolerance=1e-3)
  expect_equal(cv_result$Folds, 4)
  expect_equal(cv_result$`Convergence Warnings`, 0)
  expect_equal(cv_result$Family, "gaussian")
  expect_equal(cv_result$Link, "identity")

  ### LM
  cv_result_lm <- cross_validate_fn_single(dat, lmer_model_fn,
                                        fold_evaluation_lm_lmer,
                                        eval_aggregation_lm_lmer,
                                        eval_model_specifics = list(
                                          family="gaussian", link=default_link(NULL, "gaussian"),
                                          REML=FALSE),
                                        folds_col =".folds",
                                        model_formula="score~diagnosis",
                                        model_verbose = FALSE)

})



test_that("binomial models with cross_validate_single_fn()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  glmer_model_fn <- function(train_set,
                            test_set,
                            fold,
                            model_verbose,
                            model_formula){

    y_col <- extract_y(model_formula)

    model <- run_basic_model(lme4::glmer, model_formula, data = train_set,
                             family="binomial",
                             warn_info=list(model_formula=model_formula,
                                            fold=fold,
                                            model_verbose=model_verbose))

    if (is.null(model)){
      # Create a list of NA predictions the length of y_column
      predictions = rep(NA, length(test_set[[y_col]])) # TODO remove list() ???
    } else {
      predictions = stats::predict(model, test_set, allow.new.levels=TRUE)
    }

    predictions_and_targets <- tibble::tibble("target"=test_set[[y_col]],
                                                   "prediction"=predictions,
                                                   "fold"=fold)

    return(list(predictions_and_targets=predictions_and_targets,
                model=model))

  }


  cv_result <- cross_validate_fn_single(dat, glmer_model_fn,
                                        fold_evaluation_binomial_glm_glmer,
                                        eval_aggregation_binomial_glm_glmer,
                                        eval_model_specifics = list(
                                          family="binomial", link=default_link(NULL, "binomial"),
                                          positive=1, cutoff=0.5),
                                        folds_col =".folds",
                                        model_formula="diagnosis~score+(1|session)",
                                        model_verbose = FALSE)

  expect_equal(cv_result$AUC, 0.856, tolerance=1e-3)
  expect_equal(cv_result$`Lower CI`, 0.701, tolerance=1e-3)
  expect_equal(cv_result$`Upper CI`, 1.0, tolerance=1e-3)
  expect_equal(cv_result$Kappa, 0.658, tolerance=1e-3)
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
  expect_equal(cv_result$Family, "binomial")
  expect_equal(cv_result$Link, "logit")

})
