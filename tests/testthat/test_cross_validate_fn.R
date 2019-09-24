library(cvms)
context("cross_validate_fn()")


test_that("binomial glm models work with cross_validate_fn()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  glm_model_fn <- function(data, formula){
    glm(formula = formula, data = data, family = "binomial")
  }

  CVbinomlist <- cross_validate_fn(dat,
                                   glm_model_fn,
                                   formulas = c("diagnosis~score","diagnosis~age"),
                                   fold_cols = '.folds', type = 'binomial',
                                   model_verbose = FALSE,
                                   positive = 1)

  expect_equal(CVbinomlist$AUC, c(0.7615741, 0.1666667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.58511535, 0.01748744), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9380328, 0.3158459), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4927536, -0.3636364), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8888889,0.6666667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7777778, 0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7619048, 0.5), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3,0.2), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111,0.3333333), tolerance=1e-3)
  expect_equal(CVbinomlist$MCC, c(0.5048268, -0.4082483), tolerance=1e-3)
  expect_equal(CVbinomlist$Folds, c(4,4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1,1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlist$Family, c('binomial','binomial'))
  expect_equal(CVbinomlist$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlist$Fixed, c('score','age'))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(colnames(CVbinomlist$ROC[[1]]), c("Sensitivities","Specificities"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlist$ROC[[1]]),29)

})

test_that("gaussian lm model with cross_validate_fn()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  lm_model_fn <- function(data, formula){
    lm(formula = formula, data = data)
  }
  # summary(lmm <- lm_model_fn(dat, "score ~ diagnosis"))
  # MuMIn::AICc(lmm, REML = F) # The one used in the package

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
                            model_fn = lm_model_fn,
                            formulas = "score~diagnosis",
                            fold_cols = '.folds',
                            type = 'gaussian',
                            model_verbose = FALSE)

  expect_equal(CVed$RMSE, 17.16817, tolerance=1e-3)
  expect_equal(CVed$MAE, 14.26914, tolerance=1e-3)
  expect_equal(CVed$r2m, 0.2640793, tolerance=1e-3)
  expect_equal(CVed$r2c, 0.2640793, tolerance=1e-3)
  expect_equal(CVed$AIC, 194.6904, tolerance=1e-3)
  expect_equal(CVed$AICc, 195.9963, tolerance=1e-3)
  expect_equal(CVed$BIC, 198.0243, tolerance=1e-3)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Fold Columns`, 1)
  expect_equal(CVed$`Convergence Warnings`, 0)
  expect_equal(CVed$Family, 'gaussian')
  expect_equal(CVed$Dependent, 'score')
  expect_equal(CVed$Fixed, 'diagnosis')

})

test_that("binomial svm models from e1071 work with cross_validate_fn()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  svm_model_fn <- function(data, formula){

    e1071::svm(formula = formula, # converted to formula object within custom_fit_model()
               data = data,
               kernel = "linear",
               cost = 10,
               scale = FALSE,
               type = "C-classification")
  }
  # sm_ <- svm_model_fn(data = dat, formula = "diagnosis~score")

  CVbinomlist <- cross_validate_fn(dat,
                                   svm_model_fn,
                                   formulas = c("diagnosis~score","diagnosis~age"),
                                   fold_cols = '.folds', type = 'binomial',
                                   model_verbose = FALSE,
                                   positive = 1)

  expect_equal(CVbinomlist$AUC, c(0.736111111111111, 0.5), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.572405153339587, 0.5), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.899817068882636, 0.5), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.492753623188406, 0), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8888889,1.0), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7777778, NaN), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7619048, 0.6), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3,0.0), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111,0.5), tolerance=1e-3)
  expect_equal(CVbinomlist$MCC, c(0.5048268, 0), tolerance=1e-3)
  expect_equal(CVbinomlist$Folds, c(4,4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1,1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlist$Family, c('binomial','binomial'))
  expect_equal(CVbinomlist$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlist$Fixed, c('score','age'))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(colnames(CVbinomlist$ROC[[1]]), c("Sensitivities","Specificities"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlist$ROC[[1]]),3)

})

test_that("gaussian svm models from e1071 work with cross_validate_fn()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  svm_model_fn <- function(data, formula){

    e1071::svm(formula = formula, # converted to formula object within custom_fit_model()
               data = data,
               kernel = "linear",
               cost = 10,
               scale = FALSE,
               type = "eps-regression")
  }

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
                            model_fn = svm_model_fn,
                            formulas = "score~diagnosis",
                            fold_cols = '.folds',
                            type = 'gaussian',
                            model_verbose = FALSE)

  expect_equal(CVed$RMSE, 18.01026, tolerance=1e-3)
  expect_equal(CVed$MAE, 15.27778, tolerance=1e-3)
  expect_equal(CVed$r2m, NaN, tolerance=1e-3)
  expect_equal(CVed$r2c, NaN, tolerance=1e-3)
  expect_equal(CVed$AIC, NaN, tolerance=1e-3)
  expect_equal(CVed$AICc, NaN, tolerance=1e-3)
  expect_equal(CVed$BIC, NaN, tolerance=1e-3)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Fold Columns`, 1)
  expect_equal(CVed$`Convergence Warnings`, 0)
  expect_equal(CVed$`Singular Fit Messages`, 0)
  expect_equal(CVed$Family, 'gaussian')
  expect_equal(CVed$Dependent, 'score')
  expect_equal(CVed$Fixed, 'diagnosis')

  expect_equal(colnames(CVed$Coefficients[[1]]),
               c("term", "estimate", "Fold",
                 "Fold Column", "p.value"))
  expect_equal(CVed$Coefficients[[1]]$term,
               rep(c("(Intercept)","diagnosis"), 4))
  expect_equal(CVed$Coefficients[[1]]$estimate,
               c(40.1, -10, 50.1, -20, 40.1, -10, 45, -10))
  expect_equal(CVed$Coefficients[[1]]$Fold,
               c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L))
  expect_equal(CVed$Coefficients[[1]]$`Fold Column`,
               rep(".folds", 8))
  expect_equal(CVed$Coefficients[[1]]$p.value,
               rep(NA, 8))

})

test_that("binomial naiveBayes models from e1071 work with cross_validate_fn()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  nb_model_fn <- function(data, formula){

    e1071::naiveBayes(formula = formula, # converted to formula object within custom_fit_model()
                      data = data)
  }

  # nb_ <- nb_model_fn(data = dat, formula = as.formula("diagnosis~score"))

  nb_predict_fn <- function(test_data, model){
    stats::predict(object = model, newdata = test_data, type = "raw", allow.new.levels = TRUE)[,2]
  }

  # nb_predict_fn(data = dat, model = nb_)
  # preds <- predict(nb_, dat, type = "raw", allow.new.levels = TRUE)

  expect_error(cross_validate_fn(dat,
                                 nb_model_fn,
                                 formulas = c("diagnosis~score", "diagnosis~age"),
                                 fold_cols = '.folds',
                                 type = 'binomial',
                                 model_verbose = FALSE,
                                 predict_type = "raw",
                                 positive = 1),
               paste0("cross_validate_fn(): When type/family is binomial, ",
                      "the predictions must be a vector or matrix / data frame ",
                      "with one column but was a matrix with 2 columns. ",
                      "Did you specify 'predict_type' or 'predict_fn' correctly?"), fixed = TRUE)

  expect_error(cross_validate_fn(dat,
                                   nb_model_fn,
                                   formulas = c("diagnosis~score","diagnosis~age"),
                                   fold_cols = '.folds',
                                   type = 'binomial',
                                   model_verbose = FALSE,
                                   predict_type = "raw",
                                   predict_fn = nb_predict_fn,
                                   positive = 1),
               "cross_validate_fn(): Both 'predict_type' and 'predict_fn' were specified. Please specify only one of them.",
               fixed = TRUE)

  CVbinomlist <- cross_validate_fn(dat,
                                   nb_model_fn,
                                   formulas = c("diagnosis~score","diagnosis~age"),
                                   fold_cols = '.folds',
                                   type = 'binomial',
                                   model_verbose = FALSE,
                                   predict_fn = nb_predict_fn,
                                   positive = 1)

  expect_equal(CVbinomlist$AUC, c(0.743055555555555, 0.125), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.555996282730279, 0), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.930114828380832, 0.264544385449311), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.492753623188406, -0.666666666666667), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.583333333333333, 0), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.888888888888889, 0.333333333333333), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.777777777777778, 0), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.761904761904762, 0.333333333333333), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.736111111111111, 0.166666666666667), tolerance=1e-3)
  expect_equal(CVbinomlist$MCC, c(0.504826790279024, -0.666666666666667), tolerance=1e-3)
  expect_equal(CVbinomlist$Folds, c(4,4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1,1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlist$Family, c('binomial','binomial'))
  expect_equal(CVbinomlist$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlist$Fixed, c('score','age'))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(colnames(CVbinomlist$ROC[[1]]), c("Sensitivities","Specificities"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlist$ROC[[1]]),29)

})

test_that("binomial nnet models work with cross_validate_fn()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  nnet_model_fn <- function(data, formula){

    nnet::nnet(formula = formula, # converted to formula object within custom_fit_model()
               data = data,
               size = 50)
  }
  # nn <- nnet_model_fn(data = dat, formula = as.formula("diagnosis~score"))
  # predict(nn, dat, type = "raw", allow.new.levels = TRUE)

  CVbinomlist <- cross_validate_fn(dat,
                                   nnet_model_fn,
                                   formulas = c("diagnosis~score","diagnosis~age"),
                                   fold_cols = '.folds', type = 'binomial',
                                   predict_type = "raw",
                                   model_verbose = FALSE,
                                   positive = 1)

  expect_equal(CVbinomlist$AUC, c(0.638888888888889, 0.0416666666666667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.42727682717822, 0), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.850500950599558, 0.103151638535899), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.133333333333333, -0.8), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.555555555555556, 0.166666666666667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.466666666666667, 0), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.666666666666667, 0.2), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.518518518518519, NaN), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.5,0.5), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.569444444444444, 0.0833333333333333), tolerance=1e-3)
  expect_equal(CVbinomlist$MCC, c(0.136082763487954, -0.816496580927726), tolerance=1e-3)
  expect_equal(CVbinomlist$Folds, c(4,4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1,1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlist$Family, c('binomial','binomial'))
  expect_equal(CVbinomlist$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlist$Fixed, c('score','age'))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(colnames(CVbinomlist$ROC[[1]]), c("Sensitivities","Specificities"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlist$ROC[[1]]),20)

})

test_that("gaussian nnet models work with cross_validate_fn()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(4)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  nnet_model_fn <- function(data, formula){

    nnet::nnet(formula = formula, # converted to formula object within custom_fit_model()
               data = data,
               size = 10,
               linout = TRUE)
  }
  # nn <- nnet_model_fn(data = dat, formula = as.formula("score~diagnosis+age"))
  # predict(nn, dat, type="raw", allow.new.levels = TRUE)

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
                            model_fn = nnet_model_fn,
                            formulas = "score~diagnosis",
                            fold_cols = '.folds',
                            type = 'gaussian',
                            model_verbose = FALSE)

  expect_equal(CVed$RMSE, 16.5414, tolerance=1e-3)
  expect_equal(CVed$MAE, 13.76884, tolerance=1e-3)
  expect_equal(CVed$r2m, NaN, tolerance=1e-3)
  expect_equal(CVed$r2c, NaN, tolerance=1e-3)
  expect_equal(CVed$AIC, NaN, tolerance=1e-3)
  expect_equal(CVed$AICc, NaN, tolerance=1e-3)
  expect_equal(CVed$BIC, NaN, tolerance=1e-3)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Fold Columns`, 1)
  expect_equal(CVed$`Convergence Warnings`, 0)
  expect_equal(CVed$`Singular Fit Messages`, 0)
  expect_equal(CVed$Family, 'gaussian')
  expect_equal(CVed$Dependent, 'score')
  expect_equal(CVed$Fixed, 'diagnosis')

  expect_equal(colnames(CVed$Coefficients[[1]]),
               c("term", "estimate", "Fold",
                 "Fold Column", "p.value"))
  expect_equal(head(CVed$Coefficients[[1]]$term,10),
               c("b->h1", "i1->h1", "b->h2", "i1->h2", "b->h3", "i1->h3", "b->h4",
                 "i1->h4", "b->h5", "i1->h5"), 4)
  expect_equal(head(CVed$Coefficients[[1]]$estimate,10),
               c(-2.62129760502992, -1.6849555195295, -1.60212105846947, -2.11737643288526,
                 -5.97450438031554, -5.67581875347773, 7.8349209144499, -7.10879712805517,
                 3.56900047453588, -4.49050093869156))
  expect_equal(CVed$Coefficients[[1]]$Fold,
               rep(1:4, each=31))
  expect_equal(CVed$Coefficients[[1]]$`Fold Column`,
               rep(".folds", 124))
  expect_equal(CVed$Coefficients[[1]]$p.value,
               rep(NA, 124))
})

test_that("binomial randomForest models work with cross_validate_fn()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  rf_model_fn <- function(data, formula){

    randomForest::randomForest(
      formula = formula,
      data = data)
  }
  # nn <- nnet_model_fn(data = dat, formula = as.formula("diagnosis~score"))
  # predict(nn, dat, type = "raw", allow.new.levels = TRUE)

  CVbinomlist <- cross_validate_fn(dat,
                                   rf_model_fn,
                                   formulas = c("diagnosis~score","diagnosis~age"),
                                   fold_cols = '.folds', type = 'binomial',
                                   # predict_type = "raw",
                                   model_verbose = FALSE,
                                   positive = 1)

  expect_equal(CVbinomlist$AUC, c(0.555555555555556, 0.333333333333333), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.367800267130833, 0.161076004187493), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.743310843980278, 0.505590662479174), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.10958904109589, -0.296296296296296), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5,0.5), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.611111111111111, 0.166666666666667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.461538461538462, 0.285714285714286), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.647058823529412, 0.333333333333333), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.48, 0.363636363636364), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2, 0.2), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.433333333333333, 0.7), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.555555555555556, 0.333333333333333), tolerance=1e-3)
  expect_equal(CVbinomlist$MCC, c(0.109847007276218, -0.356348322549899), tolerance=1e-3)
  expect_equal(CVbinomlist$Folds, c(4,4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1,1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlist$Family, c('binomial','binomial'))
  expect_equal(CVbinomlist$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlist$Fixed, c('score','age'))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(colnames(CVbinomlist$ROC[[1]]), c("Sensitivities","Specificities"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlist$ROC[[1]]),3)

})

test_that("gaussian randomForest models work with cross_validate_fn()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(4)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  rf_model_fn <- function(data, formula){

    randomForest::randomForest(
      formula = formula,
      data = data)
  }
  # rf <- rf_model_fn(data = dat, formula = as.formula("score~diagnosis+age"))
  # predict(rf, dat, allow.new.levels = TRUE)

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
                            model_fn = rf_model_fn,
                            formulas = "score~diagnosis",
                            fold_cols = '.folds',
                            type = 'gaussian',
                            model_verbose = FALSE)

  expect_equal(CVed$RMSE, 16.56476, tolerance=1e-3)
  expect_equal(CVed$MAE, 13.77846, tolerance=1e-3)
  expect_equal(CVed$r2m, NaN, tolerance=1e-3)
  expect_equal(CVed$r2c, NaN, tolerance=1e-3)
  expect_equal(CVed$AIC, NaN, tolerance=1e-3)
  expect_equal(CVed$AICc, NaN, tolerance=1e-3)
  expect_equal(CVed$BIC, NaN, tolerance=1e-3)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Fold Columns`, 1)
  expect_equal(CVed$`Convergence Warnings`, 0)
  expect_equal(CVed$`Singular Fit Messages`, 0)
  expect_equal(CVed$Family, 'gaussian')
  expect_equal(CVed$Dependent, 'score')
  expect_equal(CVed$Fixed, 'diagnosis')

  expect_equal(colnames(CVed$Coefficients[[1]]),
               c("term", "estimate", "std.error", "statistic",
                 "Fold", "Fold Column", "p.value"))
  expect_equal(CVed$Coefficients[[1]]$term,
               NA, 4)
  expect_equal(CVed$Coefficients[[1]]$estimate,
               NA)
  expect_equal(CVed$Coefficients[[1]]$Fold,
               NA)
  expect_equal(CVed$Coefficients[[1]]$`Fold Column`,
               NA)
  expect_equal(CVed$Coefficients[[1]]$p.value,
               NA)
})
