library(cvms)
context("cross_validate_fn()")


test_that("binomial glm model works with cross_validate_fn()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  glm_model_fn <- function(train_data, formula){
    glm(formula = formula, data = train_data, family = "binomial")
  }

  CVbinomlist <- cross_validate_fn(dat,
                                   glm_model_fn,
                                   formulas = c("diagnosis~score","diagnosis~age"),
                                   fold_cols = '.folds', type = 'binomial',
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
  expect_equal(CVbinomlist$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))

  # Check error when no model_fn is provided
  expect_error(cross_validate_fn(dat,
                    model_fn = NULL,
                    formulas = c("diagnosis~score","diagnosis~age"),
                    fold_cols = '.folds', type = 'binomial'),
               "'model_fn' was NULL.", fixed = TRUE)

  # Check error when no model_fn is provided
  expect_error(cross_validate_fn(dat,
                                 model_fn = 3,
                                 formulas = c("diagnosis~score","diagnosis~age"),
                                 fold_cols = '.folds', type = 'binomial'),
               "could not find function \"model_fn\"", fixed = TRUE)

  expect_error(cross_validate_fn(dat,
                                 model_fn = glm_model_fn,
                                 formulas = c("score","diagnosis~age"),
                                 fold_cols = '.folds', type = 'binomial'),
               "The model formula does not contain a dependent variable.",
               fixed = TRUE)

  expect_error(cross_validate_fn(dat,
                                 model_fn = glm_model_fn,
                                 formulas = c("score","diagnosis~age"),
                                 fold_cols = '.folds',
                                 type = 'fishcat'),
               "Only 'gaussian', 'binomial', and 'multinomial' evaluation types are currently allowed.",
               fixed = TRUE)

  # wrong predict fn
  wrong_predict_fn <- function(test_data, model, formula = NULL){
    tibble::tibble("1" = stats::predict(object = model, newdata = test_data,
                                        type = "response", allow.new.levels = TRUE),
                   "2" = stats::predict(object = model, newdata = test_data,
                                        type = "response", allow.new.levels = TRUE))
  }

  expect_error(cross_validate_fn(dat,
                                 model_fn = glm_model_fn,
                                 formulas = c("diagnosis~score","diagnosis~age"),
                                 fold_cols = '.folds',
                                 predict_fn = wrong_predict_fn,
                                 type = 'binomial'),
               paste0("When type/family is binomial, the predictions must ",
                      "be a vector or matrix / data frame with one column ",
                      "but was a data frame with 2 columns. Did you specify",
                      " 'predict_type' or 'predict_fn' correctly?"),
               fixed = TRUE)

  expect_error(cross_validate_fn(dat,
                                 model_fn = glm_model_fn,
                                 formulas = c("diagnosis~score","diagnosis~age"),
                                 fold_cols = '.folds',
                                 predict_fn = function(test_data, model, formula = NULL){NULL},
                                 type = 'binomial'),
               paste0("cross_validate_fn(): predictions were NULL."),
               fixed = TRUE)
  expect_error(cross_validate_fn(dat,
                                 model_fn = glm_model_fn,
                                 formulas = c("diagnosis~score","diagnosis~age"),
                                 fold_cols = '.folds',
                                 predict_fn = function(test_data, model, formula = NULL){lm},
                                 type = 'binomial'),
               paste0("cross_validate_fn(): Could not use the obtained predictions. ",
                      "Did you specify 'predict_type' or 'predict_fn' correctly? ",
                      "The original error was: Error in as.vector(x, mode): cannot coerce ",
                      "type 'closure' to vector of type 'any'"),
               fixed = TRUE)
  expect_error(cross_validate_fn(dat,
                                 model_fn = glm_model_fn,
                                 formulas = c("diagnosis~score","diagnosis~age"),
                                 fold_cols = '.folds',
                                 predict_fn = NULL,
                                 predict_type = "lol",
                                 type = 'binomial'),
               paste0("cross_validate_fn(): Could not use the specified 'predict_type.' ",
                      "Try changing 'predict_type' or pass a custom 'predict_fn'. ",
                      "The original error was: Error in match.arg(type): 'arg' should ",
                      "be one of "),   # was "“link”, “response”, “terms”" but fails due to locale settings
               fixed = TRUE)
  expect_error(cross_validate_fn(dat,
                                 model_fn = glm_model_fn,
                                 formulas = c("diagnosis~score","diagnosis~age"),
                                 fold_cols = '.folds',
                                 predict_fn = wrong_predict_fn,
                                 predict_type = "lol",
                                 type = 'binomial'),
               paste0("cross_validate_fn(): Both 'predict_type' and 'predict_fn' ",
                      "were specified. Please specify only one of them."),
               fixed = TRUE)
  expect_error(cross_validate_fn(dat,
                                 model_fn = glm_model_fn,
                                 formulas = c("diagnosis~score","diagnosis~age"),
                                 fold_cols = '.folds',
                                 predict_fn = function(test_data, model, formula = NULL){c("a","b","d")},
                                 type = 'binomial'),
               paste0("cross_validate_fn(): The number of predictions did not match the number of rows in the test set."),
               fixed = TRUE)
  expect_error(cross_validate_fn(dat,
                                 model_fn = glm_model_fn,
                                 formulas = c("diagnosis~score","diagnosis~age"),
                                 fold_cols = '.folds',
                                 predict_fn = function(test_data, model, formula = NULL){head(LETTERS, nrow(test_data))},
                                 type = 'binomial'),
               paste0("cross_validate_fn(): Could not convert predictions to type numeric."),
               fixed = TRUE)
  expect_error(cross_validate_fn(dat,
                                 model_fn = glm_model_fn,
                                 formulas = c("diagnosis~score","diagnosis~age"),
                                 fold_cols = '.folds',
                                 predict_fn = function(test_data, model, formula = NULL){stop("predict_fn error")},
                                 type = 'binomial'),
               paste0("cross_validate_fn(): Got the following error while using ",
                      "specified 'predict_fn': Error in ",
                      "user_predict_fn(test_data = test_data, model = model, ",
                      "formula = stats::as.formula(formula)): predict_fn error"),
               fixed = TRUE)
  expect_error(cross_validate_fn(dat,
                                 model_fn = glm_model_fn,
                                 formulas = c("diagnosis~score","diagnosis~age"),
                                 fold_cols = '.folds',
                                 predict_fn = function(t_data, model, formula = NULL){NULL},
                                 type = 'binomial'),
               paste0("cross_validate_fn(): Got the following error while using ",
                      "specified 'predict_fn': Error in user_predict_fn(test_data = ",
                      "test_data, model = model, formula = stats::as.formula(formula)): ",
                      "unused argument (test_data = test_data)"),
               fixed = TRUE)


})

test_that("gaussian lm model works with cross_validate_fn()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  lm_model_fn <- function(train_data, formula){
    lm(formula = formula, data = train_data)
  }
  # summary(lmm <- lm_model_fn(dat, "score ~ diagnosis"))
  # MuMIn::AICc(lmm, REML = F) # The one used in the package

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
                            model_fn = lm_model_fn,
                            formulas = "score~diagnosis",
                            fold_cols = '.folds',
                            type = 'gaussian')

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
  expect_equal(CVed$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))

})

test_that("binomial svm models from e1071 work with cross_validate_fn()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  svm_model_fn <- function(train_data, formula){

    # NOTE: formula must be first when calling svm()
    e1071::svm(formula = formula, # converted to formula object within custom_fit_model()
               data = train_data,
               kernel = "linear",
               cost = 10,
               scale = FALSE,
               type = "C-classification")
  }
  # sm_ <- svm_model_fn(train_data = data, formula = as.formula("diagnosis~score"))

  CVbinomlist <- cross_validate_fn(dat,
                                   svm_model_fn,
                                   formulas = c("diagnosis~score","diagnosis~age"),
                                   fold_cols = '.folds', type = 'binomial',
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
  expect_equal(CVbinomlist$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))

})


test_that("gaussian svm models from e1071 work with cross_validate_fn()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  svm_model_fn <- function(train_data, formula){

    e1071::svm(formula = formula, # converted to formula object within custom_fit_model()
               data = train_data,
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
                            type = 'gaussian')

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
  expect_equal(CVed$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))


})

test_that("binomial naiveBayes models from e1071 work with cross_validate_fn()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  nb_model_fn <- function(train_data, formula){

    e1071::naiveBayes(formula = formula, # converted to formula object within custom_fit_model()
                      data = train_data)
  }

  # nb_ <- nb_model_fn(data = dat, formula = as.formula("diagnosis~score"))

  nb_predict_fn <- function(test_data, model, formula = NULL){
    stats::predict(object = model, newdata = test_data, type = "raw", allow.new.levels = TRUE)[,2]
  }

  # nb_predict_fn(data = dat, model = nb_)
  # preds <- predict(nb_, dat, type = "raw", allow.new.levels = TRUE)

  expect_error(cross_validate_fn(dat,
                                 nb_model_fn,
                                 formulas = c("diagnosis~score", "diagnosis~age"),
                                 fold_cols = '.folds',
                                 type = 'binomial',
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
  expect_equal(CVbinomlist$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))

})

test_that("binomial nnet models work with cross_validate_fn()",{

  testthat::skip("mac and ubuntu give different warnings")
  # Tested on both platforms on travis as well
  # Local test is a mix of ubuntu and mac derived results/predictions
  # so wouldn't run perfectly on either

  # Load data and fold it
  set_seed_for_R_compatibility(10)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  nnet_model_fn <- function(train_data, formula){

    nnet::nnet(formula = formula, # converted to formula object within custom_fit_model()
               data = train_data,
               size = 50)
  }
  # nn <- nnet_model_fn(data = dat, formula = as.formula("diagnosis~score"))
  # predict(nn, dat, type = "raw", allow.new.levels = TRUE)

  CVbinomlist <- cross_validate_fn(dat,
                                   nnet_model_fn,
                                   formulas = c("diagnosis~score","diagnosis~age"),
                                   fold_cols = '.folds', type = 'binomial',
                                   predict_type = "raw",
                                   positive = 1)

  expect_equal(CVbinomlist$AUC, c(0.668981481481482, 0.5625), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.466988044012812, 0.334899025834253), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`,c(0.870974918950151, 0.790100974165747), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.202898550724638, -0.153846153846154), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.416666666666667, 0.5), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.777777777777778, 0.333333333333333), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`,c(0.555555555555556, 0.333333333333333), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.666666666666667, 0.5), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.476190476190476, 0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.1666667, 0.2000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3,0.6), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.597222222222222, 0.416666666666667), tolerance=1e-3)
  expect_equal(CVbinomlist$MCC, c(0.207869854820775, -0.166666666666667), tolerance=1e-3)
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
  expect_equal(nrow(CVbinomlist$ROC[[1]]),18)
  expect_equal(CVbinomlist$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))

  expect_equal(CVbinomlist$Predictions[[1]]$Prediction,
               c(0.65065179574317, 0.651590466950271, 0, 1, 0.630141605640758,
                 0.651547221432569, 0.637622766392249, 0.697372951073726, 0, 1,
                 0.627914003186229, 0.485877637042375, 1, 0.606554847890491, 0.446686243554834,
                 1, 0.571420802528193, 0, 1, 0.571422843868964, 0.571420670478912,
                 0.666666600173345, 0, 0, 0.666666696548977, 0, 0, 1, 0.666666600173345,
                 0.666666600173345))

})

test_that("gaussian nnet models work with cross_validate_fn()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(4)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  nnet_model_fn <- function(train_data, formula){

    nnet::nnet(formula = formula, # converted to formula object within custom_fit_model()
               data = train_data,
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
                            type = 'gaussian')

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
  expect_equal(CVed$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))
})

test_that("multinomial nnet models work with cross_validate_fn()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)

  # Create and fold dataset
  data_mc <- multiclass_probability_tibble(
    num_classes = 3, num_observations = 50,
    apply_softmax = TRUE, FUN = runif,
    class_name = "predictor_")
  class_names <- paste0("class_", c(1,2,3))
  data_mc[["target"]] <- factor(sample(x = class_names,
                                size = 50, replace = TRUE))
  dat <- groupdata2::fold(data_mc, k = 4)

  multinom_model_fn <- function(train_data, formula){

    nnet::multinom(formula = formula, # converted to formula object within custom_fit_model()
                   data = train_data)
  }
  if (FALSE){
  # nn <- multinom_model_fn(train_data = dat,
  #                         formula = "target ~ predictor_1 + predictor_2 + predictor_3")
  # nn2 <- nnet::multinom(formula = "target ~ predictor_1 + predictor_2 + predictor_3", # converted to formula object within custom_fit_model()
  #                       data = dat)
  # fm <- "target ~ predictor_1 + predictor_2 + predictor_3"
  # nn3 <- nnet::multinom(formula = fm, # converted to formula object within custom_fit_model()
  #                       data = dat)
  # # predict(nn, dat, type = "probs", allow.new.levels = TRUE)
  # broom::tidy(nn3)

  # summary(nn)
  }

  CVmultinomlist <- cross_validate_fn(dat,
                                   multinom_model_fn,
                                   formulas = c("target ~ predictor_1 + predictor_2 + predictor_3",
                                                "target ~ predictor_1"),
                                   fold_cols = '.folds', type = 'multinomial',
                                   predict_type = "probs",
                                   positive = 1)

  expect_equal(CVmultinomlist$AUC, c(0.338293650793651, 0.38640873015873), tolerance=1e-3)
  expect_equal(CVmultinomlist$`Lower CI`, c(0.174368857734496, 0.229532941913765), tolerance=1e-3)
  expect_equal(CVmultinomlist$`Upper CI`, c(0.502218443852806, 0.543284518403696), tolerance=1e-3)
  expect_equal(CVmultinomlist$Kappa, c(-0.234940592679204, -0.0826903354960317), tolerance=1e-3)
  expect_equal(CVmultinomlist$Sensitivity, c(0.177248677248677, 0.283068783068783), tolerance=1e-3)
  expect_equal(CVmultinomlist$Specificity, c(0.585648148148148, 0.642361111111111), tolerance=1e-3)
  expect_equal(CVmultinomlist$`Pos Pred Value`, c(0.182234432234432, 0.254255548373195), tolerance=1e-3)
  expect_equal(CVmultinomlist$`Neg Pred Value`, c(0.582223196827659, 0.646699553676298), tolerance=1e-3)
  expect_equal(CVmultinomlist$F1, c(0.179096139880454, 0.259451659451659), tolerance=1e-3)
  expect_equal(CVmultinomlist$Prevalence, c(0.333333333333333, 0.333333333333333), tolerance=1e-3)
  expect_equal(CVmultinomlist$`Detection Rate`, c(0.06, 0.1), tolerance=1e-3)
  expect_equal(CVmultinomlist$`Detection Prevalence`, c(0.33333, 0.3333), tolerance=1e-3)
  expect_equal(CVmultinomlist$`Balanced Accuracy`, c(0.381448412698413, 0.462714947089947), tolerance=1e-3)
  expect_equal(CVmultinomlist$MCC, c(-0.236236444573951, -0.0854444055482041), tolerance=1e-3)
  expect_equal(CVmultinomlist$Folds, c(4,4))
  expect_equal(CVmultinomlist$`Fold Columns`, c(1,1))
  expect_equal(CVmultinomlist$`Convergence Warnings`, c(0,0))
  expect_equal(CVmultinomlist$Family, c('multinomial','multinomial'))
  expect_equal(CVmultinomlist$Dependent, c("target", "target"))
  expect_equal(CVmultinomlist$Fixed, c("predictor_1+predictor_2+predictor_3", "predictor_1"))

  expect_equal(CVmultinomlist$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))

  # Enter sub tibbles
  class_level_results <- CVmultinomlist$`Class Level Results`

  expect_equal(class_level_results[[1]]$Class,
               c("class_1", "class_2", "class_3"))
  expect_equal(class_level_results[[2]]$Class,
               c("class_1", "class_2", "class_3"))
  expect_equal(class_level_results[[1]]$`Balanced Accuracy`,
               c(0.418650793650794, 0.302083333333333, 0.423611111111111))
  expect_equal(class_level_results[[2]]$`Balanced Accuracy`,
               c(0.452380952380952, 0.407986111111111, 0.527777777777778))
  expect_equal(class_level_results[[1]]$F1,
               c(0.148148148148148, 0.153846153846154, 0.235294117647059))
  expect_equal(class_level_results[[2]]$F1,
               c(0.0952380952380952, 0.228571428571429, 0.454545454545455))
  expect_equal(class_level_results[[1]]$Sensitivity,
               c(0.142857142857143, 0.166666666666667, 0.222222222222222))
  expect_equal(class_level_results[[2]]$Sensitivity,
               c(0.0714285714285714, 0.222222222222222, 0.555555555555556))
  expect_equal(class_level_results[[1]]$Specificity,
               c(0.694444444444444, 0.4375, 0.625))
  expect_equal(class_level_results[[2]]$Specificity,
               c(0.833333333333333, 0.59375, 0.5))
  expect_equal(class_level_results[[1]]$`Pos Pred Value`,
               c(0.153846153846154, 0.142857142857143, 0.25))
  expect_equal(class_level_results[[2]]$`Pos Pred Value`,
               c(0.142857142857143, 0.235294117647059, 0.384615384615385))
  expect_equal(class_level_results[[1]]$`Neg Pred Value`,
               c(0.675675675675676, 0.482758620689655, 0.588235294117647))
  expect_equal(class_level_results[[2]]$`Neg Pred Value`,
               c(0.697674418604651, 0.575757575757576, 0.666666666666667))
  expect_equal(class_level_results[[1]]$AUC,
               c(0.341269841269841, 0.274305555555556, 0.399305555555556))
  expect_equal(class_level_results[[2]]$AUC,
               c(0.259920634920635, 0.364583333333333, 0.534722222222222))
  expect_equal(class_level_results[[1]]$`Lower CI`,
               c(0.162964881346796, 0.128288746158909, 0.231852945697782))
  expect_equal(class_level_results[[2]]$`Lower CI`,
               c(0.114038680733862, 0.204985144784346, 0.369575000223086))
  expect_equal(class_level_results[[1]]$`Upper CI`,
               c(0.519574801192887, 0.420322364952202, 0.566758165413329))
  expect_equal(class_level_results[[2]]$`Upper CI`,
               c(0.405802589107408, 0.524181521882321, 0.699869444221358))
  expect_equal(class_level_results[[1]]$Kappa,
               c(-0.166328600405679, -0.381909547738693, -0.156583629893238))
  expect_equal(class_level_results[[2]]$Kappa,
               c(-0.112412177985948, -0.186291739894552, 0.0506329113924052))
  expect_equal(class_level_results[[1]]$MCC,
               c(-0.166542870566493, -0.384959426774726, -0.157207036380633))
  expect_equal(class_level_results[[2]]$MCC,
               c(-0.123237455089644, -0.186471812823331, 0.0533760512683624))
  expect_equal(class_level_results[[1]]$`Detection Rate`,
               c(0.04, 0.06, 0.08))
  expect_equal(class_level_results[[2]]$`Detection Rate`,
               c(0.02, 0.08, 0.2))
  expect_equal(class_level_results[[1]]$`Detection Prevalence`,
               c(0.26, 0.42, 0.32))
  expect_equal(class_level_results[[2]]$`Detection Prevalence`,
               c(0.14, 0.34, 0.52))
  expect_equal(class_level_results[[1]]$Prevalence,
               c(0.28, 0.36, 0.36))
  expect_equal(class_level_results[[2]]$Prevalence,
               c(0.28, 0.36, 0.36))
  expect_equal(class_level_results[[1]]$Support,
               c(14, 18, 18))
  expect_equal(class_level_results[[2]]$Support,
               c(14, 18, 18))

  clr_confmat_1 <- dplyr::bind_rows(class_level_results[[1]]$`Confusion Matrix`)
  clr_confmat_2 <- dplyr::bind_rows(class_level_results[[2]]$`Confusion Matrix`)
  clr_confmat <- dplyr::bind_rows(clr_confmat_1, clr_confmat_2)
  expect_equal(clr_confmat$`Fold Column`,
               rep(".folds",24))
  expect_equal(clr_confmat$Prediction,
               rep(c("0","1"),12))
  expect_equal(clr_confmat$Target,
               rep(c("0","0","1","1"),6))
  expect_equal(clr_confmat$Pos_0,
               rep(c("TP","FN","FP","TN"),6))
  expect_equal(clr_confmat$Pos_1,
               rep(c("TN","FP","FN","TP"),6))
  expect_equal(clr_confmat$N,
               c(25L, 11L, 12L, 2L, 14L, 18L, 15L, 3L, 20L, 12L, 14L, 4L, 30L,
                 6L, 13L, 1L, 19L, 13L, 14L, 4L, 16L, 16L, 8L, 10L))

  # ROC
  expect_is(class_level_results[[1]]$ROC[[1]], "tbl_df")
  expect_equal(colnames(class_level_results[[1]]$ROC[[1]]), c("Sensitivities","Specificities"))
  expect_equal(nrow(class_level_results[[1]]$ROC[[1]]),51)

  # Predictions
  expect_is(CVmultinomlist$Predictions[[1]], "tbl_df")
  expect_equal(colnames(CVmultinomlist$Predictions[[1]]),
               c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(nrow(CVmultinomlist$Predictions[[1]]),50)


})

test_that("binomial randomForest models work with cross_validate_fn()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  rf_model_fn <- function(train_data, formula){

    randomForest::randomForest(
      formula = formula,
      data = train_data)
  }
  # nn <- nnet_model_fn(data = dat, formula = as.formula("diagnosis~score"))
  # predict(nn, dat, type = "raw", allow.new.levels = TRUE)

  CVbinomlist <- cross_validate_fn(dat,
                                   rf_model_fn,
                                   formulas = c("diagnosis~score","diagnosis~age"),
                                   fold_cols = '.folds', type = 'binomial',
                                   # predict_type = "raw",
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
  expect_equal(CVbinomlist$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))


})

test_that("gaussian randomForest models work with cross_validate_fn()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(4)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  rf_model_fn <- function(train_data, formula){

    randomForest::randomForest(
      formula = formula,
      data = train_data)
  }
  # rf <- rf_model_fn(data = dat, formula = as.formula("score~diagnosis+age"))
  # predict(rf, dat, allow.new.levels = TRUE)

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
                            model_fn = rf_model_fn,
                            formulas = "score~diagnosis",
                            fold_cols = '.folds',
                            type = 'gaussian')

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
  expect_equal(CVed$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))
})

test_that("binomial keras models work with cross_validate_fn()",{

  testthat::skip("keras and tensorflow take too long and have too many dependencies")

  # Uncomment to run (avoids having keras as dependency just for this)
#   # skip_test_if_old_R_version()
#   skip_if_no_keras()
#   library(keras)
#   suppressMessages(use_session_with_seed(42))
#
#   # Load data and fold it
#   set_seed_for_R_compatibility(4)
#   dat <- groupdata2::fold(participant.scores, k = 4,
#                           cat_col = 'diagnosis',
#                           id_col = 'participant')
#
#   extract_data <- function(data, formula){
#
#     # Extract vars
#     variables <- rownames(attr(terms.formula(formula), "factors"))
#     target_var <- variables[[1]]
#     predictor_vars <- tail(variables, (length(variables)-1))
#
#     term_labels <- attr(terms.formula(formula), "term.labels")
#
#     stopifnot(length(predictor_vars) == length(term_labels))
#
#     # Extract predictors
#     x <- data %>% dplyr::ungroup() %>%
#       dplyr::select(!!!predictor_vars) %>%
#       as.matrix()
#
#     # Extract targets
#     y <- as.matrix(data[[target_var]])
#
#     list("X" = x, "y" = y)
#   }
#
#   keras_model_fn <- function(train_data, formula){
#
#     # Extract variables and make sure formula doesn't have interaction terms, etc.
#     # We could technically use interactions here
#     # but a neural will find interactions anyway
#     data_list <- extract_data(train_data, formula)
#     x_train <- data_list[["X"]]
#     y_train <- to_categorical(data_list[["y"]])
#
#     # Model
#
#     # Specify model
#     model <- keras_model_sequential()
#     model %>%
#       layer_dense(units = 25, activation = 'relu', input_shape = c(ncol(x_train))) %>%
#       layer_dense(units = 25, activation = 'relu') %>%
#       layer_dense(units = 2, activation = "softmax") # output layer
#
#     # Compile model
#     model %>% compile(
#       loss = 'categorical_crossentropy',
#       optimizer = optimizer_rmsprop(),
#       metrics = list("accuracy")
#     )
#
#     # Fit model
#     model %>% fit(
#       x_train,
#       y_train,
#       epochs = 5,
#       batch_size = NULL
#     )
#
#     model
#   }
#
#   keras_predict_fn <- function(test_data, model, formula = NULL){
#
#     # Prepare data
#     data_list <- extract_data(test_data, formula)
#     x_test <- data_list[["X"]]
#
#     # Predict test set
#     predict(model, x_test)[,2]
#
#   }
#
#   # kn <- keras_model_fn(train_data = dat, formula = stats::as.formula("diagnosis~score+age"))
#   # keras_predict_fn(test_data = dat, model = kn, formula = stats::as.formula("score~diagnosis+age"))
#
#   # Cross-validate the data
#   CVbinomlist <- cross_validate_fn(
#     dat,
#     model_fn = keras_model_fn,
#     formulas = "diagnosis~score+age",
#     fold_cols = '.folds',
#     type = 'binomial',
#     predict_fn = keras_predict_fn
#   )
#
#   expect_equal(CVbinomlist$AUC, 0.5833333, tolerance=1e-3)
#   expect_equal(CVbinomlist$`Lower CI`, 0.3624639, tolerance=1e-3)
#   expect_equal(CVbinomlist$`Upper CI`, 0.8042027, tolerance=1e-3)
#   expect_equal(CVbinomlist$Kappa, 0.1666667, tolerance=1e-3)
#   expect_equal(CVbinomlist$Sensitivity, 0.6666667, tolerance=1e-3)
#   expect_equal(CVbinomlist$Specificity, 0.5, tolerance=1e-3)
#   expect_equal(CVbinomlist$`Pos Pred Value`, 0.6666667, tolerance=1e-3)
#   expect_equal(CVbinomlist$`Neg Pred Value`, 0.5, tolerance=1e-3)
#   expect_equal(CVbinomlist$F1, 0.6666667, tolerance=1e-3)
#   expect_equal(CVbinomlist$Prevalence, 0.6, tolerance=1e-3)
#   expect_equal(CVbinomlist$`Detection Rate`, 0.4, tolerance=1e-3)
#   expect_equal(CVbinomlist$`Detection Prevalence`, 0.6, tolerance=1e-3)
#   expect_equal(CVbinomlist$`Balanced Accuracy`, 0.5833333, tolerance=1e-3)
#   expect_equal(CVbinomlist$MCC, 0.1666667, tolerance=1e-3)
#   expect_equal(CVbinomlist$Folds, 4)
#   expect_equal(CVbinomlist$`Fold Columns`, 1)
#   expect_equal(CVbinomlist$`Convergence Warnings`, 0)
#   expect_equal(CVbinomlist$Family, 'binomial')
#   expect_equal(CVbinomlist$Dependent, 'diagnosis')
#   expect_equal(CVbinomlist$Fixed, "score+age")
#
#   # Enter sub tibbles
#   expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
#   expect_is(CVbinomlist$ROC[[1]], "tbl_df")
#   expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
#   expect_equal(colnames(CVbinomlist$ROC[[1]]), c("Sensitivities","Specificities"))
#   expect_equal(nrow(CVbinomlist$Predictions[[1]]),30)
#   expect_equal(nrow(CVbinomlist$ROC[[1]]),31)
})

test_that("binomial tidymodels work with cross_validate_fn()",{

  testthat::skip("tidymodels have too many dependencies")

  # # Load data and fold it
  # set_seed_for_R_compatibility(1)
  # dat <- groupdata2::fold(participant.scores, k = 4,
  #                         cat_col = 'diagnosis',
  #                         id_col = 'participant')
  # dat[["diagnosis"]] <- factor(dat[["diagnosis"]])
  #
  # library(tidymodels)
  #
  # tidyrf_model_fn <- function(train_data, formula){
  #
  #   rand_forest(trees = 100, mode = "classification") %>%
  #     set_engine("randomForest") %>%
  #     fit(formula, data = train_data)
  # }
  #
  # tidyrf_predict_fn <- function(test_data, model, formula = NULL){
  #   stats::predict(object = model, new_data = test_data, type = "prob")[[2]]
  # }
  #
  # CVbinomlist <- cross_validate_fn(dat,
  #                                  model_fn = tidyrf_model_fn,
  #                                  formulas = c("diagnosis~score", "diagnosis~age"),
  #                                  fold_cols = '.folds',
  #                                  type = 'binomial',
  #                                  predict_fn = tidyrf_predict_fn,
  #                                  positive = 1)
  #
  #
  # # rf <- tidyrf_model_fn(train_data = dat, formula = as.formula("diagnosis~score"))
  # # tidyrf_predict_fn(model = rf, test_data = dat)
  #
  # expect_equal(CVbinomlist$AUC, c(0.659722222222222, 0.208333333333333), tolerance=1e-3)
  # # expect_equal(CVbinomlist$`Lower CI`, c(0.367800267130833, 0.161076004187493), tolerance=1e-3)
  # # expect_equal(CVbinomlist$`Upper CI`, c(0.743310843980278, 0.505590662479174), tolerance=1e-3)
  # # expect_equal(CVbinomlist$Kappa, c(0.10958904109589, -0.296296296296296), tolerance=1e-3)
  # # expect_equal(CVbinomlist$Sensitivity, c(0.5,0.5), tolerance=1e-3)
  # # expect_equal(CVbinomlist$Specificity, c(0.611111111111111, 0.166666666666667), tolerance=1e-3)
  # # expect_equal(CVbinomlist$`Pos Pred Value`, c(0.461538461538462, 0.285714285714286), tolerance=1e-3)
  # # expect_equal(CVbinomlist$`Neg Pred Value`, c(0.647058823529412, 0.333333333333333), tolerance=1e-3)
  # # expect_equal(CVbinomlist$F1, c(0.48, 0.363636363636364), tolerance=1e-3)
  # # expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  # # expect_equal(CVbinomlist$`Detection Rate`, c(0.2, 0.2), tolerance=1e-3)
  # # expect_equal(CVbinomlist$`Detection Prevalence`, c(0.433333333333333, 0.7), tolerance=1e-3)
  # # expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.555555555555556, 0.333333333333333), tolerance=1e-3)
  # # expect_equal(CVbinomlist$MCC, c(0.109847007276218, -0.356348322549899), tolerance=1e-3)
  # expect_equal(CVbinomlist$Folds, c(4,4))
  # expect_equal(CVbinomlist$`Fold Columns`, c(1,1))
  # expect_equal(CVbinomlist$`Convergence Warnings`, c(0,0))
  # expect_equal(CVbinomlist$Family, c('binomial','binomial'))
  # expect_equal(CVbinomlist$Dependent, c('diagnosis','diagnosis'))
  # expect_equal(CVbinomlist$Fixed, c('score','age'))
  #
  # # Enter sub tibbles
  # expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  # expect_is(CVbinomlist$ROC[[1]], "tbl_df")
  # expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  # expect_equal(colnames(CVbinomlist$ROC[[1]]), c("Sensitivities","Specificities"))
  # expect_equal(nrow(CVbinomlist$Predictions[[1]]),30)
  # expect_equal(nrow(CVbinomlist$ROC[[1]]),23)

})

# Metrics list arg

test_that("binomial glm model with metrics list works with cross_validate_fn()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  glm_model_fn <- function(train_data, formula){
    glm(formula = formula, data = train_data, family = "binomial")
  }

  CVbinomlist <- cross_validate_fn(dat,
                                   glm_model_fn,
                                   formulas = c("diagnosis~score","diagnosis~age"),
                                   fold_cols = '.folds', type = 'binomial',
                                   metrics = list("Balanced Accuracy" = FALSE,
                                                  "Accuracy" = TRUE,
                                                  "Specificity" = FALSE),
                                   positive = 1)

  expect_equal(CVbinomlist$Accuracy, c(0.766666666666667, 0.4), tolerance=1e-3)
  expect_equal(colnames(CVbinomlist),
               c("Accuracy", "F1", "Sensitivity", "Pos Pred Value", "Neg Pred Value",
                 "AUC", "Lower CI", "Upper CI", "Kappa", "MCC", "Detection Rate",
                 "Detection Prevalence", "Prevalence", "Predictions", "ROC", "Confusion Matrix",
                 "Coefficients", "Folds", "Fold Columns", "Convergence Warnings",
                 "Other Warnings", "Warnings and Messages", "Family", "Dependent",
                 "Fixed"))
})

test_that("gaussian lm model with metrics list works with cross_validate_fn()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  lm_model_fn <- function(train_data, formula){
    lm(formula = formula, data = train_data)
  }

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
                            model_fn = lm_model_fn,
                            formulas = "score~diagnosis",
                            fold_cols = '.folds',
                            metrics = list("RMSE" = FALSE,
                                           "r2m" = FALSE),
                            type = 'gaussian')

  expect_equal(colnames(CVed),
               c("MAE", "r2c", "AIC", "AICc", "BIC", "Predictions", "Results",
                 "Coefficients", "Folds", "Fold Columns", "Convergence Warnings",
                 "Other Warnings", "Warnings and Messages", "Family", "Dependent",
                 "Fixed"))

})

test_that("multinomial nnet model with metrics list works with cross_validate_fn()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)

  # Create and fold dataset
  data_mc <- multiclass_probability_tibble(
    num_classes = 3, num_observations = 50,
    apply_softmax = TRUE, FUN = runif,
    class_name = "predictor_")
  class_names <- paste0("class_", c(1,2,3))
  data_mc[["target"]] <- factor(sample(x = class_names,
                                       size = 50, replace = TRUE))
  dat <- groupdata2::fold(data_mc, k = 4)

  multinom_model_fn <- function(train_data, formula){

    nnet::multinom(formula = formula, # converted to formula object within custom_fit_model()
                   data = train_data)
  }

  CVmultinomlist <- cross_validate_fn(dat,
                                      multinom_model_fn,
                                      formulas = c("target ~ predictor_1 + predictor_2 + predictor_3",
                                                   "target ~ predictor_1"),
                                      fold_cols = '.folds', type = 'multinomial',
                                      predict_type = "probs",
                                      metrics = list(
                                        "Accuracy" = TRUE,
                                        "F1" = FALSE,
                                        "Weighted Accuracy" = TRUE,
                                        "Weighted AUC" = TRUE
                                      ),
                                      positive = 1)

  expect_equal(CVmultinomlist$`Overall Accuracy`, c(0.18, 0.3), tolerance=1e-3)
  expect_equal(CVmultinomlist$Accuracy, c(0.453333333333333, 0.533333333333333), tolerance=1e-3)
  expect_equal(CVmultinomlist$`Balanced Accuracy`, c(0.381448412698413, 0.462714947089947), tolerance=1e-3)
  expect_equal(CVmultinomlist$`Weighted Accuracy`, c(0.4464, 0.5264), tolerance=1e-3)
  expect_equal(CVmultinomlist$`Weighted AUC`, c(0.338055555555556, 0.396527777777778), tolerance=1e-3)
  expect_equal(colnames(CVmultinomlist),
               c("Overall Accuracy", "Balanced Accuracy", "Accuracy", "Weighted Accuracy",
                 "Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value",
                 "AUC", "Weighted AUC", "Lower CI", "Upper CI", "Kappa", "MCC",
                 "Detection Rate", "Detection Prevalence", "Prevalence", "Class Level Results",
                 "Predictions", "Confusion Matrix", "Coefficients", "Folds", "Fold Columns",
                 "Convergence Warnings", "Other Warnings", "Warnings and Messages",
                 "Family", "Dependent", "Fixed"))

  # Enter sub tibbles
  class_level_results <- CVmultinomlist$`Class Level Results`

  expect_equal(class_level_results[[1]]$Class,
               c("class_1", "class_2", "class_3"))
  expect_equal(class_level_results[[2]]$Class,
               c("class_1", "class_2", "class_3"))
  expect_equal(class_level_results[[1]]$Accuracy,
               c(0.54, 0.34, 0.48))
  expect_equal(class_level_results[[2]]$Accuracy,
               c(0.62, 0.46, 0.52))
  expect_equal(class_level_results[[1]]$`Balanced Accuracy`,
               c(0.418650793650794, 0.302083333333333, 0.423611111111111))
  expect_equal(class_level_results[[2]]$`Balanced Accuracy`,
               c(0.452380952380952, 0.407986111111111, 0.527777777777778))
  expect_equal(colnames(class_level_results[[1]]),
               c("Class", "Balanced Accuracy", "Accuracy", "Sensitivity", "Specificity",
                 "Pos Pred Value", "Neg Pred Value", "AUC", "Lower CI", "Upper CI",
                 "Kappa", "MCC", "Detection Rate", "Detection Prevalence", "Prevalence",
                 "Support", "ROC", "Confusion Matrix"))

})
