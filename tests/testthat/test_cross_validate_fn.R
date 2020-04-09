library(cvms)
context("cross_validate_fn()")

test_that("binomial glm model works with cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  # Not used atm.
  dat2 <- groupdata2::fold(participant.scores,
    k = 4,
    num_fold_cols = 3,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  glm_model_fn <- function(train_data, formula, hyperparameters) {
    glm(formula = formula, data = train_data, family = "binomial")
  }

  glm_predict_fn <- example_predict_functions("glm_binomial")

  CVbinomlist <- cross_validate_fn(
    data = dat,
    model_fn = glm_model_fn,
    predict_fn = glm_predict_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", type = "binomial",
    metrics = list(
      "AIC" = TRUE, "AICc" = TRUE,
      "BIC" = TRUE
    ),
    positive = 1
  )

  expect_equal(CVbinomlist$AUC, c(0.7615741, 0.1666667), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Lower CI`, c(0.58511535, 0.01748744), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9380328, 0.3158459), tolerance = 1e-5)
  expect_equal(CVbinomlist$Kappa, c(0.4927536, -0.3636364), tolerance = 1e-5)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333, 0.0000000), tolerance = 1e-5)
  expect_equal(CVbinomlist$Specificity, c(0.8888889, 0.6666667), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7777778, 0.0000000), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7619048, 0.5), tolerance = 1e-5)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance = 1e-5)
  expect_equal(CVbinomlist$Prevalence, c(0.4, 0.4), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333, 0.0000000), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3, 0.2), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111, 0.3333333), tolerance = 1e-5)
  expect_equal(CVbinomlist$MCC, c(0.5048268, -0.4082483), tolerance = 1e-5)
  expect_equal(CVbinomlist$AIC, c(27.30328, 33.25823), tolerance = 1e-5)
  expect_equal(CVbinomlist$AICc, c(27.92233, 33.87728), tolerance = 1e-5)
  expect_equal(CVbinomlist$BIC, c(29.52586, 35.48081), tolerance = 1e-5)
  expect_equal(CVbinomlist$Folds, c(4, 4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1, 1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0, 0))
  expect_equal(CVbinomlist$Family, c("binomial", "binomial"))
  expect_equal(CVbinomlist$Dependent, c("diagnosis", "diagnosis"))
  expect_equal(CVbinomlist$Fixed, c("score", "age"))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]]$.folds, "roc")
  expect_equal(
    colnames(CVbinomlist$Predictions[[1]]),
    c("Fold Column", "Fold", "Observation", "Target", "Prediction", "Predicted Class")
  )
  expect_equal(sum(CVbinomlist$ROC[[1]]$.folds$sensitivities), 17.7222222222222, tolerance = 1e-5)
  expect_equal(nrow(CVbinomlist$Predictions[[1]]), 30)
  expect_equal(
    CVbinomlist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

  expect_equal(
    colnames(CVbinomlist),
    c(
      "Fixed", "Balanced Accuracy", "F1", "Sensitivity", "Specificity", "Pos Pred Value",
      "Neg Pred Value", "AUC", "Lower CI", "Upper CI", "Kappa", "MCC",
      "Detection Rate", "Detection Prevalence", "Prevalence", "AIC",
      "AICc", "BIC", "Predictions", "ROC", "Confusion Matrix", "Results",
      "Coefficients", "Folds", "Fold Columns", "Convergence Warnings",
      "Other Warnings", "Warnings and Messages", "Positive Class",
      "Family", "Dependent"
    )
  )


  # Check error when no model_fn is provided
  expect_error(
    xpectr::strip_msg(cross_validate_fn(dat,
    model_fn = NULL,
    predict_fn = glm_predict_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", type = "binomial"
  )),
  xpectr::strip(paste0(
    "1 assertions failed:\n * Variable 'model_fn': Must be a fun",
    "ction, not 'NULL'."
  )),
  fixed = TRUE
  )

  # Check error when no predict_fn is provided
  expect_error(
    xpectr::strip_msg(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    predict_fn = NULL,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", type = "binomial"
  )),
  xpectr::strip(paste0(
    "1 assertions failed:\n * Variable 'predict_fn': Must be a f",
    "unction, not 'NULL'."
  )),
  fixed = TRUE
  )

  # Check error when wrong model_fn type is provided
  expect_error(
    xpectr::strip_msg(cross_validate_fn(dat,
    model_fn = 3,
    predict_fn = glm_predict_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", type = "binomial"
  )),
  xpectr::strip(paste0(
    "1 assertions failed:\n * Variable 'model_fn': Must be a fun",
    "ction, not 'double'."
  )),
  fixed = TRUE
  )

  # Check error when wrong predict_fn type is provided
  expect_error(
    xpectr::strip_msg(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    predict_fn = 3,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", type = "binomial"
  )),
    xpectr::strip(paste0(
    "1 assertions failed:\n * Variable 'predict_fn': Must be a f",
    "unction, not 'double'."
  )),
  fixed = TRUE
  )

  expect_error(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    predict_fn = glm_predict_fn,
    formulas = c("score", "diagnosis~age"),
    fold_cols = ".folds", type = "binomial"
  ),
  "The model formula does not contain a dependent variable.",
  fixed = TRUE
  )

  expect_error(
    xpectr::strip_msg(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    predict_fn = glm_predict_fn,
    formulas = c("score", "diagnosis~age"),
    fold_cols = ".folds",
    type = "fishcat"
  )),
    xpectr::strip(paste0(
    "1 assertions failed:\n * Variable 'family/type': Must be element",
    " of set\n * {'gaussian','binomial','multinomial'}, but is 'f",
    "ishcat'."
  )),
  fixed = TRUE
  )

  # wrong predict fn
  wrong_predict_fn <- function(test_data, model, formula = NULL, hyperparameters = NULL, train_data = NULL) {
    tibble::tibble(
      "1" = stats::predict(
        object = model, newdata = test_data,
        type = "response", allow.new.levels = TRUE
      ),
      "2" = stats::predict(
        object = model, newdata = test_data,
        type = "response", allow.new.levels = TRUE
      )
    )
  }

  expect_error(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds",
    predict_fn = wrong_predict_fn,
    type = "binomial"
  ),
  paste0(
    "When 'type'/'family' is 'binomial', the predictions must ",
    "be a vector or matrix / data frame with one column ",
    "but was a data frame with 2 columns. Did you specify",
    " 'predict_fn' correctly?"
  ),
  fixed = TRUE
  )

  expect_error(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds",
    predict_fn = function(test_data, model, formula = NULL,
                          hyperparameters = NULL, train_data = NULL) {
      NULL
    },
    type = "binomial"
  ),
  paste0("cross_validate_fn(): predictions were NULL."),
  fixed = TRUE
  )
  expect_error(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds",
    predict_fn = function(test_data, model, formula = NULL,
                          hyperparameters = NULL, train_data = NULL) {
      lm
    },
    type = "binomial"
  ),
  paste0(
    "Could not use the obtained predictions. ",
    "Did you specify 'predict_fn' correctly? ",
    "The original error was: Error in as.vector(x, mode): cannot coerce ",
    "type 'closure' to vector of type 'any'"
  ),
  fixed = TRUE
  )
  expect_error(
    xpectr::strip_msg(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds",
    predict_fn = NULL,
    type = "binomial"
  )),
    xpectr::strip(paste0(
    "1 assertions failed:\n * Variable 'predict_fn': Must be a f",
    "unction, not 'NULL'."
  )),
  fixed = TRUE
  )
  expect_error(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds",
    predict_fn = function(test_data, model, formula = NULL,
                          hyperparameters = NULL,
                          train_data = NULL) {
      c("a", "b", "d")
    },
    type = "binomial"
  ),
  paste0("The number of predictions did not match the number of rows in the test set."),
  fixed = TRUE
  )
  expect_error(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds",
    predict_fn = function(test_data, model, formula = NULL,
                          hyperparameters = NULL,
                          train_data = NULL) {
      head(LETTERS, nrow(test_data))
    },
    type = "binomial"
  ),
  paste0("Could not convert predictions to type numeric."),
  fixed = TRUE
  )
  expect_error(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds",
    predict_fn = function(test_data, model, formula = NULL,
                          hyperparameters = NULL, train_data = NULL) {
      stop("predict_fn error")
    },
    type = "binomial"
  ),
  paste0(
    "Got the following error while using ",
    "specified 'predict_fn': Error in ",
    "user_predict_fn(test_data = test_data, model = model, ",
    "formula = stats::as.formula(formula), : predict_fn error"
  ),
  fixed = TRUE
  )
  expect_error(
    xpectr::strip_msg(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds",
    predict_fn = function(t_data, model, formula = NULL,
                          hyperparameters = NULL, train_data = NULL) {
      NULL
    },
    type = "binomial"
  )),
    xpectr::strip(paste0(
      "1 assertions failed:\n* Variable 'predict_fn argument names'",
      ": Must be a identical to\n* (test_data,model,formula,hyper",
      "parameters)."
  )),
  fixed = TRUE
  )

  expect_equal(
    run_predict_fn(
      test_data = data.frame(),
      train_data = NULL,
      model = NULL,
      model_formula = "",
      y_col = "",
      user_predict_fn = NULL,
      model_specifics = list()
    ),
    structure(list(prediction = logical(0)),
      row.names = integer(0),
      class = c(
        "tbl_df",
        "tbl", "data.frame"
      )
    )
  )
})

test_that("gaussian lm model works with cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  lm_model_fn <- function(train_data, formula, hyperparameters) {
    lm(formula = formula, data = train_data)
  }
  # summary(lmm <- lm_model_fn(dat, "score ~ diagnosis"))
  # MuMIn::AICc(lmm, REML = F) # The one used in the package

  lm_predict_fn <- example_predict_functions("lm")

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
    model_fn = lm_model_fn,
    predict_fn = lm_predict_fn,
    formulas = "score~diagnosis",
    fold_cols = ".folds",
    metrics = "all",
    type = "gaussian"
  )

  expect_equal(CVed$RMSE, 17.16817, tolerance = 1e-5)
  expect_equal(CVed$MAE, 14.26914, tolerance = 1e-5)
  expect_equal(CVed$`NRMSE(RNG)`, 0.316612, tolerance = 1e-5)
  expect_equal(CVed$`NRMSE(IQR)`, 0.9073991, tolerance = 1e-5)
  expect_equal(CVed$`NRMSE(STD)`, 0.9081804, tolerance = 1e-5)
  expect_equal(CVed$`NRMSE(AVG)`, 0.4335854, tolerance = 1e-5)
  expect_equal(CVed$RAE, 0.9859855, tolerance = 1e-5)
  expect_equal(CVed$RRSE, 0.9805019, tolerance = 1e-5)
  expect_equal(CVed$MAPE, 0.4886251, tolerance = 1e-5)
  expect_equal(CVed$MSE, 299.26, tolerance = 1e-5)
  expect_equal(CVed$TAE, 105.1333, tolerance = 1e-5)
  expect_equal(CVed$TSE, 2156.193, tolerance = 1e-5)
  expect_equal(CVed$r2m, 0.2640793, tolerance = 1e-5)
  expect_equal(CVed$r2c, 0.2640793, tolerance = 1e-5)
  expect_equal(CVed$AIC, 194.6904, tolerance = 1e-5)
  expect_equal(CVed$AICc, 195.9963, tolerance = 1e-5)
  expect_equal(CVed$BIC, 198.0243, tolerance = 1e-5)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Fold Columns`, 1)
  expect_equal(CVed$`Convergence Warnings`, 0)
  expect_equal(CVed$Family, "gaussian")
  expect_equal(CVed$Dependent, "score")
  expect_equal(CVed$Fixed, "diagnosis")
  expect_equal(
    CVed$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

  expect_equal(
    colnames(CVed),
    c("Fixed", "RMSE", "MAE", "NRMSE(RNG)", "NRMSE(IQR)", "NRMSE(STD)",
    "NRMSE(AVG)", "RSE", "RRSE", "RAE", "RMSLE", "MALE", "MAPE",
    "MSE", "TAE", "TSE", "r2m", "r2c", "AIC", "AICc", "BIC", "Predictions",
    "Results", "Coefficients", "Folds", "Fold Columns", "Convergence Warnings",
    "Other Warnings", "Warnings and Messages", "Family", "Dependent"
    )
  )

  # Error when formulas have random effects but lm model

  # Cross-validate the model function
  warnings_and_messages <- dplyr::bind_rows(
    suppressWarnings(
      cross_validate_fn(
        dat,
        model_fn = lm_model_fn,
        predict_fn = lm_predict_fn,
        formulas = c(
          "score~diagnosis+(1|session)",
          "score~age+(1|session)"
        ),
        type = "gaussian",
        fold_cols = ".folds"
      )
    )$`Warnings and Messages`
  )

  expect_equal(
    warnings_and_messages$`Fold Column`,
    rep(".folds", 8)
  )
  expect_equal(
    warnings_and_messages$Fold,
    c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L)
  )
  expect_equal(
    warnings_and_messages$Function,
    rep("predict_fn", 8)
  )
  expect_equal(
    warnings_and_messages$Message,
    rep("prediction from a rank-deficient fit may be misleading", 8)
  )
})

test_that("binomial glm model with preprocess_fn works with cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)

  # Not used atm.
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    num_fold_cols = 3,
    cat_col = "diagnosis",
    id_col = "participant"
  ) %>%
    dplyr::mutate(diagnosis = as.factor(diagnosis))

  glm_model_fn <- example_model_functions("glm_binomial")

  glm_predict_fn <- example_predict_functions("glm_binomial")

  # The example fn requires formula, but we need it hardcoded for test
  glm_preprocess_fn <- function(train_data, test_data, formula, hyperparameters) {

    # Create recipes object
    recipe_object <- recipes::recipe(

      # Note: If we hardcoded the formula instead of using the formula argument
      # we could preprocess the train/test splits once
      # instead of for every formula
      # Tip: Use `y ~ .` to include all predictors (where `y` is your dependent variable)
      formula = diagnosis ~ .,
      data = train_data
    ) %>%

      # Add preprocessing steps
      # Note: We could add specific variable to each step
      # instead of just selecting all numeric variables
      recipes::step_center(recipes::all_numeric()) %>%
      recipes::step_scale(recipes::all_numeric()) %>%

      # Find parameters from the training set
      recipes::prep(training = train_data)

    # Apply preprocessing to the partitions
    train_data <- recipes::bake(recipe_object, train_data)
    test_data <- recipes::bake(recipe_object, test_data)

    # Extract the preprocessing parameters
    means <- recipe_object$steps[[1]]$means
    sds <- recipe_object$steps[[2]]$sds

    # Add preprocessing parameters to a tibble
    tidy_parameters <- tibble::tibble("Measure" = c("Mean", "SD")) %>%
      dplyr::bind_cols(dplyr::bind_rows(means, sds))

    list(
      "train" = train_data,
      "test" = test_data,
      "parameters" = tidy_parameters
    )
  }

  CVbinomlist_prep_all <- cross_validate_fn(
    data = dat,
    model_fn = glm_model_fn,
    predict_fn = glm_predict_fn,
    preprocess_fn = glm_preprocess_fn,
    preprocess_once = FALSE,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = paste0(".folds_", 1:3),
    type = "binomial",
    metrics = list(
      "AIC" = TRUE,
      "AICc" = TRUE,
      "BIC" = TRUE
    ),
    positive = 2
  )

  CVbinomlist_prep_once <- cross_validate_fn(
    data = dat,
    model_fn = glm_model_fn,
    predict_fn = glm_predict_fn,
    preprocess_fn = glm_preprocess_fn,
    preprocess_once = TRUE,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = paste0(".folds_", 1:3),
    type = "binomial",
    metrics = list(
      "AIC" = TRUE,
      "AICc" = TRUE,
      "BIC" = TRUE
    ),
    positive = 2
  )

  expect_identical(
    CVbinomlist_prep_all,
    CVbinomlist_prep_once
  )

  expect_identical(
    CVbinomlist_prep_once$Preprocess,
    CVbinomlist_prep_all$Preprocess
  )

  all_preprocess_params <- dplyr::bind_rows(CVbinomlist_prep_once$Preprocess)
  expect_equal(
    all_preprocess_params$`Fold Column`,
    rep(rep(paste0(".folds_", 1:3), each = 8), 2)
  )
  expect_equal(
    all_preprocess_params$Fold,
    rep(rep(1:4, each = 2), 6)
  )
  expect_equal(
    all_preprocess_params$Measure,
    rep(c("Mean", "SD"), 24)
  )
  expect_equal(all_preprocess_params$age,
    rep(c(
      28.875, 7.39160805002656, 28.2857142857143, 5.12974518999587,
      27.25, 7.51953976389975, 29.2857142857143, 7.93185260290972,
      26.125, 5.36747450936617, 30.7142857142857, 7.13542470454883,
      27.375, 7.59182913171901, 29.8571428571429, 7.35721220494362,
      28.625, 7.62611360364191, 28.5714285714286, 4.73889679747754,
      28.625, 7.24006065312091, 27.7142857142857, 8.33752275644785
    ), 2),
    tolerance = 1e-6
  )
  expect_equal(all_preprocess_params$score,
    rep(c(
      37.75, 18.6925932785759, 41.2380952380952, 19.7177705684612,
      36.2916666666667, 19.4276342104357, 40.2857142857143, 19.4220051929322,
      37.9583333333333, 18.4708446328065, 39.1904761904762, 19.8459543676263,
      39.25, 19.7070942865099, 38.7142857142857, 19.6268766163719,
      37.1666666666667, 19.4794577789609, 39.7142857142857, 19.0030072808039,
      38.2916666666667, 20.0227678377406, 40.1904761904762, 18.8245027759541
    ), 2),
    tolerance = 1e-6
  )
  expect_equal(all_preprocess_params$session,
    rep(c(
      2, 0.834057656228299, 2, 0.836660026534076, 2, 0.834057656228299,
      2, 0.836660026534076, 2, 0.834057656228299, 2, 0.836660026534076,
      2, 0.834057656228299, 2, 0.836660026534076, 2, 0.834057656228299,
      2, 0.836660026534076, 2, 0.834057656228299, 2, 0.836660026534076
    ), 2),
    tolerance = 1e-6
  )


  expect_equal(CVbinomlist_prep_once$AUC, c(0.744598765432099, 0.256944444444444), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$`Lower CI`, c(0.557694303281996, 0.088133274673661), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$`Upper CI`, c(0.931503227582202, 0.430603742698332), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$Kappa, c(0.47135955831608, -0.212121212121212), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$Sensitivity, c(0.87037037037037, 0.722222222222222), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$Specificity, c(0.583333333333333, 0.0833333333333333), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$`Pos Pred Value`, c(0.757936507936508, 0.541666666666667), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$`Neg Pred Value`, c(0.751851851851852, 0.166666666666667), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$F1, c(0.810166441745389, 0.619047619047619), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$Prevalence, c(0.6, 0.6), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$`Detection Rate`, c(0.522222222222222, 0.433333333333333), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$`Detection Prevalence`, c(0.688888888888889, 0.8), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$`Balanced Accuracy`, c(0.726851851851852, 0.402777777777778), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$MCC, c(0.480888760816756, -0.23814483610392), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$AIC, c(27.1161243598453, 33.3259599731777), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$AICc, c(27.7351719788929, 33.9450075922254), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$BIC, c(29.3387006279166, 35.5485362412491), tolerance = 1e-5)
  expect_equal(CVbinomlist_prep_once$Folds, c(12, 12))
  expect_equal(CVbinomlist_prep_once$`Fold Columns`, c(3, 3))
  expect_equal(CVbinomlist_prep_once$`Convergence Warnings`, c(0, 0))
  expect_equal(CVbinomlist_prep_once$Family, c("binomial", "binomial"))
  expect_equal(CVbinomlist_prep_once$Dependent, c("diagnosis", "diagnosis"))
  expect_equal(CVbinomlist_prep_once$Fixed, c("score", "age"))

  # Enter sub tibbles
  expect_is(CVbinomlist_prep_once$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist_prep_once$ROC[[1]]$.folds_1, "roc")
  expect_equal(length(CVbinomlist_prep_once$ROC), 2)
  expect_equal(length(CVbinomlist_prep_once$ROC[[1]]), 3)
  expect_equal(length(CVbinomlist_prep_once$ROC[[2]]), 3)
  expect_equal(
    names(CVbinomlist_prep_once$ROC[[1]]),
    c(".folds_1", ".folds_2", ".folds_3")
  )
  expect_equal(
    as.numeric(CVbinomlist_prep_once$ROC[[2]]$.folds_3$auc),
    0.125
  )
  expect_equal(
    CVbinomlist_prep_once$ROC[[2]]$.folds_3$direction,
    "<"
  )
  expect_equal(
    CVbinomlist_prep_once$ROC[[2]]$.folds_3$sensitivities,
    c(
      1, 0.833333333333333, 0.666666666666667, 0.5, 0.333333333333333,
      0.166666666666667, 0.166666666666667, 0.166666666666667, 0.166666666666667,
      0, 0
    )
  )
  expect_equal(
    CVbinomlist_prep_once$ROC[[2]]$.folds_3$specificities,
    c(0, 0, 0, 0, 0, 0, 0.25, 0.5, 0.75, 0.75, 1)
  )

  expect_equal(
    colnames(CVbinomlist_prep_once$Predictions[[1]]),
    c("Fold Column", "Fold", "Observation", "Target", "Prediction", "Predicted Class")
  )
  expect_equal(nrow(CVbinomlist_prep_once$Predictions[[1]]), 90)
  expect_equal(
    CVbinomlist_prep_once$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

  # Check error when no model_fn is provided
  expect_error(
    xpectr::strip_msg(cross_validate_fn(dat,
    model_fn = glm_model_fn,
    predict_fn = glm_predict_fn,
    preprocess_fn = "notAFunction",
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds_1", type = "binomial"
  )),
    xpectr::strip(paste0(
    "1 assertions failed:\n * Variable 'preprocess_fn': Must be ",
    "a function (or 'NULL'), not\n * 'character'."
  )),
  fixed = TRUE
  )
})

test_that("binomial svm models from e1071 work with cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  svm_model_fn <- example_model_functions("svm_binomial")
  svm_predict_fn <- example_predict_functions("svm_binomial")

  # m <- svm_model_fn(dat, as.formula("diagnosis~score"), hyperparameters = hparams)
  # svm_predict_fn(dat, m, NULL, NULL)

  hparams <- list(
    "kernel" = "linear",
    "cost" = 10
  )

  CVbinomlist <- cross_validate_fn(dat,
    model_fn = svm_model_fn,
    predict_fn = svm_predict_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", type = "binomial",
    hyperparameters = hparams,
    positive = 1
  )

  expect_equal(CVbinomlist$AUC, c(0.733796296296296, 0.479166666666667), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Lower CI`, c(0.528997365284296, 0.241598686439125), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Upper CI`, c(0.938595227308296, 0.716734646894208), tolerance = 1e-5)
  expect_equal(CVbinomlist$Kappa, c(0.393939393939394, -0.0869565217391305), tolerance = 1e-5)
  expect_equal(CVbinomlist$Sensitivity, c(0.416666666666667, 0.25), tolerance = 1e-5)
  expect_equal(CVbinomlist$Specificity, c(0.944444444444444, 0.666666666666667), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.833333333333333, 0.333333333333333), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.708333333333333, 0.571428571428571), tolerance = 1e-5)
  expect_equal(CVbinomlist$F1, c(0.555555555555556, 0.285714285714286), tolerance = 1e-5)
  expect_equal(CVbinomlist$Prevalence, c(0.4, 0.4), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.166666666666667, 0.1), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.2, 0.3), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.680555555555556, 0.458333333333333), tolerance = 1e-5)
  expect_equal(CVbinomlist$MCC, c(0.442268981335852, -0.0890870806374748), tolerance = 1e-5)
  expect_equal(CVbinomlist$Folds, c(4, 4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1, 1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0, 0))
  expect_equal(CVbinomlist$Family, c("binomial", "binomial"))
  expect_equal(CVbinomlist$Dependent, c("diagnosis", "diagnosis"))
  expect_equal(CVbinomlist$Fixed, c("score", "age"))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]]$.folds, "roc")
  expect_equal(
    colnames(CVbinomlist$Predictions[[1]]),
    c("Fold Column", "Fold", "Observation", "Target", "Prediction", "Predicted Class")
  )
  expect_equal(
    names(CVbinomlist$ROC[[1]]$.folds),
    c(
      "percent", "sensitivities", "specificities", "thresholds",
      "direction", "cases", "controls", "fun.sesp", "auc", "call",
      "original.predictor", "original.response", "predictor", "response",
      "levels"
    )
  )
  expect_equal(CVbinomlist$ROC[[1]]$.folds$sensitivities,
    c(
      1, 1, 1, 1, 1, 1, 0.944444444444444, 0.888888888888889, 0.888888888888889,
      0.888888888888889, 0.833333333333333, 0.833333333333333, 0.777777777777778,
      0.666666666666667, 0.611111111111111, 0.555555555555556, 0.5,
      0.444444444444444, 0.388888888888889, 0.388888888888889, 0.388888888888889,
      0.333333333333333, 0.277777777777778, 0.222222222222222, 0.222222222222222,
      0.166666666666667, 0.111111111111111, 0.0555555555555556, 0
    ),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlist$ROC[[1]]$.folds$specificities,
    c(
      0, 0.0833333333333333, 0.166666666666667, 0.25, 0.333333333333333,
      0.416666666666667, 0.416666666666667, 0.416666666666667, 0.5,
      0.583333333333333, 0.583333333333333, 0.666666666666667, 0.666666666666667,
      0.666666666666667, 0.666666666666667, 0.666666666666667, 0.666666666666667,
      0.666666666666667, 0.666666666666667, 0.75, 0.833333333333333,
      0.833333333333333, 0.833333333333333, 0.833333333333333, 0.916666666666667,
      1, 1, 1, 1
    ),
    tolerance = 1e-5
  )
  expect_equal(nrow(CVbinomlist$Predictions[[1]]), 30)
  expect_equal(
    CVbinomlist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("gaussian svm models from e1071 work with cross_validate_fn()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  svm_model_fn <- function(train_data, formula, hyperparameters) {
    e1071::svm(
      formula = formula, # converted to formula object within fit_model()
      data = train_data,
      kernel = "linear",
      cost = 10,
      scale = FALSE,
      type = "eps-regression"
    )
  }

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
    model_fn = svm_model_fn,
    predict_fn = example_predict_functions("svm_gaussian"),
    formulas = "score~diagnosis",
    fold_cols = ".folds",
    type = "gaussian"
  )

  expect_equal(
    colnames(CVed),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
    "Predictions", "Results", "Coefficients", "Folds", "Fold Columns",
    "Convergence Warnings", "Other Warnings", "Warnings and Messages",
    "Family", "Dependent")
  )
  expect_equal(CVed$RMSE, 18.01026, tolerance = 1e-5)
  expect_equal(CVed$MAE, 15.27778, tolerance = 1e-5)
  expect_equal(CVed$`NRMSE(IQR)`, 0.949782, tolerance = 1e-5)
  expect_equal(CVed$RRSE, 1.02155, tolerance = 1e-5)
  expect_equal(CVed$RAE, 1.0538027, tolerance = 1e-5)
  expect_equal(CVed$RMSLE, 0.4828787, tolerance = 1e-5)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Fold Columns`, 1)
  expect_equal(CVed$`Convergence Warnings`, 0)
  expect_equal(CVed$Family, "gaussian")
  expect_equal(CVed$Dependent, "score")
  expect_equal(CVed$Fixed, "diagnosis")

  expect_equal(
    colnames(CVed$Coefficients[[1]]),
    c("Fold Column", "Fold", "term", "estimate", "p.value")
  )
  expect_equal(
    CVed$Coefficients[[1]]$term,
    rep(c("(Intercept)", "diagnosis"), 4)
  )
  expect_equal(
    CVed$Coefficients[[1]]$estimate,
    c(40.1, -10, 50.1, -20, 40.1, -10, 45, -10)
  )
  expect_equal(
    CVed$Coefficients[[1]]$Fold,
    c(1L, 1L, 2L, 2L, 3L, 3L, 4L, 4L)
  )
  expect_equal(
    CVed$Coefficients[[1]]$`Fold Column`,
    rep(".folds", 8)
  )
  expect_equal(
    CVed$Coefficients[[1]]$p.value,
    rep(NA, 8)
  )
  expect_equal(
    CVed$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("gaussian svm models with hparams and preprocessing work with cross_validate_fn()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  svm_model_fn <- function(train_data, formula, hyperparameters) {
    warning("This is a model_fn warning")
    message("This is a model_fn message")
    e1071::svm(
      formula = formula, # converted to formula object within fit_model()
      data = train_data,
      kernel = hyperparameters[["kernel"]],
      cost = hyperparameters[["cost"]],
      scale = FALSE,
      type = "eps-regression"
    )
  }

  # broom::tidy(svm_model_fn(train_data = dat, formula = as.formula("score~diagnosis", train_data = NULL),
  #              hyperparameters = list(
  #                "kernel" = "linear", "cost" = 10)))

  svm_predict_fn <- function(test_data, model, formula, hyperparameters, train_data = NULL) {
    warning("This is a predict_fn warning")
    message("This is a predict_fn message")

    stats::predict(model, test_data, allow.new.levels = TRUE)
  }

  svm_preprocess_fn <- function(train_data, test_data, formula, hyperparameters) {

    # Test that warnings and messages are caught
    warning("This is a preprocess_fn warning")
    message("This is a preprocess_fn message")

    # Create recipes object
    recipe_object <- recipes::recipe(

      # We hardcode the formula instead of using the formula argument
      # so we can preprocess the train/test splits once
      # instead of for every formula
      # The dot means "all variables except for diagnosis"
      formula = score ~ .,
      data = train_data
    ) %>%

      # Add preprocessing steps
      # Note that scaling seems to make the model converge to the same results
      # for every hparams combination, which is great but we
      # prefer differences in our tests
      recipes::step_center(recipes::all_numeric()) %>%
      # recipes::step_scale(age, score) %>%

      # Find parameters from the training set
      recipes::prep(training = train_data)

    # Apply preprocessing to the partitions
    train_data <- recipes::bake(recipe_object, train_data)
    test_data <- recipes::bake(recipe_object, test_data)

    # Extract the preprocessing parameters
    means <- recipe_object$steps[[1]]$means
    # sds <- recipe_object$steps[[2]]$sds

    # Add preprocessing parameters to a tibble
    tidy_parameters <- tibble::tibble("Measure" = c(
      "Mean" # , "SD"
    )) %>%
      dplyr::bind_cols(dplyr::bind_rows(
        means # , sds
      ))

    list(
      "train" = train_data,
      "test" = test_data,
      "parameters" = tidy_parameters
    )
  }


  hparams <- list(
    ".n" = 5,
    "kernel" = c("linear", "polynomial", "sigmoid"),
    "cost" = c(1, 5, 10)
  )

  # Note for debugging: Remember that ".n" in hparams causes it to sample
  # hparams combinations, so run with the seed every time

  # Cross-validate the data
  suppressMessages(suppressWarnings(
    CVed <- cross_validate_fn(dat,
      model_fn = svm_model_fn,
      predict_fn = svm_predict_fn,
      preprocess_fn = svm_preprocess_fn,
      preprocess_once = FALSE, # TODO Try with TRUE as well
      hyperparameters = hparams,
      formulas = "score~diagnosis",
      fold_cols = ".folds",
      type = "gaussian"
    )
  ))

  expect_equal(CVed$RMSE,
    c(
      19.9498105361079, 18.028011231846,
      19.724604489995, 19.3906795721289,
      18.3020558729458
    ),
    tolerance = 1e-5
  )
  expect_equal(CVed$MAE,
    c(
      16.195563745625, 15.2813142143035,
      16.0889298392363, 15.8807791330459,
      15.3388888924368
    ),
    tolerance = 1e-5
  )
  expect_equal(CVed$Folds, rep(4, 5))
  expect_equal(CVed$`Fold Columns`, rep(1, 5))
  expect_equal(CVed$`Convergence Warnings`, rep(0, 5))
  expect_equal(CVed$Family, rep("gaussian", 5))
  expect_equal(CVed$Dependent, rep("score", 5))
  expect_equal(CVed$Fixed, rep("diagnosis", 5))

  expect_equal(
    colnames(CVed$Coefficients[[1]]),
    c(
      "Fold Column", "Fold", "term",
      "estimate", "std.error", "statistic", "p.value"
    )
  )

  expect_equal(
    CVed$Coefficients[[1]],
    structure(list(
      `Fold Column` = c(NA, NA, NA, NA),
      Fold = c(NA, NA, NA, NA),
      term = c(NA, NA, NA, NA),
      estimate = c(NA, NA, NA, NA),
      std.error = c(NA, NA, NA, NA),
      statistic = c(NA, NA, NA, NA),
      p.value = c(NA, NA, NA, NA)
    ),
    row.names = c(NA, -4L),
    class = c("tbl_df", "tbl", "data.frame")
    )
  )

  # Note: When fixing the double warning in predict_fn, this will fail
  expect_equal(
    CVed$`Warnings and Messages`[[1]]$`Fold Column`,
    rep(".folds", 24)
  )

  expect_equal(
    CVed$`Warnings and Messages`[[1]]$Fold,
    c(
      1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L,
      3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L
    )
  )
  expect_equal(
    CVed$`Warnings and Messages`[[1]]$Function,
    c(
      "model_fn", "model_fn", "predict_fn", "predict_fn", "preprocess_fn",
      "preprocess_fn", "model_fn", "model_fn", "predict_fn", "predict_fn",
      "preprocess_fn", "preprocess_fn", "model_fn", "model_fn", "predict_fn",
      "predict_fn", "preprocess_fn", "preprocess_fn", "model_fn", "model_fn",
      "predict_fn", "predict_fn", "preprocess_fn", "preprocess_fn"
    )
  )
  expect_equal(
    CVed$`Warnings and Messages`[[1]]$Type,
    c(
      "warning", "message", "warning", "message", "warning", "message",
      "warning", "message", "warning", "message", "warning", "message",
      "warning", "message", "warning", "message", "warning", "message",
      "warning", "message", "warning", "message", "warning", "message"
    )
  )
  expect_equal(
    CVed$`Warnings and Messages`[[1]]$Message,
    c(
      "This is a model_fn warning", "This is a model_fn message\n",
      "This is a predict_fn warning", "This is a predict_fn message\n",
      "This is a preprocess_fn warning", "This is a preprocess_fn message\n",
      "This is a model_fn warning", "This is a model_fn message\n",
      "This is a predict_fn warning", "This is a predict_fn message\n",
      "This is a preprocess_fn warning", "This is a preprocess_fn message\n",
      "This is a model_fn warning", "This is a model_fn message\n",
      "This is a predict_fn warning", "This is a predict_fn message\n",
      "This is a preprocess_fn warning", "This is a preprocess_fn message\n",
      "This is a model_fn warning", "This is a model_fn message\n",
      "This is a predict_fn warning", "This is a predict_fn message\n",
      "This is a preprocess_fn warning", "This is a preprocess_fn message\n"
    )
  )

  preprocess_params <- CVed$Preprocess[[1]]
  expect_equal(
    colnames(preprocess_params),
    c(
      "Fold Column", "Fold", "Measure",
      "age", "diagnosis", "session",
      "score"
    )
  )
  expect_equal(
    preprocess_params$`Fold Column`,
    rep(".folds", 4)
  )
  expect_equal(
    preprocess_params$Fold,
    1:4
  )
  expect_equal(
    preprocess_params$Measure,
    rep("Mean", 4)
  )
  expect_equal(
    preprocess_params$age,
    c(
      28.875, 28.2857142857143,
      27.25, 29.2857142857143
    )
  )
  expect_equal(
    preprocess_params$diagnosis,
    c(0.625, 0.571428571428571, 0.625, 0.571428571428571)
  )
  expect_equal(
    preprocess_params$session,
    c(2, 2, 2, 2)
  )
  expect_equal(
    preprocess_params$score,
    c(37.75, 41.2380952380952, 36.2916666666667, 40.2857142857143)
  )
})

test_that("binomial naiveBayes models from e1071 work with cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  nb_model_fn <- function(train_data, formula, hyperparameters) {
    e1071::naiveBayes(
      formula = formula, # converted to formula object within fit_model()
      data = train_data
    )
  }

  # nb_ <- nb_model_fn(data = dat, formula = as.formula("diagnosis~score"))

  nb_predict_fn <- function(test_data, model, formula = NULL, hyperparameters, train_data = NULL) {
    stats::predict(
      object = model, newdata = test_data, type = "raw",
      allow.new.levels = TRUE
    )[, 2]
  }

  nb_wrong_predict_fn <- function(test_data, model, formula = NULL, hyperparameters, train_data = NULL) {
    stats::predict(
      object = model, newdata = test_data, type = "raw",
      allow.new.levels = TRUE
    )
  }

  # nb_predict_fn(data = dat, model = nb_)
  # preds <- predict(nb_, dat, type = "raw", allow.new.levels = TRUE)

  expect_error(cross_validate_fn(dat,
    model_fn = nb_model_fn,
    predict_fn = nb_wrong_predict_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds",
    type = "binomial",
    positive = 1
  ),
  paste0(
    "When 'type'/'family' is 'binomial', ",
    "the predictions must be a vector or matrix / data frame ",
    "with one column but was a matrix with 2 columns. ",
    "Did you specify 'predict_fn' correctly?"
  ),
  fixed = TRUE
  )


  CVbinomlist <- cross_validate_fn(dat,
    model_fn = nb_model_fn,
    predict_fn = nb_predict_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds",
    type = "binomial",
    positive = 1
  )

  expect_equal(CVbinomlist$AUC, c(0.743055555555555, 0.125), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Lower CI`, c(0.555996282730279, 0), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Upper CI`, c(0.930114828380832, 0.264544385449311), tolerance = 1e-5)
  expect_equal(CVbinomlist$Kappa, c(0.492753623188406, -0.666666666666667), tolerance = 1e-5)
  expect_equal(CVbinomlist$Sensitivity, c(0.583333333333333, 0), tolerance = 1e-5)
  expect_equal(CVbinomlist$Specificity, c(0.888888888888889, 0.333333333333333), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.777777777777778, 0), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.761904761904762, 0.333333333333333), tolerance = 1e-5)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance = 1e-5)
  expect_equal(CVbinomlist$Prevalence, c(0.4, 0.4), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333, 0.0000000), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3, 0.4), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.736111111111111, 0.166666666666667), tolerance = 1e-5)
  expect_equal(CVbinomlist$MCC, c(0.504826790279024, -0.666666666666667), tolerance = 1e-5)
  expect_equal(CVbinomlist$Folds, c(4, 4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1, 1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0, 0))
  expect_equal(CVbinomlist$Family, c("binomial", "binomial"))
  expect_equal(CVbinomlist$Dependent, c("diagnosis", "diagnosis"))
  expect_equal(CVbinomlist$Fixed, c("score", "age"))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]]$.folds, "roc")
  expect_equal(length(CVbinomlist$ROC), 2)
  expect_equal(
    colnames(CVbinomlist$Predictions[[1]]),
    c(
      "Fold Column", "Fold", "Observation",
      "Target", "Prediction", "Predicted Class"
    )
  )
  expect_equal(nrow(CVbinomlist$Predictions[[1]]), 30)
  expect_equal(
    CVbinomlist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("binomial nnet models work with cross_validate_fn()", {

  # testthat::skip("mac and ubuntu give different warnings")
  testthat::skip_on_os("windows")
  testthat::skip_on_os("linux")
  testthat::skip_on_os("solaris")
  # Tested on both platforms on travis as well
  # Local test is a mix of ubuntu and mac derived results/predictions
  # so wouldn't run perfectly on either

  # Load data and fold it
  xpectr::set_test_seed(10)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  nnet_model_fn <- function(train_data, formula, hyperparameters) {
    nnet::nnet(
      formula = formula, # converted to formula object within fit_model()
      data = train_data,
      size = 50
    )
  }
  # nn <- nnet_model_fn(data = dat, formula = as.formula("diagnosis~score"))
  # predict(nn, dat, type = "raw", allow.new.levels = TRUE)

  nnet_predict_fn <- example_predict_functions("nnet_binomial")

  CVbinomlist <- cross_validate_fn(dat,
    model_fn = nnet_model_fn,
    predict_fn = nnet_predict_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", type = "binomial",
    positive = 1
  )

  expect_equal(CVbinomlist$AUC, c(0.611111111111111, 0.25), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Lower CI`, c(0.390732639152796, 0.0356744348642742), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Upper CI`, c(0.831489583069426, 0.464325565135726), tolerance = 1e-5)
  expect_equal(CVbinomlist$Kappa, c(0.202898550724638, -0.206896551724138), tolerance = 1e-5)
  expect_equal(CVbinomlist$Sensitivity, c(0.416666666666667, 0.75), tolerance = 1e-5)
  expect_equal(CVbinomlist$Specificity, c(0.777777777777778, 0), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.555555555555556, 0.333333333333333), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.666666666666667, 0.0), tolerance = 1e-5)
  expect_equal(CVbinomlist$F1, c(0.476190476190476, 0.461538461538462), tolerance = 1e-5)
  expect_equal(CVbinomlist$Prevalence, c(0.4, 0.4), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.1666667, 0.3000000), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3, 0.9), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.597222222222222, 0.375), tolerance = 1e-5)
  expect_equal(CVbinomlist$MCC, c(0.207869854820775, -0.408248290463863), tolerance = 1e-5)
  expect_equal(CVbinomlist$Folds, c(4, 4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1, 1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0, 0))
  expect_equal(CVbinomlist$Family, c("binomial", "binomial"))
  expect_equal(CVbinomlist$Dependent, c("diagnosis", "diagnosis"))
  expect_equal(CVbinomlist$Fixed, c("score", "age"))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]]$.folds, "roc")
  expect_equal(length(CVbinomlist$ROC), 2)
  expect_equal(
    colnames(CVbinomlist$Predictions[[1]]),
    c(
      "Fold Column", "Fold", "Observation",
      "Target", "Prediction", "Predicted Class"
    )
  )
  expect_equal(nrow(CVbinomlist$Predictions[[1]]), 30)
  expect_equal(CVbinomlist$ROC[[1]]$.folds$direction, "<")
  expect_equal(as.numeric(CVbinomlist$ROC[[1]]$.folds$auc), 0.6111111, tolerance = 1e-5)
  expect_equal(CVbinomlist$ROC[[1]]$.folds$thresholds,
    c(
      -Inf, 8.48402440378548e-07, 9.16581800969544e-05, 0.000182091056054732,
      0.000182363624985208, 0.000182649770463709, 0.0877260312252673,
      0.299068543231709, 0.425185694307719, 0.479254602781408, 0.532506217663556,
      0.539323658984721, 0.546642253280643, 0.564646428023567, 0.58403993602655,
      0.59060162523757, 0.594430000088956, 0.613459083405036, 0.63368526859679,
      0.63661340313648, 0.646432602162526, 0.66064998851649, 0.66782162508638,
      0.674808670406118, 0.840014561587637, 0.999952406924702, 0.999985574696044,
      Inf
    ),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlist$ROC[[1]]$.folds$sensitivities,
    c(
      1, 1, 1, 1, 0.944444444444444, 0.888888888888889, 0.888888888888889,
      0.888888888888889, 0.833333333333333, 0.777777777777778, 0.722222222222222,
      0.666666666666667, 0.611111111111111, 0.611111111111111, 0.555555555555556,
      0.5, 0.444444444444444, 0.444444444444444, 0.388888888888889,
      0.388888888888889, 0.333333333333333, 0.277777777777778, 0.277777777777778,
      0.277777777777778, 0.277777777777778, 0.277777777777778, 0.222222222222222,
      0
    ),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlist$ROC[[1]]$.folds$specificities,
    c(
      0, 0.0833333333333333, 0.166666666666667, 0.25, 0.25, 0.25,
      0.333333333333333, 0.416666666666667, 0.416666666666667, 0.416666666666667,
      0.416666666666667, 0.416666666666667, 0.416666666666667, 0.5,
      0.5, 0.5, 0.5, 0.583333333333333, 0.583333333333333, 0.666666666666667,
      0.666666666666667, 0.666666666666667, 0.75, 0.833333333333333,
      0.916666666666667, 1, 1, 1
    ),
    tolerance = 1e-5
  )

  expect_equal(
    CVbinomlist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

  expect_equal(
    CVbinomlist$Predictions[[1]]$Prediction,
    c(
      0.680095458717956, 0.635540210902409, 1.27010565974986e-06,
      1, 0.631830326291172, 0.655178608954501, 0.666121368078478, 0.669521882094281,
      4.26699221007237e-07, 1, 0.637686595370551, 0.427503656454022,
      0.999971149392087, 0.580648781236971, 0.422867732161415, 0.999933664457318,
      0.5950878405189, 0.175269354302002, 1, 0.587431090816128, 0.593772159659012,
      0.548644074810162, 0.000182046254534159, 0.000182708148532306,
      0.531005549108794, 0.000182135857575305, 0.000182591392395112,
      1, 0.534006886218318, 0.544640431751124
    )
  )
})

test_that("gaussian nnet models work with cross_validate_fn()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(4)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  nnet_model_fn <- function(train_data, formula, hyperparameters) {
    nnet::nnet(
      formula = formula, # converted to formula object within fit_model()
      data = train_data,
      size = 10,
      linout = TRUE
    )
  }
  # nn <- nnet_model_fn(data = dat, formula = as.formula("score~diagnosis+age"))
  # predict(nn, dat, type="raw", allow.new.levels = TRUE)

  nnet_predict_fn <- example_predict_functions("nnet_gaussian")

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
    model_fn = nnet_model_fn,
    predict_fn = nnet_predict_fn,
    formulas = "score~diagnosis",
    fold_cols = ".folds",
    type = "gaussian"
  )

  expect_equal(CVed$RMSE, 16.5414, tolerance = 1e-5)
  expect_equal(CVed$MAE, 13.76884, tolerance = 1e-5)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Fold Columns`, 1)
  expect_equal(CVed$`Convergence Warnings`, 0)
  expect_equal(CVed$Family, "gaussian")
  expect_equal(CVed$Dependent, "score")
  expect_equal(CVed$Fixed, "diagnosis")

  expect_equal(
    colnames(CVed$Coefficients[[1]]),
    c(
      "Fold Column", "Fold", "term",
      "estimate", "p.value"
    )
  )
  expect_equal(
    head(CVed$Coefficients[[1]]$term, 10),
    c(
      "b->h1", "i1->h1", "b->h2", "i1->h2", "b->h3", "i1->h3", "b->h4",
      "i1->h4", "b->h5", "i1->h5"
    ), 4
  )
  expect_equal(
    head(CVed$Coefficients[[1]]$estimate, 10),
    c(
      -2.62129760502992, -1.6849555195295, -1.60212105846947, -2.11737643288526,
      -5.97450438031554, -5.67581875347773, 7.8349209144499, -7.10879712805517,
      3.56900047453588, -4.49050093869156
    )
  )
  expect_equal(
    CVed$Coefficients[[1]]$Fold,
    rep(1:4, each = 31)
  )
  expect_equal(
    CVed$Coefficients[[1]]$`Fold Column`,
    rep(".folds", 124)
  )
  expect_equal(
    CVed$Coefficients[[1]]$p.value,
    rep(NA, 124)
  )
  expect_equal(
    CVed$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("multinomial nnet models work with cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)

  # Create and fold dataset
  data_mc <- multiclass_probability_tibble(
    num_classes = 3, num_observations = 50,
    apply_softmax = TRUE, FUN = runif,
    class_name = "predictor_"
  )
  class_names <- paste0("class_", c(1, 2, 3))
  data_mc[["target"]] <- factor(sample(
    x = class_names,
    size = 50, replace = TRUE
  ))
  dat <- groupdata2::fold(data_mc, k = 4, num_fold_cols = 3)

  multinom_model_fn <- function(train_data, formula, hyperparameters) {
    nnet::multinom(
      formula = formula, # converted to formula object within fit_model()
      data = train_data
    )
  }

  multinom_predict_fn <- example_predict_functions("nnet_multinom")


  CVmultinomlist <- cross_validate_fn(dat,
    model_fn = multinom_model_fn,
    predict_fn = multinom_predict_fn,
    formulas = c(
      "target ~ predictor_1 + predictor_2 + predictor_3",
      "target ~ predictor_1"
    ),
    fold_cols = c(".folds_1"),
    type = "multinomial",
    metrics = "all"
  )

  expect_equal(CVmultinomlist$AUC, c(0.338771310993533, 0.382495590828924), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Kappa, c(-0.234940592679204, -0.0826903354960317), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Sensitivity, c(0.177248677248677, 0.283068783068783), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Specificity, c(0.585648148148148, 0.642361111111111), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Pos Pred Value`, c(0.182234432234432, 0.254255548373195), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Neg Pred Value`, c(0.582223196827659, 0.646699553676298), tolerance = 1e-5)
  expect_equal(CVmultinomlist$F1, c(0.179096139880454, 0.259451659451659), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Prevalence, c(0.333333333333333, 0.333333333333333), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Detection Rate`, c(0.06, 0.1), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Detection Prevalence`, c(0.3333333, 0.333333), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Balanced Accuracy`, c(0.381448412698413, 0.462714947089947), tolerance = 1e-5)
  expect_equal(CVmultinomlist$MCC, c(-0.241950698231126, -0.0777714613817967), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Folds, c(4, 4))
  expect_equal(CVmultinomlist$`Fold Columns`, c(1, 1))
  expect_equal(CVmultinomlist$`Convergence Warnings`, c(0, 0))
  expect_equal(CVmultinomlist$Family, c("multinomial", "multinomial"))
  expect_equal(CVmultinomlist$Dependent, c("target", "target"))
  expect_equal(CVmultinomlist$Fixed, c("predictor_1+predictor_2+predictor_3", "predictor_1"))

  expect_equal(
    CVmultinomlist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

  expect_equal(
    colnames(CVmultinomlist),
    c(
      "Fixed", "Overall Accuracy", "Balanced Accuracy", "Weighted Balanced Accuracy",
      "Accuracy", "Weighted Accuracy", "F1", "Weighted F1", "Sensitivity",
      "Weighted Sensitivity", "Specificity", "Weighted Specificity",
      "Pos Pred Value", "Weighted Pos Pred Value", "Neg Pred Value",
      "Weighted Neg Pred Value", "AUC", "Kappa", "Weighted Kappa",
      "MCC", "Detection Rate", "Weighted Detection Rate",
      "Detection Prevalence", "Weighted Detection Prevalence", "Prevalence",
      "Weighted Prevalence", "False Neg Rate", "Weighted False Neg Rate",
      "False Pos Rate", "Weighted False Pos Rate", "False Discovery Rate",
      "Weighted False Discovery Rate", "False Omission Rate", "Weighted False Omission Rate",
      "Threat Score", "Weighted Threat Score", "AIC", "AICc", "BIC",
      "Predictions", "Confusion Matrix", "Results", "Class Level Results",
      "Coefficients", "Folds", "Fold Columns", "Convergence Warnings",
      "Other Warnings", "Warnings and Messages", "Family", "Dependent"
    )
  )

  # Enter sub tibbles
  class_level_results <- CVmultinomlist$`Class Level Results`

  expect_equal(
    class_level_results[[1]]$Class,
    c("class_1", "class_2", "class_3")
  )
  expect_equal(
    class_level_results[[2]]$Class,
    c("class_1", "class_2", "class_3")
  )
  expect_equal(
    class_level_results[[1]]$`Balanced Accuracy`,
    c(0.418650793650794, 0.302083333333333, 0.423611111111111)
  )
  expect_equal(
    class_level_results[[2]]$`Balanced Accuracy`,
    c(0.452380952380952, 0.407986111111111, 0.527777777777778)
  )
  expect_equal(
    class_level_results[[1]]$F1,
    c(0.148148148148148, 0.153846153846154, 0.235294117647059)
  )
  expect_equal(
    class_level_results[[2]]$F1,
    c(0.0952380952380952, 0.228571428571429, 0.454545454545455)
  )
  expect_equal(
    class_level_results[[1]]$Sensitivity,
    c(0.142857142857143, 0.166666666666667, 0.222222222222222)
  )
  expect_equal(
    class_level_results[[2]]$Sensitivity,
    c(0.0714285714285714, 0.222222222222222, 0.555555555555556)
  )
  expect_equal(
    class_level_results[[1]]$Specificity,
    c(0.694444444444444, 0.4375, 0.625)
  )
  expect_equal(
    class_level_results[[2]]$Specificity,
    c(0.833333333333333, 0.59375, 0.5)
  )
  expect_equal(
    class_level_results[[1]]$`Pos Pred Value`,
    c(0.153846153846154, 0.142857142857143, 0.25)
  )
  expect_equal(
    class_level_results[[2]]$`Pos Pred Value`,
    c(0.142857142857143, 0.235294117647059, 0.384615384615385)
  )
  expect_equal(
    class_level_results[[1]]$`Neg Pred Value`,
    c(0.675675675675676, 0.482758620689655, 0.588235294117647)
  )
  expect_equal(
    class_level_results[[2]]$`Neg Pred Value`,
    c(0.697674418604651, 0.575757575757576, 0.666666666666667)
  )
  expect_equal(
    class_level_results[[1]]$Kappa,
    c(-0.166328600405679, -0.381909547738693, -0.156583629893238)
  )
  expect_equal(
    class_level_results[[2]]$Kappa,
    c(-0.112412177985948, -0.186291739894552, 0.0506329113924052)
  )
  expect_equal(
    class_level_results[[1]]$`Detection Rate`,
    c(0.04, 0.06, 0.08)
  )
  expect_equal(
    class_level_results[[2]]$`Detection Rate`,
    c(0.02, 0.08, 0.2)
  )
  expect_equal(
    class_level_results[[1]]$`Detection Prevalence`,
    c(0.26, 0.42, 0.32)
  )
  expect_equal(
    class_level_results[[2]]$`Detection Prevalence`,
    c(0.14, 0.34, 0.52)
  )
  expect_equal(
    class_level_results[[1]]$Prevalence,
    c(0.28, 0.36, 0.36)
  )
  expect_equal(
    class_level_results[[2]]$Prevalence,
    c(0.28, 0.36, 0.36)
  )
  expect_equal(
    class_level_results[[1]]$Support,
    c(14, 18, 18)
  )
  expect_equal(
    class_level_results[[2]]$Support,
    c(14, 18, 18)
  )

  clr_confmat_1 <- dplyr::bind_rows(class_level_results[[1]]$`Confusion Matrix`)
  clr_confmat_2 <- dplyr::bind_rows(class_level_results[[2]]$`Confusion Matrix`)
  clr_confmat <- dplyr::bind_rows(clr_confmat_1, clr_confmat_2)
  expect_equal(
    clr_confmat$`Fold Column`,
    rep(".folds_1", 24)
  )
  expect_equal(
    clr_confmat$Prediction,
    rep(c("0", "1"), 12)
  )
  expect_equal(
    clr_confmat$Target,
    rep(c("0", "0", "1", "1"), 6)
  )
  expect_equal(
    clr_confmat$Pos_0,
    rep(c("TP", "FN", "FP", "TN"), 6)
  )
  expect_equal(
    clr_confmat$Pos_1,
    rep(c("TN", "FP", "FN", "TP"), 6)
  )
  expect_equal(
    clr_confmat$N,
    c(
      25L, 11L, 12L, 2L, 14L, 18L, 15L, 3L, 20L, 12L, 14L, 4L, 30L,
      6L, 13L, 1L, 19L, 13L, 14L, 4L, 16L, 16L, 8L, 10L
    )
  )

  # Predictions
  expect_is(CVmultinomlist$Predictions[[1]], "tbl_df")
  expect_equal(
    colnames(CVmultinomlist$Predictions[[1]]),
    c("Fold Column", "Fold", "Observation", "Target", "Prediction", "Predicted Class")
  )
  expect_equal(nrow(CVmultinomlist$Predictions[[1]]), 50)

  # ROC in Results
  expect_equal(
    colnames(CVmultinomlist$Results[[1]]),
    c(
      "Fold Column", "Overall Accuracy", "Balanced Accuracy", "Weighted Balanced Accuracy",
      "Accuracy", "Weighted Accuracy", "F1", "Weighted F1", "Sensitivity",
      "Weighted Sensitivity", "Specificity", "Weighted Specificity",
      "Pos Pred Value", "Weighted Pos Pred Value", "Neg Pred Value",
      "Weighted Neg Pred Value", "AUC", "Kappa", "Weighted Kappa",
      "MCC", "Detection Rate", "Weighted Detection Rate",
      "Detection Prevalence", "Weighted Detection Prevalence", "Prevalence",
      "Weighted Prevalence", "False Neg Rate", "Weighted False Neg Rate",
      "False Pos Rate", "Weighted False Pos Rate", "False Discovery Rate",
      "Weighted False Discovery Rate", "False Omission Rate", "Weighted False Omission Rate",
      "Threat Score", "Weighted Threat Score", "AIC", "AICc", "BIC",
      "ROC"
    )
  )
  expect_equal(
    as.numeric(CVmultinomlist$Results[[1]]$ROC[[1]]$auc),
    0.338771310993533
  )
  expect_equal(
    CVmultinomlist$Results[[1]]$ROC[[1]]$rocs$`class_1/class_2`[[1]]$sensitivities,
    c(
      1, 0.944444444444444, 0.888888888888889, 0.833333333333333,
      0.833333333333333, 0.777777777777778, 0.722222222222222, 0.666666666666667,
      0.666666666666667, 0.611111111111111, 0.555555555555556, 0.555555555555556,
      0.5, 0.5, 0.5, 0.444444444444444, 0.388888888888889, 0.388888888888889,
      0.388888888888889, 0.333333333333333, 0.277777777777778, 0.222222222222222,
      0.166666666666667, 0.166666666666667, 0.111111111111111, 0.111111111111111,
      0.111111111111111, 0.111111111111111, 0.0555555555555556, 0.0555555555555556,
      0, 0, 0
    )
  )
  expect_equal(
    CVmultinomlist$Results[[1]]$ROC[[1]]$rocs$`class_1/class_2`[[2]]$sensitivities,
    c(
      1, 0.928571428571429, 0.857142857142857, 0.785714285714286,
      0.785714285714286, 0.714285714285714, 0.714285714285714, 0.642857142857143,
      0.642857142857143, 0.571428571428571, 0.5, 0.428571428571429,
      0.357142857142857, 0.357142857142857, 0.357142857142857, 0.285714285714286,
      0.285714285714286, 0.214285714285714, 0.214285714285714, 0.214285714285714,
      0.214285714285714, 0.214285714285714, 0.142857142857143, 0.142857142857143,
      0.142857142857143, 0.0714285714285714, 0.0714285714285714, 0.0714285714285714,
      0.0714285714285714, 0, 0, 0, 0
    )
  )
  expect_equal(
    CVmultinomlist$Results[[1]]$ROC[[1]]$rocs$`class_2/class_3`[[1]]$sensitivities,
    c(
      1, 0.944444444444444, 0.888888888888889, 0.888888888888889,
      0.833333333333333, 0.777777777777778, 0.722222222222222, 0.666666666666667,
      0.666666666666667, 0.611111111111111, 0.555555555555556, 0.555555555555556,
      0.5, 0.444444444444444, 0.444444444444444, 0.444444444444444,
      0.388888888888889, 0.388888888888889, 0.388888888888889, 0.388888888888889,
      0.333333333333333, 0.333333333333333, 0.277777777777778, 0.277777777777778,
      0.222222222222222, 0.166666666666667, 0.111111111111111, 0.111111111111111,
      0.111111111111111, 0.111111111111111, 0.111111111111111, 0.0555555555555556,
      0.0555555555555556, 0, 0, 0, 0
    )
  )
  expect_equal(
    CVmultinomlist$Results[[1]]$ROC[[1]]$rocs$`class_2/class_3`[[2]]$sensitivities,
    c(
      1, 0.944444444444444, 0.944444444444444, 0.888888888888889,
      0.888888888888889, 0.888888888888889, 0.833333333333333, 0.777777777777778,
      0.722222222222222, 0.666666666666667, 0.611111111111111, 0.555555555555556,
      0.555555555555556, 0.5, 0.444444444444444, 0.444444444444444,
      0.388888888888889, 0.388888888888889, 0.333333333333333, 0.333333333333333,
      0.333333333333333, 0.333333333333333, 0.277777777777778, 0.277777777777778,
      0.222222222222222, 0.222222222222222, 0.222222222222222, 0.222222222222222,
      0.222222222222222, 0.166666666666667, 0.111111111111111, 0.111111111111111,
      0.111111111111111, 0.111111111111111, 0.0555555555555556, 0.0555555555555556,
      0
    )
  )
})

test_that("binomial randomForest models work with cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  rf_model_fn <- function(train_data, formula, hyperparameters) {
    randomForest::randomForest(
      formula = formula,
      data = train_data
    )
  }

  # m <- rf_model_fn(dat, as.formula("diagnosis ~ age"), NULL)
  rf_predict_fn <- example_predict_functions("randomForest_binomial")

  CVbinomlist <- cross_validate_fn(dat,
    model_fn = rf_model_fn,
    predict_fn = rf_predict_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", type = "binomial",
    positive = 1
  )


  numeric_between_na <- function(x, min_ = -0.000000001, max_ = 1.000000001) {
    all(unlist(lapply(x, function(i) {
      is.na(i) || (is.numeric(i) && is_between_(i, min_, max_))
    })))
  }

  # Because the actual values fail on winbuilder (not appveyor)
  # perhaps to do with compilation of randomForest?
  # We simply check that the values are useful
  # As it's not actually that relevant that this specific model function
  # gives the same results
  expect_true(numeric_between_na(CVbinomlist$AUC))
  expect_true(numeric_between_na(CVbinomlist$`Lower CI`))
  expect_true(numeric_between_na(CVbinomlist$`Upper CI`))
  expect_true(numeric_between_na(CVbinomlist$Kappa, min_ = -1))
  expect_true(numeric_between_na(CVbinomlist$Sensitivity))
  expect_true(numeric_between_na(CVbinomlist$Specificity))
  expect_true(numeric_between_na(CVbinomlist$`Pos Pred Value`))
  expect_true(numeric_between_na(CVbinomlist$`Neg Pred Value`))
  expect_true(numeric_between_na(CVbinomlist$F1))
  expect_true(numeric_between_na(CVbinomlist$Prevalence))
  expect_true(numeric_between_na(CVbinomlist$`Detection Rate`))
  expect_true(numeric_between_na(CVbinomlist$`Detection Prevalence`))
  expect_true(numeric_between_na(CVbinomlist$`Balanced Accuracy`))
  expect_true(numeric_between_na(CVbinomlist$MCC, min_ = -1))

  expect_equal(CVbinomlist$Folds, c(4, 4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1, 1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0, 0))
  expect_equal(CVbinomlist$Family, c("binomial", "binomial"))
  expect_equal(CVbinomlist$Dependent, c("diagnosis", "diagnosis"))
  expect_equal(CVbinomlist$Fixed, c("score", "age"))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]]$.folds, "roc")
  expect_equal(CVbinomlist$ROC[[1]]$.folds$direction, "<")
  expect_equal(as.numeric(CVbinomlist$ROC[[1]]$.folds$auc), 0.648148148148148, tolerance = 1e-5)
  expect_equal(
    colnames(CVbinomlist$Predictions[[1]]),
    c(
      "Fold Column", "Fold", "Observation",
      "Target", "Prediction", "Predicted Class"
    )
  )
  expect_equal(nrow(CVbinomlist$Predictions[[1]]), 30)
  expect_equal(
    CVbinomlist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

  # The actual values
  # Skip on windows. Appveyour works but winbuilder gives different results
  skip_on_os(os = "windows")

  expect_equal(CVbinomlist$AUC, c(0.648148148148148, 0.166666666666667), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Lower CI`, c(0.446244646035729, 0.00877666333061852), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Upper CI`, c(0.850051650260568, 0.324556670002715), tolerance = 1e-5)
  expect_equal(CVbinomlist$Kappa, c(0.054054054054054, -0.538461538461538), tolerance = 1e-5)
  expect_equal(CVbinomlist$Sensitivity, c(0.5, 0.25), tolerance = 1e-5)
  expect_equal(CVbinomlist$Specificity, c(0.555555555555556, 0.166666666666667), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.428571428571429, 0.166666666666667), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.625, 0.25), tolerance = 1e-5)
  expect_equal(CVbinomlist$F1, c(0.461538461538462, 0.2), tolerance = 1e-5)
  expect_equal(CVbinomlist$Prevalence, c(0.4, 0.4), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2, 0.1), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.466666666666667, 0.6), tolerance = 1e-5)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.527777777777778, 0.208333333333333), tolerance = 1e-5)
  expect_equal(CVbinomlist$MCC, c(0.0545544725589981, -0.583333333333333), tolerance = 1e-5)
})

test_that("multinomial randomForest models work with cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  # Create and fold dataset
  data_mc <- multiclass_probability_tibble(
    num_classes = 3, num_observations = 50,
    apply_softmax = TRUE, FUN = runif,
    class_name = "predictor_"
  )
  class_names <- paste0("class_", c(1, 2, 3))
  data_mc[["target"]] <- factor(sample(
    x = class_names,
    size = 50, replace = TRUE
  ))
  dat <- groupdata2::fold(data_mc, k = 4)

  rf_model_fn <- function(train_data, formula, hyperparameters) {
    randomForest::randomForest(
      formula = formula,
      data = train_data
    )
  }

  rf_predict_fn <- example_predict_functions("randomForest_multinomial")

  # m <- rf_model_fn(dat, as.formula("target ~ predictor_1"), NULL)
  # rf_predict_fn(dat, m, NULL, NULL)

  expect_error(cross_validate_fn(dat,
    model_fn = rf_model_fn,
    predict_fn = rf_predict_fn,
    formulas = c(
      "target ~ predictor_1 + predictor_2 + predictor_3",
      "target ~ predictor_1"
    ),
    fold_cols = ".folds",
    type = "multinomial",
    metrics = list(
      "Accuracy" = TRUE,
      "F1" = FALSE,
      "Weighted Accuracy" = TRUE,
      "Weighted AUC" = TRUE
    )
  ),
  "'metrics_list' contained unknown metric names: Weighted AUC.",
  fixed = TRUE
  )

  CVmultinomlist <- cross_validate_fn(dat,
    model_fn = rf_model_fn,
    predict_fn = rf_predict_fn,
    formulas = c(
      "target ~ predictor_1 + predictor_2 + predictor_3",
      "target ~ predictor_1"
    ),
    fold_cols = ".folds",
    type = "multinomial",
    metrics = list(
      "Accuracy" = TRUE,
      "F1" = FALSE,
      "Weighted Accuracy" = TRUE,
      "AUC" = TRUE
    )
  )

  numeric_between_na <- function(x, min_ = -0.000000001, max_ = 1.000000001) {
    all(unlist(lapply(x, function(i) {
      is.na(i) || (is.numeric(i) && is_between_(i, min_, max_))
    })))
  }

  # Because the actual values fail on winbuilder (not appveyor)
  # perhaps to do with compilation of randomForest?
  # We simply check that the values are useful
  # As it's not actually that relevant that this specific model function
  # gives the same results
  expect_true(numeric_between_na(CVmultinomlist$AUC))
  expect_true(numeric_between_na(CVmultinomlist$Kappa, min_ = -1))
  expect_true(numeric_between_na(CVmultinomlist$Sensitivity))
  expect_true(numeric_between_na(CVmultinomlist$Specificity))
  expect_true(numeric_between_na(CVmultinomlist$`Pos Pred Value`))
  expect_true(numeric_between_na(CVmultinomlist$`Neg Pred Value`))
  expect_true(numeric_between_na(CVmultinomlist$Prevalence))
  expect_true(numeric_between_na(CVmultinomlist$`Detection Rate`))
  expect_true(numeric_between_na(CVmultinomlist$`Detection Prevalence`))
  expect_true(numeric_between_na(CVmultinomlist$`Balanced Accuracy`))
  expect_true(numeric_between_na(CVmultinomlist$MCC, min_ = -1))

  expect_equal(CVmultinomlist$Folds, c(4, 4))
  expect_equal(CVmultinomlist$`Fold Columns`, c(1, 1))
  expect_equal(CVmultinomlist$`Convergence Warnings`, c(0, 0))
  expect_equal(CVmultinomlist$Family, c("multinomial", "multinomial"))
  expect_equal(CVmultinomlist$Dependent, c("target", "target"))
  expect_equal(CVmultinomlist$Fixed, c("predictor_1+predictor_2+predictor_3", "predictor_1"))

  # Enter sub tibbles
  expect_is(CVmultinomlist$Predictions[[1]], "tbl_df")
  expect_equal(
    colnames(CVmultinomlist$Predictions[[1]]),
    c(
      "Fold Column", "Fold", "Observation",
      "Target", "Prediction", "Predicted Class"
    )
  )
  expect_equal(nrow(CVmultinomlist$Predictions[[1]]), 50)
  expect_equal(
    CVmultinomlist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

  # The actual values
  # Skip on windows. Appveyour works but winbuilder gives different results
  skip_on_os(os = "windows")

  expect_equal(CVmultinomlist$AUC, c(0.378637566137566, 0.35082304526749), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Kappa, c(-0.0868137955528609, -0.188689820770385), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Sensitivity, c(0.275132275132275, 0.208994708994709), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Specificity, c(0.636574074074074, 0.605324074074074), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Pos Pred Value`, c(0.276709401709402, 0.200478468899522), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Neg Pred Value`, c(0.633749133749134, 0.605017921146953), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Prevalence, c(0.333333, 0.333333), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Detection Rate`, c(0.0933333333333333, 0.0733333333333333), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Detection Prevalence`, c(0.333333, 0.333333), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Balanced Accuracy`, c(0.455853174603175, 0.407159391534392), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Overall Accuracy`, c(0.28, 0.22), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Accuracy, c(0.52, 0.48), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Weighted Accuracy`, c(0.5152, 0.4752), tolerance = 1e-5)
  expect_equal(CVmultinomlist$MCC, c(-0.0913229553926603, -0.186939898098708), tolerance = 1e-5)

  expect_equal(
    colnames(CVmultinomlist),
    c(
      "Fixed", "Overall Accuracy", "Balanced Accuracy", "Accuracy", "Weighted Accuracy",
      "Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "AUC",
      "Kappa", "MCC", "Detection Rate", "Detection Prevalence", "Prevalence",
      "Predictions", "Confusion Matrix", "Results", "Class Level Results",
      "Coefficients", "Folds", "Fold Columns", "Convergence Warnings",
      "Other Warnings", "Warnings and Messages", "Family", "Dependent"
    )
  )

  # Enter sub tibbles
  class_level_results <- CVmultinomlist$`Class Level Results`

  expect_equal(
    class_level_results[[1]]$Class,
    c("class_1", "class_2", "class_3")
  )
  expect_equal(
    class_level_results[[2]]$Class,
    c("class_1", "class_2", "class_3")
  )
  expect_equal(
    class_level_results[[1]]$Accuracy,
    c(0.58, 0.54, 0.44)
  )
  expect_equal(
    class_level_results[[2]]$Accuracy,
    c(0.54, 0.48, 0.42)
  )
  expect_equal(
    class_level_results[[1]]$`Balanced Accuracy`,
    c(0.468253968253968, 0.470486111111111, 0.428819444444444)
  )
  expect_equal(
    class_level_results[[2]]$`Balanced Accuracy`,
    c(0.396825396825397, 0.447916666666667, 0.376736111111111)
  )
  expect_equal(
    colnames(class_level_results[[1]]),
    c(
      "Class", "Balanced Accuracy", "Accuracy", "Sensitivity", "Specificity",
      "Pos Pred Value", "Neg Pred Value", "Kappa", "Detection Rate",
      "Detection Prevalence", "Prevalence",
      "Support", "Results", "Confusion Matrix"
    )
  )
})

test_that("gaussian randomForest models work with cross_validate_fn()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(4)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  rf_model_fn <- function(train_data, formula, hyperparameters) {
    randomForest::randomForest(
      formula = formula,
      data = train_data
    )
  }
  # rf <- rf_model_fn(data = dat, formula = as.formula("score~diagnosis+age"))
  # predict(rf, dat, allow.new.levels = TRUE)

  rf_predict_fn <- example_predict_functions("randomForest_gaussian")

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
    model_fn = rf_model_fn,
    predict_fn = rf_predict_fn,
    formulas = "score~diagnosis",
    fold_cols = ".folds",
    metrics = "all",
    type = "gaussian"
  )

  expect_equal(CVed$RMSE, 16.56476, tolerance = 1e-5)
  expect_equal(CVed$MAE, 13.77846, tolerance = 1e-5)
  expect_equal(CVed$r2m, NaN, tolerance = 1e-5)
  expect_equal(CVed$r2c, NaN, tolerance = 1e-5)
  expect_equal(CVed$AIC, NaN, tolerance = 1e-5)
  expect_equal(CVed$AICc, NaN, tolerance = 1e-5)
  expect_equal(CVed$BIC, NaN, tolerance = 1e-5)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Fold Columns`, 1)
  expect_equal(CVed$`Convergence Warnings`, 0)
  expect_equal(CVed$Family, "gaussian")
  expect_equal(CVed$Dependent, "score")
  expect_equal(CVed$Fixed, "diagnosis")

  expect_equal(
    colnames(CVed$Coefficients[[1]]),
    c(
      "Fold Column", "Fold", "term", "estimate",
      "std.error", "statistic", "p.value"
    )
  )
  expect_equal(
    CVed$Coefficients[[1]]$term,
    rep(NA, 4)
  )
  expect_equal(
    CVed$Coefficients[[1]]$estimate,
    rep(NA, 4)
  )
  expect_equal(
    CVed$Coefficients[[1]]$Fold,
    rep(NA, 4)
  )
  expect_equal(
    CVed$Coefficients[[1]]$`Fold Column`,
    rep(NA, 4)
  )
  expect_equal(
    CVed$Coefficients[[1]]$p.value,
    rep(NA, 4)
  )
  expect_equal(
    CVed$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("binomial keras models work with cross_validate_fn()", {
  testthat::skip("keras and tensorflow take too long and have too many dependencies")

  # Uncomment to run (avoids having keras as dependency just for this)
  #   # skip_test_if_old_R_version()
  #   skip_if_no_keras()
  #   library(keras)
  #   suppressMessages(use_session_with_seed(42))
  #
  #   # Load data and fold it
  #   xpectr::set_test_seed(4)
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
  #   expect_equal(CVbinomlist$AUC, 0.5833333, tolerance=1e-5)
  #   expect_equal(CVbinomlist$`Lower CI`, 0.3624639, tolerance=1e-5)
  #   expect_equal(CVbinomlist$`Upper CI`, 0.8042027, tolerance=1e-5)
  #   expect_equal(CVbinomlist$Kappa, 0.1666667, tolerance=1e-5)
  #   expect_equal(CVbinomlist$Sensitivity, 0.6666667, tolerance=1e-5)
  #   expect_equal(CVbinomlist$Specificity, 0.5, tolerance=1e-5)
  #   expect_equal(CVbinomlist$`Pos Pred Value`, 0.6666667, tolerance=1e-5)
  #   expect_equal(CVbinomlist$`Neg Pred Value`, 0.5, tolerance=1e-5)
  #   expect_equal(CVbinomlist$F1, 0.6666667, tolerance=1e-5)
  #   expect_equal(CVbinomlist$Prevalence, 0.6, tolerance=1e-5)
  #   expect_equal(CVbinomlist$`Detection Rate`, 0.4, tolerance=1e-5)
  #   expect_equal(CVbinomlist$`Detection Prevalence`, 0.6, tolerance=1e-5)
  #   expect_equal(CVbinomlist$`Balanced Accuracy`, 0.5833333, tolerance=1e-5)
  #   expect_equal(CVbinomlist$MCC, 0.1666667, tolerance=1e-5)
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

test_that("binomial tidymodels work with cross_validate_fn()", {
  testthat::skip("tidymodels have too many dependencies")

  # # Load data and fold it
  # xpectr::set_test_seed(1)
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
  # expect_equal(CVbinomlist$AUC, c(0.659722222222222, 0.208333333333333), tolerance=1e-5)
  # # expect_equal(CVbinomlist$`Lower CI`, c(0.367800267130833, 0.161076004187493), tolerance=1e-5)
  # # expect_equal(CVbinomlist$`Upper CI`, c(0.743310843980278, 0.505590662479174), tolerance=1e-5)
  # # expect_equal(CVbinomlist$Kappa, c(0.10958904109589, -0.296296296296296), tolerance=1e-5)
  # # expect_equal(CVbinomlist$Sensitivity, c(0.5,0.5), tolerance=1e-5)
  # # expect_equal(CVbinomlist$Specificity, c(0.611111111111111, 0.166666666666667), tolerance=1e-5)
  # # expect_equal(CVbinomlist$`Pos Pred Value`, c(0.461538461538462, 0.285714285714286), tolerance=1e-5)
  # # expect_equal(CVbinomlist$`Neg Pred Value`, c(0.647058823529412, 0.333333333333333), tolerance=1e-5)
  # # expect_equal(CVbinomlist$F1, c(0.48, 0.363636363636364), tolerance=1e-5)
  # # expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-5)
  # # expect_equal(CVbinomlist$`Detection Rate`, c(0.2, 0.2), tolerance=1e-5)
  # # expect_equal(CVbinomlist$`Detection Prevalence`, c(0.433333333333333, 0.7), tolerance=1e-5)
  # # expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.555555555555556, 0.333333333333333), tolerance=1e-5)
  # # expect_equal(CVbinomlist$MCC, c(0.109847007276218, -0.356348322549899), tolerance=1e-5)
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

test_that("binomial glm model with metrics list works with cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  glm_model_fn <- example_model_functions("glm_binomial")

  glm_predict_fn <- example_predict_functions("glm_binomial")

  CVbinomlist <- cross_validate_fn(dat,
    model_fn = glm_model_fn,
    predict_fn = glm_predict_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", type = "binomial",
    metrics = list(
      "Balanced Accuracy" = FALSE,
      "Accuracy" = TRUE,
      "Specificity" = FALSE,
      "AIC" = TRUE,
      "AICc" = TRUE,
      "BIC" = TRUE
    ),
    positive = 1
  )

  expect_equal(CVbinomlist$Accuracy, c(0.766666666666667, 0.4), tolerance = 1e-5)
  expect_equal(CVbinomlist$AIC, c(27.303279438339, 33.2582292059047), tolerance = 1e-5)
  expect_equal(CVbinomlist$AICc, c(27.9223270573866, 33.8772768249523), tolerance = 1e-5)
  expect_equal(CVbinomlist$BIC, c(29.5258557064103, 35.4808054739761), tolerance = 1e-5)
  expect_equal(
    colnames(CVbinomlist),
    c(
      "Fixed", "Accuracy", "F1", "Sensitivity", "Pos Pred Value", "Neg Pred Value",
      "AUC", "Lower CI", "Upper CI", "Kappa", "MCC", "Detection Rate",
      "Detection Prevalence", "Prevalence", "AIC", "AICc", "BIC", "Predictions",
      "ROC", "Confusion Matrix", "Results", "Coefficients", "Folds",
      "Fold Columns", "Convergence Warnings", "Other Warnings", "Warnings and Messages",
      "Positive Class", "Family", "Dependent"
    )
  )
})

test_that("gaussian lm model with metrics list works with cross_validate_fn()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  lm_model_fn <- example_model_functions("lm")
  lm_predict_fn <- example_predict_functions("lm")

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
    model_fn = lm_model_fn,
    predict_fn = lm_predict_fn,
    formulas = "score~diagnosis",
    fold_cols = ".folds",
    metrics = list(
      "RMSE" = FALSE,
      "r2m" = FALSE,
      "r2c" = TRUE,
      "AIC" = TRUE,
      "AICc" = TRUE,
      "BIC" = TRUE
    ),
    type = "gaussian"
  )

  expect_equal(
    colnames(CVed),
    c("Fixed", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE", "r2c",
    "AIC", "AICc", "BIC", "Predictions", "Results", "Coefficients",
    "Folds", "Fold Columns", "Convergence Warnings", "Other Warnings",
    "Warnings and Messages", "Family", "Dependent")
  )
})

test_that("multinomial nnet model with metrics list works with cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)

  # Create and fold dataset
  data_mc <- multiclass_probability_tibble(
    num_classes = 3, num_observations = 50,
    apply_softmax = TRUE, FUN = runif,
    class_name = "predictor_"
  )
  class_names <- paste0("class_", c(1, 2, 3))
  data_mc[["target"]] <- factor(sample(
    x = class_names,
    size = 50, replace = TRUE
  ))
  dat <- groupdata2::fold(data_mc, k = 4)

  multinom_model_fn <- function(train_data, formula, hyperparameters) {
    nnet::multinom(
      formula = formula, # converted to formula object within fit_model()
      data = train_data
    )
  }

  multinom_predict_fn <- example_predict_functions("nnet_multinom")

  CVmultinomlist <- cross_validate_fn(dat,
    model_fn = multinom_model_fn,
    predict_fn = multinom_predict_fn,
    formulas = c(
      "target ~ predictor_1 + predictor_2 + predictor_3",
      "target ~ predictor_1"
    ),
    fold_cols = ".folds", type = "multinomial",
    metrics = list(
      "Accuracy" = TRUE,
      "F1" = FALSE,
      "Weighted Accuracy" = TRUE,
      "AUC" = TRUE
    )
  )

  expect_equal(CVmultinomlist$`Overall Accuracy`, c(0.18, 0.3), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Accuracy, c(0.453333333333333, 0.533333333333333), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Balanced Accuracy`, c(0.381448412698413, 0.462714947089947), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Weighted Accuracy`, c(0.4464, 0.5264), tolerance = 1e-5)
  expect_equal(
    colnames(CVmultinomlist),
    c(
      "Fixed", "Overall Accuracy", "Balanced Accuracy", "Accuracy", "Weighted Accuracy",
      "Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "AUC",
      "Kappa", "MCC", "Detection Rate", "Detection Prevalence", "Prevalence",
      "Predictions", "Confusion Matrix", "Results", "Class Level Results", "Coefficients",
      "Folds", "Fold Columns", "Convergence Warnings", "Other Warnings",
      "Warnings and Messages", "Family", "Dependent"
    )
  )

  # Enter sub tibbles
  class_level_results <- CVmultinomlist$`Class Level Results`

  expect_equal(
    class_level_results[[1]]$Class,
    c("class_1", "class_2", "class_3")
  )
  expect_equal(
    class_level_results[[2]]$Class,
    c("class_1", "class_2", "class_3")
  )
  expect_equal(
    class_level_results[[1]]$Accuracy,
    c(0.54, 0.34, 0.48)
  )
  expect_equal(
    class_level_results[[2]]$Accuracy,
    c(0.62, 0.46, 0.52)
  )
  expect_equal(
    class_level_results[[1]]$`Balanced Accuracy`,
    c(0.418650793650794, 0.302083333333333, 0.423611111111111)
  )
  expect_equal(
    class_level_results[[2]]$`Balanced Accuracy`,
    c(0.452380952380952, 0.407986111111111, 0.527777777777778)
  )
  expect_equal(
    colnames(class_level_results[[1]]),
    c(
      "Class", "Balanced Accuracy", "Accuracy", "Sensitivity", "Specificity",
      "Pos Pred Value", "Neg Pred Value", "Kappa", "Detection Rate",
      "Detection Prevalence", "Prevalence", "Support", "Results", "Confusion Matrix"
    )
  )
})


# Random predictions
# If these fail on different platforms, it's likely that it's my package
# that's doing something wrong. Otherwise it's probably the model functions (nnet, randomForest)
# that have been compiled different on windows and linux, or fail to use the
# same random seed in their (assumed) c/c++ implementations

test_that("binomial random predictions work with cross_validate_fn()", {

  # SHOULD work cross platform

  # Load data and fold it
  xpectr::set_test_seed(10)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )
  dat[["diagnosis"]] <- factor(dat[["diagnosis"]])

  glm_model_fn <- function(train_data, formula, hyperparameters) {
    glm(formula = formula, data = train_data, family = "binomial")
  }

  random_predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
    runif(nrow(test_data), min = 0, max = 1)
  }

  CVrandom <- cross_validate_fn(dat,
    model_fn = glm_model_fn,
    predict_fn = random_predict_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", type = "binomial",
    positive = 1
  )

  expect_equal(CVrandom$AUC, c(0.638888888888889, 0.699074074074074), tolerance = 1e-5)
  expect_equal(CVrandom$`Lower CI`, c(0.434338960822277, 0.477736689228572), tolerance = 1e-5)
  expect_equal(CVrandom$`Upper CI`, c(0.843438816955501, 0.920411458919576), tolerance = 1e-5)
  expect_equal(CVrandom$Kappa, c(0.268292682926829, 0.246575342465753), tolerance = 1e-5)
  expect_equal(CVrandom$Sensitivity, c(0.916666666666667, 0.583333333333333), tolerance = 1e-5)
  expect_equal(CVrandom$Specificity, c(0.388888888888889, 0.666666666666667), tolerance = 1e-5)
  expect_equal(CVrandom$`Pos Pred Value`, c(0.5, 0.538461538461539), tolerance = 1e-5)
  expect_equal(CVrandom$`Neg Pred Value`, c(0.875, 0.705882352941176), tolerance = 1e-5)
  expect_equal(CVrandom$F1, c(0.647058823529412, 0.56), tolerance = 1e-5)
  expect_equal(CVrandom$Prevalence, c(0.4, 0.4), tolerance = 1e-5)
  expect_equal(CVrandom$`Detection Rate`, c(0.366666666666667, 0.233333333333333), tolerance = 1e-5)
  expect_equal(CVrandom$`Detection Prevalence`, c(0.733333333333333, 0.433333333333333), tolerance = 1e-5)
  expect_equal(CVrandom$`Balanced Accuracy`, c(0.652777777777778, 0.625), tolerance = 1e-5)
  expect_equal(CVrandom$MCC, c(0.338501600193165, 0.24715576637149), tolerance = 1e-5)
  expect_equal(CVrandom$Folds, c(4, 4))
  expect_equal(CVrandom$`Fold Columns`, c(1, 1))
  expect_equal(CVrandom$`Convergence Warnings`, c(0, 0))
  expect_equal(CVrandom$Family, c("binomial", "binomial"))
  expect_equal(CVrandom$Dependent, c("diagnosis", "diagnosis"))
  expect_equal(CVrandom$Fixed, c("score", "age"))

  # Enter sub tibbles
  expect_is(CVrandom$Predictions[[1]], "tbl_df")
  expect_is(CVrandom$ROC[[1]]$.folds, "roc")
  expect_equal(
    colnames(CVrandom$Predictions[[1]]),
    c(
      "Fold Column", "Fold", "Observation",
      "Target", "Prediction", "Predicted Class"
    )
  )
  expect_equal(
    CVrandom$Predictions[[1]]$Observation,
    c(
      7L, 8L, 9L, 13L, 14L, 15L, 1L, 2L, 3L, 16L, 17L, 18L, 28L,
      29L, 30L, 4L, 5L, 6L, 25L, 26L, 27L, 10L, 11L, 12L, 19L, 20L,
      21L, 22L, 23L, 24L
    )
  )
  expect_equal(sort(CVrandom$Predictions[[1]]$Observation), 1:30)
  expect_equal(nrow(CVrandom$Predictions[[1]]), 30)
  expect_equal(
    CVrandom$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

  expect_equal(names(CVrandom$ROC[[1]]), ".folds")
  expect_equal(
    names(CVrandom$ROC[[1]]$.folds),
    c(
      "percent", "sensitivities", "specificities", "thresholds",
      "direction", "cases", "controls", "fun.sesp", "auc", "call",
      "original.predictor", "original.response", "predictor", "response",
      "levels"
    )
  )
  expect_equal(CVrandom$ROC[[1]]$.folds$direction, "<")
  expect_equal(CVrandom$ROC[[1]]$.folds$thresholds,
    c(
      -Inf, 0.015738278045319, 0.0244654030539095, 0.0578450820175931,
      0.0933510899776593, 0.108683879952878, 0.137825432349928, 0.176372073940001,
      0.205349709256552, 0.224021246074699, 0.237385168555193, 0.247873432002962,
      0.26268216711469, 0.314905963721685, 0.375657401280478, 0.398409646586515,
      0.423670300748199, 0.453490617917851, 0.463453618925996, 0.468184761935845,
      0.471038468647748, 0.479620903264731, 0.495692970696837, 0.539321022573858,
      0.578289209399372, 0.656093266210519, 0.745043152943254, 0.781334373983555,
      0.817583099356852, 0.885025866678916, Inf
    ),
    tolerance = 1e-5
  )
  expect_equal(CVrandom$ROC[[1]]$.folds$sensitivities,
    c(
      1, 1, 1, 0.944444444444444, 0.888888888888889, 0.888888888888889,
      0.888888888888889, 0.833333333333333, 0.777777777777778, 0.722222222222222,
      0.722222222222222, 0.666666666666667, 0.611111111111111, 0.611111111111111,
      0.555555555555556, 0.555555555555556, 0.5, 0.444444444444444,
      0.444444444444444, 0.444444444444444, 0.444444444444444, 0.388888888888889,
      0.388888888888889, 0.333333333333333, 0.277777777777778, 0.277777777777778,
      0.222222222222222, 0.166666666666667, 0.111111111111111, 0.0555555555555556,
      0
    ),
    tolerance = 1e-5
  )
  expect_equal(CVrandom$ROC[[1]]$.folds$specificities,
    c(
      0, 0.0833333333333333, 0.166666666666667, 0.166666666666667,
      0.166666666666667, 0.25, 0.333333333333333, 0.333333333333333,
      0.333333333333333, 0.333333333333333, 0.416666666666667, 0.416666666666667,
      0.416666666666667, 0.5, 0.5, 0.583333333333333, 0.583333333333333,
      0.583333333333333, 0.666666666666667, 0.75, 0.833333333333333,
      0.833333333333333, 0.916666666666667, 0.916666666666667, 0.916666666666667,
      1, 1, 1, 1, 1, 1
    ),
    tolerance = 1e-5
  )

  expect_equal(CVrandom$Predictions[[1]]$Prediction,
    c(
      0.275483862496912, 0.228903944836929, 0.0144339059479535, 0.728964562527835,
      0.249880471732467, 0.161183276679367, 0.0170426501426846, 0.486100345151499,
      0.102900171885267, 0.801547004608437, 0.354328064946458, 0.936432539252564,
      0.245866392273456, 0.473141461377963, 0.191560871200636, 0.583221969893202,
      0.459473189897835, 0.467434047954157, 0.399832555558532, 0.505285596242175,
      0.0318881559651345, 0.114467588020489, 0.468935475917533, 0.396986737614498,
      0.833619194105268, 0.761121743358672, 0.573356448905542, 0.447508045937866,
      0.0838020080700517, 0.219138547312468
    ),
    tolerance = 1e-4
  )
  expect_equal(CVrandom$Predictions[[2]]$Prediction,
    c(
      0.651655666995794, 0.567737752571702, 0.113508982118219, 0.595925305271521,
      0.358049975009635, 0.428809418343008, 0.0519033221062273, 0.264177667442709,
      0.398790730861947, 0.836134143406525, 0.864721225807443, 0.615352416876704,
      0.775109896436334, 0.355568691389635, 0.405849972041324, 0.706646913895383,
      0.838287665275857, 0.239589131204411, 0.770771533250809, 0.355897744419053,
      0.535597037756816, 0.0930881295353174, 0.169803041499108, 0.899832450784743,
      0.422637606970966, 0.747746467823163, 0.822652579983696, 0.95465364633128,
      0.685444509377703, 0.500503229675815
    ),
    tolerance = 1e-4
  )

  # Check manually
  man_confmat <- confusion_matrix(
    targets = CVrandom$Predictions[[1]]$Target,
    predictions = as.integer(CVrandom$Predictions[[1]]$`Predicted Class`),
    positive = 1
  )

  expect_equal(man_confmat[["Sensitivity"]][[1]], CVrandom$Sensitivity[[1]])
  expect_equal(man_confmat[["Specificity"]][[1]], CVrandom$Specificity[[1]])
  expect_equal(man_confmat[["Balanced Accuracy"]][[1]], CVrandom$`Balanced Accuracy`[[1]])
})

test_that("gaussian random predictions work with cross_validate_fn()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  lm_model_fn <- function(train_data, formula, hyperparameters) {
    lm(formula = formula, data = train_data)
  }

  random_predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
    runif(nrow(test_data), min = 10, max = 70)
  }

  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
    model_fn = lm_model_fn,
    predict_fn = random_predict_fn,
    formulas = "score~diagnosis",
    fold_cols = ".folds",
    metrics = "all",
    type = "gaussian"
  )

  expect_equal(CVed$RMSE, 28.12423, tolerance = 1e-5)
  expect_equal(CVed$MAE, 24.58774, tolerance = 1e-5)
  expect_equal(CVed$r2m, 0.2640793, tolerance = 1e-5)
  expect_equal(CVed$r2c, 0.2640793, tolerance = 1e-5)
  expect_equal(CVed$AIC, 194.6904, tolerance = 1e-5)
  expect_equal(CVed$AICc, 195.9963, tolerance = 1e-5)
  expect_equal(CVed$BIC, 198.0243, tolerance = 1e-5)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Fold Columns`, 1)
  expect_equal(CVed$`Convergence Warnings`, 0)
  expect_equal(CVed$Family, "gaussian")
  expect_equal(CVed$Dependent, "score")
  expect_equal(CVed$Fixed, "diagnosis")
  expect_equal(
    CVed$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("multinomial random predictions work with cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)

  # Create and fold dataset
  data_mc <- multiclass_probability_tibble(
    num_classes = 3, num_observations = 50,
    apply_softmax = TRUE, FUN = runif,
    class_name = "predictor_"
  )
  class_names <- paste0("class_", c(1, 2, 3))
  data_mc[["target"]] <- factor(sample(
    x = class_names,
    size = 50, replace = TRUE
  ))
  dat <- groupdata2::fold(data_mc, k = 4)

  multinom_model_fn <- function(train_data, formula, hyperparameters) {
    nnet::multinom(
      formula = formula, # converted to formula object within fit_model()
      data = train_data
    )
  }

  random_predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
    multiclass_probability_tibble(
      num_classes = 3, num_observations = nrow(test_data),
      apply_softmax = TRUE, FUN = runif,
      class_name = "class_"
    )
  }

  CVmultinomlist <- cross_validate_fn(dat,
    model_fn = multinom_model_fn,
    predict_fn = random_predict_fn,
    formulas = c(
      "target ~ predictor_1 + predictor_2 + predictor_3",
      "target ~ predictor_1"
    ),
    fold_cols = ".folds", type = "multinomial",
    metrics = list("AUC" = TRUE)
  )

  expect_equal(
    colnames(CVmultinomlist),
    c(
      "Fixed", "Overall Accuracy", "Balanced Accuracy", "F1", "Sensitivity",
      "Specificity", "Pos Pred Value", "Neg Pred Value", "AUC", "Kappa",
      "MCC", "Detection Rate", "Detection Prevalence", "Prevalence",
      "Predictions", "Confusion Matrix", "Results", "Class Level Results",
      "Coefficients", "Folds", "Fold Columns", "Convergence Warnings",
      "Other Warnings", "Warnings and Messages", "Family", "Dependent"
    )
  )

  expect_equal(CVmultinomlist$Kappa, c(0.0944809694238845, -0.13019211491683), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Sensitivity, c(0.391534391534392, 0.238095238095238), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Specificity, c(0.701388888888889, 0.623842592592593), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Pos Pred Value`, c(0.395833333333333, 0.249404761904762), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Neg Pred Value`, c(0.700367647058823, 0.620697167755991), tolerance = 1e-5)
  expect_equal(CVmultinomlist$F1, c(0.39281045751634, 0.240196078431373), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Prevalence, c(0.333333333333333, 0.333333333333333), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Detection Rate`, c(0.133333333333333, 0.08), tolerance = 1e-5)
  expect_equal(CVmultinomlist$`Detection Prevalence`, c(0.33333, 0.3333), tolerance = 1e-3)
  expect_equal(CVmultinomlist$`Balanced Accuracy`, c(0.54646164021164, 0.430968915343915), tolerance = 1e-5)
  expect_equal(CVmultinomlist$MCC, c(0.0987954675442327, -0.133172303210133), tolerance = 1e-5)
  expect_equal(CVmultinomlist$Folds, c(4, 4))
  expect_equal(CVmultinomlist$`Fold Columns`, c(1, 1))
  expect_equal(CVmultinomlist$`Convergence Warnings`, c(0, 0))
  expect_equal(CVmultinomlist$Family, c("multinomial", "multinomial"))
  expect_equal(CVmultinomlist$Dependent, c("target", "target"))
  expect_equal(CVmultinomlist$Fixed, c("predictor_1+predictor_2+predictor_3", "predictor_1"))

  expect_equal(
    CVmultinomlist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0),
      Function = character(0), Type = character(0), Message = character(0)
    ),
    row.names = c(NA, 0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )

  # Enter sub tibbles
  class_level_results <- CVmultinomlist$`Class Level Results`

  expect_equal(
    class_level_results[[1]]$Class,
    c("class_1", "class_2", "class_3")
  )
  expect_equal(
    class_level_results[[2]]$Class,
    c("class_1", "class_2", "class_3")
  )
  expect_equal(
    class_level_results[[2]]$`Balanced Accuracy`,
    c(0.371031746031746, 0.454861111111111, 0.467013888888889)
  )
  expect_equal(
    class_level_results[[1]]$`Balanced Accuracy`,
    c(0.476190476190476, 0.553819444444444, 0.609375)
  )
  expect_equal(
    class_level_results[[2]]$F1,
    c(0.176470588235294, 0.25, 0.294117647058824)
  )
  expect_equal(
    class_level_results[[1]]$F1,
    c(0.266666666666667, 0.411764705882353, 0.5)
  )
  expect_equal(
    class_level_results[[2]]$Sensitivity,
    c(0.214285714285714, 0.222222222222222, 0.277777777777778)
  )
  expect_equal(
    class_level_results[[1]]$Sensitivity,
    c(0.285714285714286, 0.388888888888889, 0.5)
  )
  expect_equal(
    class_level_results[[2]]$Specificity,
    c(0.527777777777778, 0.6875, 0.65625)
  )
  expect_equal(
    class_level_results[[1]]$Specificity,
    c(0.666666666666667, 0.71875, 0.71875)
  )
  expect_equal(
    class_level_results[[2]]$`Pos Pred Value`,
    c(0.15, 0.285714285714286, 0.3125)
  )
  expect_equal(
    class_level_results[[1]]$`Pos Pred Value`,
    c(0.25, 0.4375, 0.5)
  )
  expect_equal(
    class_level_results[[2]]$`Neg Pred Value`,
    c(0.633333333333333, 0.611111111111111, 0.617647058823529)
  )
  expect_equal(
    class_level_results[[1]]$`Neg Pred Value`,
    c(0.705882352941177, 0.676470588235294, 0.71875)
  )
  expect_equal(
    class_level_results[[2]]$Kappa,
    c(-0.228070175438597, -0.094890510948905, -0.0676156583629893)
  )
  expect_equal(
    class_level_results[[1]]$Kappa,
    c(-0.0456273764258555, 0.110320284697509, 0.21875)
  )
  expect_equal(
    class_level_results[[2]]$`Detection Rate`,
    c(0.06, 0.08, 0.10)
  )
  expect_equal(
    class_level_results[[1]]$`Detection Rate`,
    c(0.08, 0.14, 0.18)
  )
  expect_equal(
    class_level_results[[2]]$`Detection Prevalence`,
    c(0.4, 0.28, 0.32)
  )
  expect_equal(
    class_level_results[[1]]$`Detection Prevalence`,
    c(0.32, 0.32, 0.36)
  )
  expect_equal(
    class_level_results[[2]]$Prevalence,
    c(0.28, 0.36, 0.36)
  )
  expect_equal(
    class_level_results[[1]]$Prevalence,
    c(0.28, 0.36, 0.36)
  )
  expect_equal(
    class_level_results[[2]]$Support,
    c(14, 18, 18)
  )
  expect_equal(
    class_level_results[[1]]$Support,
    c(14, 18, 18)
  )

  clr_confmat_1 <- dplyr::bind_rows(class_level_results[[1]]$`Confusion Matrix`)
  clr_confmat_2 <- dplyr::bind_rows(class_level_results[[2]]$`Confusion Matrix`)
  clr_confmat <- dplyr::bind_rows(clr_confmat_2, clr_confmat_1)
  expect_equal(
    clr_confmat$`Fold Column`,
    rep(".folds", 24)
  )
  expect_equal(
    clr_confmat$Prediction,
    rep(c("0", "1"), 12)
  )
  expect_equal(
    clr_confmat$Target,
    rep(c("0", "0", "1", "1"), 6)
  )
  expect_equal(
    clr_confmat$Pos_0,
    rep(c("TP", "FN", "FP", "TN"), 6)
  )
  expect_equal(
    clr_confmat$Pos_1,
    rep(c("TN", "FP", "FN", "TP"), 6)
  )
  expect_equal(
    clr_confmat$N,
    c(
      19L, 17L, 11L, 3L, 22L, 10L, 14L, 4L, 21L, 11L, 13L, 5L, 24L,
      12L, 10L, 4L, 23L, 9L, 11L, 7L, 23L, 9L, 9L, 9L
    )
  )

  # ROC
  expect_is(CVmultinomlist$Results[[1]]$ROC[[1]], "mv.multiclass.roc") # TODO fix here or in code?
  expect_equal(
    CVmultinomlist$Results[[1]]$ROC[[1]]$rocs$`class_1/class_2`[[1]]$sensitivities,
    c(
      1, 0.944444444444444, 0.944444444444444, 0.888888888888889,
      0.888888888888889, 0.888888888888889, 0.833333333333333, 0.777777777777778,
      0.722222222222222, 0.666666666666667, 0.666666666666667, 0.611111111111111,
      0.611111111111111, 0.611111111111111, 0.555555555555556, 0.5,
      0.444444444444444, 0.388888888888889, 0.333333333333333, 0.333333333333333,
      0.277777777777778, 0.277777777777778, 0.222222222222222, 0.222222222222222,
      0.166666666666667, 0.111111111111111, 0.0555555555555556, 0.0555555555555556,
      0.0555555555555556, 0, 0, 0, 0
    )
  )
  expect_equal(
    CVmultinomlist$Results[[1]]$ROC[[1]]$rocs$`class_1/class_2`[[2]]$sensitivities,
    c(
      1, 0.928571428571429, 0.857142857142857, 0.785714285714286,
      0.785714285714286, 0.785714285714286, 0.785714285714286, 0.785714285714286,
      0.785714285714286, 0.785714285714286, 0.785714285714286, 0.714285714285714,
      0.642857142857143, 0.642857142857143, 0.642857142857143, 0.642857142857143,
      0.642857142857143, 0.571428571428571, 0.5, 0.5, 0.428571428571429,
      0.357142857142857, 0.285714285714286, 0.285714285714286, 0.285714285714286,
      0.285714285714286, 0.285714285714286, 0.285714285714286, 0.285714285714286,
      0.214285714285714, 0.142857142857143, 0.0714285714285714, 0
    )
  )
  expect_equal(
    CVmultinomlist$Results[[1]]$ROC[[1]]$rocs$`class_1/class_2`[[1]]$specificities,
    c(
      0, 0, 0.0714285714285714, 0.0714285714285714, 0.142857142857143,
      0.214285714285714, 0.214285714285714, 0.214285714285714, 0.214285714285714,
      0.214285714285714, 0.285714285714286, 0.285714285714286, 0.357142857142857,
      0.428571428571429, 0.428571428571429, 0.428571428571429, 0.428571428571429,
      0.428571428571429, 0.428571428571429, 0.5, 0.5, 0.571428571428571,
      0.571428571428571, 0.642857142857143, 0.642857142857143, 0.642857142857143,
      0.642857142857143, 0.714285714285714, 0.785714285714286, 0.785714285714286,
      0.857142857142857, 0.928571428571429, 1
    )
  )
  expect_equal(
    CVmultinomlist$Results[[1]]$ROC[[1]]$rocs$`class_1/class_2`[[2]]$specificities,
    c(
      0, 0, 0, 0, 0.0555555555555556, 0.111111111111111, 0.166666666666667,
      0.222222222222222, 0.277777777777778, 0.333333333333333, 0.388888888888889,
      0.388888888888889, 0.388888888888889, 0.444444444444444, 0.5,
      0.555555555555556, 0.611111111111111, 0.611111111111111, 0.611111111111111,
      0.666666666666667, 0.666666666666667, 0.666666666666667, 0.666666666666667,
      0.722222222222222, 0.777777777777778, 0.833333333333333, 0.888888888888889,
      0.944444444444444, 1, 1, 1, 1, 1
    )
  )
  expect_equal(
    CVmultinomlist$Results[[1]]$ROC[[1]]$rocs$`class_2/class_3`[[1]]$sensitivities,
    c(
      1, 1, 0.944444444444444, 0.944444444444444, 0.944444444444444,
      0.888888888888889, 0.888888888888889, 0.833333333333333, 0.833333333333333,
      0.777777777777778, 0.777777777777778, 0.722222222222222, 0.722222222222222,
      0.666666666666667, 0.666666666666667, 0.666666666666667, 0.611111111111111,
      0.611111111111111, 0.611111111111111, 0.555555555555556, 0.5,
      0.444444444444444, 0.388888888888889, 0.388888888888889, 0.333333333333333,
      0.333333333333333, 0.333333333333333, 0.333333333333333, 0.277777777777778,
      0.277777777777778, 0.222222222222222, 0.222222222222222, 0.166666666666667,
      0.111111111111111, 0.111111111111111, 0.0555555555555556, 0
    )
  )
  expect_equal(
    CVmultinomlist$Results[[1]]$ROC[[1]]$rocs$`class_2/class_3`[[2]]$sensitivities,
    c(
      1, 0.944444444444444, 0.944444444444444, 0.944444444444444,
      0.888888888888889, 0.888888888888889, 0.888888888888889, 0.888888888888889,
      0.888888888888889, 0.888888888888889, 0.888888888888889, 0.888888888888889,
      0.888888888888889, 0.833333333333333, 0.833333333333333, 0.777777777777778,
      0.722222222222222, 0.666666666666667, 0.666666666666667, 0.666666666666667,
      0.611111111111111, 0.555555555555556, 0.555555555555556, 0.5,
      0.444444444444444, 0.388888888888889, 0.388888888888889, 0.333333333333333,
      0.277777777777778, 0.277777777777778, 0.277777777777778, 0.222222222222222,
      0.166666666666667, 0.166666666666667, 0.111111111111111, 0.0555555555555556,
      0
    )
  )
  expect_equal(
    CVmultinomlist$Results[[1]]$ROC[[1]]$rocs$`class_2/class_3`[[1]]$specificities,
    c(
      0, 0.0555555555555556, 0.0555555555555556, 0.111111111111111,
      0.166666666666667, 0.166666666666667, 0.222222222222222, 0.222222222222222,
      0.277777777777778, 0.277777777777778, 0.333333333333333, 0.333333333333333,
      0.388888888888889, 0.388888888888889, 0.444444444444444, 0.5,
      0.5, 0.555555555555556, 0.611111111111111, 0.611111111111111,
      0.611111111111111, 0.611111111111111, 0.611111111111111, 0.666666666666667,
      0.666666666666667, 0.722222222222222, 0.777777777777778, 0.833333333333333,
      0.833333333333333, 0.888888888888889, 0.888888888888889, 0.944444444444444,
      0.944444444444444, 0.944444444444444, 1, 1, 1
    )
  )
  expect_equal(
    CVmultinomlist$Results[[1]]$ROC[[1]]$rocs$`class_2/class_3`[[2]]$specificities,
    c(
      0, 0, 0.0555555555555556, 0.111111111111111, 0.111111111111111,
      0.166666666666667, 0.222222222222222, 0.277777777777778, 0.333333333333333,
      0.388888888888889, 0.444444444444444, 0.5, 0.555555555555556,
      0.555555555555556, 0.611111111111111, 0.611111111111111, 0.611111111111111,
      0.611111111111111, 0.666666666666667, 0.722222222222222, 0.722222222222222,
      0.722222222222222, 0.777777777777778, 0.777777777777778, 0.777777777777778,
      0.777777777777778, 0.833333333333333, 0.833333333333333, 0.833333333333333,
      0.888888888888889, 0.944444444444444, 0.944444444444444, 0.944444444444444,
      1, 1, 1, 1
    )
  )

  # Predictions
  expect_is(CVmultinomlist$Predictions[[1]], "tbl_df")
  expect_equal(
    colnames(CVmultinomlist$Predictions[[1]]),
    c(
      "Fold Column", "Fold", "Observation",
      "Target", "Prediction", "Predicted Class"
    )
  )
  expect_equal(nrow(CVmultinomlist$Predictions[[1]]), 50)
})

test_that("gaussian results are returned in correct order from cross_validate_fn()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  lm_model_fn <- function(train_data, formula, hyperparameters) {
    lm(formula = formula, data = train_data)
  }

  random_predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
    if ("age" %in% as.character(formula)) {
      return(runif(nrow(test_data), min = 10, max = 70) + 100)
    }
    else {
      return(runif(nrow(test_data), min = 10, max = 70))
    }
  }
  xpectr::set_test_seed(1)
  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
    model_fn = lm_model_fn,
    predict_fn = random_predict_fn,
    formulas = c("score~diagnosis", "score~age"),
    fold_cols = ".folds",
    metrics = "all",
    type = "gaussian"
  )

  expect_equal(CVed$RMSE, c(24.7354111109333, 104.248387787132), tolerance = 1e-5)
  expect_equal(CVed$MAE, c(20.4910080229165, 101.812209742364), tolerance = 1e-5)
  expect_equal(CVed$r2m, c(0.264079271789992, 0.012016907605709), tolerance = 1e-5)
  expect_equal(CVed$r2c, c(0.264079271789992, 0.012016907605709), tolerance = 1e-5)
  expect_equal(CVed$AIC, c(194.690399609051, 201.738710942015), tolerance = 1e-5)
  expect_equal(CVed$AICc, c(195.996281961992, 203.044593294956), tolerance = 1e-5)
  expect_equal(CVed$BIC, c(198.024264011158, 205.072575344122), tolerance = 1e-5)
  expect_equal(CVed$Fixed, c("diagnosis", "age"))

  xpectr::set_test_seed(1)
  # Cross-validate the data
  CVed <- cross_validate_fn(dat,
    model_fn = lm_model_fn,
    predict_fn = random_predict_fn,
    formulas = c("score~age", "score~diagnosis"),
    fold_cols = ".folds",
    metrics = "all",
    type = "gaussian"
  )

  expect_equal(CVed$RMSE, c(104.248387787132, 24.7354111109333), tolerance = 1e-5)
  expect_equal(CVed$MAE, c(101.812209742364, 20.4910080229165), tolerance = 1e-5)
  expect_equal(CVed$r2m, c(0.012016907605709, 0.264079271789992), tolerance = 1e-5)
  expect_equal(CVed$r2c, c(0.012016907605709, 0.264079271789992), tolerance = 1e-5)
  expect_equal(CVed$AIC, c(201.738710942015, 194.690399609051), tolerance = 1e-5)
  expect_equal(CVed$AICc, c(203.044593294956, 195.996281961992), tolerance = 1e-5)
  expect_equal(CVed$BIC, c(205.072575344122, 198.024264011158), tolerance = 1e-5)
  expect_equal(CVed$Fixed, c("age", "diagnosis"))
})

test_that("binomial results are returned in correct order from cross_validate_fn()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  glm_model_fn <- example_model_functions("glm_binomial")

  random_predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
    if ("age" %in% as.character(formula)) {
      return(runif(nrow(test_data), min = 0, max = 0.5))
    }
    else {
      return(runif(nrow(test_data), min = 0.5, max = 1))
    }
  }
  xpectr::set_test_seed(1)
  # Cross-validate the data
  CVbinom <- cross_validate_fn(dat,
    model_fn = glm_model_fn,
    predict_fn = random_predict_fn,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds",
    metrics = list("AIC" = TRUE),
    type = "binomial"
  )

  expect_equal(CVbinom$`Balanced Accuracy`, c(0.5, 0.5), tolerance = 1e-5)
  expect_equal(CVbinom$`Pos Pred Value`, c(0.6, NaN), tolerance = 1e-5)
  expect_equal(CVbinom$`Neg Pred Value`, c(NaN, 0.4), tolerance = 1e-5)
  expect_equal(CVbinom$AIC, c(27.30328, 33.25823), tolerance = 1e-5)
  expect_equal(CVbinom$Fixed, c("score", "age"))
  expect_equal(
    dplyr::bind_rows(CVbinom$Predictions)$`Predicted Class`,
    as.character(rep(c(1, 0), each = 30))
  )

  xpectr::set_test_seed(1)
  # Cross-validate the data
  CVbinom <- cross_validate_fn(dat,
    model_fn = glm_model_fn,
    predict_fn = random_predict_fn,
    formulas = c("diagnosis~age", "diagnosis~score"),
    fold_cols = ".folds",
    metrics = list("AIC" = TRUE),
    type = "binomial"
  )

  expect_equal(CVbinom$`Balanced Accuracy`, c(0.5, 0.5), tolerance = 1e-5)
  expect_equal(CVbinom$`Pos Pred Value`, c(NaN, 0.6), tolerance = 1e-5)
  expect_equal(CVbinom$`Neg Pred Value`, c(0.4, NaN), tolerance = 1e-5)
  expect_equal(CVbinom$AIC, c(33.25823, 27.30328), tolerance = 1e-5)
  expect_equal(CVbinom$Fixed, c("age", "score"))
  expect_equal(
    dplyr::bind_rows(CVbinom$Predictions)$`Predicted Class`,
    as.character(rep(c(0, 1), each = 30))
  )
})

# TODO Add order tests for multinomial
# TODO Make sure every column is in the right order (add order tests)

test_that("lmer model with provided hparams grid works with cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  lmer_model_fn <- function(train_data, formula, hyperparameters) {
    warning(paste0(
      "something: ", hyperparameters[["something"]],
      " else: ", hyperparameters[["else"]],
      " REML: ", hyperparameters[["REML"]]
    ))
    lme4::lmer(formula = formula, data = train_data, hyperparameters[["REML"]])
  }

  lmer_predict_fn <- example_predict_functions("lmer")

  hparams <- tibble::tibble(
    "REML" = c(TRUE, FALSE),
    "something" = c(1, 2),
    "else" = c(3, 4)
  )

  # Cross-validate the data
  suppressWarnings(CVed <- cross_validate_fn(dat,
    model_fn = lmer_model_fn,
    predict_fn = lmer_predict_fn,
    hyperparameters = hparams,
    formulas = "score~diagnosis+(1|session)",
    fold_cols = ".folds",
    type = "gaussian"
  ))

  expect_equal(
    dplyr::bind_rows(CVed$`Warnings and Messages`)$Message,
    c(
      "something: 1 else: 3 REML: TRUE", "something: 1 else: 3 REML: TRUE",
      "something: 1 else: 3 REML: TRUE", "something: 1 else: 3 REML: TRUE",
      "something: 2 else: 4 REML: FALSE", "something: 2 else: 4 REML: FALSE",
      "something: 2 else: 4 REML: FALSE", "something: 2 else: 4 REML: FALSE"
    )
  )

  expect_equal(CVed$RMSE, c(9.63400886475202, 9.65948766929366), tolerance = 1e-4)

  expect_identical(
    dplyr::bind_rows(CVed$HParams),
    hparams
  )
})


# generated tests

test_that("generated tests for gaussian models in cross_validate_fn()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
                          k = 3,
                          cat_col = "diagnosis",
                          id_col = "participant"
  )
  # ####
  # xpectr::gxs_function(fn = cross_validate_fn,
  #                      args_values = list(
  #                        "data" = list(dat, participant.scores, NA, 1),
  #                        "formulas" = list(
  #                          "score ~ diagnosis + (1|session)", as.formula("score ~ diagnosis"),
  #                          function(x,y,z){x+y+z}, "score + diagnosis",
  #                          "score  diagnosis", NA
  #                        ),
  #                        "model_fn" = list(
  #                          function(train_data, formula, hyperparameters) {
  #                            # Expected hyperparameters:
  #                            #  - REML
  #                            if (!"REML" %in% names(hyperparameters))
  #                              stop("'hyperparameters' must include 'REML'")
  #                            lme4::lmer(
  #                              formula = formula,
  #                              data = train_data,
  #                              REML = hyperparameters[["REML"]]
  #                            )
  #                          },
  #                          function(train_data, formula, hyperparameters, xx) {
  #                            lme4::lmer(
  #                              formula = formula,
  #                              data = train_data,
  #                              REML = FALSE
  #                            )
  #                          },
  #                          function(formula, hyperparameters, xx) {
  #                            lme4::lmer(
  #                              formula = formula,
  #                              data = NULL,
  #                              REML = FALSE
  #                            )
  #                          }, NA, 1,
  #                          function(train_data, formula, hyperparameters) {
  #                            NULL
  #                          }
  #                        ),
  #                        "predict_fn" = list(
  #                          function(test_data, model, formula, hyperparameters, train_data) {
  #                            stats::predict(
  #                              object = model,
  #                              newdata = test_data,
  #                              type = "response",
  #                              allow.new.levels = TRUE
  #                            )
  #                          },
  #                          function(test_data, model, formula) {
  #                            stats::predict(
  #                              object = model,
  #                              newdata = test_data,
  #                              type = "response",
  #                              allow.new.levels = TRUE
  #                            )
  #                          },
  #                          function(test_data, model, formula, hyperparameters, train_data, xx) {
  #                            stats::predict(
  #                              object = model,
  #                              newdata = test_data,
  #                              type = "response",
  #                              allow.new.levels = TRUE
  #                            )
  #                          },
  #                          function(test_data, model, formula, hyperparameters, train_data) {
  #                            NULL
  #                          }, NA, 1
  #                        ),
  #                        "preprocess_fn" = list(
  #                          function(train_data, test_data, formula, hyperparameters) {
  #
  #                            # Create simplified version of the formula
  #                            # as recipe does not like inline functions
  #                            # like log() or random effect structures like (1|z)
  #                            # Example:
  #                            # "y ~ log(x) + (1 | z)"  becomes  "y ~ x + z"
  #                            formula <- simplify_formula(formula, train_data)
  #
  #                            # Create recipes object
  #                            recipe_object <- recipes::recipe(
  #
  #                              # Note: If we hardcoded the formula instead of using the formula argument
  #                              # we could preprocess the train/test splits once
  #                              # instead of for every formula
  #                              # Tip: Use `y ~ .` to include all predictors (where `y` is your dependent variable)
  #                              formula = formula,
  #                              data = train_data
  #                            ) %>%
  #
  #                              # Add preprocessing steps
  #                              # Note: We could add specific variable to each step
  #                              # instead of just selecting all numeric predictors
  #                              recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
  #                              recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
  #
  #                              # Find parameters from the training set
  #                              recipes::prep(training = train_data)
  #
  #                            # Apply preprocessing to the partitions
  #                            train_data <- recipes::bake(recipe_object, train_data)
  #                            test_data <- recipes::bake(recipe_object, test_data)
  #
  #                            # Extract the preprocessing parameters
  #                            means <- recipe_object$steps[[1]]$means
  #                            sds <- recipe_object$steps[[2]]$sds
  #
  #                            # Add preprocessing parameters to a tibble
  #                            tidy_parameters <- tibble::tibble("Measure" = c("Mean", "SD")) %>%
  #                              dplyr::bind_cols(dplyr::bind_rows(means, sds))
  #
  #                            list(
  #                              "train" = train_data,
  #                              "test" = test_data,
  #                              "parameters" = tidy_parameters
  #                            )
  #                          },
  #                          function(train_data, test_data, formula) {
  #                            list(
  #                              "train" = train_data,
  #                              "test" = test_data,
  #                              "parameters" = NULL
  #                            )
  #                          },
  #                          function(train_data, test_data, formula, hyperparameters, xx) {
  #                            list(
  #                              "train" = train_data,
  #                              "test" = test_data,
  #                              "parameters" = NULL
  #                            )
  #                          },
  #                          NA, 1
  #                        ),
  #                        "preprocess_once" = list(FALSE, TRUE),
  #                        "hyperparameters" = list(
  #                          list("REML" = FALSE),
  #                          list("REML" = TRUE, "something" = 10),
  #                          NA, 1
  #                        ),
  #                        "fold_cols" = list(".folds", c(".folds", ".folds_2"),
  #                                           NA, 1, list(".folds")),
  #                        "type" = list("gaussian", "binomial", "multinomial", "xx"),
  #                        "cutoff" = list(0.5, 0.1, 2, 0, -2, NA),
  #                        "positive" = list(2,1,"c",NA),
  #                        "metrics" = list(gaussian_metrics(nrmse_iqr = TRUE), binomial_metrics()),
  #                        "rm_nc" = list(FALSE, TRUE),
  #                        "verbose" = list(FALSE)
  #                      ), indentation = 2, tolerance = "1e-5")

  ## Testing 'cross_validate_fn'                                              ####
  ## Initially generated by xpectr
  # Testing different combinations of argument values

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  xpectr::set_test_seed(42)
  # Assigning output
  output_12059 <- cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE)
  # Testing class
  expect_equal(
    class(output_12059),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_12059[["Fixed"]],
    "diagnosis",
    fixed = TRUE)
  expect_equal(
    output_12059[["RMSE"]],
    9.61075,
    tolerance = 1e-5)
  expect_equal(
    output_12059[["MAE"]],
    7.15392,
    tolerance = 1e-5)
  expect_equal(
    output_12059[["NRMSE(IQR)"]],
    0.38694,
    tolerance = 1e-5)
  expect_equal(
    output_12059[["RRSE"]],
    0.51802,
    tolerance = 1e-5)
  expect_equal(
    output_12059[["RAE"]],
    0.47155,
    tolerance = 1e-5)
  expect_equal(
    output_12059[["RMSLE"]],
    0.2666,
    tolerance = 1e-5)
  expect_equal(
    output_12059[["Folds"]],
    3,
    tolerance = 1e-5)
  expect_equal(
    output_12059[["Fold Columns"]],
    1,
    tolerance = 1e-5)
  expect_equal(
    output_12059[["Convergence Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_12059[["Other Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_12059[["Family"]],
    "gaussian",
    fixed = TRUE)
  expect_equal(
    output_12059[["Dependent"]],
    "score",
    fixed = TRUE)
  expect_equal(
    output_12059[["Random"]],
    "(1|session)",
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(output_12059),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
      "Predictions", "Results", "Coefficients", "Preprocess", "Folds",
      "Fold Columns", "Convergence Warnings", "Other Warnings", "Warnings and Messages",
      "Family", "HParams", "Dependent", "Random"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_12059),
    c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list",
      "character", "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_12059),
    c("character", "double", "double", "double", "double", "double",
      "double", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list", "character",
      "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_12059),
    c(1L, 20L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_12059)),
    character(0),
    fixed = TRUE)

  # Testing cross_validate_fn(data = participant.scores,...
  # Changed from baseline: data = participant.sc...
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_11765 <- xpectr::capture_side_effects(cross_validate_fn(data = participant.scores, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_11765[['error']]),
    xpectr::strip("1 assertions failed:\n * the following 'fold_cols' columns were not in 'data': .folds"),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_11765[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = NA, formulas = "sco...
  # Changed from baseline: data = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_16870 <- xpectr::capture_side_effects(cross_validate_fn(data = NA, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16870[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'data': Must be of type 'data.frame', not 'logical'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16870[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = 1, formulas = "scor...
  # Changed from baseline: data = 1
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_13841 <- xpectr::capture_side_effects(cross_validate_fn(data = 1, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13841[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'data': Must be of type 'data.frame', not 'double'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13841[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = NULL, formulas = "s...
  # Changed from baseline: data = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_17698 <- xpectr::capture_side_effects(cross_validate_fn(data = NULL, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17698[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'data': Must be of type 'data.frame', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17698[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = as....
  # Changed from baseline: formulas = as.formula...
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_14976 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = as.formula("score ~ diagnosis"), model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14976[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'formulas': Must be of type 'character', not 'formula'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14976[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = fun...
  # Changed from baseline: formulas = function(x...
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_17176 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = function(x, y, z) {
      x + y + z
  }, model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17176[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'formulas': Must be of type 'character', not 'closure'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17176[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: formulas = "score + d...
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19919 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score + diagnosis", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19919[['error']]),
    xpectr::strip("The model formula does not contain a dependent variable."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19919[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: formulas = "score dia...
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_13800 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score  diagnosis", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13800[['error']]),
    xpectr::strip("The model formula does not contain a dependent variable."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13800[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = NA,...
  # Changed from baseline: formulas = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_17774 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = NA, model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17774[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'formulas': Contains missing values (element 1)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17774[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = NUL...
  # Changed from baseline: formulas = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19347 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = NULL, model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19347[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'formulas': Must be of type 'character', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19347[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: model_fn = function(f...
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_12121 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(formula, hyperparameters, xx) {
      lme4::lmer(formula = formula, data = NULL, REML = FALSE)
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_12121[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'model_fn argument names': Must be a identical to\n * (train_data,formula,hyperparameters)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_12121[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: model_fn = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_16516 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = NA, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16516[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'model_fn': Must be a function, not 'logical'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16516[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: model_fn = 1
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_11255 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = 1, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_11255[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'model_fn': Must be a function, not 'double'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_11255[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: model_fn = function(t...
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_12672 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      NULL
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_12672[['error']]),
    xpectr::strip("\n--------------------------------------------------\ncross_validate_fn(): Boundary (Singular) Fit Message:\nIn model:\nscore ~ diagnosis + (1|session)\nFor fold column:\n.folds\nIn fold:\n1\nHyperparameters:\nREML : FALSE\n'model_fn' returned 'NULL'. Must return a fitted model object."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_12672[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: model_fn = function(t...
  xpectr::set_test_seed(42)
  # Assigning output

  ## Testing 'cross_validate_fn(data = dat, formulas = "sc...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters, xx) {
      lme4::lmer(formula = formula, data = train_data, REML = FALSE)
    }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
                     allow.new.levels = TRUE)
    }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
        recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
        recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
        recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
        dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
    }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'model_fn argument names': Must be a identical to\n * (train_data,formula,hyperparameters)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: model_fn = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_10133 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = NULL, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_10133[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'model_fn': Must be a function, not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_10133[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: predict_fn = function...
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_13823 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13823[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'predict_fn argument names': Must be a identical to (test_data,model,formula,hyperparameters,train_data)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13823[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: predict_fn = function...
  xpectr::set_test_seed(42)

  ## Testing 'cross_validate_fn(data = dat, formulas = "sc...'              ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Assigning side effects
  side_effects_19148 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
        stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
    }, predict_fn = function(test_data, model, formula, hyperparameters, xx) {
      stats::predict(object = model, newdata = test_data, type = "response",
                     allow.new.levels = TRUE)
    }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
        recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
        recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
        recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
        dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
    }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE)
    , reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'predict_fn argument names': Must be a identical to (test_data,model,formula,hyperparameters,train_data)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_19148[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: predict_fn = function...
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_13403 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      NULL
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13403[['error']]),
    xpectr::strip("\n-------------------------------------\ncross_validate_fn(): Error:\nIn formula:\nscore ~ diagnosis + (1|session)\nFor fold column:\n.folds\nIn fold:\n1\nError in run_predict_fn(test_data = test_data, train_data = train_data, : cross_validate_fn(): predictions were NULL.\n"),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13403[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: predict_fn = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_14820 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = NA, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14820[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'predict_fn': Must be a function, not 'logical'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14820[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: predict_fn = 1
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_15995 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = 1, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_15995[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'predict_fn': Must be a function, not 'double'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_15995[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: predict_fn = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_14935 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = NULL, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14935[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'predict_fn': Must be a function, not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14935[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: preprocess_fn = funct...
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_11862 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula) {
      list(train = train_data, test = test_data, parameters = NULL)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_11862[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'preprocess_fn argument names': Must be a identical to\n * (train_data,test_data,formula,hyperparameters)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_11862[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: preprocess_fn = funct...
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_18273 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters, xx) {
      list(train = train_data, test = test_data, parameters = NULL)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_18273[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'preprocess_fn argument names': Must be a identical to\n * (train_data,test_data,formula,hyperparameters)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_18273[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: preprocess_fn = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_16684 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = NA, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16684[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'preprocess_fn': Must be a function (or 'NULL'), not 'logical'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16684[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: preprocess_fn = 1
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_17942 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = 1, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17942[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'preprocess_fn': Must be a function (or 'NULL'), not 'double'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17942[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: preprocess_fn = NULL
  xpectr::set_test_seed(42)
  # Assigning output
  output_11079 <- cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = NULL, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE)
  # Testing class
  expect_equal(
    class(output_11079),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_11079[["Fixed"]],
    "diagnosis",
    fixed = TRUE)
  expect_equal(
    output_11079[["RMSE"]],
    9.61075,
    tolerance = 1e-5)
  expect_equal(
    output_11079[["MAE"]],
    7.15392,
    tolerance = 1e-5)
  expect_equal(
    output_11079[["NRMSE(IQR)"]],
    0.38694,
    tolerance = 1e-5)
  expect_equal(
    output_11079[["RRSE"]],
    0.51802,
    tolerance = 1e-5)
  expect_equal(
    output_11079[["RAE"]],
    0.47155,
    tolerance = 1e-5)
  expect_equal(
    output_11079[["RMSLE"]],
    0.2666,
    tolerance = 1e-5)
  expect_equal(
    output_11079[["Folds"]],
    3,
    tolerance = 1e-5)
  expect_equal(
    output_11079[["Fold Columns"]],
    1,
    tolerance = 1e-5)
  expect_equal(
    output_11079[["Convergence Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_11079[["Other Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_11079[["Family"]],
    "gaussian",
    fixed = TRUE)
  expect_equal(
    output_11079[["Dependent"]],
    "score",
    fixed = TRUE)
  expect_equal(
    output_11079[["Random"]],
    "(1|session)",
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(output_11079),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
      "Predictions", "Results", "Coefficients", "Folds", "Fold Columns",
      "Convergence Warnings", "Other Warnings", "Warnings and Messages",
      "Family", "HParams", "Dependent", "Random"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_11079),
    c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "list", "list", "list", "integer", "integer", "integer",
      "integer", "list", "character", "list", "character",
      "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_11079),
    c("character", "double", "double", "double", "double", "double",
      "double", "list", "list", "list", "integer", "integer", "integer",
      "integer", "list", "character", "list", "character", "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_11079),
    c(1L, 19L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_11079)),
    character(0),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: preprocess_once = TRUE
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_17237 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = TRUE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  # expect_equal(
  #   xpectr::strip(side_effects_17237[['error']]), # NOTE: deleted part of msg below manually
  #   xpectr::strip("1 assertions failed:\n * Variable 'formula': Must be a formula, not 'NULL'.\n Possibly caused by 'preprocessing_once' being TRUE?"),
  #   fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17237[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: preprocess_once = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_14112 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = NULL, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14112[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'preprocess_once': Must be of type 'logical flag', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14112[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: hyperparameters = lis...
  xpectr::set_test_seed(42)
  # Assigning output
  output_18209 <- cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = TRUE, something = 10), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE)
  # Testing class
  expect_equal(
    class(output_18209),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_18209[["Fixed"]],
    "diagnosis",
    fixed = TRUE)
  expect_equal(
    output_18209[["RMSE"]],
    9.59511,
    tolerance = 1e-5)
  expect_equal(
    output_18209[["MAE"]],
    7.20095,
    tolerance = 1e-5)
  expect_equal(
    output_18209[["NRMSE(IQR)"]],
    0.3862,
    tolerance = 1e-5)
  expect_equal(
    output_18209[["RRSE"]],
    0.51736,
    tolerance = 1e-5)
  expect_equal(
    output_18209[["RAE"]],
    0.47467,
    tolerance = 1e-5)
  expect_equal(
    output_18209[["RMSLE"]],
    0.26603,
    tolerance = 1e-5)
  expect_equal(
    output_18209[["Folds"]],
    3,
    tolerance = 1e-5)
  expect_equal(
    output_18209[["Fold Columns"]],
    1,
    tolerance = 1e-5)
  expect_equal(
    output_18209[["Convergence Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_18209[["Other Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_18209[["Family"]],
    "gaussian",
    fixed = TRUE)
  expect_equal(
    output_18209[["Dependent"]],
    "score",
    fixed = TRUE)
  expect_equal(
    output_18209[["Random"]],
    "(1|session)",
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(output_18209),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
      "Predictions", "Results", "Coefficients", "Preprocess", "Folds",
      "Fold Columns", "Convergence Warnings", "Other Warnings", "Warnings and Messages",
      "Family", "HParams", "Dependent", "Random"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_18209),
    c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list",
      "character", "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_18209),
    c("character", "double", "double", "double", "double", "double",
      "double", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list", "character",
      "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_18209),
    c(1L, 20L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_18209)),
    character(0),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: hyperparameters = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_16470 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = NA, fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16470[['error']]),
    xpectr::strip("Assertion failed. One of the following must apply:\n * checkmate::check_data_frame(hyperparameters): Must be of type 'data.frame' (or 'NULL'), not 'logical'\n * checkmate::check_list(hyperparameters): Must be of type 'list' (or 'NULL'), not 'logical'"),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16470[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: hyperparameters = 1
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_17829 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = 1, fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17829[['error']]),
    xpectr::strip("Assertion failed. One of the following must apply:\n * checkmate::check_data_frame(hyperparameters): Must be of type 'data.frame' (or 'NULL'), not 'double'\n * checkmate::check_list(hyperparameters): Must be of type 'list' (or 'NULL'), not 'double'"),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17829[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: hyperparameters = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_15530 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = NULL, fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_15530[['error']]),
    xpectr::strip("'hyperparameters' must include 'REML'"),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_15530[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  if (!is_tibble_v2()){
    # Testing cross_validate_fn(data = dat, formulas = "sc...
    # Changed from baseline: fold_cols = c(".folds...
    xpectr::set_test_seed(42)
    # Testing side effects
    # Assigning side effects
    side_effects_15297 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
        if (!"REML" %in% names(hyperparameters))
            stop("'hyperparameters' must include 'REML'")
        lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
    }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
        stats::predict(object = model, newdata = test_data, type = "response",
            allow.new.levels = TRUE)
    }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
        formula <- simplify_formula(formula, train_data)
        recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
            recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
            recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
            recipes::prep(training = train_data)
        train_data <- recipes::bake(recipe_object, train_data)
        test_data <- recipes::bake(recipe_object, test_data)
        means <- recipe_object$steps[[1]]$means
        sds <- recipe_object$steps[[2]]$sds
        tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
            dplyr::bind_cols(dplyr::bind_rows(means, sds))
        list(train = train_data, test = test_data, parameters = tidy_parameters)
    }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = c(".folds", ".folds_2"), type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
    expect_equal(
      xpectr::strip(side_effects_15297[['error']]),
      xpectr::strip("1 assertions failed:\n * the following 'fold_cols' columns were not in 'data': .folds_2"),
      fixed = TRUE)
    expect_equal(
      xpectr::strip(side_effects_15297[['error_class']]),
      xpectr::strip(c("simpleError", "error", "condition")),
      fixed = TRUE)
  }

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: fold_cols = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_17893 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = NA, type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17893[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'fold_cols': Contains missing values (element 1)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17893[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: fold_cols = 1
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_10233 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = 1, type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_10233[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'fold_cols': Must be of type 'character', not 'double'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_10233[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: fold_cols = list(".fo...
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_14772 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = list(".folds"), type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14772[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'fold_cols': Must be of type 'character', not 'list'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14772[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: fold_cols = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_17323 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = NULL, type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17323[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'fold_cols': Must be of type 'character', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17323[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: type = "binomial"
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_16927 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "binomial", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16927[['error']]),
    xpectr::strip("'metrics_list' contained unknown metric names: NRMSE(IQR)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16927[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: type = "multinomial"
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_14776 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "multinomial", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14776[['error']]),
    xpectr::strip("'metrics_list' contained unknown metric names: NRMSE(IQR)."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14776[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: type = "xx"
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_18612 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "xx", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_18612[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'family/type': Must be element of set {'gaussian','binomial','multinomial'}, but is 'xx'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_18612[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: type = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_14380 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = NULL, cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14380[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'family/type': Must be a subset of {'gaussian','binomial','multinomial'}, not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14380[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: cutoff = 0.1
  xpectr::set_test_seed(42)
  # Assigning output
  output_12447 <- cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.1, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE)
  # Testing class
  expect_equal(
    class(output_12447),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_12447[["Fixed"]],
    "diagnosis",
    fixed = TRUE)
  expect_equal(
    output_12447[["RMSE"]],
    9.61075,
    tolerance = 1e-5)
  expect_equal(
    output_12447[["MAE"]],
    7.15392,
    tolerance = 1e-5)
  expect_equal(
    output_12447[["NRMSE(IQR)"]],
    0.38694,
    tolerance = 1e-5)
  expect_equal(
    output_12447[["RRSE"]],
    0.51802,
    tolerance = 1e-5)
  expect_equal(
    output_12447[["RAE"]],
    0.47155,
    tolerance = 1e-5)
  expect_equal(
    output_12447[["RMSLE"]],
    0.2666,
    tolerance = 1e-5)
  expect_equal(
    output_12447[["Folds"]],
    3,
    tolerance = 1e-5)
  expect_equal(
    output_12447[["Fold Columns"]],
    1,
    tolerance = 1e-5)
  expect_equal(
    output_12447[["Convergence Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_12447[["Other Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_12447[["Family"]],
    "gaussian",
    fixed = TRUE)
  expect_equal(
    output_12447[["Dependent"]],
    "score",
    fixed = TRUE)
  expect_equal(
    output_12447[["Random"]],
    "(1|session)",
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(output_12447),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
      "Predictions", "Results", "Coefficients", "Preprocess", "Folds",
      "Fold Columns", "Convergence Warnings", "Other Warnings", "Warnings and Messages",
      "Family", "HParams", "Dependent", "Random"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_12447),
    c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list",
      "character", "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_12447),
    c("character", "double", "double", "double", "double", "double",
      "double", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list", "character",
      "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_12447),
    c(1L, 20L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_12447)),
    character(0),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: cutoff = 2
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_10706 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 2, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_10706[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'cutoff': Element 1 is not <= 1."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_10706[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: cutoff = 0
  xpectr::set_test_seed(42)
  # Assigning output
  output_10994 <- cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE)
  # Testing class
  expect_equal(
    class(output_10994),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_10994[["Fixed"]],
    "diagnosis",
    fixed = TRUE)
  expect_equal(
    output_10994[["RMSE"]],
    9.61075,
    tolerance = 1e-5)
  expect_equal(
    output_10994[["MAE"]],
    7.15392,
    tolerance = 1e-5)
  expect_equal(
    output_10994[["NRMSE(IQR)"]],
    0.38694,
    tolerance = 1e-5)
  expect_equal(
    output_10994[["RRSE"]],
    0.51802,
    tolerance = 1e-5)
  expect_equal(
    output_10994[["RAE"]],
    0.47155,
    tolerance = 1e-5)
  expect_equal(
    output_10994[["RMSLE"]],
    0.2666,
    tolerance = 1e-5)
  expect_equal(
    output_10994[["Folds"]],
    3,
    tolerance = 1e-5)
  expect_equal(
    output_10994[["Fold Columns"]],
    1,
    tolerance = 1e-5)
  expect_equal(
    output_10994[["Convergence Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_10994[["Other Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_10994[["Family"]],
    "gaussian",
    fixed = TRUE)
  expect_equal(
    output_10994[["Dependent"]],
    "score",
    fixed = TRUE)
  expect_equal(
    output_10994[["Random"]],
    "(1|session)",
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(output_10994),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
      "Predictions", "Results", "Coefficients", "Preprocess", "Folds",
      "Fold Columns", "Convergence Warnings", "Other Warnings", "Warnings and Messages",
      "Family", "HParams", "Dependent", "Random"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_10994),
    c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list",
      "character", "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_10994),
    c("character", "double", "double", "double", "double", "double",
      "double", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list", "character",
      "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_10994),
    c(1L, 20L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_10994)),
    character(0),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: cutoff = -2
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_13162 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = -2, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13162[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'cutoff': Element 1 is not >= 0."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_13162[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: cutoff = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_15186 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = NA, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_15186[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'cutoff': May not be NA."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_15186[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: cutoff = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_16620 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = NULL, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16620[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'cutoff': Must be of type 'number', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16620[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: positive = 1
  xpectr::set_test_seed(42)
  # Assigning output
  output_14068 <- cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 1, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE)
  # Testing class
  expect_equal(
    class(output_14068),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_14068[["Fixed"]],
    "diagnosis",
    fixed = TRUE)
  expect_equal(
    output_14068[["RMSE"]],
    9.61075,
    tolerance = 1e-5)
  expect_equal(
    output_14068[["MAE"]],
    7.15392,
    tolerance = 1e-5)
  expect_equal(
    output_14068[["NRMSE(IQR)"]],
    0.38694,
    tolerance = 1e-5)
  expect_equal(
    output_14068[["RRSE"]],
    0.51802,
    tolerance = 1e-5)
  expect_equal(
    output_14068[["RAE"]],
    0.47155,
    tolerance = 1e-5)
  expect_equal(
    output_14068[["RMSLE"]],
    0.2666,
    tolerance = 1e-5)
  expect_equal(
    output_14068[["Folds"]],
    3,
    tolerance = 1e-5)
  expect_equal(
    output_14068[["Fold Columns"]],
    1,
    tolerance = 1e-5)
  expect_equal(
    output_14068[["Convergence Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_14068[["Other Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_14068[["Family"]],
    "gaussian",
    fixed = TRUE)
  expect_equal(
    output_14068[["Dependent"]],
    "score",
    fixed = TRUE)
  expect_equal(
    output_14068[["Random"]],
    "(1|session)",
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(output_14068),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
      "Predictions", "Results", "Coefficients", "Preprocess", "Folds",
      "Fold Columns", "Convergence Warnings", "Other Warnings", "Warnings and Messages",
      "Family", "HParams", "Dependent", "Random"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_14068),
    c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list",
      "character", "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_14068),
    c("character", "double", "double", "double", "double", "double",
      "double", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list", "character",
      "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_14068),
    c(1L, 20L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_14068)),
    character(0),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: positive = "c"
  xpectr::set_test_seed(42)
  # Assigning output
  output_19128 <- cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = "c", metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE)
  # Testing class
  expect_equal(
    class(output_19128),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_19128[["Fixed"]],
    "diagnosis",
    fixed = TRUE)
  expect_equal(
    output_19128[["RMSE"]],
    9.61075,
    tolerance = 1e-5)
  expect_equal(
    output_19128[["MAE"]],
    7.15392,
    tolerance = 1e-5)
  expect_equal(
    output_19128[["NRMSE(IQR)"]],
    0.38694,
    tolerance = 1e-5)
  expect_equal(
    output_19128[["RRSE"]],
    0.51802,
    tolerance = 1e-5)
  expect_equal(
    output_19128[["RAE"]],
    0.47155,
    tolerance = 1e-5)
  expect_equal(
    output_19128[["RMSLE"]],
    0.2666,
    tolerance = 1e-5)
  expect_equal(
    output_19128[["Folds"]],
    3,
    tolerance = 1e-5)
  expect_equal(
    output_19128[["Fold Columns"]],
    1,
    tolerance = 1e-5)
  expect_equal(
    output_19128[["Convergence Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_19128[["Other Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_19128[["Family"]],
    "gaussian",
    fixed = TRUE)
  expect_equal(
    output_19128[["Dependent"]],
    "score",
    fixed = TRUE)
  expect_equal(
    output_19128[["Random"]],
    "(1|session)",
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(output_19128),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
      "Predictions", "Results", "Coefficients", "Preprocess", "Folds",
      "Fold Columns", "Convergence Warnings", "Other Warnings", "Warnings and Messages",
      "Family", "HParams", "Dependent", "Random"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_19128),
    c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list",
      "character", "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_19128),
    c("character", "double", "double", "double", "double", "double",
      "double", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list", "character",
      "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_19128),
    c(1L, 20L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_19128)),
    character(0),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: positive = NA
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_12936 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = NA, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_12936[['error']]),
    xpectr::strip("Assertion failed. One of the following must apply:\n * checkmate::check_choice(positive): Must be element of set {'1','2'}, but is 'NA'\n * checkmate::check_string(positive): May not be NA"),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_12936[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: positive = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_14590 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = NULL, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14590[['error']]),
    xpectr::strip("Assertion failed. One of the following must apply:\n * checkmate::check_choice(positive): Must be a subset of {'1','2'}, not 'NULL'\n * checkmate::check_string(positive): Must be of type 'string', not 'NULL'"),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14590[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: metrics = binomial_me...
  xpectr::set_test_seed(42)
  # Assigning output
  output_13323 <- cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = binomial_metrics(), rm_nc = FALSE, verbose = FALSE)
  # Testing class
  expect_equal(
    class(output_13323),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_13323[["Fixed"]],
    "diagnosis",
    fixed = TRUE)
  expect_equal(
    output_13323[["RMSE"]],
    9.61075,
    tolerance = 1e-5)
  expect_equal(
    output_13323[["MAE"]],
    7.15392,
    tolerance = 1e-5)
  expect_equal(
    output_13323[["NRMSE(IQR)"]],
    0.38694,
    tolerance = 1e-5)
  expect_equal(
    output_13323[["RRSE"]],
    0.51802,
    tolerance = 1e-5)
  expect_equal(
    output_13323[["RAE"]],
    0.47155,
    tolerance = 1e-5)
  expect_equal(
    output_13323[["RMSLE"]],
    0.2666,
    tolerance = 1e-5)
  expect_equal(
    output_13323[["Folds"]],
    3,
    tolerance = 1e-5)
  expect_equal(
    output_13323[["Fold Columns"]],
    1,
    tolerance = 1e-5)
  expect_equal(
    output_13323[["Convergence Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_13323[["Other Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_13323[["Family"]],
    "gaussian",
    fixed = TRUE)
  expect_equal(
    output_13323[["Dependent"]],
    "score",
    fixed = TRUE)
  expect_equal(
    output_13323[["Random"]],
    "(1|session)",
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(output_13323),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
      "Predictions", "Results", "Coefficients", "Preprocess", "Folds",
      "Fold Columns", "Convergence Warnings", "Other Warnings", "Warnings and Messages",
      "Family", "HParams", "Dependent", "Random"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_13323),
    c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list",
      "character", "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_13323),
    c("character", "double", "double", "double", "double", "double",
      "double", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list", "character",
      "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_13323),
    c(1L, 20L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_13323)),
    character(0),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: metrics = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_16508 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = NULL, rm_nc = FALSE, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16508[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'metrics': Must be of type 'list', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_16508[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: rm_nc = TRUE
  xpectr::set_test_seed(42)
  # Assigning output
  output_12580 <- cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = TRUE, verbose = FALSE)
  # Testing class
  expect_equal(
    class(output_12580),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    output_12580[["Fixed"]],
    "diagnosis",
    fixed = TRUE)
  expect_equal(
    output_12580[["RMSE"]],
    9.61075,
    tolerance = 1e-5)
  expect_equal(
    output_12580[["MAE"]],
    7.15392,
    tolerance = 1e-5)
  expect_equal(
    output_12580[["NRMSE(IQR)"]],
    0.38694,
    tolerance = 1e-5)
  expect_equal(
    output_12580[["RRSE"]],
    0.51802,
    tolerance = 1e-5)
  expect_equal(
    output_12580[["RAE"]],
    0.47155,
    tolerance = 1e-5)
  expect_equal(
    output_12580[["RMSLE"]],
    0.2666,
    tolerance = 1e-5)
  expect_equal(
    output_12580[["Folds"]],
    3,
    tolerance = 1e-5)
  expect_equal(
    output_12580[["Fold Columns"]],
    1,
    tolerance = 1e-5)
  expect_equal(
    output_12580[["Convergence Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_12580[["Other Warnings"]],
    0,
    tolerance = 1e-5)
  expect_equal(
    output_12580[["Family"]],
    "gaussian",
    fixed = TRUE)
  expect_equal(
    output_12580[["Dependent"]],
    "score",
    fixed = TRUE)
  expect_equal(
    output_12580[["Random"]],
    "(1|session)",
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(output_12580),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
      "Predictions", "Results", "Coefficients", "Preprocess", "Folds",
      "Fold Columns", "Convergence Warnings", "Other Warnings", "Warnings and Messages",
      "Family", "HParams", "Dependent", "Random"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(output_12580),
    c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list",
      "character", "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(output_12580),
    c("character", "double", "double", "double", "double", "double",
      "double", "list", "list", "list", "list", "integer", "integer",
      "integer", "integer", "list", "character", "list", "character",
      "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(output_12580),
    c(1L, 20L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(output_12580)),
    character(0),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: rm_nc = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_14785 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = NULL, verbose = FALSE), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14785[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'rm_nc': Must be of type 'logical flag', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_14785[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  # Testing cross_validate_fn(data = dat, formulas = "sc...
  # Changed from baseline: verbose = NULL
  xpectr::set_test_seed(42)
  # Testing side effects
  # Assigning side effects
  side_effects_17663 <- xpectr::capture_side_effects(cross_validate_fn(data = dat, formulas = "score ~ diagnosis + (1|session)", model_fn = function(train_data, formula, hyperparameters) {
      if (!"REML" %in% names(hyperparameters))
          stop("'hyperparameters' must include 'REML'")
      lme4::lmer(formula = formula, data = train_data, REML = hyperparameters[["REML"]])
  }, predict_fn = function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(object = model, newdata = test_data, type = "response",
          allow.new.levels = TRUE)
  }, preprocess_fn = function(train_data, test_data, formula, hyperparameters) {
      formula <- simplify_formula(formula, train_data)
      recipe_object <- recipes::recipe(formula = formula, data = train_data) %>%
          recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%
          recipes::prep(training = train_data)
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds
      tidy_parameters <- tibble::tibble(Measure = c("Mean", "SD")) %>%
          dplyr::bind_cols(dplyr::bind_rows(means, sds))
      list(train = train_data, test = test_data, parameters = tidy_parameters)
  }, preprocess_once = FALSE, hyperparameters = list(REML = FALSE), fold_cols = ".folds", type = "gaussian", cutoff = 0.5, positive = 2, metrics = gaussian_metrics(nrmse_iqr = TRUE), rm_nc = FALSE, verbose = NULL), reset_seed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17663[['error']]),
    xpectr::strip("1 assertions failed:\n * Variable 'verbose': Must be of type 'logical flag', not 'NULL'."),
    fixed = TRUE)
  expect_equal(
    xpectr::strip(side_effects_17663[['error_class']]),
    xpectr::strip(c("simpleError", "error", "condition")),
    fixed = TRUE)

  ## Finished testing 'cross_validate_fn'                                     ####
  #

})
