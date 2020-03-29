library(cvms)
context("cross_validate()")

# NOTICE:
# Numbers tested are the results I got and not "what should be"
# This will allow me to see if something changes, but it shouldn't give false confidence.


# test_that("binomial models work with cross_validate()",{
#
#   # Load data and fold it
#   xpectr::set_test_seed(1)
#   dat <- groupdata2::fold(participant.scores, k = 4,
#                           num_fold_cols = 3,
#                           cat_col = 'diagnosis',
#                           id_col = 'participant')
#
#   CVbinomlist <- cross_validate(dat,
#                                 formulas = c("diagnosis~score", "diagnosis~age"),
#                                 fold_cols = c('.folds_1','.folds_2','.folds_3'),
#                                 family = 'binomial',
#                                 REML = FALSE, verbose = FALSE,
#                                 positive = 1 )
# })

test_that("binomial models work with cross_validate()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", family = "binomial",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  expect_equal(CVbinomlist$AUC, c(0.7615741, 0.1666667), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.58511535, 0.01748744), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9380328, 0.3158459), tolerance = 1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4927536, -0.3636364), tolerance = 1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333, 0.0000000), tolerance = 1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8888889, 0.6666667), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7777778, 0.0000000), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7619048, 0.5), tolerance = 1e-3)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance = 1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4, 0.4), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333, 0.0000000), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3, 0.2), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111, 0.3333333), tolerance = 1e-3)
  expect_equal(CVbinomlist$MCC, c(0.5048268, -0.4082483), tolerance = 1e-3)
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
    c(
      "Fold Column", "Fold", "Observation",
      "Target", "Prediction", "Predicted Class"
    )
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
  expect_equal(CVbinomlist$ROC[[1]]$.folds$levels, c("0", "1"))
  expect_equal(as.numeric(CVbinomlist$ROC[[1]]$.folds$auc), 0.761574074074074, tolerance = 1e-5)
  expect_equal(CVbinomlist$ROC[[1]]$.folds$direction, "<")
  expect_equal(CVbinomlist$ROC[[1]]$.folds$sensitivities,
    c(
      1, 1, 1, 0.944444444444444, 0.944444444444444, 0.944444444444444,
      0.888888888888889, 0.888888888888889, 0.888888888888889, 0.888888888888889,
      0.833333333333333, 0.777777777777778, 0.722222222222222, 0.666666666666667,
      0.666666666666667, 0.666666666666667, 0.611111111111111, 0.5,
      0.5, 0.444444444444444, 0.388888888888889, 0.388888888888889,
      0.333333333333333, 0.277777777777778, 0.222222222222222, 0.166666666666667,
      0.111111111111111, 0.0555555555555556, 0
    ),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlist$ROC[[1]]$.folds$specificities,
    c(
      0, 0.0833333333333333, 0.166666666666667, 0.166666666666667,
      0.25, 0.333333333333333, 0.333333333333333, 0.416666666666667,
      0.5, 0.583333333333333, 0.583333333333333, 0.583333333333333,
      0.583333333333333, 0.583333333333333, 0.666666666666667, 0.75,
      0.75, 0.75, 0.833333333333333, 0.833333333333333, 0.833333333333333,
      0.916666666666667, 1, 1, 1, 1, 1, 1, 1
    ),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlist$ROC[[1]]$.folds$thresholds,
    c(
      -Inf, 0.0923605619969366, 0.112757724455647, 0.144237359545136,
      0.165941018105731, 0.212151384771614, 0.258156534624716, 0.318495485800134,
      0.411849382147996, 0.484386733644502, 0.533712764066178, 0.56976858247031,
      0.586948588473742, 0.59573874641954, 0.627303515132141, 0.668895524575313,
      0.698028863110633, 0.713631833116223, 0.72744063532645, 0.748457739195168,
      0.76725510377552, 0.802294462575759, 0.837689984778296, 0.848041386220925,
      0.855631927354967, 0.87591323408248, 0.909484002715809, 0.929306616108083,
      Inf
    ),
    tolerance = 1e-5
  )
  expect_equal(nrow(CVbinomlist$Predictions[[1]]), 30)
  expect_equal(
    CVbinomlist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0), Function = character(0),
      Type = character(0), Message = character(0)
    ),
    row.names = integer(0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("binomial models checks that dependent variable is numeric with cross_validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(7)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  ) %>%
    dplyr::mutate(diagnosis = factor(diagnosis))

  # dat %>% str()

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", family = "binomial",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  expect_equal(CVbinomlist$AUC, c(0.7476852, 0.4583333), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.5621978, 0.2309283), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9331726, 0.6857384), tolerance = 1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4285714, 0.0000000), tolerance = 1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333, 0.0000000), tolerance = 1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8333333, 1.0000000), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7, NaN), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.75, 0.60), tolerance = 1e-3)
  expect_equal(CVbinomlist$F1, c(0.6363636, NA), tolerance = 1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4, 0.4), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333, 0.0000000), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3333, 0.0), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7083333, 0.5000000), tolerance = 1e-3)
  expect_equal(CVbinomlist$MCC, c(0.4330127, 0.0000000), tolerance = 1e-3)
  expect_equal(CVbinomlist$Folds, c(4, 4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1, 1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0, 0))
  expect_equal(CVbinomlist$Family, c("binomial", "binomial"))
  expect_equal(CVbinomlist$Dependent, c("diagnosis", "diagnosis"))
  expect_equal(CVbinomlist$Fixed, c("score", "age"))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_equal(
    colnames(CVbinomlist$Predictions[[1]]),
    c(
      "Fold Column", "Fold", "Observation",
      "Target", "Prediction", "Predicted Class"
    )
  )
  expect_equal(nrow(CVbinomlist$Predictions[[1]]), 30)

  expect_is(CVbinomlist$ROC[[1]]$.folds, "roc")
  expect_equal(
    names(CVbinomlist$ROC[[1]]$.folds),
    c(
      "percent", "sensitivities", "specificities", "thresholds",
      "direction", "cases", "controls", "fun.sesp", "auc", "call",
      "original.predictor", "original.response", "predictor", "response",
      "levels"
    )
  )
  expect_equal(CVbinomlist$ROC[[1]]$.folds$levels, c("0", "1"))
  expect_equal(as.numeric(CVbinomlist$ROC[[1]]$.folds$auc), 0.747685185185185, tolerance = 1e-5)
  expect_equal(CVbinomlist$ROC[[1]]$.folds$direction, "<")
  expect_equal(CVbinomlist$ROC[[1]]$.folds$sensitivities,
    c(
      1, 1, 0.944444444444444, 0.944444444444444, 0.944444444444444,
      0.888888888888889, 0.888888888888889, 0.888888888888889, 0.833333333333333,
      0.833333333333333, 0.833333333333333, 0.833333333333333, 0.777777777777778,
      0.722222222222222, 0.666666666666667, 0.666666666666667, 0.611111111111111,
      0.555555555555556, 0.555555555555556, 0.5, 0.444444444444444,
      0.388888888888889, 0.388888888888889, 0.333333333333333, 0.277777777777778,
      0.222222222222222, 0.166666666666667, 0.111111111111111, 0.0555555555555556,
      0
    ),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlist$ROC[[1]]$.folds$specificities,
    c(
      0, 0.0833333333333333, 0.0833333333333333, 0.166666666666667,
      0.25, 0.25, 0.333333333333333, 0.416666666666667, 0.416666666666667,
      0.5, 0.583333333333333, 0.666666666666667, 0.666666666666667,
      0.666666666666667, 0.666666666666667, 0.75, 0.75, 0.75, 0.833333333333333,
      0.833333333333333, 0.833333333333333, 0.833333333333333, 0.916666666666667,
      0.916666666666667, 0.916666666666667, 0.916666666666667, 1, 1,
      1, 1
    ),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlist$ROC[[1]]$.folds$thresholds,
    c(
      -Inf, 0.0474149270573646, 0.0804638631263696, 0.124520148716463,
      0.150590529552897, 0.198808108757015, 0.313431488336455, 0.384099214345533,
      0.40604895253334, 0.438640212416923, 0.481802525310446, 0.516282646461478,
      0.566341981618961, 0.617497207831997, 0.638965035749196, 0.675452184439646,
      0.705257363311605, 0.718228755022429, 0.732032329405983, 0.751876043966269,
      0.764568494056223, 0.770445420121476, 0.775314148242544, 0.817370482282321,
      0.870500677113118, 0.885092557373248, 0.891679194000951, 0.904215196791356,
      0.934020744586008, Inf
    ),
    tolerance = 1e-5
  )

  expect_equal(
    CVbinomlist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0), Function = character(0),
      Type = character(0),  Message = character(0)
    ),
    row.names = integer(0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("binomial models work with cross_validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(20)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )


  CVbinomlistrand <- cross_validate(dat,
    formulas = c("diagnosis~score + (1|session)", "diagnosis~age"),
    fold_cols = ".folds",
    family = "binomial",
    verbose = FALSE,
    positive = 1
  )

  expect_equal(CVbinomlistrand$AUC, c(0.8472222, 0.4166667), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$`Lower CI`, c(0.6952664, 0.1964321), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$`Upper CI`, c(0.9991780, 0.6369012), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$Kappa, c(0.58333333, -0.08695652), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$Sensitivity, c(0.75, 0.25), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$Specificity, c(0.8333333, 0.6666667), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$`Pos Pred Value`, c(0.750, 0.3333333), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$`Neg Pred Value`, c(0.8333333, 0.5714286), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$F1, c(0.750, 0.2857143), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$Prevalence, c(0.4, 0.4), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$`Detection Rate`, c(0.3, 0.1), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$`Detection Prevalence`, c(0.4, 0.3), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$`Balanced Accuracy`, c(0.7916667, 0.4583333), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$MCC, c(0.58333333, -0.08908708), tolerance = 1e-3)
  expect_equal(CVbinomlistrand$Folds, c(4, 4))
  expect_equal(CVbinomlistrand$`Fold Columns`, c(1, 1))
  expect_equal(CVbinomlistrand$`Convergence Warnings`, c(0, 0))
  expect_equal(CVbinomlistrand$Family, c("binomial", "binomial"))
  expect_equal(CVbinomlistrand$Dependent, c("diagnosis", "diagnosis"))
  expect_equal(CVbinomlistrand$Fixed, c("score", "age"))
  expect_equal(CVbinomlistrand$Random, c("(1|session)", NA))


  # Enter sub tibbles
  expect_is(CVbinomlistrand$Predictions[[1]], "tbl_df")
  expect_equal(
    colnames(CVbinomlistrand$Predictions[[1]]),
    c(
      "Fold Column", "Fold", "Observation",
      "Target", "Prediction", "Predicted Class"
    )
  )
  expect_equal(nrow(CVbinomlistrand$Predictions[[1]]), 30)

  expect_is(CVbinomlistrand$ROC[[1]]$.folds, "roc")
  expect_equal(
    names(CVbinomlistrand$ROC[[1]]$.folds),
    c(
      "percent", "sensitivities", "specificities", "thresholds",
      "direction", "cases", "controls", "fun.sesp", "auc", "call",
      "original.predictor", "original.response", "predictor", "response",
      "levels"
    )
  )
  expect_equal(CVbinomlistrand$ROC[[1]]$.folds$levels, c("0", "1"))
  expect_equal(as.numeric(CVbinomlistrand$ROC[[1]]$.folds$auc), 0.847222222222222, tolerance = 1e-5)
  expect_equal(CVbinomlistrand$ROC[[1]]$.folds$direction, "<")
  expect_equal(CVbinomlistrand$ROC[[1]]$.folds$sensitivities,
    c(
      1, 0.888888888888889, 0.833333333333333, 0.833333333333333,
      0.833333333333333, 0.833333333333333, 0.833333333333333, 0.833333333333333,
      0.833333333333333, 0.833333333333333, 0.833333333333333, 0.777777777777778,
      0.722222222222222, 0.666666666666667, 0.666666666666667, 0.611111111111111,
      0.555555555555556, 0.5, 0.444444444444444, 0.388888888888889,
      0.333333333333333, 0.277777777777778, 0.222222222222222, 0.166666666666667,
      0.111111111111111, 0.0555555555555556, 0
    ),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlistrand$ROC[[1]]$.folds$specificities,
    c(
      0, 0.25, 0.25, 0.333333333333333, 0.416666666666667, 0.5, 0.583333333333333,
      0.666666666666667, 0.75, 0.833333333333333, 0.916666666666667,
      0.916666666666667, 0.916666666666667, 0.916666666666667, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
    ),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlistrand$ROC[[1]]$.folds$thresholds,
    c(
      -Inf, 0.00100117667657215, 0.0179527181952166, 0.0339994582131968,
      0.10447675558254, 0.181964804133168, 0.192102155750595, 0.252253108215509,
      0.408057876585855, 0.599746714157418, 0.715748901518313, 0.77267721501017,
      0.807815775362585, 0.825448120881781, 0.844429058082737, 0.854512408532099,
      0.880611797051915, 0.906411982969134, 0.926191328135861, 0.951022352464516,
      0.967489298252778, 0.974834920088241, 0.978073203960057, 0.982593357075956,
      0.990122822131509, 0.993513013802942, Inf
    ),
    tolerance = 1e-5
  )

  expect_is(CVbinomlistrand$Predictions[[2]], "tbl_df")
  expect_equal(
    colnames(CVbinomlistrand$Predictions[[2]]),
    c(
      "Fold Column", "Fold", "Observation",
      "Target", "Prediction", "Predicted Class"
    )
  )
  expect_equal(nrow(CVbinomlistrand$Predictions[[2]]), 30)
  expect_is(CVbinomlistrand$ROC[[2]]$.folds, "roc")
  expect_equal(
    names(CVbinomlistrand$ROC[[2]]$.folds),
    c(
      "percent", "sensitivities", "specificities", "thresholds",
      "direction", "cases", "controls", "fun.sesp", "auc", "call",
      "original.predictor", "original.response", "predictor", "response",
      "levels"
    )
  )
  expect_equal(CVbinomlistrand$ROC[[2]]$.folds$levels, c("0", "1"))
  expect_equal(as.numeric(CVbinomlistrand$ROC[[2]]$.folds$auc), 0.416666666666667)
  expect_equal(CVbinomlistrand$ROC[[2]]$.folds$direction, "<")

  expect_equal(
    CVbinomlistrand$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0), Function = character(0),
      Type = character(0), Message = character(0)
    ),
    row.names = integer(0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("gaussian model with cross_validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  # Cross-validate the data
  CVed <- cross_validate(dat, "score~diagnosis",
    fold_cols = ".folds",
    family = "gaussian",
    REML = FALSE,
    verbose = FALSE,
    metrics = list("r2m" = TRUE, "r2c" = TRUE)
  )


  ## Testing 'CVed'                                                         ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(CVed),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    CVed[["Fixed"]],
    "diagnosis",
    fixed = TRUE)
  expect_equal(
    CVed[["RMSE"]],
    17.16817,
    tolerance = 1e-4)
  expect_equal(
    CVed[["MAE"]],
    14.26914,
    tolerance = 1e-4)
  expect_equal(
    CVed[["NRMSE(IQR)"]],
    0.9074,
    tolerance = 1e-4)
  expect_equal(
    CVed[["RRSE"]],
    0.9805,
    tolerance = 1e-4)
  expect_equal(
    CVed[["RAE"]],
    0.98599,
    tolerance = 1e-4)
  expect_equal(
    CVed[["RMSLE"]],
    0.47943,
    tolerance = 1e-4)
  expect_equal(
    CVed[["r2m"]],
    0.26408,
    tolerance = 1e-4)
  expect_equal(
    CVed[["r2c"]],
    0.26408,
    tolerance = 1e-4)
  expect_equal(
    CVed[["AIC"]],
    194.6904,
    tolerance = 1e-4)
  expect_equal(
    CVed[["AICc"]],
    195.99628,
    tolerance = 1e-4)
  expect_equal(
    CVed[["BIC"]],
    198.02426,
    tolerance = 1e-4)
  expect_equal(
    CVed[["Folds"]],
    4,
    tolerance = 1e-4)
  expect_equal(
    CVed[["Fold Columns"]],
    1,
    tolerance = 1e-4)
  expect_equal(
    CVed[["Convergence Warnings"]],
    0,
    tolerance = 1e-4)
  expect_equal(
    CVed[["Singular Fit Messages"]],
    0,
    tolerance = 1e-4)
  expect_equal(
    CVed[["Other Warnings"]],
    0,
    tolerance = 1e-4)
  expect_equal(
    CVed[["Family"]],
    "gaussian",
    fixed = TRUE)
  expect_equal(
    CVed[["Dependent"]],
    "score",
    fixed = TRUE)
  # Testing column names
  expect_equal(
    names(CVed),
    c("Fixed", "RMSE", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE",
      "r2m", "r2c", "AIC", "AICc", "BIC", "Predictions", "Results",
      "Coefficients", "Folds", "Fold Columns", "Convergence Warnings",
      "Singular Fit Messages", "Other Warnings", "Warnings and Messages",
      "Family", "Dependent"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(CVed),
    c("character", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
      "list", "list", "list", "integer", "integer", "integer", "integer",
      "integer", "list", "character", "character"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(CVed),
    c("character", "double", "double", "double", "double", "double",
      "double", "double", "double", "double", "double", "double",
      "list", "list", "list", "integer", "integer", "integer", "integer",
      "integer", "list", "character", "character"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(CVed),
    c(1L, 23L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(CVed)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'CVed'                                                ####

  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Fold Columns`, 1)
  expect_equal(CVed$`Convergence Warnings`, 0)
  expect_equal(CVed$Family, "gaussian")
  expect_equal(CVed$Dependent, "score")
  expect_equal(CVed$Fixed, "diagnosis")
  expect_equal(
    CVed$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0), Function = character(0),
      Type = character(0), Message = character(0)
    ),
    row.names = integer(0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("gaussian mixed models with cross_validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  # Cross-validate the data
  CVed <- cross_validate(dat, c("score~diagnosis + (1|session)", "score~age + (1|session)"),
    fold_cols = ".folds",
    family = "gaussian",
    REML = FALSE,
    verbose = FALSE,
    metrics = list("r2m" = TRUE, "r2c" = TRUE)
  )

  expect_equal(CVed$RMSE, c(9.65949, 15.20226), tolerance = 1e-3)
  expect_equal(CVed$MAE, c(7.145933, 13.577082), tolerance = 1e-3)
  expect_equal(CVed$r2m, c(0.28219291, 0.01319592), tolerance = 1e-3)
  expect_equal(CVed$r2c, c(0.8043140, 0.5016056), tolerance = 1e-3)
  expect_equal(CVed$AIC, c(175.9497, 194.6358), tolerance = 1e-3)
  expect_equal(CVed$AICc, c(178.2523, 196.9384), tolerance = 1e-3)
  expect_equal(CVed$BIC, c(180.3948, 199.0809), tolerance = 1e-3)
  expect_equal(CVed$Folds, c(4, 4))
  expect_equal(CVed$`Fold Columns`, c(1, 1))
  expect_equal(CVed$`Convergence Warnings`, c(0, 0))
  expect_equal(CVed$Family, c("gaussian", "gaussian"))
  expect_equal(CVed$Dependent, c("score", "score"))
  expect_equal(CVed$Fixed, c("diagnosis", "age"))
  expect_equal(CVed$Random, c("(1|session)", "(1|session)"))
  expect_equal(
    CVed$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0), Function = character(0),
      Type = character(0), Message = character(0)
    ),
    row.names = integer(0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("binomial models work with control specified in cross_validate()", {
  testthat::skip("mac and ubuntu give different warnings")
  # TODO fix such that travis get same results

  skip("testing different optimizers is too difficult given platform differences")

  # Load data and fold it
  xpectr::set_test_seed(7)
  dat <- groupdata2::fold(participant.scores,
    k = 3,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  tryCatch(
    {
      cross_validate(
        dat,
        formulas = c("diagnosis~score + (1|session)"),
        fold_cols = ".folds",
        family = "binomial",
        REML = FALSE,
        control = lme4::glmerControl(optimizer = "bobyqa"),
        verbose = FALSE,
        positive = 1
      )
    },
    warning = function(w) {
      expect_true(grepl("unable to evaluate scaled gradient", as.character(w), ignore.case = TRUE))
    }
  )


  cv_Nelder_Mead <- cross_validate(
    dat %>% dplyr::bind_rows(dat, dat, dat, dat),
    formulas = c("diagnosis~score + age + (1|session)"),
    fold_cols = ".folds",
    family = "binomial",
    REML = FALSE,
    control = lme4::glmerControl(optimizer = "Nelder_Mead"),
    verbose = FALSE,
    positive = 1
  )

  cv_bobyqa <- cross_validate(
    dat %>% dplyr::bind_rows(dat, dat, dat, dat),
    formulas = c("diagnosis~score + age + (1|session)"),
    fold_cols = ".folds",
    family = "binomial",
    REML = FALSE,
    control = lme4::glmerControl(optimizer = "bobyqa"),
    verbose = FALSE,
    positive = 1
  )

  # Gather the results from the two different optimizers
  cv <- cv_Nelder_Mead %>%
    dplyr::bind_rows(cv_bobyqa)

  # Appears to be different on linux
  expect_true(cv$`Balanced Accuracy`[[1]] != cv$`Balanced Accuracy`[[2]]) # , c(0.736111111111111, 0.777777777777778))
  expect_true(cv$AUC[[1]] != cv$AUC[[2]]) # c(0.824074074074074, 0.875))
  expect_true(cv$F1[[1]] != cv$F1[[2]]) # c(0.666666666666667, 0.727272727272727))
  expect_equal(cv$Fixed, c("score+age", "score+age"))
  expect_equal(cv$Random, c("(1|session)", "(1|session)"))
  expect_equal(cv$Dependent, c("diagnosis", "diagnosis"))
})

test_that("binomial models gives warnings with control specified in cross_validate()", {
  testthat::skip("mac and ubuntu give different warnings")
  # Tested on both platforms on travis as well
  # Local test should run on mac as is

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(7)
  dat <- groupdata2::fold(participant.scores,
    k = 3,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  # Singular fit message

  cv_process <- tryCatch({
    purrr::map(.x = 1, .f = purrr::quietly(function(.x) {
      xpectr::set_test_seed(2)
      cross_validate(dat,
        formulas = c("diagnosis ~ score + age + (1|session) + (1|age)"),
        fold_cols = ".folds",
        family = "binomial",
        REML = FALSE,
        control = lme4::glmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 100)
        ),
        verbose = FALSE
      )
    }))
  })

  expect_equal(cv_process[[1]]$messages,
    "\n--------------------------------------------------\ncross_validate(): Boundary (Singular) Fit Message:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n1\nboundary (singular) fit: see ?isSingular\n",
    fixed = TRUE
  )

  ### NOTE: The warnings are different between mac and linux
  # So we cannot check the below :/
  expect_equal(cv_process[[1]]$warnings,
    c(
      "\n-------------------------------------\ncross_validate(): Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n1\nmaxfun < 10 * length(par)^2 is not recommended.",
      "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n1\nconvergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded",
      "\n-------------------------------------\ncross_validate(): Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n2\nmaxfun < 10 * length(par)^2 is not recommended.",
      "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n2\nconvergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded",
      "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n2\nModel failed to converge with max|grad| = 0.0405867 (tol = 0.001, component 1)",
      "\n-------------------------------------\ncross_validate(): Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n3\nmaxfun < 10 * length(par)^2 is not recommended.",
      "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n3\nconvergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded",
      "\n-------------------------------------\ncross_validate(): Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n3\nunable to evaluate scaled gradient",
      "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n3\nModel failed to converge: degenerate  Hessian with 1 negative eigenvalues"
    ),
    fixed = TRUE
  )


  # xpectr::set_test_seed(2)
  # cv_messages <- suppressMessages(cross_validate(dat,
  #                               formulas = c("diagnosis ~ score + age + (1|session) + (1|age)"),
  #                               fold_cols = '.folds',
  #                               family='binomial',
  #                               REML = FALSE,
  #                               control = lme4::glmerControl(optimizer="bobyqa",
  #                                                            optCtrl=list(maxfun=100))))
  # expect_equal(cv_messages$`Singular Fit Messages`, 2)
})

test_that("gaussian models work with control specified in cross_validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(2)
  dat <- groupdata2::fold(participant.scores,
    k = 3,
    cat_col = "diagnosis",
    id_col = "participant"
  )


  CVgausslistrand <- cross_validate(dat,
    formulas = c("score~diagnosis + (1|session)"),
    fold_cols = ".folds",
    family = "gaussian",
    REML = FALSE,
    control = lme4::lmerControl(
      optimizer = "Nelder_Mead",
      optCtrl = list(maxfun = 1000000)
    ),
    verbose = FALSE
  )

  expect_equal(CVgausslistrand$RMSE, c(10.44299), tolerance = 1e-3)
  expect_equal(CVgausslistrand$RMSLE, c(0.2959932), tolerance = 1e-3)
  expect_equal(CVgausslistrand$`Convergence Warnings`, c(0))

  expect_equal(evaluate_promise(cross_validate(
    dat,
    formulas = c("score~diagnosis + (1|session)"),
    fold_cols = ".folds",
    family = "gaussian",
    REML = FALSE,
    control = lme4::lmerControl(
      optimizer = "bobyqa",
      optCtrl = list(maxfun = 10)
    ),
    verbose = FALSE
  ))$warnings,
  c(
    "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\nscore~diagnosis + (1|session)\nFor fold column:\n.folds\nIn fold:\n1\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"bobyqa\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(maxfun = 10))), model_verbose : FALSE, family : gaussian, is_special_fn : TRUE\nconvergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded",
    "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\nscore~diagnosis + (1|session)\nFor fold column:\n.folds\nIn fold:\n1\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"bobyqa\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(maxfun = 10))), model_verbose : FALSE, family : gaussian, is_special_fn : TRUE\nModel failed to converge with max|grad| = 0.429297 (tol = 0.002, component 1)",
    "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\nscore~diagnosis + (1|session)\nFor fold column:\n.folds\nIn fold:\n2\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"bobyqa\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(maxfun = 10))), model_verbose : FALSE, family : gaussian, is_special_fn : TRUE\nconvergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded",
    "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\nscore~diagnosis + (1|session)\nFor fold column:\n.folds\nIn fold:\n3\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"bobyqa\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(maxfun = 10))), model_verbose : FALSE, family : gaussian, is_special_fn : TRUE\nconvergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded"
  ),
  fixed = TRUE
  )


  # TODO When counting singular (boundary fit) messages, uncomment and change expected warning/message
  # # Warning because of too few iterations
  # expect_warning(cross_validate(dat,
  #                               formulas = c("age~diagnosis*score + (score|session) + (1|score)"),
  #                               fold_cols = '.folds',
  #                               family='gaussian',
  #                               REML = FALSE,
  #                               control = lme4::lmerControl(optimizer="Nelder_Mead",
  #                                                            optCtrl=list(maxfun=100)),
  #                               verbose=FALSE), "cross_validate(): Convergence Warning:", fixed = TRUE)
})

test_that("model using dot in formula ( y ~ . ) works with cross_validate()", {

  # We wish to test if using the dot "y~." method in the model formula
  # correctly leaves out .folds column.

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  ) %>%
    dplyr::select(-c(participant, session))

  # Expect no warnings
  # https://stackoverflow.com/questions/22003306/is-there-something-in-testthat-like-expect-no-warnings
  expect_warning(cross_validate(dat,
    formulas = c("diagnosis~."),
    fold_cols = ".folds", family = "binomial",
    REML = FALSE, verbose = FALSE
  ),
  regexp = NA
  )

  # Expect no warnings
  # https://stackoverflow.com/questions/22003306/is-there-something-in-testthat-like-expect-no-warnings
  expect_warning(cross_validate(dat,
    formulas = c("score~."),
    fold_cols = ".folds", family = "gaussian",
    REML = FALSE, verbose = FALSE
  ),
  regexp = NA
  )
})

test_that("model using inline functions in formulas works with cross_validate()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
                          k = 4,
                          cat_col = "diagnosis",
                          id_col = "participant"
  )

  # Expect no warnings
  # https://stackoverflow.com/questions/22003306/is-there-something-in-testthat-like-expect-no-warnings
  expect_warning(cv1 <- cross_validate(dat,
                                formulas = c("diagnosis~log(score)",
                                             "diagnosis~log(score) + (1|session)"),
                                fold_cols = ".folds", family = "binomial",
                                REML = FALSE, verbose = FALSE
  ),
  regexp = NA
  )


  # Testing values
  expect_equal(cv1$Fixed, c("log(score)", "log(score)"), fixed = TRUE)
  expect_equal(cv1$Random, c(NA, "(1|session)"), fixed = TRUE)
  expect_equal(cv1$F1, c(0.78947, 0.85714), tolerance = 1e-4)


  # Expect no warnings
  expect_warning(cv2 <- cross_validate(
    dat,
    formulas = c("score~log(age)", "score~log(age)+(1|session)"),
    fold_cols = ".folds", family = "gaussian",
    REML = FALSE, verbose = FALSE
  ),
  regexp = NA
  )

  # Testing values
  expect_equal(cv2$Fixed, c("log(age)", "log(age)"), fixed = TRUE)
  expect_equal(cv2$Random, c(NA, "(1|session)"), fixed = TRUE)
  expect_equal(cv2$RMSE, c(20.67139, 15.19049), tolerance = 1e-4)
})

test_that("binomial models work with repeated cross_validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(2)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant",
    num_fold_cols = 2
  )

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = c(".folds_1", ".folds_2"), family = "binomial",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  expect_equal(CVbinomlist$AUC, c(0.750, 0.2291667), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.5638021, 0.1037390), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9361979, 0.3645035), tolerance = 1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4606625, -0.3043478), tolerance = 1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333, 0.1250000), tolerance = 1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8611111, 0.5833333), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7388889, 0.1666667), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7559524, 0.5000000), tolerance = 1e-3)
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance = 1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4, 0.4), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333, 0.0500000), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3166667, 0.3), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7222222, 0.3541667), tolerance = 1e-3)
  expect_equal(CVbinomlist$MCC, c(0.4689197, -0.3118048), tolerance = 1e-3)
  expect_equal(CVbinomlist$Folds, c(8, 8))
  expect_equal(CVbinomlist$`Fold Columns`, c(2, 2))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0, 0))
  expect_equal(CVbinomlist$Family, c("binomial", "binomial"))
  expect_equal(CVbinomlist$Dependent, c("diagnosis", "diagnosis"))
  expect_equal(CVbinomlist$Fixed, c("score", "age"))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$Results[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]]$.folds_1, "roc")
  expect_is(CVbinomlist$`Confusion Matrix`[[1]], "tbl_df")
  expect_equal(
    colnames(CVbinomlist$Predictions[[1]]),
    c(
      "Fold Column", "Fold", "Observation",
      "Target", "Prediction", "Predicted Class"
    )
  )
  expect_equal(
    colnames(CVbinomlist$Results[[1]]),
    c(
      "Fold Column", "Balanced Accuracy", "F1",
      "Sensitivity", "Specificity", "Pos Pred Value",
      "Neg Pred Value", "AUC", "Lower CI", "Upper CI",
      "Kappa", "MCC", "Detection Rate", "Detection Prevalence",
      "Prevalence"
    )
  )
  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column", "Prediction", "Target", "Pos_0", "Pos_1", "N"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]), 60)

  expect_equal(CVbinomlist$ROC[[1]]$.folds_1$direction, "<")
  expect_equal(as.numeric(CVbinomlist$ROC[[1]]$.folds_1$auc), 0.74537037037037)
  expect_equal(CVbinomlist$ROC[[1]]$.folds_1$thresholds,
    c(
      -Inf, 0.0674387702549503, 0.091762887125031, 0.112813610322215,
      0.165052601019878, 0.227423806423394, 0.302221473822472, 0.380785717586265,
      0.437738151479785, 0.483586410050964, 0.517829000923097, 0.556320081310892,
      0.582348401542413, 0.595977361608476, 0.641975043550198, 0.683724065998779,
      0.691111330225435, 0.702694250196696, 0.715948808241982, 0.725216199854617,
      0.748708837071827, 0.776959885099219, 0.81565409306014, 0.850432570801589,
      0.862043303565538, 0.882622758109746, 0.904347165731837, 0.910863952937387,
      Inf
    ),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlist$ROC[[1]]$.folds_1$sensitivities,
    c(
      1, 1, 0.944444444444444, 0.944444444444444, 0.944444444444444,
      0.888888888888889, 0.888888888888889, 0.888888888888889, 0.888888888888889,
      0.888888888888889, 0.833333333333333, 0.777777777777778, 0.722222222222222,
      0.722222222222222, 0.666666666666667, 0.666666666666667, 0.611111111111111,
      0.555555555555556, 0.5, 0.444444444444444, 0.444444444444444,
      0.388888888888889, 0.333333333333333, 0.277777777777778, 0.222222222222222,
      0.166666666666667, 0.111111111111111, 0.0555555555555556, 0
    ),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlist$ROC[[1]]$.folds_1$specificities,
    c(
      0, 0.0833333333333333, 0.0833333333333333, 0.166666666666667,
      0.25, 0.25, 0.333333333333333, 0.416666666666667, 0.5, 0.583333333333333,
      0.583333333333333, 0.583333333333333, 0.583333333333333, 0.666666666666667,
      0.666666666666667, 0.75, 0.75, 0.833333333333333, 0.833333333333333,
      0.833333333333333, 0.916666666666667, 0.916666666666667, 0.916666666666667,
      0.916666666666667, 0.916666666666667, 0.916666666666667, 0.916666666666667,
      1, 1
    ),
    tolerance = 1e-5
  )

  expect_equal(
    colnames(CVbinomlist$Coefficients[[1]]),
    c("Fold Column", "Fold", "term", "estimate", "std.error", "statistic", "p.value")
  )
  expect_equal(
    colnames(CVbinomlist$Coefficients[[2]]),
    c("Fold Column", "Fold", "term", "estimate", "std.error", "statistic", "p.value")
  )
  expect_equal(
    CVbinomlist$Coefficients[[1]]$p.value,
    c(
      0.01294663, 0.01931905, 0.04833047, 0.05703608, 0.01900735,
      0.03169528, 0.04338194, 0.05071058, 0.01639887, 0.02528367,
      0.04118387, 0.04851447, 0.01412963, 0.02178988, 0.04010255, 0.04752187
    )
  )
  expect_equal(
    CVbinomlist$Coefficients[[2]]$p.value,
    c(
      0.62478703, 0.84891972, 0.18479764, 0.14117487, 0.09443436,
      0.12527121, 0.13628239, 0.10142803, 0.66873537, 0.89953532, 0.21621108,
      0.18124974, 0.67252682, 0.45631539, 0.87784992, 0.77268412
    )
  )
  expect_equal(
    CVbinomlist$Coefficients[[1]]$estimate,
    c(
      5.26664582, -0.12300571, 2.56877552, -0.05531371, 3.03166220,
      -0.06342121, 2.80320088, -0.06211859, 3.61808397, -0.07362534,
      2.70911710, -0.06030082, 4.53407887, -0.10779184, 2.62880059, -0.05770449
    )
  )
  expect_equal(
    CVbinomlist$Coefficients[[2]]$estimate,
    c(
      0.818615019, -0.010725093, -2.665418817, 0.106774730, 4.483382847,
      -0.141769999, -3.054394678, 0.116528272, 0.714444780, -0.007070299,
      -3.594942567, 0.123832276, -0.701387195, 0.044667112, -0.340316825,
      0.024469156
    )
  )
  expect_equal(
    CVbinomlist$Coefficients[[1]]$std.error,
    c(
      2.11917509, 0.05258190, 1.30100740, 0.02906606, 1.29260651, 0.02952251,
      1.38771363, 0.03179176, 1.50758829, 0.03291193, 1.32690970, 0.03056558,
      1.84766496, 0.04698838, 1.28066063, 0.02911987
    )
  )
  expect_equal(
    CVbinomlist$Coefficients[[2]]$std.error,
    c(
      1.67379503, 0.05630060, 2.00992729, 0.07256528, 2.68071734, 0.09247766,
      2.05023360, 0.07114190, 1.66971492, 0.05600309, 2.90696307, 0.09262526,
      1.65937069, 0.05996167, 2.21422931, 0.08470771
    )
  )
  expect_equal(CVbinomlist$Coefficients[[1]]$statistic,
    c(
      2.485234, -2.339317, 1.974451, -1.903034, 2.345387, -2.148233,
      2.020014, -1.953921, 2.399915, -2.237041, 2.041674, -1.972834,
      2.453951, -2.294011, 2.052691, -1.981619
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist$Coefficients[[2]]$statistic,
    c(
      0.4890772, -0.1904970, -1.3261270, 1.4714300, 1.6724564, -1.5330188,
      -1.4897789, 1.6379696, 0.4278843, -0.1262484, -1.2366661, 1.3369170,
      -0.4226826, 0.7449278, -0.1536954, 0.2888657
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist$Coefficients[[1]]$Fold, c(1, 1, 2, 2, 3, 3, 4, 4, 1, 1, 2, 2, 3, 3, 4, 4))
  expect_equal(CVbinomlist$Coefficients[[2]]$Fold, c(1, 1, 2, 2, 3, 3, 4, 4, 1, 1, 2, 2, 3, 3, 4, 4))
  expect_equal(CVbinomlist$Coefficients[[1]]$`Fold Column`, rep(c(".folds_1", ".folds_2"), each = 8))
  expect_equal(CVbinomlist$Coefficients[[2]]$`Fold Column`, rep(c(".folds_1", ".folds_2"), each = 8))

  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$`Fold Column`, rep(c(".folds_1", ".folds_2"), each = 4))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$Prediction, as.character(c(0, 1, 0, 1, 0, 1, 0, 1)))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$Target, as.character(c(0, 0, 1, 1, 0, 0, 1, 1)))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$Pos_0, rep(c("TP", "FN", "FP", "TN"), 2))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$Pos_1, rep(c("TN", "FP", "FN", "TP"), 2))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7, 5, 2, 16, 7, 5, 3, 15))

  expect_equal(CVbinomlist$Results[[1]]$`Fold Column`, c(".folds_1", ".folds_2"))
  expect_equal(CVbinomlist$Results[[1]]$`Balanced Accuracy`, c(0.7361111, 0.7083333), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$F1, c(0.6666667, 0.6363636), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$Sensitivity, c(0.5833333, 0.5833333), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$Specificity, c(0.8888889, 0.8333333), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Pos Pred Value`, c(0.7777778, 0.7), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Neg Pred Value`, c(0.7619048, 0.75), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$AUC, c(0.7453704, 0.7546296), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Lower CI`, c(0.5545666, 0.5730375), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Upper CI`, c(0.9361741, 0.9362218), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$Kappa, c(0.4927536, 0.4285714), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Detection Rate`, c(0.2333333, 0.2333333), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Detection Prevalence`, c(0.3000000, 0.33333), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$Prevalence, c(0.4, 0.4), tolerance = 1e-3)
  expect_equal(CVbinomlist$Results[[1]]$MCC, c(0.5048268, 0.4330127), tolerance = 1e-3)

  expect_equal(
    CVbinomlist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0), Function = character(0),
      Type = character(0), Message = character(0)
    ),
    row.names = integer(0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("binomial models work with positive as.character in cross_validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(2)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant",
    num_fold_cols = 2
  ) %>%
    dplyr::mutate(diagnosis = factor(ifelse(diagnosis == 0, "E", "B")))

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = c(".folds_1", ".folds_2"), family = "binomial",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column", "Prediction", "Target", "Pos_B", "Pos_E", "N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16, 2, 5, 7, 15, 3, 5, 7))
  expect_equal(CVbinomlist$F1, c(0.8049933, 0.5384615), tolerance = 1e-3)

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = c(".folds_1", ".folds_2"), family = "binomial",
    REML = FALSE, verbose = FALSE, positive = "E"
  )

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column", "Prediction", "Target", "Pos_B", "Pos_E", "N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16, 2, 5, 7, 15, 3, 5, 7))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance = 1e-3)

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = c(".folds_1", ".folds_2"), family = "binomial",
    REML = FALSE, verbose = FALSE, positive = "B"
  )

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column", "Prediction", "Target", "Pos_B", "Pos_E", "N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16, 2, 5, 7, 15, 3, 5, 7))
  expect_equal(CVbinomlist$F1, c(0.8049933, 0.5384615), tolerance = 1e-3)

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = c(".folds_1", ".folds_2"), family = "binomial",
    REML = FALSE, verbose = FALSE, positive = 1
  )

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column", "Prediction", "Target", "Pos_B", "Pos_E", "N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16, 2, 5, 7, 15, 3, 5, 7))
  expect_equal(CVbinomlist$F1, c(0.8049933, 0.5384615), tolerance = 1e-3)

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = c(".folds_1", ".folds_2"), family = "binomial",
    REML = FALSE, verbose = FALSE, positive = 2
  )

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column", "Prediction", "Target", "Pos_B", "Pos_E", "N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16, 2, 5, 7, 15, 3, 5, 7))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance = 1e-3)

  expect_error(
    cross_validate(dat,
      formulas = c("diagnosis~score", "diagnosis~age"),
      fold_cols = c(".folds_1", ".folds_2"), family = "binomial",
      REML = FALSE, verbose = FALSE, positive = "C"
    ),
    "When 'positive' is a character, it must correspond to a factor level in the dependent variable.\n'positive' is C and levels are B and E."
  )

  # Interchanging the level names

  xpectr::set_test_seed(2)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant",
    num_fold_cols = 2
  ) %>%
    dplyr::mutate(diagnosis = factor(ifelse(diagnosis == 0, "B", "E")))

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = c(".folds_1", ".folds_2"), family = "binomial",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column", "Prediction", "Target", "Pos_B", "Pos_E", "N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7, 5, 2, 16, 7, 5, 3, 15))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance = 1e-3)

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = c(".folds_1", ".folds_2"), family = "binomial",
    REML = FALSE, verbose = FALSE, positive = "E"
  )

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column", "Prediction", "Target", "Pos_B", "Pos_E", "N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7, 5, 2, 16, 7, 5, 3, 15))
  expect_equal(CVbinomlist$F1, c(0.8049933, 0.5384615), tolerance = 1e-3)

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = c(".folds_1", ".folds_2"), family = "binomial",
    REML = FALSE, verbose = FALSE, positive = "B"
  )

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column", "Prediction", "Target", "Pos_B", "Pos_E", "N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7, 5, 2, 16, 7, 5, 3, 15))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance = 1e-3)

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = c(".folds_1", ".folds_2"), family = "binomial",
    REML = FALSE, verbose = FALSE, positive = 1
  )

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column", "Prediction", "Target", "Pos_B", "Pos_E", "N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7, 5, 2, 16, 7, 5, 3, 15))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance = 1e-3)

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = c(".folds_1", ".folds_2"), family = "binomial",
    REML = FALSE, verbose = FALSE, positive = 2
  )

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column", "Prediction", "Target", "Pos_B", "Pos_E", "N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7, 5, 2, 16, 7, 5, 3, 15))
  expect_equal(CVbinomlist$F1, c(0.8049933, 0.5384615), tolerance = 1e-3)


  expect_equal(
    CVbinomlist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0), Function = character(0),
      Type = character(0), Message = character(0)
    ),
    row.names = integer(0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

test_that("gaussian models work with repeated cross_validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant",
    num_fold_cols = 2
  )

  CVgausslist <- cross_validate(dat,
    formulas = c("score~diagnosis", "score~age"),
    fold_cols = c(".folds_1", ".folds_2"), family = "gaussian",
    metrics = list("MALE" = TRUE),
    REML = FALSE, verbose = FALSE
  )


  expect_equal(CVgausslist$RMSE, c(16.69535, 20.07280), tolerance = 1e-3)
  expect_equal(CVgausslist$MAE, c(13.92099, 16.60674), tolerance = 1e-3)
  expect_equal(CVgausslist$`NRMSE(IQR)`, c(0.842571059775381, 1.03282819160309), tolerance = 1e-3)
  expect_equal(CVgausslist$RRSE, c(0.917347061955258, 1.09615517380249), tolerance = 1e-3)
  expect_equal(CVgausslist$RAE, c(0.925413461506034, 1.10589002785508), tolerance = 1e-3)
  expect_equal(CVgausslist$RMSLE, c(0.4737409, 0.5595062), tolerance = 1e-3)
  expect_equal(CVgausslist$MALE, c(0.3837563, 0.4488940), tolerance = 1e-3)
  expect_equal(CVgausslist$AIC, c(194.6793, 201.9189), tolerance = 1e-3)
  expect_equal(CVgausslist$AICc, c(195.9852, 203.2248), tolerance = 1e-3)
  expect_equal(CVgausslist$BIC, c(198.0132, 205.2527), tolerance = 1e-3)
  expect_equal(CVgausslist$Folds, c(8, 8))
  expect_equal(CVgausslist$`Fold Columns`, c(2, 2))
  expect_equal(CVgausslist$`Convergence Warnings`, c(0, 0))
  expect_equal(CVgausslist$Family, c("gaussian", "gaussian"))
  expect_equal(CVgausslist$Dependent, c("score", "score"))
  expect_equal(CVgausslist$Fixed, c("diagnosis", "age"))

  # Enter sub tibbles
  expect_is(CVgausslist$Results[[1]], "tbl_df")
  expect_is(CVgausslist$Coefficients[[1]], "tbl_df")
  expect_equal(
    colnames(CVgausslist$Results[[1]]),
    c("Fold Column", "Fold", "RMSE", "MAE", "NRMSE(IQR)", "RRSE",
    "RAE", "RMSLE", "MALE", "AIC", "AICc", "BIC")
  )
  expect_equal(
    CVgausslist$Results[[1]]$`Fold Column`,
    rep(c(".folds_1", ".folds_2"), each = 4)
  )
  expect_equal(
    colnames(CVgausslist$Predictions[[1]]),
    c(
      "Fold Column", "Fold", "Observation",
      "Target", "Prediction"
    )
  )
  expect_equal(unique(CVgausslist$Predictions[[1]]$`Fold Column`), c(".folds_1", ".folds_2"))
  expect_equal(
    colnames(CVgausslist$Coefficients[[1]]),
    c("Fold Column", "Fold", "term", "estimate", "std.error", "statistic", "p.value")
  )
  expect_equal(
    CVgausslist$Coefficients[[1]]$p.value,
    c(
      8.230130e-09, 1.290208e-02, 1.346678e-08, 1.073278e-02, 2.434650e-09,
      1.980414e-03, 9.363412e-08, 4.950473e-02, 6.908574e-09, 1.352584e-02,
      9.209672e-08, 3.018268e-02, 1.700920e-08, 1.994213e-02, 2.078447e-09, 1.105365e-03
    )
  )
  expect_equal(
    CVgausslist$Coefficients[[2]]$p.value,
    c(
      0.009808606, 0.637825565, 0.258490506, 0.636485814, 0.010557346,
      0.646231138, 0.085798324, 0.557360724, 0.151732090, 0.638915931,
      0.093280236, 0.851553515, 0.017961529, 0.953518671, 0.021408386, 0.658577591
    )
  )
  expect_equal(CVgausslist$Coefficients[[1]]$estimate,
    c(
      49.55556, -18.88889, 53.33333, -21.16667, 51.00000, -23.53333,
      49.77778, -16.61111, 49.55556, -18.55556, 49.77778, -18.52778,
      51.00000, -18.80000, 53.33333, -25.58333
    ),
    tolerance = 1e-5
  )
  expect_equal(CVgausslist$Coefficients[[2]]$estimate,
    c(
      45.14324082, -0.25604297, 29.32953312, 0.42100977, 43.24298347,
      -0.25509419, 30.55040872, 0.33242507, 28.88530466, 0.34729296,
      35.47615039, 0.12093154, 40.14314003, -0.03262612, 46.87397730,
      -0.27329111
    ),
    tolerance = 1e-5
  )
  expect_equal(CVgausslist$Coefficients[[1]]$std.error,
    c(
      5.518360, 6.980235, 5.656682, 7.483087, 5.301864, 6.706387,
      5.984982, 7.917387, 5.463479, 6.910816, 5.978424, 7.908712,
      5.922223, 7.491086, 5.036852, 6.663130
    ),
    tolerance = 1e-6
  )
  expect_equal(CVgausslist$Coefficients[[2]]$std.error,
    c(
      15.9672206, 0.5363914, 25.1784502, 0.8765252, 15.4721646,
      0.5481382, 16.8586576, 0.5565485, 19.4513547, 0.7299321,
      20.0765470, 0.6374769, 15.6973841, 0.5533890, 18.6950224,
      0.6087937
    ),
    tolerance = 1e-6
  )
  expect_equal(CVgausslist$Coefficients[[1]]$statistic,
    c(
      8.980123, -2.706053, 9.428378, -2.828601, 9.619258,
      -3.509093, 8.317114, -2.098055, 9.070329, -2.685002,
      8.326238, -2.342705, 8.611631, -2.509650, 10.588623,
      -3.839537
    ),
    tolerance = 1e-6
  )
  expect_equal(CVgausslist$Coefficients[[2]]$statistic,
    c(
      2.82724476, -0.47734352, 1.16486650, 0.48031677,
      2.79488905, -0.46538302, 1.81214955, 0.59729753,
      1.48500220, 0.47578805, 1.76704442, 0.18970340,
      2.55731400, -0.05895694, 2.50729719, -0.44890594
    ),
    tolerance = 1e-6
  )

  expect_equal(
    CVgausslist$`Warnings and Messages`[[1]],
    structure(list(
      `Fold Column` = character(0), Fold = integer(0), Function = character(0),
      Type = character(0), Message = character(0)
    ),
    row.names = integer(0L), class = c("tbl_df", "tbl", "data.frame")
    )
  )
})

# TODO Check preprocessing with random effects as well!
test_that("preprocessing works with binomial models in cross_validate()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  CVbinomlist_no_preprocessing <- cross_validate(
    dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", family = "binomial",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  CVbinomlist_standardize <- cross_validate(
    dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", family = "binomial",
    preprocessing = "standardize",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  CVbinomlist_center <- cross_validate(
    dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", family = "binomial",
    preprocessing = "center",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  CVbinomlist_scale <- cross_validate(
    dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", family = "binomial",
    preprocessing = "scale",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  CVbinomlist_range <- cross_validate(
    dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", family = "binomial",
    preprocessing = "range",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  # NOTE for range version:
  # recipe::step_range : "When a new data point is outside of the ranges seen in the training set,
  # the new values are truncated at min or max."
  # Hence, we might get different predictions for that specific cv

  expect_error(
    xpectr::strip_msg(cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", family = "binomial",
    preprocessing = "standardization",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )),
  xpectr::strip(paste0(
    "1 assertions failed:\n * Variable 'preprocessing': Must be ",
    "element of set\n * {'standardize','scale','center','range'},",
    " but is 'standardization'."
  )),
  fixed = TRUE
  )

  expect_equal(
    colnames(CVbinomlist_standardize),
    colnames(CVbinomlist_center)
  )
  expect_equal(
    colnames(CVbinomlist_standardize),
    colnames(CVbinomlist_scale)
  )
  expect_equal(
    colnames(CVbinomlist_standardize),
    colnames(CVbinomlist_range)
  )
  expect_equal(
    colnames(CVbinomlist_standardize),
    c(
      "Fixed", "Balanced Accuracy", "F1", "Sensitivity", "Specificity", "Pos Pred Value",
      "Neg Pred Value", "AUC", "Lower CI", "Upper CI", "Kappa", "MCC",
      "Detection Rate", "Detection Prevalence", "Prevalence", "Predictions",
      "ROC", "Confusion Matrix", "Results", "Coefficients", "Preprocess",
      "Folds", "Fold Columns", "Convergence Warnings", "Singular Fit Messages",
      "Other Warnings", "Warnings and Messages", "Family", "Dependent"
    )
  )
  expect_equal(
    colnames(CVbinomlist_no_preprocessing),
    c(
      "Fixed", "Balanced Accuracy", "F1", "Sensitivity", "Specificity", "Pos Pred Value",
      "Neg Pred Value", "AUC", "Lower CI", "Upper CI", "Kappa", "MCC",
      "Detection Rate", "Detection Prevalence", "Prevalence", "Predictions",
      "ROC", "Confusion Matrix", "Results", "Coefficients",
      "Folds", "Fold Columns", "Convergence Warnings", "Singular Fit Messages",
      "Other Warnings", "Warnings and Messages", "Family", "Dependent"
    )
  )

  # No preprocessing
  expect_equal(CVbinomlist_no_preprocessing$`Balanced Accuracy`, c(0.7361111, 0.3333333), tolerance = 1e-3)
  expect_equal(CVbinomlist_no_preprocessing$Coefficients[[1]]$estimate,
    c(
      3.19756037998759, -0.0678513281319974, 3.34308245247553, -0.0729927574874602,
      4.14788399490261, -0.0972715556624782, 2.49701032610746, -0.0536164806074169
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_no_preprocessing$Coefficients[[2]]$estimate,
    c(
      -2.31297492899443, 0.100335811507117, 1.7302557200586, -0.050812829705776,
      -1.16399460049258, 0.0626171124232321, 0.613751298912592, -0.0111166003849735
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_no_preprocessing$Predictions[[1]]$Prediction,
    c(
      0.773796146713643, 0.369523238509566, 0.0912557913458876, 0.892058194778159,
      0.736201417552938, 0.552827586629779, 0.830792778437875, 0.604289894202438,
      0.175457433384499, 0.931703421562706, 0.830792778437875, 0.514597941502579,
      0.92690981065346, 0.687473913088783, 0.586709578310841, 0.718679853099963,
      0.267467733090702, 0.0934653326479855, 0.8597682733868, 0.248845336158729,
      0.132050116263308, 0.650317136061844, 0.454175525786427, 0.156424602826963,
      0.844587191118717, 0.708583813132483, 0.587187598636642, 0.851495581323134,
      0.760714060837398, 0.708583813132483
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_no_preprocessing$Predictions[[2]]$Prediction,
    c(
      0.710491235215637, 0.710491235215637, 0.710491235215637, 0.44870042677685,
      0.44870042677685, 0.44870042677685, 0.636813564574957, 0.636813564574957,
      0.636813564574957, 0.671284217511584, 0.671284217511584, 0.671284217511584,
      0.388244841966772, 0.388244841966772, 0.388244841966772, 0.724119218089194,
      0.724119218089194, 0.724119218089194, 0.698413191776537, 0.698413191776537,
      0.698413191776537, 0.593946106206426, 0.593946106206426, 0.593946106206426,
      0.577764489649215, 0.577764489649215, 0.577764489649215, 0.566880950613583,
      0.566880950613583, 0.566880950613583
    ),
    tolerance = 1e-6
  )


  # Standardize
  expect_equal(
    colnames(CVbinomlist_standardize$Preprocess[[1]]),
    c("Fold Column", "Fold", "Measure", "score")
  )
  expect_equal(
    colnames(CVbinomlist_standardize$Preprocess[[2]]),
    c("Fold Column", "Fold", "Measure", "age")
  )
  expect_equal(CVbinomlist_standardize$Preprocess[[1]]$score,
    c(
      37.75, 18.6925932785759, 41.2380952380952, 19.7177705684612,
      36.2916666666667, 19.4276342104357, 40.2857142857143, 19.4220051929322
    ),
    tolerance = 1e-3
  )
  expect_equal(CVbinomlist_standardize$Preprocess[[1]]$Measure,
    rep(c("Mean", "SD"), 4),
    tolerance = 1e-3
  )
  expect_equal(CVbinomlist_standardize$Preprocess[[2]]$age,
    c(
      28.875, 7.39160805002656, 28.2857142857143, 5.12974518999587,
      27.25, 7.51953976389975, 29.2857142857143, 7.93185260290972
    ),
    tolerance = 1e-3
  )
  expect_equal(CVbinomlist_standardize$`Balanced Accuracy`, c(0.7361111, 0.3333333), tolerance = 1e-3)
  expect_equal(CVbinomlist_standardize$Coefficients[[1]]$estimate,
    c(
      0.636172743004689, -1.26831728018262, 0.333000167516465, -1.43925444529707,
      0.617737120651834, -1.88975620249066, 0.337032107351522, -1.041339564784
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_standardize$Coefficients[[2]]$estimate,
    c(
      0.584221628273588, 0.741642992041957, 0.292978536952361, -0.260656868773284,
      0.542321713040495, 0.470851866767076, 0.288193716209797, -0.0881752356990601
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_standardize$Predictions[[1]]$Prediction,
    c(
      0.773796146713643, 0.369523238509566, 0.0912557913458876, 0.892058194778159,
      0.736201417552938, 0.552827586629779, 0.830792778437875, 0.604289894202438,
      0.175457433384499, 0.931703421562706, 0.830792778437875, 0.514597941502579,
      0.92690981065346, 0.687473913088783, 0.586709578310841, 0.718679853099963,
      0.267467733090702, 0.0934653326479855, 0.8597682733868, 0.248845336158729,
      0.132050116263308, 0.650317136061844, 0.454175525786427, 0.156424602826963,
      0.844587191118717, 0.708583813132483, 0.587187598636642, 0.851495581323134,
      0.760714060837398, 0.708583813132483
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_standardize$Predictions[[2]]$Prediction,
    c(
      0.710491235215637, 0.710491235215637, 0.710491235215637, 0.44870042677685,
      0.44870042677685, 0.44870042677685, 0.636813564574957, 0.636813564574957,
      0.636813564574957, 0.671284217511584, 0.671284217511584, 0.671284217511584,
      0.388244841966772, 0.388244841966772, 0.388244841966772, 0.724119218089194,
      0.724119218089194, 0.724119218089194, 0.698413191776537, 0.698413191776537,
      0.698413191776537, 0.593946106206426, 0.593946106206426, 0.593946106206426,
      0.577764489649215, 0.577764489649215, 0.577764489649215, 0.566880950613583,
      0.566880950613583, 0.566880950613583
    ),
    tolerance = 1e-6
  )

  # Scale
  expect_equal(
    colnames(CVbinomlist_scale$Preprocess[[1]]),
    c("Fold Column", "Fold", "Measure", "score")
  )
  expect_equal(
    colnames(CVbinomlist_scale$Preprocess[[2]]),
    c("Fold Column", "Fold", "Measure", "age")
  )
  expect_equal(CVbinomlist_scale$Preprocess[[1]]$score,
    c(
      18.6925932785759, 19.7177705684612,
      19.4276342104357, 19.4220051929322
    ),
    tolerance = 1e-3
  )
  expect_equal(CVbinomlist_scale$Preprocess[[1]]$Measure,
    rep(c("SD"), 4),
    tolerance = 1e-3
  )
  expect_equal(CVbinomlist_scale$Preprocess[[2]]$age,
    c(
      7.39160805002656, 5.12974518999587,
      7.51953976389975, 7.93185260290972
    ),
    tolerance = 1e-3
  )
  expect_equal(CVbinomlist_scale$`Balanced Accuracy`, c(0.7361111, 0.3333333), tolerance = 1e-3)
  expect_equal(CVbinomlist_scale$Coefficients[[1]]$estimate,
    c(
      3.19756037998759, -1.26831728018262, 3.34308245247554, -1.43925444529707,
      4.14788399490261, -1.88975620249067, 2.49701032610746, -1.041339564784
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_scale$Coefficients[[2]]$estimate,
    c(
      -2.31297492899443, 0.741642992041957, 1.7302557200586, -0.260656868773284,
      -1.16399460049259, 0.470851866767077, 0.61375129891259, -0.0881752356990586
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_scale$Predictions[[1]]$Prediction,
    c(
      0.773796146713643, 0.369523238509566, 0.0912557913458876, 0.892058194778159,
      0.736201417552938, 0.552827586629779, 0.830792778437875, 0.604289894202438,
      0.175457433384499, 0.931703421562706, 0.830792778437875, 0.514597941502579,
      0.92690981065346, 0.687473913088783, 0.586709578310841, 0.718679853099963,
      0.267467733090702, 0.0934653326479855, 0.8597682733868, 0.248845336158729,
      0.132050116263308, 0.650317136061844, 0.454175525786427, 0.156424602826963,
      0.844587191118717, 0.708583813132483, 0.587187598636642, 0.851495581323134,
      0.760714060837398, 0.708583813132483
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_scale$Predictions[[2]]$Prediction,
    c(
      0.710491235215637, 0.710491235215637, 0.710491235215637, 0.44870042677685,
      0.44870042677685, 0.44870042677685, 0.636813564574957, 0.636813564574957,
      0.636813564574957, 0.671284217511584, 0.671284217511584, 0.671284217511584,
      0.388244841966772, 0.388244841966772, 0.388244841966772, 0.724119218089194,
      0.724119218089194, 0.724119218089194, 0.698413191776537, 0.698413191776537,
      0.698413191776537, 0.593946106206426, 0.593946106206426, 0.593946106206426,
      0.577764489649215, 0.577764489649215, 0.577764489649215, 0.566880950613583,
      0.566880950613583, 0.566880950613583
    ),
    tolerance = 1e-6
  )

  # Center
  expect_equal(
    colnames(CVbinomlist_center$Preprocess[[1]]),
    c("Fold Column", "Fold", "Measure", "score")
  )
  expect_equal(
    colnames(CVbinomlist_center$Preprocess[[2]]),
    c("Fold Column", "Fold", "Measure", "age")
  )
  expect_equal(CVbinomlist_center$Preprocess[[1]]$score,
    c(
      37.75, 41.2380952380952,
      36.2916666666667, 40.2857142857143
    ),
    tolerance = 1e-3
  )
  expect_equal(CVbinomlist_center$Preprocess[[1]]$Measure,
    rep(c("Mean"), 4),
    tolerance = 1e-3
  )
  expect_equal(CVbinomlist_center$Preprocess[[2]]$age,
    c(
      28.875, 28.2857142857143,
      27.25, 29.2857142857143
    ),
    tolerance = 1e-3
  )
  expect_equal(CVbinomlist_center$`Balanced Accuracy`, c(0.7361111, 0.3333333), tolerance = 1e-3)
  expect_equal(CVbinomlist_center$Coefficients[[1]]$estimate,
    c(
      0.636172743004689, -0.0678513281319975, 0.333000167516464,
      -0.0729927574874603, 0.617737120651835, -0.0972715556624782,
      0.337032107351522, -0.0536164806074169
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_center$Coefficients[[2]]$estimate,
    c(
      0.584221628273588, 0.100335811507118, 0.292978536952361, -0.0508128297057761,
      0.542321713040494, 0.0626171124232322, 0.288193716209797, -0.0111166003849736
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_center$Predictions[[1]]$Prediction,
    c(
      0.773796146713643, 0.369523238509566, 0.0912557913458876, 0.892058194778159,
      0.736201417552938, 0.552827586629779, 0.830792778437875, 0.604289894202438,
      0.175457433384499, 0.931703421562706, 0.830792778437875, 0.514597941502579,
      0.92690981065346, 0.687473913088783, 0.586709578310841, 0.718679853099963,
      0.267467733090702, 0.0934653326479855, 0.8597682733868, 0.248845336158729,
      0.132050116263308, 0.650317136061844, 0.454175525786427, 0.156424602826963,
      0.844587191118717, 0.708583813132483, 0.587187598636642, 0.851495581323134,
      0.760714060837398, 0.708583813132483
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_center$Predictions[[2]]$Prediction,
    c(
      0.710491235215637, 0.710491235215637, 0.710491235215637, 0.44870042677685,
      0.44870042677685, 0.44870042677685, 0.636813564574957, 0.636813564574957,
      0.636813564574957, 0.671284217511584, 0.671284217511584, 0.671284217511584,
      0.388244841966772, 0.388244841966772, 0.388244841966772, 0.724119218089194,
      0.724119218089194, 0.724119218089194, 0.698413191776537, 0.698413191776537,
      0.698413191776537, 0.593946106206426, 0.593946106206426, 0.593946106206426,
      0.577764489649215, 0.577764489649215, 0.577764489649215, 0.566880950613583,
      0.566880950613583, 0.566880950613583
    ),
    tolerance = 1e-6
  )

  # Range
  expect_equal(
    colnames(CVbinomlist_range$Preprocess[[1]]),
    c("Fold Column", "Fold", "Measure", "score")
  )
  expect_equal(
    colnames(CVbinomlist_range$Preprocess[[2]]),
    c("Fold Column", "Fold", "Measure", "age")
  )
  expect_equal(CVbinomlist_range$Preprocess[[1]]$score,
    c(10, 78, 14, 81, 10, 81, 10, 81),
    tolerance = 1e-3
  )
  expect_equal(CVbinomlist_range$Preprocess[[1]]$Measure,
    rep(c("Min", "Max"), 4),
    tolerance = 1e-3
  )
  expect_equal(CVbinomlist_range$Preprocess[[2]]$age,
    c(20, 43, 21, 34, 20, 43, 20, 43),
    tolerance = 1e-3
  )
  expect_equal(CVbinomlist_range$`Balanced Accuracy`, c(0.736111111111111, 0.416666666666667), tolerance = 1e-3)
  expect_equal(CVbinomlist_range$Coefficients[[1]]$estimate,
    c(
      2.51904709866762, -4.61389031297583, 2.3211838476511, -4.89051475165984,
      3.17516843827782, -6.90628045203595, 1.96084552003329, -3.8067701231266
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_range$Coefficients[[2]]$estimate,
    c(
      -0.306258698852079, 2.3077236646637, 0.663186296237301, -0.660566786175088,
      0.0883476479720599, 1.44019358573434, 0.391419291213123, -0.255681808854391
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_range$Predictions[[1]]$Prediction,
    c(
      0.773796146713643, 0.369523238509566, 0.10959904472987, 0.892058194778159,
      0.736201417552938, 0.552827586629779, 0.830792778437875, 0.604289894202438,
      0.175457433384499, 0.910616345870464, 0.830792778437875, 0.514597941502578,
      0.910616345870464, 0.687473913088783, 0.586709578310841, 0.718679853099963,
      0.267467733090702, 0.0934653326479854, 0.8597682733868, 0.248845336158729,
      0.132050116263308, 0.650317136061844, 0.454175525786427, 0.156424602826963,
      0.844587191118717, 0.708583813132483, 0.587187598636643, 0.851495581323134,
      0.760714060837398, 0.708583813132483
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_range$Predictions[[2]]$Prediction,
    c(
      0.710491235215637, 0.710491235215637, 0.710491235215637, 0.44870042677685,
      0.44870042677685, 0.44870042677685, 0.636813564574957, 0.636813564574957,
      0.636813564574957, 0.65997578207063, 0.65997578207063, 0.65997578207063,
      0.500654877141082, 0.500654877141082, 0.500654877141082, 0.724119218089194,
      0.724119218089194, 0.724119218089194, 0.698413191776537, 0.698413191776537,
      0.698413191776537, 0.593946106206426, 0.593946106206426, 0.593946106206426,
      0.577764489649215, 0.577764489649215, 0.577764489649215, 0.566880950613583,
      0.566880950613583, 0.566880950613583
    ),
    tolerance = 1e-6
  )


  expect_equal(
    CVbinomlist_no_preprocessing$Predictions[[1]]$Prediction,
    CVbinomlist_standardize$Predictions[[1]]$Prediction
  )
  expect_equal(
    CVbinomlist_no_preprocessing$Predictions[[1]]$Prediction,
    CVbinomlist_scale$Predictions[[1]]$Prediction
  )
  expect_equal(
    CVbinomlist_no_preprocessing$Predictions[[1]]$Prediction,
    CVbinomlist_center$Predictions[[1]]$Prediction
  )
  # Note: Ranged version is not identical to the non-preprocessed version
  # as recipes::step_range truncates values in the test set that lie below or above
  # the min/max in the training set.


  # get_split <- function(fold = 2){
  #   train_data <- dat %>%
  #     dplyr::filter(.data$.folds != fold)
  #   test_data <- dat %>%
  #     dplyr::filter(.data$.folds == fold)
  #   preprocessed_split <- example_preprocess_functions("range")(
  #     train_data = train_data,
  #     test_data = test_data,
  #     formula = as.formula("diagnosis ~ age"),
  #     NULL)
  #   list("no_preprocessing" = list("train" = train_data, "test" = test_data),
  #        "ranged" = preprocessed_split)
  # }
  #
  # train_test <- get_split(2)
  #
  # glm_no_preprocessing <- glm(diagnosis ~ age, data = train_test$no_preprocessing$train)
  # glm_ranged <- glm(diagnosis ~ age, data = train_test$ranged$train)
  # predictions_no_preprocessing <- predict(glm_no_preprocessing, newdata = train_test$no_preprocessing$test)
  # predictions_ranged <- predict(glm_ranged, newdata = train_test$ranged$test)


  expect_equal(CVbinomlist_no_preprocessing$AUC, c(0.7615741, 0.1666667), tolerance = 1e-3)
  expect_equal(CVbinomlist_standardize$AUC, c(0.7615741, 0.1666667), tolerance = 1e-3)
  expect_equal(CVbinomlist_scale$AUC, c(0.7615741, 0.1666667), tolerance = 1e-3)
  expect_equal(CVbinomlist_center$AUC, c(0.7615741, 0.1666667), tolerance = 1e-3)
  expect_equal(CVbinomlist_range$AUC, c(0.7615741, 0.1666667), tolerance = 1e-3)

  expect_equal(CVbinomlist_no_preprocessing$MCC, c(0.5048268, -0.4082483), tolerance = 1e-3)
  expect_equal(CVbinomlist_standardize$MCC, c(0.5048268, -0.4082483), tolerance = 1e-3)
  expect_equal(CVbinomlist_scale$MCC, c(0.5048268, -0.4082483), tolerance = 1e-3)
  expect_equal(CVbinomlist_center$MCC, c(0.5048268, -0.4082483), tolerance = 1e-3)
  expect_equal(CVbinomlist_range$MCC, c(0.504826790279024, -0.272165526975909), tolerance = 1e-3) # Why is this different?
})

test_that("preprocessing works with binomial mixed models in cross_validate()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 3,
    cat_col = "diagnosis",
    id_col = "participant"
  ) %>%
    dplyr::mutate(session = as.factor(session)) %>%
    dplyr::slice(rep(1:30, each = 3))

  CVbinomlist_no_preprocessing <- cross_validate(
    dat,
    formulas = c("diagnosis~score+(1|session)"),
    fold_cols = ".folds", family = "binomial",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  CVbinomlist_standardize <- cross_validate(
    dat,
    formulas = c("diagnosis~score+(1|session)"),
    fold_cols = ".folds", family = "binomial",
    preprocessing = "standardize",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  CVbinomlist_center <- cross_validate(
    dat,
    formulas = c("diagnosis~score+(1|session)"),
    fold_cols = ".folds", family = "binomial",
    preprocessing = "center",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  CVbinomlist_scale <- cross_validate(
    dat,
    formulas = c("diagnosis~score+(1|session)"),
    fold_cols = ".folds", family = "binomial",
    preprocessing = "scale",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  CVbinomlist_range <- cross_validate(
    dat,
    formulas = c("diagnosis~score+(1|session)"),
    fold_cols = ".folds", family = "binomial",
    preprocessing = "range",
    REML = FALSE, verbose = FALSE,
    positive = 1
  )

  # NOTE for range version:
  # recipe::step_range : "When a new data point is outside of the ranges seen in the training set,
  # the new values are truncated at min or max."
  # Hence, we might get different predictions for that specific cv

  expect_equal(
    colnames(CVbinomlist_standardize),
    colnames(CVbinomlist_center)
  )
  expect_equal(
    colnames(CVbinomlist_standardize),
    colnames(CVbinomlist_scale)
  )
  expect_equal(
    colnames(CVbinomlist_standardize),
    colnames(CVbinomlist_range)
  )
  expect_equal(
    colnames(CVbinomlist_standardize),
    c(
      "Fixed", "Balanced Accuracy", "F1", "Sensitivity", "Specificity", "Pos Pred Value",
      "Neg Pred Value", "AUC", "Lower CI", "Upper CI", "Kappa", "MCC",
      "Detection Rate", "Detection Prevalence", "Prevalence", "Predictions",
      "ROC", "Confusion Matrix", "Results", "Coefficients", "Preprocess",
      "Folds", "Fold Columns", "Convergence Warnings", "Singular Fit Messages",
      "Other Warnings", "Warnings and Messages", "Family", "Dependent",
      "Random"
    )
  )
  expect_equal(
    colnames(CVbinomlist_no_preprocessing),
    c(
      "Fixed", "Balanced Accuracy", "F1", "Sensitivity", "Specificity", "Pos Pred Value",
      "Neg Pred Value", "AUC", "Lower CI", "Upper CI", "Kappa", "MCC",
      "Detection Rate", "Detection Prevalence", "Prevalence", "Predictions",
      "ROC", "Confusion Matrix", "Results", "Coefficients",
      "Folds", "Fold Columns", "Convergence Warnings", "Singular Fit Messages",
      "Other Warnings", "Warnings and Messages", "Family", "Dependent",
      "Random"
    )
  )

  expect_equal(CVbinomlist_standardize$Preprocess[[1]]$score,
    c(
      39.3809523809524, 18.3632070530147, 38.3333333333333, 19.6682157217532,
      38.5555555555556, 19.3494836035298
    ),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlist_scale$Preprocess[[1]]$score,
    c(18.3632070530147, 19.6682157217532, 19.3494836035298),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlist_center$Preprocess[[1]]$score,
    c(39.3809523809524, 38.3333333333333, 38.5555555555556),
    tolerance = 1e-5
  )
  expect_equal(CVbinomlist_range$Preprocess[[1]]$score,
    c(11, 78, 10, 81, 10, 81),
    tolerance = 1e-5
  )

  # No preprocessing
  expect_equal(CVbinomlist_no_preprocessing$`Balanced Accuracy`, c(0.8611111), tolerance = 1e-3)
  expect_equal(CVbinomlist_no_preprocessing$Coefficients[[1]]$estimate,
    c(
      8.3579464874966, -0.193892587463713, 55.2463242203065, -1.33929033061194,
      7.64561163963771, -0.168532399577982
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_no_preprocessing$Predictions[[1]]$Prediction,
    c(
      0.324456114093013, 0.324456114093013, 0.324456114093013, 0.109287077948817,
      0.109287077948817, 0.109287077948817, 0.0175510977497869, 0.0175510977497869,
      0.0175510977497869, 0.950291428617545, 0.950291428617545, 0.950291428617545,
      0.980403748861395, 0.980403748861395, 0.980403748861395, 0.950508742035668,
      0.950508742035668, 0.950508742035668, 0.856588042939281, 0.856588042939281,
      0.856588042939281, 0.913842836648199, 0.913842836648199, 0.913842836648199,
      0.958873004064922, 0.958873004064922, 0.958873004064922, 0.681645372889445,
      0.681645372889445, 0.681645372889445, 0.838249513761936, 0.838249513761936,
      0.838249513761936, 1.44502905719389e-10, 1.44502905719389e-10,
      1.44502905719389e-10, 0.681645372889445, 0.681645372889445, 0.681645372889445,
      3.72801881555979e-08, 3.72801881555979e-08, 3.72801881555979e-08,
      1.1697916866856e-07, 1.1697916866856e-07, 1.1697916866856e-07,
      0.999999987178186, 0.999999987178186, 0.999999987178186, 0.999761693013033,
      0.999761693013033, 0.999761693013033, 0.999994784274243, 0.999994784274243,
      0.999994784274243, 0.166242005117161, 0.166242005117161, 0.166242005117161,
      0.373839375611282, 0.373839375611282, 0.373839375611282, 0.0788323931366848,
      0.0788323931366848, 0.0788323931366848, 0.218328494540638, 0.218328494540638,
      0.218328494540638, 0.264759089772758, 0.264759089772758, 0.264759089772758,
      0.392711382027383, 0.392711382027383, 0.392711382027383, 0.85297773335794,
      0.85297773335794, 0.85297773335794, 0.945569734698759, 0.945569734698759,
      0.945569734698759, 0.981033143319363, 0.981033143319363, 0.981033143319363,
      0.872881689544893, 0.872881689544893, 0.872881689544893, 0.975814737517958,
      0.975814737517958, 0.975814737517958, 0.996428661773137, 0.996428661773137,
      0.996428661773137
    ),
    tolerance = 1e-6
  )


  # Standardize
  expect_equal(CVbinomlist_standardize$`Balanced Accuracy`, c(0.8611111), tolerance = 1e-3)
  expect_equal(CVbinomlist_standardize$Coefficients[[1]]$estimate,
    c(
      0.722270851420555, -3.56049245923584, 3.90685791812926, -26.3414459230668,
      1.14775661484688, -3.2610246203655
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_standardize$Predictions[[1]]$Prediction,
    c(
      0.324456114093013, 0.324456114093013, 0.324456114093013, 0.109287077948817,
      0.109287077948817, 0.109287077948817, 0.0175510977497869, 0.0175510977497869,
      0.0175510977497869, 0.950291428617545, 0.950291428617545, 0.950291428617545,
      0.980403748861395, 0.980403748861395, 0.980403748861395, 0.950508742035668,
      0.950508742035668, 0.950508742035668, 0.856588042939281, 0.856588042939281,
      0.856588042939281, 0.913842836648199, 0.913842836648199, 0.913842836648199,
      0.958873004064922, 0.958873004064922, 0.958873004064922, 0.681645372889445,
      0.681645372889445, 0.681645372889445, 0.838249513761936, 0.838249513761936,
      0.838249513761936, 1.44502905719389e-10, 1.44502905719389e-10,
      1.44502905719389e-10, 0.681645372889445, 0.681645372889445, 0.681645372889445,
      3.72801881555979e-08, 3.72801881555979e-08, 3.72801881555979e-08,
      1.1697916866856e-07, 1.1697916866856e-07, 1.1697916866856e-07,
      0.999999987178186, 0.999999987178186, 0.999999987178186, 0.999761693013033,
      0.999761693013033, 0.999761693013033, 0.999994784274243, 0.999994784274243,
      0.999994784274243, 0.166242005117161, 0.166242005117161, 0.166242005117161,
      0.373839375611282, 0.373839375611282, 0.373839375611282, 0.0788323931366848,
      0.0788323931366848, 0.0788323931366848, 0.218328494540638, 0.218328494540638,
      0.218328494540638, 0.264759089772758, 0.264759089772758, 0.264759089772758,
      0.392711382027383, 0.392711382027383, 0.392711382027383, 0.85297773335794,
      0.85297773335794, 0.85297773335794, 0.945569734698759, 0.945569734698759,
      0.945569734698759, 0.981033143319363, 0.981033143319363, 0.981033143319363,
      0.872881689544893, 0.872881689544893, 0.872881689544893, 0.975814737517958,
      0.975814737517958, 0.975814737517958, 0.996428661773137, 0.996428661773137,
      0.996428661773137
    ),
    tolerance = 1e-6
  )

  # Scale
  expect_equal(CVbinomlist_scale$`Balanced Accuracy`, c(0.8611111), tolerance = 1e-3)
  expect_equal(CVbinomlist_scale$Coefficients[[1]]$estimate,
    c(
      8.35794641518746, -3.56048970107377, 55.2462927019219, -26.3414369433445,
      7.64561182809043, -3.26101498104044
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_scale$Predictions[[1]]$Prediction,
    c(
      0.324456114093013, 0.324456114093013, 0.324456114093013, 0.109287077948817,
      0.109287077948817, 0.109287077948817, 0.0175510977497869, 0.0175510977497869,
      0.0175510977497869, 0.950291428617545, 0.950291428617545, 0.950291428617545,
      0.980403748861395, 0.980403748861395, 0.980403748861395, 0.950508742035668,
      0.950508742035668, 0.950508742035668, 0.856588042939281, 0.856588042939281,
      0.856588042939281, 0.913842836648199, 0.913842836648199, 0.913842836648199,
      0.958873004064922, 0.958873004064922, 0.958873004064922, 0.681645372889445,
      0.681645372889445, 0.681645372889445, 0.838249513761936, 0.838249513761936,
      0.838249513761936, 1.44502905719389e-10, 1.44502905719389e-10,
      1.44502905719389e-10, 0.681645372889445, 0.681645372889445, 0.681645372889445,
      3.72801881555979e-08, 3.72801881555979e-08, 3.72801881555979e-08,
      1.1697916866856e-07, 1.1697916866856e-07, 1.1697916866856e-07,
      0.999999987178186, 0.999999987178186, 0.999999987178186, 0.999761693013033,
      0.999761693013033, 0.999761693013033, 0.999994784274243, 0.999994784274243,
      0.999994784274243, 0.166242005117161, 0.166242005117161, 0.166242005117161,
      0.373839375611282, 0.373839375611282, 0.373839375611282, 0.0788323931366848,
      0.0788323931366848, 0.0788323931366848, 0.218328494540638, 0.218328494540638,
      0.218328494540638, 0.264759089772758, 0.264759089772758, 0.264759089772758,
      0.392711382027383, 0.392711382027383, 0.392711382027383, 0.85297773335794,
      0.85297773335794, 0.85297773335794, 0.945569734698759, 0.945569734698759,
      0.945569734698759, 0.981033143319363, 0.981033143319363, 0.981033143319363,
      0.872881689544893, 0.872881689544893, 0.872881689544893, 0.975814737517958,
      0.975814737517958, 0.975814737517958, 0.996428661773137, 0.996428661773137,
      0.996428661773137
    ),
    tolerance = 1e-6
  )

  # Center
  expect_equal(CVbinomlist_center$`Balanced Accuracy`, c(0.8611111), tolerance = 1e-3)
  expect_equal(CVbinomlist_center$Coefficients[[1]]$estimate,
    c(
      0.722270851499559, -0.193892736123874, 3.90685679177423, -1.33929005889398,
      1.14775662092732, -0.168532902834402
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_center$Predictions[[1]]$Prediction,
    c(
      0.324456114093013, 0.324456114093013, 0.324456114093013, 0.109287077948817,
      0.109287077948817, 0.109287077948817, 0.0175510977497869, 0.0175510977497869,
      0.0175510977497869, 0.950291428617545, 0.950291428617545, 0.950291428617545,
      0.980403748861395, 0.980403748861395, 0.980403748861395, 0.950508742035668,
      0.950508742035668, 0.950508742035668, 0.856588042939281, 0.856588042939281,
      0.856588042939281, 0.913842836648199, 0.913842836648199, 0.913842836648199,
      0.958873004064922, 0.958873004064922, 0.958873004064922, 0.681645372889445,
      0.681645372889445, 0.681645372889445, 0.838249513761936, 0.838249513761936,
      0.838249513761936, 1.44502905719389e-10, 1.44502905719389e-10,
      1.44502905719389e-10, 0.681645372889445, 0.681645372889445, 0.681645372889445,
      3.72801881555979e-08, 3.72801881555979e-08, 3.72801881555979e-08,
      1.1697916866856e-07, 1.1697916866856e-07, 1.1697916866856e-07,
      0.999999987178186, 0.999999987178186, 0.999999987178186, 0.999761693013033,
      0.999761693013033, 0.999761693013033, 0.999994784274243, 0.999994784274243,
      0.999994784274243, 0.166242005117161, 0.166242005117161, 0.166242005117161,
      0.373839375611282, 0.373839375611282, 0.373839375611282, 0.0788323931366848,
      0.0788323931366848, 0.0788323931366848, 0.218328494540638, 0.218328494540638,
      0.218328494540638, 0.264759089772758, 0.264759089772758, 0.264759089772758,
      0.392711382027383, 0.392711382027383, 0.392711382027383, 0.85297773335794,
      0.85297773335794, 0.85297773335794, 0.945569734698759, 0.945569734698759,
      0.945569734698759, 0.981033143319363, 0.981033143319363, 0.981033143319363,
      0.872881689544893, 0.872881689544893, 0.872881689544893, 0.975814737517958,
      0.975814737517958, 0.975814737517958, 0.996428661773137, 0.996428661773137,
      0.996428661773137
    ),
    tolerance = 1e-6
  )

  # Range
  expect_equal(CVbinomlist_range$`Balanced Accuracy`, c(0.8611111), tolerance = 1e-3)
  expect_equal(CVbinomlist_range$Coefficients[[1]]$estimate,
    c(
      6.22513139971877, -12.9908044682518, 41.8533992397597, -95.0895775189958,
      5.9602950441032, -11.9658077360469
    ),
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_range$Predictions[[1]]$Prediction,
    c(
      0.324456042975257, 0.324456042975257, 0.324456042975257, 0.109287076272275,
      0.109287076272275, 0.109287076272275, 0.030970688236543, 0.030970688236543,
      0.030970688236543, 0.94029055481341, 0.94029055481341, 0.94029055481341,
      0.980403758381411, 0.980403758381411, 0.980403758381411, 0.950508787930829,
      0.950508787930829, 0.950508787930829, 0.856588029494522, 0.856588029494522,
      0.856588029494522, 0.913842865244269, 0.913842865244269, 0.913842865244269,
      0.958873043191456, 0.958873043191456, 0.958873043191456, 0.681645466159717,
      0.681645466159717, 0.681645466159717, 0.838248709938151, 0.838248709938151,
      0.838248709938151, 1.44504001024224e-10, 1.44504001024224e-10,
      1.44504001024224e-10, 0.681645466159717, 0.681645466159717, 0.681645466159717,
      3.72802314439356e-08, 3.72802314439356e-08, 3.72802314439356e-08,
      1.16979759155026e-07, 1.16979759155026e-07, 1.16979759155026e-07,
      0.999999987178107, 0.999999987178107, 0.999999987178107, 0.99976169099732,
      0.99976169099732, 0.99976169099732, 0.999994784245105, 0.999994784245105,
      0.999994784245105, 0.166241799664311, 0.166241799664311, 0.166241799664311,
      0.373839317386791, 0.373839317386791, 0.373839317386791, 0.078832328751711,
      0.078832328751711, 0.078832328751711, 0.218328276983336, 0.218328276983336,
      0.218328276983336, 0.264758980767326, 0.264758980767326, 0.264758980767326,
      0.392711467485616, 0.392711467485616, 0.392711467485616, 0.852977807679943,
      0.852977807679943, 0.852977807679943, 0.945569828688762, 0.945569828688762,
      0.945569828688762, 0.981033200177857, 0.981033200177857, 0.981033200177857,
      0.872881766816275, 0.872881766816275, 0.872881766816275, 0.975814792859071,
      0.975814792859071, 0.975814792859071, 0.996428676339139, 0.996428676339139,
      0.996428676339139
    ),
    tolerance = 1e-6
  )


  expect_equal(CVbinomlist_no_preprocessing$Predictions[[1]]$Prediction,
    CVbinomlist_standardize$Predictions[[1]]$Prediction,
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_no_preprocessing$Predictions[[1]]$Prediction,
    CVbinomlist_scale$Predictions[[1]]$Prediction,
    tolerance = 1e-6
  )
  expect_equal(CVbinomlist_no_preprocessing$Predictions[[1]]$Prediction,
    CVbinomlist_center$Predictions[[1]]$Prediction,
    tolerance = 1e-6
  )
  # Note: Ranged version is not identical to the non-preprocessed version
  # as recipes::step_range truncates values in the test set that lie below or above
  # the min/max in the training set.

  expect_equal(CVbinomlist_no_preprocessing$AUC, 0.8912037, tolerance = 1e-3)
  expect_equal(CVbinomlist_standardize$AUC, 0.8912037, tolerance = 1e-3)
  expect_equal(CVbinomlist_scale$AUC, 0.8912037, tolerance = 1e-3)
  expect_equal(CVbinomlist_center$AUC, 0.8912037, tolerance = 1e-3)
  expect_equal(CVbinomlist_range$AUC, 0.8912037, tolerance = 1e-3)

  expect_equal(CVbinomlist_no_preprocessing$MCC, 0.7222222, tolerance = 1e-3)
  expect_equal(CVbinomlist_standardize$MCC, 0.7222222, tolerance = 1e-3)
  expect_equal(CVbinomlist_scale$MCC, 0.7222222, tolerance = 1e-3)
  expect_equal(CVbinomlist_center$MCC, 0.7222222, tolerance = 1e-3)
  expect_equal(CVbinomlist_range$MCC, 0.7222222, tolerance = 1e-3) # Why is this different?
})


test_that("that singular fit messages are caught, counted and messaged about in cross_validate()", {

  # skip_test_if_old_R_version()

  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  expect_warning(
    expect_message(
      CVbinom <- cross_validate(
        dat,
        formulas = c("diagnosis~score+(1|participant)+(1|session)"),
        family = "binomial",
        REML = FALSE,
        verbose = FALSE,
        positive = 2
      ),
      "Boundary \\(Singular\\) Fit Message"
    ),
    "cross_validate(): Convergence Warning:",
    fixed = TRUE
  )

  expect_equal(CVbinom$`Singular Fit Messages`, 3)

  # Can't expect the same warnings on mac and ubuntu
  # so we just check that the singular fit message is there
  expect_true(
    "boundary (singular) fit: see ?isSingular\n" %in%
      CVbinom$`Warnings and Messages`[[1]]$Message
  )
})

test_that("the expected errors are thrown by cross_validate()", {


  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    num_fold_cols = 3,
    cat_col = "diagnosis",
    id_col = "participant"
  )
  dat[[".folds_3"]] <- as.character(dat[[".folds_3"]])

  expect_error(cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = paste0(".folds_", 1:3), family = "binomial",
    REML = FALSE, verbose = FALSE,
    positive = 1
  ),
  "At least one of the fold columns is not a factor.",
  fixed = TRUE
  )
  expect_error(
    xpectr::strip_msg(cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = paste0(".folds_", 1), family = "fdsfs",
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
  suppressWarnings(expect_error(cross_validate(
    dat,
    formulas = "diagnosis~score",
    fold_cols = ".folds_1", family = "binomial",
    link = "probit", REML = FALSE, verbose = FALSE,
    positive = 1
  ),
  "The `link` argument of `cross_validate()` was deprecated in cvms 1.0.0 and is now defunct.",
  fixed = TRUE
  ))

  # **** NOTE!! ****: The two below will only throw warnings once per session
  # So test_this() won't necessarily see them, while the overall testing should!
  expect_warning(cross_validate(dat,
    models = "diagnosis~score",
    fold_cols = ".folds_1", family = "binomial",
    REML = FALSE, verbose = FALSE,
    positive = 1
  ),
  "The `models` argument of `cross_validate()` is deprecated as of cvms 1.0.0.
Please use the `formulas` argument instead.",
  fixed = TRUE
  )
  expect_warning(cross_validate(dat,
    formulas = "diagnosis~score",
    fold_cols = ".folds_1", family = "binomial",
    REML = FALSE, model_verbose = FALSE,
    positive = 1
  ),
  "The `model_verbose` argument of `cross_validate()` is deprecated as of cvms 1.0.0.
Please use the `verbose` argument instead.",
  fixed = TRUE
  )

  # With rm_nc, the non-converged model is removed
  # and we only have an empty data frame left
  # Note, this may fail on ubuntu?
  xpectr::set_test_seed(2)
  expect_identical(
    xpectr::suppress_mw(
      cross_validate(
        dplyr::sample_frac(dat, 0.2),
        formulas = c("diagnosis~score*age+(1|session)"),
        family = "gaussian",
        fold_cols = ".folds_1",
        REML = FALSE,
        verbose = FALSE,
        control = lme4::lmerControl(
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 10)
        ),
        rm_nc = TRUE
      )
    ),
    structure(list(
      Fixed = character(0), RMSE = numeric(0), MAE = numeric(0),
      `NRMSE(IQR)` = numeric(0), RRSE = numeric(0), RAE = numeric(0),
      RMSLE = numeric(0), AIC = numeric(0), AICc = numeric(0), BIC = numeric(0),
      Predictions = logical(0), Results = list(), Coefficients = list(),
      Folds = integer(0), `Fold Columns` = integer(0), `Convergence Warnings` = integer(0),
      `Singular Fit Messages` = integer(0), `Other Warnings` = integer(0),
      `Warnings and Messages` = list(), Family = character(0),
      Dependent = character(0),
      Random = character(0)
    ), class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, 0L))
  )
})

test_that("verbose reports the correct model functions in cross_validate()", {


  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    num_fold_cols = 3,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  if (!is_tibble_v2()){

    # Test the list of verbose messages
    # glm()
    expect_equal(evaluate_promise(cross_validate(dat,
      formulas = c("diagnosis~score"),
      fold_cols = paste0(".folds_", 1), family = "binomial",
      REML = FALSE, verbose = TRUE,
      positive = 1
    ))$messages,
    c("Will cross-validate 1 models. This requires fitting 4 model instances.\n",
    "\n--------------------------\ncross_validate(): Message:\nIn model:\ndiagnosis~score\nFor fold column:\n.folds_1\nIn fold:\n1\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = c(\"bobyqa\", \"Nelder_Mead\"), restart_edge = FALSE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, \n    relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(), tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE)), model_verbose : TRUE, family : binomial, is_special_fn : TRUE\ncross_validate(): Used glm() to fit the model.'\n",
    "\n--------------------------\ncross_validate(): Message:\nIn model:\ndiagnosis~score\nFor fold column:\n.folds_1\nIn fold:\n2\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = c(\"bobyqa\", \"Nelder_Mead\"), restart_edge = FALSE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, \n    relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(), tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE)), model_verbose : TRUE, family : binomial, is_special_fn : TRUE\ncross_validate(): Used glm() to fit the model.'\n",
    "\n--------------------------\ncross_validate(): Message:\nIn model:\ndiagnosis~score\nFor fold column:\n.folds_1\nIn fold:\n3\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = c(\"bobyqa\", \"Nelder_Mead\"), restart_edge = FALSE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, \n    relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(), tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE)), model_verbose : TRUE, family : binomial, is_special_fn : TRUE\ncross_validate(): Used glm() to fit the model.'\n",
    "\n--------------------------\ncross_validate(): Message:\nIn model:\ndiagnosis~score\nFor fold column:\n.folds_1\nIn fold:\n4\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = c(\"bobyqa\", \"Nelder_Mead\"), restart_edge = FALSE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, \n    relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(), tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE)), model_verbose : TRUE, family : binomial, is_special_fn : TRUE\ncross_validate(): Used glm() to fit the model.'\n"
    ),
    fixed = TRUE
    )

    # glmer
    expect_equal(evaluate_promise(cross_validate(dat,
      formulas = c("diagnosis~score+(1|session)"),
      fold_cols = paste0(".folds_", 1), family = "binomial",
      REML = FALSE, verbose = TRUE,
      positive = 1
    ))$messages,
    c("Will cross-validate 1 models. This requires fitting 4 model instances.\n",
    "\n--------------------------\ncross_validate(): Message:\nIn model:\ndiagnosis~score+(1|session)\nFor fold column:\n.folds_1\nIn fold:\n1\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = c(\"bobyqa\", \"Nelder_Mead\"), restart_edge = FALSE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, \n    relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(), tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE)), model_verbose : TRUE, family : binomial, is_special_fn : TRUE\ncross_validate(): Used lme4::glmer() to fit the model.'\n",
    "\n--------------------------\ncross_validate(): Message:\nIn model:\ndiagnosis~score+(1|session)\nFor fold column:\n.folds_1\nIn fold:\n2\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = c(\"bobyqa\", \"Nelder_Mead\"), restart_edge = FALSE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, \n    relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(), tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE)), model_verbose : TRUE, family : binomial, is_special_fn : TRUE\ncross_validate(): Used lme4::glmer() to fit the model.'\n",
    "\n--------------------------\ncross_validate(): Message:\nIn model:\ndiagnosis~score+(1|session)\nFor fold column:\n.folds_1\nIn fold:\n3\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = c(\"bobyqa\", \"Nelder_Mead\"), restart_edge = FALSE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, \n    relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(), tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE)), model_verbose : TRUE, family : binomial, is_special_fn : TRUE\ncross_validate(): Used lme4::glmer() to fit the model.'\n",
    "\n--------------------------\ncross_validate(): Message:\nIn model:\ndiagnosis~score+(1|session)\nFor fold column:\n.folds_1\nIn fold:\n4\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = c(\"bobyqa\", \"Nelder_Mead\"), restart_edge = FALSE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, \n    relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list(), tolPwrss = 1e-07, compDev = TRUE, nAGQ0initStep = TRUE)), model_verbose : TRUE, family : binomial, is_special_fn : TRUE\ncross_validate(): Used lme4::glmer() to fit the model.'\n"
    ),
    fixed = TRUE
    )
  }

  # lm
  expect_equal(evaluate_promise(cross_validate(dat,
    formulas = c("score~diagnosis"),
    fold_cols = paste0(".folds_", 1), family = "gaussian",
    REML = FALSE, verbose = TRUE,
    positive = 1
  ))$messages,
  c("Will cross-validate 1 models. This requires fitting 4 model instances.\n",
  "\n--------------------------\ncross_validate(): Message:\nIn model:\nscore~diagnosis\nFor fold column:\n.folds_1\nIn fold:\n1\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"nloptwrap\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : gaussian, is_special_fn : TRUE\ncross_validate(): Used lm() to fit the model.'\n",
  "\n--------------------------\ncross_validate(): Message:\nIn model:\nscore~diagnosis\nFor fold column:\n.folds_1\nIn fold:\n2\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"nloptwrap\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : gaussian, is_special_fn : TRUE\ncross_validate(): Used lm() to fit the model.'\n",
  "\n--------------------------\ncross_validate(): Message:\nIn model:\nscore~diagnosis\nFor fold column:\n.folds_1\nIn fold:\n3\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"nloptwrap\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : gaussian, is_special_fn : TRUE\ncross_validate(): Used lm() to fit the model.'\n",
  "\n--------------------------\ncross_validate(): Message:\nIn model:\nscore~diagnosis\nFor fold column:\n.folds_1\nIn fold:\n4\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"nloptwrap\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : gaussian, is_special_fn : TRUE\ncross_validate(): Used lm() to fit the model.'\n"
  ),
  fixed = TRUE
  )

  # lmer
  expect_equal(
    evaluate_promise(cross_validate(dat,
      formulas = c("score~diagnosis+(1|session)"),
      fold_cols = paste0(".folds_", 1), family = "gaussian",
      REML = FALSE, verbose = TRUE,
      positive = 1
    ))$messages,
    c("Will cross-validate 1 models. This requires fitting 4 model instances.\n",
    "\n--------------------------\ncross_validate(): Message:\nIn model:\nscore~diagnosis+(1|session)\nFor fold column:\n.folds_1\nIn fold:\n1\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"nloptwrap\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : gaussian, is_special_fn : TRUE\ncross_validate(): Used lme4::lmer() to fit the model.'\n",
    "\n--------------------------\ncross_validate(): Message:\nIn model:\nscore~diagnosis+(1|session)\nFor fold column:\n.folds_1\nIn fold:\n2\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"nloptwrap\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : gaussian, is_special_fn : TRUE\ncross_validate(): Used lme4::lmer() to fit the model.'\n",
    "\n--------------------------\ncross_validate(): Message:\nIn model:\nscore~diagnosis+(1|session)\nFor fold column:\n.folds_1\nIn fold:\n3\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"nloptwrap\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : gaussian, is_special_fn : TRUE\ncross_validate(): Used lme4::lmer() to fit the model.'\n",
    "\n--------------------------\ncross_validate(): Message:\nIn model:\nscore~diagnosis+(1|session)\nFor fold column:\n.folds_1\nIn fold:\n4\nHyperparameters:\nREML : FALSE, control : list(list(optimizer = \"nloptwrap\", restart_edge = TRUE, boundary.tol = 1e-05, calc.derivs = TRUE, use.last.params = FALSE, checkControl = list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), checkConv = list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", \n    tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), optCtrl = list())), model_verbose : TRUE, family : gaussian, is_special_fn : TRUE\ncross_validate(): Used lme4::lmer() to fit the model.'\n"
    )
  )
})

test_that("binomial models with metrics list work with cross_validate()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  CVbinomlist <- cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", family = "binomial",
    REML = FALSE,
    metrics = list(
      "AUC" = FALSE,
      "Accuracy" = TRUE,
      "Prevalence" = FALSE,
      "AICc" = TRUE
    ),
    verbose = FALSE,
    positive = 1
  )

  expect_equal(
    colnames(CVbinomlist),
    c(
      "Fixed", "Balanced Accuracy", "Accuracy", "F1", "Sensitivity", "Specificity",
      "Pos Pred Value", "Neg Pred Value", "Lower CI", "Upper CI", "Kappa",
      "MCC", "Detection Rate", "Detection Prevalence", "AICc", "Predictions",
      "ROC", "Confusion Matrix", "Results", "Coefficients", "Folds",
      "Fold Columns", "Convergence Warnings", "Singular Fit Messages",
      "Other Warnings", "Warnings and Messages", "Family",
      "Dependent"
    )
  )

  expect_equal(CVbinomlist$Accuracy, c(0.766666666666667, 0.4), tolerance = 1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111, 0.3333333), tolerance = 1e-3)

  expect_error(cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", family = "binomial",
    REML = FALSE,
    metrics = list(
      "AKG" = FALSE, # error here
      "Accuracy" = TRUE,
      "Prevalencer" = FALSE
    ),
    verbose = FALSE,
    positive = 1
  ),
  "'metrics_list' contained unknown metric names: AKG, Prevalencer.",
  fixed = TRUE
  )
  expect_error(
    xpectr::strip_msg(cross_validate(dat,
    formulas = c("diagnosis~score", "diagnosis~age"),
    fold_cols = ".folds", family = "binomial",
    REML = FALSE,
    metrics = list(
      "AUC" = 1,
      "Accuracy" = TRUE,
      "Prevalence" = FALSE
    ),
    verbose = FALSE,
    positive = 1
  )),
  xpectr::strip(paste0(
    "1 assertions failed:\n * Variable 'metrics': May only conta",
    "in the following types: logical."
  )),
  fixed = TRUE
  )
})

test_that("gaussian models with metrics list work with cross_validate()", {

  # skip_test_if_old_R_version()

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  # Cross-validate the data
  CVed <- cross_validate(dat, "score~diagnosis",
    fold_cols = ".folds",
    family = "gaussian",
    REML = FALSE,
    metrics = list(
      "RMSE" = FALSE,
      "r2m" = TRUE
    ),
    verbose = FALSE
  )

  expect_equal(
    colnames(CVed),
    c("Fixed", "MAE", "NRMSE(IQR)", "RRSE", "RAE", "RMSLE", "r2m",
    "AIC", "AICc", "BIC", "Predictions", "Results", "Coefficients",
    "Folds", "Fold Columns", "Convergence Warnings", "Singular Fit Messages",
    "Other Warnings", "Warnings and Messages", "Family", "Dependent"
    )
  )
  expect_equal(
    colnames(CVed$Results[[1]]),
    c("Fold Column", "Fold", "MAE", "NRMSE(IQR)", "RRSE", "RAE",
    "RMSLE", "r2m", "AIC", "AICc", "BIC")
  )

  # Cross-validate the data
  expect_error(cross_validate(dat, "score~diagnosis",
    fold_cols = ".folds",
    family = "gaussian",
    REML = FALSE,
    metrics = list(
      "Accuracy" = TRUE, # Should error in gaussian
      "r2m" = TRUE
    ),
    verbose = FALSE
  ),
  "'metrics_list' contained unknown metric names: Accuracy.",
  fixed = TRUE
  )
})
