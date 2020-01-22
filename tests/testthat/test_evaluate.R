library(cvms)
context("evaluate()")

test_that("multinomial evaluations are correct in evaluate()", {
  xpectr::set_test_seed(1)
  random_probabilities <- multiclass_probability_tibble(
    num_classes = 5,
    num_observations = 20,
    apply_softmax = FALSE # Test with as well
  )
  expect_equal(sum(random_probabilities), 51.78471, tolerance = 1e-5)

  data_ <- random_probabilities %>%
    dplyr::mutate(
      cl = as.factor(rep(1:5, each = 4)),
      cl_char = paste0("cl_", cl)
    )

  expect_error(
    evaluate(
      data = data_,
      target_col = "cl",
      prediction_cols = paste0("class_", 1:5),
      type = "multinomial",
      apply_softmax = TRUE
    ),
    "Not all levels in 'target_col' was found in 'prediction_cols'.",
    fixed = T
  )

  data_ <- data_ %>%
    dplyr::rename_at(dplyr::vars(paste0("class_", 1:5)), .funs = ~ paste0("cl_", 1:5))

  mn_eval_1 <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multinomial",
    apply_softmax = TRUE,
    metrics = list("AUC" = TRUE)
  )

  # TODO Add more tests
  expect_equal(mn_eval_1$`Overall Accuracy`, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_1$`Balanced Accuracy`, 0.5, tolerance = 1e-4)
  expect_equal(
    mn_eval_1$`Balanced Accuracy`,
    mean(mn_eval_1$`Class Level Results`[[1]]$`Balanced Accuracy`)
  )
  expect_equal(mn_eval_1$F1, NaN)
  expect_equal(mn_eval_1$Sensitivity, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_1$Specificity, 0.8, tolerance = 1e-4)
  expect_equal(mn_eval_1$`Pos Pred Value`, 0.23, tolerance = 1e-4)
  expect_equal(mn_eval_1$`Neg Pred Value`, 0.7991667, tolerance = 1e-4)
  expect_equal(mn_eval_1$AUC, 0.49375)
  expect_equal(mn_eval_1$Kappa, 0.008653846, tolerance = 1e-4)
  expect_equal(mn_eval_1$MCC, 0.0125, tolerance = 1e-4)
  expect_equal(mn_eval_1$`Detection Rate`, 0.04, tolerance = 1e-4)
  expect_equal(mn_eval_1$`Detection Prevalence`, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_1$Prevalence, 0.2, tolerance = 1e-4)
  expect_equal(as.numeric(mn_eval_1$ROC[[1]]$auc),
    0.49375,
    tolerance = 1e-4
  )
  expect_equal(
    names(mn_eval_1$ROC[[1]]$rocs),
    c(
      "cl_1/cl_2", "cl_1/cl_3", "cl_1/cl_4", "cl_1/cl_5", "cl_2/cl_3",
      "cl_2/cl_4", "cl_2/cl_5", "cl_3/cl_4", "cl_3/cl_5", "cl_4/cl_5"
    )
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_1/cl_2`[[1]]$sensitivities,
    c(1, 0.75, 0.75, 0.5, 0.25, 0.25, 0.25, 0, 0)
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_1/cl_2`[[1]]$specificities,
    c(0, 0, 0.25, 0.25, 0.25, 0.5, 0.75, 0.75, 1)
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_1/cl_2`[[2]]$sensitivities,
    c(1, 0.75, 0.5, 0.5, 0.5, 0.5, 0.25, 0, 0)
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_1/cl_2`[[2]]$specificities,
    c(0, 0, 0, 0.25, 0.5, 0.75, 0.75, 0.75, 1)
  )

  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_4/cl_5`[[1]]$sensitivities,
    c(1, 0.75, 0.75, 0.5, 0.25, 0.25, 0, 0, 0)
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_4/cl_5`[[1]]$specificities,
    c(0, 0, 0.25, 0.25, 0.25, 0.5, 0.5, 0.75, 1)
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_4/cl_5`[[2]]$sensitivities,
    c(1, 0.75, 0.5, 0.25, 0.25, 0, 0, 0, 0)
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_4/cl_5`[[2]]$specificities,
    c(0, 0, 0, 0, 0.25, 0.25, 0.5, 0.75, 1)
  )

  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$Class,
    c("cl_1", "cl_2", "cl_3", "cl_4", "cl_5")
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$`Balanced Accuracy`,
    c(0.50000, 0.37500, 0.59375, 0.50000, 0.53125),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$F1,
    c(0.2222222, NaN, 0.3333333, 0.2222222, 0.2500000),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$Sensitivity,
    c(0.25, 0.0, 0.25, 0.25, 0.25),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$Specificity,
    c(0.7500, 0.7500, 0.9375, 0.7500, 0.8125),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$`Pos Pred Value`,
    c(0.20, 0.00, 0.50, 0.20, 0.25),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$`Neg Pred Value`,
    c(0.80, 0.750, 0.8333333, 0.80, 0.81250),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$Kappa,
    c(-3.172066e-16, -2.500000e-01, 2.307692e-01, -3.172066e-16, 6.250000e-02),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$MCC,
    c(0.0000, -0.2500, 0.2500, 0.0000, 0.0625),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$`Detection Rate`,
    c(0.05, 0.00, 0.05, 0.05, 0.05),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$`Detection Prevalence`,
    c(0.25, 0.20, 0.10, 0.25, 0.20),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$Prevalence,
    c(0.2, 0.2, 0.2, 0.2, 0.2),
    tolerance = 1e-4
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$Support,
    c(4, 4, 4, 4, 4)
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Prediction,
    as.character(c(0, 1, 0, 1))
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Target,
    as.character(c(0, 0, 1, 1))
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Pos_0,
    c("TP", "FN", "FP", "TN")
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Pos_1,
    c("TN", "FP", "FN", "TP")
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$N,
    c(12, 4, 3, 1)
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Prediction,
    as.character(c(0, 1, 0, 1))
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Target,
    as.character(c(0, 0, 1, 1))
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Pos_0,
    c("TP", "FN", "FP", "TN")
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Pos_1,
    c("TN", "FP", "FN", "TP")
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$N,
    c(12, 4, 4, 0)
  )
  expect_equal(
    colnames(mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]),
    c("Class", "Prediction", "Target", "Pos_0", "Pos_1", "N")
  )
  expect_equal(
    colnames(mn_eval_1$`Confusion Matrix`[[1]]),
    c("Prediction", "Target", "N")
  )


  # Test Weighted metrics, and metrics == "all"

  xpectr::set_test_seed(1)
  mn_eval_2 <- evaluate(
    data = data_ %>% dplyr::sample_n(17),
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multinomial",
    apply_softmax = TRUE,
    metrics = "all"
  )

  # Create manual weighted mean function
  manual_weighted_mean <- function(x, w) {
    sum(x * w) / sum(w)
  }

  # Test manual weighted mean function
  expect_equal(manual_weighted_mean(x = c(0.2, 0.8), w = c(1, 1)), 0.5, tolerance = 1e-4)
  expect_equal(manual_weighted_mean(x = c(0.3, 0.7), w = c(1, 1)), 0.5, tolerance = 1e-4)
  expect_equal(manual_weighted_mean(x = c(0.5, 0.5), w = c(1, 1)), 0.5, tolerance = 1e-4)
  expect_equal(manual_weighted_mean(x = c(0.2), w = c(4)), 0.2)
  expect_equal(manual_weighted_mean(x = c(0.2, 0.8), w = c(4, 1)), 0.32, tolerance = 1e-4)

  expect_equal(mn_eval_2$`Overall Accuracy`, 0.2352941, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Balanced Accuracy`, 0.5380586, tolerance = 1e-4)
  expect_equal(
    mn_eval_2$`Balanced Accuracy`,
    mean(mn_eval_2$`Class Level Results`[[1]]$`Balanced Accuracy`)
  )
  expect_equal(mn_eval_2$`Weighted Balanced Accuracy`, 0.5236264, tolerance = 1e-4)
  expect_equal(
    mn_eval_2$`Weighted Balanced Accuracy`,
    manual_weighted_mean(
      x = mn_eval_2$`Class Level Results`[[1]]$`Balanced Accuracy`,
      w = mn_eval_2$`Class Level Results`[[1]]$Support
    )
  )
  expect_equal(mn_eval_2$Accuracy, 0.6941176, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Weighted Accuracy`, 0.6851211, tolerance = 1e-4)
  expect_equal(mn_eval_2$F1, NaN)
  expect_equal(mn_eval_2$`Weighted F1`, NaN)
  expect_equal(mn_eval_2$Sensitivity, 0.2666667, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Weighted Sensitivity`, 0.2352941, tolerance = 1e-4)
  expect_equal(mn_eval_2$Specificity, 0.8094505, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Weighted Specificity`, 0.8119586, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Pos Pred Value`, 0.2666667, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Weighted Pos Pred Value`, 0.2696078, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Neg Pred Value`, 0.8094505, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Weighted Neg Pred Value`, 0.7939237, tolerance = 1e-4)
  expect_equal(mn_eval_2$AUC, 0.528125, tolerance = 1e-4)
  expect_equal(mn_eval_2$Kappa, 0.06428773, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Weighted Kappa`, 0.04481687, tolerance = 1e-4)
  expect_equal(mn_eval_2$MCC, 0.07240413, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Weighted MCC`, 0.05207999, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Detection Rate`, 0.04705882, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Weighted Detection Rate`, 0.0449827, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Detection Prevalence`, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Weighted Detection Prevalence`, 0.1937716, tolerance = 1e-4)
  expect_equal(mn_eval_2$Prevalence, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Weighted Prevalence`, 0.2110727, tolerance = 1e-4)

  expect_equal(as.numeric(mn_eval_2$ROC[[1]]$auc), 0.528125, tolerance = 1e-4)
  expect_equal(mn_eval_2$ROC[[1]]$rocs$`cl_1/cl_2`[[1]]$direction, ">", tolerance = 1e-4)
  expect_equal(mn_eval_2$ROC[[1]]$rocs$`cl_1/cl_2`[[1]]$sensitivities,
    c(1, 0.75, 0.75, 0.5, 0.25, 0.25, 0.25, 0, 0),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$ROC[[1]]$rocs$`cl_1/cl_2`[[1]]$specificities,
    c(0, 0, 0.25, 0.25, 0.25, 0.5, 0.75, 0.75, 1),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$ROC[[1]]$rocs$`cl_3/cl_4`[[1]]$direction, ">", tolerance = 1e-4)
  expect_equal(mn_eval_2$ROC[[1]]$rocs$`cl_3/cl_4`[[1]]$sensitivities,
    c(1, 1, 1, 1, 1, 0.666666666666667, 0.333333333333333, 0),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$ROC[[1]]$rocs$`cl_3/cl_4`[[1]]$specificities,
    c(0, 0.25, 0.5, 0.75, 1, 1, 1, 1),
    tolerance = 1e-4
  )

  expect_equal(
    mn_eval_2$`Class Level Results`[[1]]$Class,
    c("cl_1", "cl_2", "cl_3", "cl_4", "cl_5")
  )
  expect_equal(mn_eval_2$`Class Level Results`[[1]]$`Balanced Accuracy`,
    c(0.5480769, 0.3461538, 0.5865385, 0.5595238, 0.6500000),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$`Class Level Results`[[1]]$F1,
    c(0.2857143, NaN, 0.3333333, 0.2857143, 0.3333333),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$`Class Level Results`[[1]]$Sensitivity,
    c(0.250, 0.00, 0.250, 0.3333333, 0.500),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$`Class Level Results`[[1]]$Specificity,
    c(0.8461538, 0.6923077, 0.9230769, 0.7857143, 0.80),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$`Class Level Results`[[1]]$`Pos Pred Value`,
    c(0.3333333, 0.0, 0.50, 0.25, 0.25),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$`Class Level Results`[[1]]$`Neg Pred Value`,
    c(0.7857143, 0.6923077, 0.80, 0.8461538, 0.9230769),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$`Class Level Results`[[1]]$Kappa,
    c(0.1052632, -0.3076923, 0.2093023, 0.1052632, 0.2093023),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$`Class Level Results`[[1]]$MCC,
    c(0.1069901, -0.3076923, 0.2278664, 0.1069901, 0.2278664),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$`Class Level Results`[[1]]$`Detection Rate`,
    c(0.05882353, 0.00, 0.05882353, 0.05882353, 0.05882353),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$`Class Level Results`[[1]]$`Detection Prevalence`,
    c(0.1764706, 0.2352941, 0.1176471, 0.2352941, 0.2352941),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_2$`Class Level Results`[[1]]$Prevalence,
    c(0.2352941, 0.2352941, 0.2352941, 0.1764706, 0.1176471),
    tolerance = 1e-4
  )
  expect_equal(
    mn_eval_2$`Class Level Results`[[1]]$Support,
    c(4, 4, 4, 3, 2)
  )

  expect_equal(
    mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Prediction,
    as.character(c(0, 1, 0, 1))
  )
  expect_equal(
    mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Target,
    as.character(c(0, 0, 1, 1))
  )
  expect_equal(
    mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Pos_0,
    c("TP", "FN", "FP", "TN")
  )
  expect_equal(
    mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Pos_1,
    c("TN", "FP", "FN", "TP")
  )
  expect_equal(
    mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$N,
    c(11, 2, 3, 1)
  )
  expect_equal(
    mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Prediction,
    as.character(c(0, 1, 0, 1))
  )
  expect_equal(
    mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Target,
    as.character(c(0, 0, 1, 1))
  )
  expect_equal(
    mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Pos_0,
    c("TP", "FN", "FP", "TN")
  )
  expect_equal(
    mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Pos_1,
    c("TN", "FP", "FN", "TP")
  )
  expect_equal(
    mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$N,
    c(9, 4, 4, 0)
  )
  expect_equal(
    colnames(mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]),
    c("Class", "Prediction", "Target", "Pos_0", "Pos_1", "N")
  )
  expect_equal(
    colnames(mn_eval_2$`Confusion Matrix`[[1]]),
    c("Prediction", "Target", "N")
  )

  # Enabling and disabling a few metrics

  xpectr::set_test_seed(1)
  mn_eval_3 <- evaluate(
    data = data_ %>% dplyr::sample_n(17),
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multinomial",
    apply_softmax = TRUE,
    metrics = list("Accuracy" = TRUE, "Overall Accuracy" = FALSE, "F1" = FALSE)
  )

  expect_true("Accuracy" %in% colnames(mn_eval_3))
  expect_equal(mn_eval_3$Accuracy, 0.6941176, tolerance = 1e-4)
  expect_true("Accuracy" %in% colnames(mn_eval_3$`Class Level Results`[[1]]))
  expect_equal(mn_eval_3$`Class Level Results`[[1]]$Accuracy,
    c(0.7058824, 0.5294118, 0.7647059, 0.7058824, 0.7647059),
    tolerance = 1e-4
  )
  expect_true("Overall Accuracy" %ni% colnames(mn_eval_3))
  expect_true("F1" %ni% colnames(mn_eval_3))
  expect_true("F1" %ni% colnames(mn_eval_3$`Class Level Results`[[1]]))


  # TODO

  # ID level
  data_ <- data_ %>%
    dplyr::mutate(id = factor(rep(1:10, each = 2)))

  xpectr::set_test_seed(9)
  suppressWarnings(
    mn_id_eval_1 <- evaluate(
      data = data_ %>% dplyr::sample_n(13),
      target_col = "cl_char",
      prediction_cols = paste0("cl_", 1:5),
      id_col = "id",
      id_method = "mean",
      type = "multinomial",
      apply_softmax = TRUE,
      metrics = "all"
    )
  )

  expect_equal(mn_id_eval_1$`Overall Accuracy`, 0.222222, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Balanced Accuracy`, 0.5535714, tolerance = 1e-4)
  expect_equal(
    mn_id_eval_1$`Balanced Accuracy`,
    mean(mn_id_eval_1$`Class Level Results`[[1]]$`Balanced Accuracy`)
  )
  expect_equal(mn_id_eval_1$`Weighted Balanced Accuracy`, 0.5178571, tolerance = 1e-4)
  expect_equal(
    mn_id_eval_1$`Weighted Balanced Accuracy`,
    manual_weighted_mean(
      x = mn_id_eval_1$`Class Level Results`[[1]]$`Balanced Accuracy`,
      w = mn_id_eval_1$`Class Level Results`[[1]]$Support
    )
  )
  expect_equal(mn_id_eval_1$Accuracy, 0.688888889, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Weighted Accuracy`, 0.6790123, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$F1, NaN)
  expect_equal(mn_id_eval_1$`Weighted F1`, NaN)
  expect_equal(mn_id_eval_1$Sensitivity, 0.3, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Weighted Sensitivity`, 0.222222, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Specificity, 0.8071429, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Weighted Specificity`, 0.8134921, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Pos Pred Value`, NaN)
  expect_equal(mn_id_eval_1$`Weighted Pos Pred Value`, NaN)
  expect_equal(mn_id_eval_1$`Neg Pred Value`, 0.8126984, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Weighted Neg Pred Value`, 0.7918871, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$AUC, 0.6, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Kappa, 0.03714286, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Weighted Kappa`, -0.003174603, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$MCC, 0.05714286, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Weighted MCC`, 0.007936508, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Detection Rate`, 0.04444444, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Weighted Detection Rate`, 0.03703704, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Detection Prevalence`, 0.2, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Weighted Detection Prevalence`, 0.1851852, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Prevalence, 0.2, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Weighted Prevalence`, 0.2098765, tolerance = 1e-4)
  expect_equal(
    mn_id_eval_1$`Class Level Results`[[1]]$Class,
    c("cl_1", "cl_2", "cl_3", "cl_4", "cl_5")
  )
  expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$`Balanced Accuracy`,
    c(0.6785714, 0.3571429, 0.3571429, 0.8750000, 0.5000000),
    tolerance = 1e-4
  )
  expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$F1,
    c(0.5, NaN, NaN, 0.5, NA),
    tolerance = 1e-4
  )
  expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$Sensitivity,
    c(0.5, 0.0, 0.0, 1.0, 0.0),
    tolerance = 1e-4
  )
  expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$Specificity,
    c(0.8571429, 0.7142857, 0.7142857, 0.7500000, 1.0000000),
    tolerance = 1e-4
  )
  expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$`Pos Pred Value`,
    c(0.5000000, 0.0000000, 0.0000000, 0.3333333, NaN),
    tolerance = 1e-4
  )
  expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$`Neg Pred Value`,
    c(0.8571429, 0.7142857, 0.7142857, 1.0000000, 0.7777778),
    tolerance = 1e-4
  )
  expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$Kappa,
    c(0.3571429, -0.2857143, -0.2857143, 0.4000000, 0.0000000),
    tolerance = 1e-4
  )
  expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$MCC,
    c(0.3571429, -0.2857143, -0.2857143, 0.5000000, 0.0000000),
    tolerance = 1e-4
  )
  expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$`Detection Rate`,
    c(0.1111111, 0.0000000, 0.0000000, 0.1111111, 0.0000000),
    tolerance = 1e-4
  )
  expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$`Detection Prevalence`,
    c(0.2222222, 0.2222222, 0.2222222, 0.3333333, 0.0000000),
    tolerance = 1e-4
  )
  expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$Prevalence,
    c(0.2222222, 0.2222222, 0.2222222, 0.1111111, 0.2222222),
    tolerance = 1e-4
  )
  expect_equal(
    mn_id_eval_1$`Class Level Results`[[1]]$Support,
    c(2, 2, 2, 1, 2)
  )
  expect_equal(
    mn_id_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Prediction,
    c("0", "1", "0", "1")
  )
  expect_equal(
    mn_id_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Target,
    c("0", "0", "1", "1")
  )
  expect_equal(
    mn_id_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Pos_0,
    c("TP", "FN", "FP", "TN")
  )
  expect_equal(
    mn_id_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Pos_1,
    c("TN", "FP", "FN", "TP")
  )
  expect_equal(
    mn_id_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$N,
    c(6, 1, 1, 1)
  )
  expect_equal(
    colnames(mn_id_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]),
    c("Class", "Prediction", "Target", "Pos_0", "Pos_1", "N")
  )
  expect_equal(
    colnames(mn_id_eval_1$`Confusion Matrix`[[1]]),
    c("Prediction", "Target", "N")
  )

  preds <- legacy_unnest(mn_id_eval_1$Predictions[[1]])
  expect_equal(
    preds$Target,
    c(
      "cl_1", "cl_1", "cl_2", "cl_2", "cl_3", "cl_3", "cl_4", "cl_5",
      "cl_5"
    )
  )
  expect_equal(
    preds$`Predicted Class`,
    c(
      "cl_3", "cl_1", "cl_3", "cl_4", "cl_2", "cl_2", "cl_4", "cl_1",
      "cl_4"
    )
  )
  expect_equal(
    preds$id,
    structure(
      c(1L, 2L, 3L, 4L, 5L, 6L, 8L, 9L, 10L),
      .Label = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
      class = "factor"
    )
  )
  expect_equal(preds$id_method, rep("mean", 9))
  expect_equal(
    preds$cl_1,
    c(
      0.155671811050252, 0.248320604698989, 0.210232666790023, 0.233435859695667,
      0.215424349733198, 0.172214912622303, 0.175725517586139, 0.318193346232363,
      0.184499511821644
    )
  )
  expect_equal(
    preds$cl_2,
    c(
      0.200809683386583, 0.174653590011229, 0.16814831665041, 0.176707798618296,
      0.274016028927649, 0.226977008772144, 0.208448230323408, 0.131459226131536,
      0.182435977816156
    )
  )
  expect_equal(
    preds$cl_3,
    c(
      0.235788915387888, 0.230943855788057, 0.234561297999098, 0.194287616631672,
      0.238843732067503, 0.225966833951102, 0.118000650430999, 0.198221952164019,
      0.176500595260772
    )
  )
  expect_equal(
    preds$cl_4,
    c(
      0.206887130859327, 0.175900816874267, 0.191066716938011, 0.259412699196439,
      0.124928637348059, 0.196731833826249, 0.260713554487017, 0.174293847520026,
      0.246626726585871
    )
  )
  expect_equal(
    preds$cl_5,
    c(
      0.20084245931595, 0.170181132627457, 0.195991001622458, 0.136156025857926,
      0.146787251923591, 0.178109410828203, 0.237112047172437, 0.177831627952058,
      0.209937188515557
    )
  )



  # Test grouping vars
  data_2 <- data_ %>%
    dplyr::mutate(fold_ = 1) %>%
    dplyr::bind_rows(data_ %>% dplyr::mutate(fold_ = 2))

  mn_id_eval_2 <- evaluate(
    data = data_2 %>% dplyr::group_by(fold_),
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    id_col = "id",
    id_method = "majority",
    type = "multinomial",
    apply_softmax = TRUE
  )

  expect_equal(mn_id_eval_2$fold_, c(1, 2))
  expect_equal(dplyr::bind_rows(mn_id_eval_2$`Class Level Results`)$fold_, rep(1:2, each = 5))
  expect_equal(
    colnames(mn_id_eval_2),
    c(
      "fold_", "Overall Accuracy", "Balanced Accuracy", "F1", "Sensitivity",
      "Specificity", "Pos Pred Value", "Neg Pred Value",
      "Kappa", "MCC", "Detection Rate", "Detection Prevalence",
      "Prevalence", "Predictions", "Confusion Matrix", "Class Level Results"
    )
  )
  expect_equal(
    colnames(mn_id_eval_2$`Class Level Results`[[1]]),
    c(
      "fold_", "Class", "Balanced Accuracy", "F1", "Sensitivity",
      "Specificity", "Pos Pred Value", "Neg Pred Value",
      "Kappa", "MCC", "Detection Rate", "Detection Prevalence",
      "Prevalence", "Support", "Confusion Matrix"
    )
  )
  expect_equal(
    mn_id_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]][, -1], # remove fold_
    mn_id_eval_2$`Class Level Results`[[2]]$`Confusion Matrix`[[1]][, -1]
  )
  expect_equal(
    colnames(mn_id_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]),
    c("fold_", "Class", "Prediction", "Target", "Pos_0", "Pos_1", "N")
  )


  # What happens when a class is not in the targets but has a probability column?

  data_3 <- random_probabilities %>%
    dplyr::mutate(
      cl = as.factor(rep(1:4, each = 5)),
      cl_char = paste0("cl_", cl)
    ) %>%
    dplyr::rename_at(dplyr::vars(paste0("class_", 1:5)), .funs = ~ paste0("cl_", 1:5))

  # Testing multinomial
  expect_warning(mb_eval <- evaluate(
    data = data_3,
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    apply_softmax = TRUE,
    type = "multinomial",
    metrics = list("AUC" = TRUE)
  ), "The following classes were not found in 'response': cl_5.",
  fixed = TRUE
  )

  expect_equal(mb_eval$`Overall Accuracy`, 0.3)
  expect_equal(mb_eval$`Balanced Accuracy`, NaN)
  expect_equal(mb_eval$F1, NaN)
  expect_equal(mb_eval$Sensitivity, NaN)
  expect_equal(mb_eval$Specificity, 0.8266667, tolerance = 1e-4)
  expect_equal(mb_eval$`Pos Pred Value`, 0.31, tolerance = 1e-4)
  expect_equal(mb_eval$`Neg Pred Value`, 0.825555555555556, tolerance = 1e-4)
  expect_equal(mb_eval$AUC, 0.49, tolerance = 1e-4)
  expect_equal(mb_eval$Kappa, 0.1133333, tolerance = 1e-4)
  expect_equal(mb_eval$MCC, 0.11849, tolerance = 1e-4)
  expect_equal(mb_eval$`Detection Rate`, 0.06, tolerance = 1e-4)
  expect_equal(mb_eval$`Detection Prevalence`, 0.2, tolerance = 1e-4)
  expect_equal(mb_eval$Prevalence, 0.2, tolerance = 1e-4)
  expect_true("cl_5" %ni% mb_eval$Predictions[[1]]$Target)
  expect_equal(
    mb_eval$`Confusion Matrix`[[1]]$Target,
    rep(paste0("cl_", 1:5), each = 5)
  )
  expect_equal(
    mb_eval$`Confusion Matrix`[[1]]$Prediction,
    rep(paste0("cl_", 1:5), 5)
  )
  expect_equal(
    mb_eval$`Confusion Matrix`[[1]]$N,
    c(
      1L, 1L, 1L, 0L, 2L, 2L, 1L, 0L, 2L, 0L, 1L, 2L, 1L, 0L, 1L,
      1L, 0L, 0L, 3L, 1L, 0L, 0L, 0L, 0L, 0L
    )
  )


  expect_equal(
    mb_eval$`Class Level Results`[[1]]$Class,
    c("cl_1", "cl_2", "cl_3", "cl_4", "cl_5")
  )
  expect_equal(
    mb_eval$`Class Level Results`[[1]]$`Balanced Accuracy`,
    c(
      0.466666666666667, 0.5, 0.566666666666667,
      0.733333333333333, NaN
    )
  )
  expect_equal(
    mb_eval$`Class Level Results`[[1]]$F1,
    c(0.2, 0.222222222222222, 0.285714285714286, 0.6, NaN)
  )
  expect_equal(
    mb_eval$`Class Level Results`[[1]]$Sensitivity,
    c(0.2, 0.2, 0.2, 0.6, NA)
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$Specificity,
    c(
      0.733333333333333, 0.8, 0.933333333333333,
      0.866666666666667, 0.8
    ),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Pos Pred Value`,
    c(0.2, 0.25, 0.5, 0.6, 0),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Neg Pred Value`,
    c(
      0.733333333333333, 0.75, 0.777777777777778,
      0.866666666666667, 1
    ),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$Kappa,
    c(
      -0.0666666666666667, -3.17206578464331e-16, 0.166666666666666,
      0.466666666666667, 0
    ),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$MCC,
    c(
      -0.0666666666666667, 0, 0.192450089729875,
      0.466666666666667, 0
    ),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Detection Rate`,
    c(0.05, 0.05, 0.05, 0.15, 0),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Detection Prevalence`,
    c(0.25, 0.2, 0.1, 0.25, 0.2),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$Prevalence,
    c(0.25, 0.25, 0.25, 0.25, 0),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$Support,
    c(5L, 5L, 5L, 5L, NaN),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$N,
    c(11, 4, 4, 1),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Confusion Matrix`[[5]]$N,
    c(16, 4, 0, 0),
    tolerance = 1e-4
  )



  # TODO test that group_by and evaluate work correctly together
})

test_that("multinomial evaluations with one predicted class column is correctly unpacked in evaluate()", {
  xpectr::set_test_seed(1)
  random_probabilities <- multiclass_probability_tibble(
    num_classes = 5,
    num_observations = 20,
    apply_softmax = FALSE # Test with as well
  )
  expect_equal(sum(random_probabilities), 51.78471, tolerance = 1e-5)

  random_classes <- argmax(random_probabilities)

  data_ <- random_probabilities %>%
    dplyr::mutate(
      cl = as.factor(rep(1:5, each = 4)),
      cl_char = paste0("cl_", cl),
      pred_cl = random_classes
    )

  data_ <- data_ %>%
    dplyr::rename_at(dplyr::vars(paste0("class_", 1:5)), .funs = ~ paste0("cl_", 1:5)) %>%
    dplyr::mutate(pred_cl_char = paste0("cl_", pred_cl))

  data_classes <- data_ %>%
    base_select(c("cl", "cl_char", "pred_cl", "pred_cl_char"))

  mn_eval_1 <- evaluate(
    data = data_classes,
    target_col = "cl_char",
    prediction_cols = "pred_cl_char",
    type = "multinomial",
    apply_softmax = FALSE,
    metrics = list("AUC" = TRUE)
  )

  # TODO Add more tests
  expect_equal(mn_eval_1$`Overall Accuracy`, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_1$`Balanced Accuracy`, 0.5, tolerance = 1e-4)
  expect_equal(
    mn_eval_1$`Balanced Accuracy`,
    mean(mn_eval_1$`Class Level Results`[[1]]$`Balanced Accuracy`)
  )
  expect_equal(mn_eval_1$F1, NaN)
  expect_equal(mn_eval_1$Sensitivity, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_1$Specificity, 0.8, tolerance = 1e-4)
  expect_equal(mn_eval_1$`Pos Pred Value`, 0.23, tolerance = 1e-4)
  expect_equal(mn_eval_1$`Neg Pred Value`, 0.7991667, tolerance = 1e-4)
  expect_equal(mn_eval_1$AUC, 0.5)
  expect_equal(mn_eval_1$Kappa, 0.008653846, tolerance = 1e-4)
  expect_equal(mn_eval_1$MCC, 0.0125, tolerance = 1e-4)
  expect_equal(mn_eval_1$`Detection Rate`, 0.04, tolerance = 1e-4)
  expect_equal(mn_eval_1$`Detection Prevalence`, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_1$Prevalence, 0.2, tolerance = 1e-4)
  expect_equal(as.numeric(mn_eval_1$ROC[[1]]$auc),
    0.5,
    tolerance = 1e-4
  )
  expect_equal(
    names(mn_eval_1$ROC[[1]]$rocs),
    c(
      "cl_1/cl_2", "cl_1/cl_3", "cl_1/cl_4", "cl_1/cl_5", "cl_2/cl_3",
      "cl_2/cl_4", "cl_2/cl_5", "cl_3/cl_4", "cl_3/cl_5", "cl_4/cl_5"
    )
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_1/cl_2`[[1]]$sensitivities,
    c(1, 0.5, 0)
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_1/cl_2`[[1]]$specificities,
    c(0, 0.25, 1)
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_1/cl_2`[[2]]$sensitivities,
    c(1, 0.75, 0)
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_1/cl_2`[[2]]$specificities,
    c(0, 0, 1)
  )

  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_4/cl_5`[[1]]$sensitivities,
    c(1, 0.5, 0)
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_4/cl_5`[[1]]$specificities,
    c(0, 0.25, 1)
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_4/cl_5`[[2]]$sensitivities,
    c(1, 0.75, 0)
  )
  expect_equal(
    mn_eval_1$ROC[[1]]$rocs$`cl_4/cl_5`[[2]]$specificities,
    c(0, 0.25, 1)
  )

  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$Class,
    c("cl_1", "cl_2", "cl_3", "cl_4", "cl_5")
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$`Balanced Accuracy`,
    c(0.50000, 0.37500, 0.59375, 0.50000, 0.53125),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$F1,
    c(0.2222222, NaN, 0.3333333, 0.2222222, 0.2500000),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$Sensitivity,
    c(0.25, 0.0, 0.25, 0.25, 0.25),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$Specificity,
    c(0.7500, 0.7500, 0.9375, 0.7500, 0.8125),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$`Pos Pred Value`,
    c(0.20, 0.00, 0.50, 0.20, 0.25),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$`Neg Pred Value`,
    c(0.80, 0.750, 0.8333333, 0.80, 0.81250),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$Kappa,
    c(-3.172066e-16, -2.500000e-01, 2.307692e-01, -3.172066e-16, 6.250000e-02),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$MCC,
    c(0.0000, -0.2500, 0.2500, 0.0000, 0.0625),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$`Detection Rate`,
    c(0.05, 0.00, 0.05, 0.05, 0.05),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$`Detection Prevalence`,
    c(0.25, 0.20, 0.10, 0.25, 0.20),
    tolerance = 1e-4
  )
  expect_equal(mn_eval_1$`Class Level Results`[[1]]$Prevalence,
    c(0.2, 0.2, 0.2, 0.2, 0.2),
    tolerance = 1e-4
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$Support,
    c(4, 4, 4, 4, 4)
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Prediction,
    as.character(c(0, 1, 0, 1))
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Target,
    as.character(c(0, 0, 1, 1))
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Pos_0,
    c("TP", "FN", "FP", "TN")
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Pos_1,
    c("TN", "FP", "FN", "TP")
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$N,
    c(12, 4, 3, 1)
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Prediction,
    as.character(c(0, 1, 0, 1))
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Target,
    as.character(c(0, 0, 1, 1))
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Pos_0,
    c("TP", "FN", "FP", "TN")
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Pos_1,
    c("TN", "FP", "FN", "TP")
  )
  expect_equal(
    mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$N,
    c(12, 4, 4, 0)
  )
  expect_equal(
    colnames(mn_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]),
    c("Class", "Prediction", "Target", "Pos_0", "Pos_1", "N")
  )
  expect_equal(
    colnames(mn_eval_1$`Confusion Matrix`[[1]]),
    c("Prediction", "Target", "N")
  )


  # Test Weighted metrics, and metrics == "all"
  # Sampled data frame
  if (TRUE) {
    xpectr::set_test_seed(1)
    mn_eval_2 <- evaluate(
      data = data_classes %>% dplyr::sample_n(17),
      target_col = "cl_char",
      prediction_cols = "pred_cl_char",
      type = "multinomial",
      apply_softmax = FALSE,
      metrics = "all"
    )

    # Create manual weighted mean function
    manual_weighted_mean <- function(x, w) {
      sum(x * w) / sum(w)
    }

    # Test manual weighted mean function
    expect_equal(manual_weighted_mean(x = c(0.2, 0.8), w = c(1, 1)), 0.5, tolerance = 1e-4)
    expect_equal(manual_weighted_mean(x = c(0.3, 0.7), w = c(1, 1)), 0.5, tolerance = 1e-4)
    expect_equal(manual_weighted_mean(x = c(0.5, 0.5), w = c(1, 1)), 0.5, tolerance = 1e-4)
    expect_equal(manual_weighted_mean(x = c(0.2), w = c(4)), 0.2)
    expect_equal(manual_weighted_mean(x = c(0.2, 0.8), w = c(4, 1)), 0.32, tolerance = 1e-4)

    expect_equal(mn_eval_2$`Overall Accuracy`, 0.2352941, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Balanced Accuracy`, 0.5380586, tolerance = 1e-4)
    expect_equal(
      mn_eval_2$`Balanced Accuracy`,
      mean(mn_eval_2$`Class Level Results`[[1]]$`Balanced Accuracy`)
    )
    expect_equal(mn_eval_2$`Weighted Balanced Accuracy`, 0.5236264, tolerance = 1e-4)
    expect_equal(
      mn_eval_2$`Weighted Balanced Accuracy`,
      manual_weighted_mean(
        x = mn_eval_2$`Class Level Results`[[1]]$`Balanced Accuracy`,
        w = mn_eval_2$`Class Level Results`[[1]]$Support
      )
    )
    expect_equal(mn_eval_2$Accuracy, 0.6941176, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Weighted Accuracy`, 0.6851211, tolerance = 1e-4)
    expect_equal(mn_eval_2$F1, NaN)
    expect_equal(mn_eval_2$`Weighted F1`, NaN)
    expect_equal(mn_eval_2$Sensitivity, 0.2666667, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Weighted Sensitivity`, 0.2352941, tolerance = 1e-4)
    expect_equal(mn_eval_2$Specificity, 0.8094505, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Weighted Specificity`, 0.8119586, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Pos Pred Value`, 0.2666667, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Weighted Pos Pred Value`, 0.2696078, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Neg Pred Value`, 0.8094505, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Weighted Neg Pred Value`, 0.7939237, tolerance = 1e-4)
    expect_equal(mn_eval_2$AUC, 0.5416667, tolerance = 1e-4)
    expect_equal(mn_eval_2$Kappa, 0.06428773, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Weighted Kappa`, 0.04481687, tolerance = 1e-4)
    expect_equal(mn_eval_2$MCC, 0.07240413, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Weighted MCC`, 0.05207999, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Detection Rate`, 0.04705882, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Weighted Detection Rate`, 0.0449827, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Detection Prevalence`, 0.2, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Weighted Detection Prevalence`, 0.1937716, tolerance = 1e-4)
    expect_equal(mn_eval_2$Prevalence, 0.2, tolerance = 1e-4)
    expect_equal(mn_eval_2$`Weighted Prevalence`, 0.2110727, tolerance = 1e-4)

    expect_equal(as.numeric(mn_eval_2$ROC[[1]]$auc), 0.5416667, tolerance = 1e-4)
    expect_equal(mn_eval_2$ROC[[1]]$rocs$`cl_3/cl_4`[[1]]$direction, ">", tolerance = 1e-4)
    expect_equal(mn_eval_2$ROC[[1]]$rocs$`cl_3/cl_4`[[1]]$sensitivities,
      c(1, 1, 0),
      tolerance = 1e-4
    )
    expect_equal(mn_eval_2$ROC[[1]]$rocs$`cl_3/cl_4`[[1]]$specificities,
      c(0.00, 0.25, 1.00),
      tolerance = 1e-4
    )

    expect_equal(
      mn_eval_2$`Class Level Results`[[1]]$Class,
      c("cl_1", "cl_2", "cl_3", "cl_4", "cl_5")
    )
    expect_equal(mn_eval_2$`Class Level Results`[[1]]$`Balanced Accuracy`,
      c(0.5480769, 0.3461538, 0.5865385, 0.5595238, 0.6500000),
      tolerance = 1e-4
    )
    expect_equal(mn_eval_2$`Class Level Results`[[1]]$F1,
      c(0.2857143, NaN, 0.3333333, 0.2857143, 0.3333333),
      tolerance = 1e-4
    )
    expect_equal(mn_eval_2$`Class Level Results`[[1]]$Sensitivity,
      c(0.250, 0.00, 0.250, 0.3333333, 0.500),
      tolerance = 1e-4
    )
    expect_equal(mn_eval_2$`Class Level Results`[[1]]$Specificity,
      c(0.8461538, 0.6923077, 0.9230769, 0.7857143, 0.80),
      tolerance = 1e-4
    )
    expect_equal(mn_eval_2$`Class Level Results`[[1]]$`Pos Pred Value`,
      c(0.3333333, 0.0, 0.50, 0.25, 0.25),
      tolerance = 1e-4
    )
    expect_equal(mn_eval_2$`Class Level Results`[[1]]$`Neg Pred Value`,
      c(0.7857143, 0.6923077, 0.80, 0.8461538, 0.9230769),
      tolerance = 1e-4
    )
    expect_equal(mn_eval_2$`Class Level Results`[[1]]$Kappa,
      c(0.1052632, -0.3076923, 0.2093023, 0.1052632, 0.2093023),
      tolerance = 1e-4
    )
    expect_equal(mn_eval_2$`Class Level Results`[[1]]$MCC,
      c(0.1069901, -0.3076923, 0.2278664, 0.1069901, 0.2278664),
      tolerance = 1e-4
    )
    expect_equal(mn_eval_2$`Class Level Results`[[1]]$`Detection Rate`,
      c(0.05882353, 0.00, 0.05882353, 0.05882353, 0.05882353),
      tolerance = 1e-4
    )
    expect_equal(mn_eval_2$`Class Level Results`[[1]]$`Detection Prevalence`,
      c(0.1764706, 0.2352941, 0.1176471, 0.2352941, 0.2352941),
      tolerance = 1e-4
    )
    expect_equal(mn_eval_2$`Class Level Results`[[1]]$Prevalence,
      c(0.2352941, 0.2352941, 0.2352941, 0.1764706, 0.1176471),
      tolerance = 1e-4
    )
    expect_equal(
      mn_eval_2$`Class Level Results`[[1]]$Support,
      c(4, 4, 4, 3, 2)
    )

    expect_equal(
      mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Prediction,
      as.character(c(0, 1, 0, 1))
    )
    expect_equal(
      mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Target,
      as.character(c(0, 0, 1, 1))
    )
    expect_equal(
      mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Pos_0,
      c("TP", "FN", "FP", "TN")
    )
    expect_equal(
      mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Pos_1,
      c("TN", "FP", "FN", "TP")
    )
    expect_equal(
      mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$N,
      c(11, 2, 3, 1)
    )
    expect_equal(
      mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Prediction,
      as.character(c(0, 1, 0, 1))
    )
    expect_equal(
      mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Target,
      as.character(c(0, 0, 1, 1))
    )
    expect_equal(
      mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Pos_0,
      c("TP", "FN", "FP", "TN")
    )
    expect_equal(
      mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$Pos_1,
      c("TN", "FP", "FN", "TP")
    )
    expect_equal(
      mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[2]]$N,
      c(9, 4, 4, 0)
    )
    expect_equal(
      colnames(mn_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]),
      c("Class", "Prediction", "Target", "Pos_0", "Pos_1", "N")
    )
    expect_equal(
      colnames(mn_eval_2$`Confusion Matrix`[[1]]),
      c("Prediction", "Target", "N")
    )
  }

  # Enabling and disabling a few metrics

  # TODO

  # ID level
  if (TRUE) {

    # ID level
    data_classes <- data_classes %>%
      dplyr::mutate(id = factor(rep(1:10, each = 2)))

    xpectr::set_test_seed(9)
    suppressWarnings(
      mn_id_eval_1 <- evaluate(
        data = data_classes %>% dplyr::sample_n(13),
        target_col = "cl_char",
        prediction_cols = "pred_cl_char",
        id_col = "id",
        id_method = "mean",
        type = "multinomial",
        apply_softmax = FALSE,
        metrics = "all"
      )
    )

    expect_equal(mn_id_eval_1$`Overall Accuracy`, 0.222222, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Balanced Accuracy`, 0.5535714, tolerance = 1e-4)
    expect_equal(
      mn_id_eval_1$`Balanced Accuracy`,
      mean(mn_id_eval_1$`Class Level Results`[[1]]$`Balanced Accuracy`)
    )
    expect_equal(mn_id_eval_1$`Weighted Balanced Accuracy`, 0.5178571, tolerance = 1e-4)
    expect_equal(
      mn_id_eval_1$`Weighted Balanced Accuracy`,
      manual_weighted_mean(
        x = mn_id_eval_1$`Class Level Results`[[1]]$`Balanced Accuracy`,
        w = mn_id_eval_1$`Class Level Results`[[1]]$Support
      )
    )
    expect_equal(mn_id_eval_1$Accuracy, 0.688888889, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Weighted Accuracy`, 0.6790123, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$F1, NaN)
    expect_equal(mn_id_eval_1$`Weighted F1`, NaN)
    expect_equal(mn_id_eval_1$Sensitivity, 0.3, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Weighted Sensitivity`, 0.222222, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$Specificity, 0.8071429, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Weighted Specificity`, 0.8134921, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Pos Pred Value`, NaN)
    expect_equal(mn_id_eval_1$`Weighted Pos Pred Value`, NaN)
    expect_equal(mn_id_eval_1$`Neg Pred Value`, 0.81111, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Weighted Neg Pred Value`, 0.7901235, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$AUC, 0.575, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$Kappa, 0.043636, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Weighted Kappa`, 0.00404, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$MCC, 0.06220355, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Weighted MCC`, 0.0135595, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Detection Rate`, 0.04444444, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Weighted Detection Rate`, 0.03703704, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Detection Prevalence`, 0.2, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Weighted Detection Prevalence`, 0.1851852, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$Prevalence, 0.2, tolerance = 1e-4)
    expect_equal(mn_id_eval_1$`Weighted Prevalence`, 0.2098765, tolerance = 1e-4)
    expect_equal(
      mn_id_eval_1$`Class Level Results`[[1]]$Class,
      c("cl_1", "cl_2", "cl_3", "cl_4", "cl_5")
    )
    expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$`Balanced Accuracy`,
      c(0.607142857142857, 0.285714285714286, 0.5, 0.875, 0.5),
      tolerance = 1e-4
    )
    expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$F1,
      c(0.4, NaN, NaN, 0.5, NA),
      tolerance = 1e-4
    )
    expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$Sensitivity,
      c(0.5, 0.0, 0.0, 1.0, 0.0),
      tolerance = 1e-4
    )
    expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$Specificity,
      c(0.714285714285714, 0.571428571428571, 1, 0.75, 1),
      tolerance = 1e-4
    )
    expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$`Pos Pred Value`,
      c(0.333333333333333, 0, NaN, 0.333333333333333, NaN),
      tolerance = 1e-4
    )
    expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$`Neg Pred Value`,
      c(
        0.833333333333333, 0.666666666666667, 0.777777777777778, 1,
        0.777777777777778
      ),
      tolerance = 1e-4
    )
    expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$Kappa,
      c(0.181818181818182, -0.363636363636363, 0, 0.4, 0),
      tolerance = 1e-4
    )
    expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$MCC,
      c(0.1889822, -0.377964, 0, 0.5, 0),
      tolerance = 1e-4
    )
    expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$`Detection Rate`,
      c(0.1111111, 0.0000000, 0.0000000, 0.1111111, 0.0000000),
      tolerance = 1e-4
    )
    expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$`Detection Prevalence`,
      c(0.3333333, 0.3333333, 0, 0.3333333, 0),
      tolerance = 1e-4
    )
    expect_equal(mn_id_eval_1$`Class Level Results`[[1]]$Prevalence,
      c(0.2222222, 0.2222222, 0.2222222, 0.1111111, 0.2222222),
      tolerance = 1e-4
    )
    expect_equal(
      mn_id_eval_1$`Class Level Results`[[1]]$Support,
      c(2, 2, 2, 1, 2)
    )
    expect_equal(
      mn_id_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Prediction,
      c("0", "1", "0", "1")
    )
    expect_equal(
      mn_id_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Target,
      c("0", "0", "1", "1")
    )
    expect_equal(
      mn_id_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Pos_0,
      c("TP", "FN", "FP", "TN")
    )
    expect_equal(
      mn_id_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$Pos_1,
      c("TN", "FP", "FN", "TP")
    )
    expect_equal(
      mn_id_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$N,
      c(5, 2, 1, 1)
    )
    expect_equal(
      colnames(mn_id_eval_1$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]),
      c("Class", "Prediction", "Target", "Pos_0", "Pos_1", "N")
    )
    expect_equal(
      colnames(mn_id_eval_1$`Confusion Matrix`[[1]]),
      c("Prediction", "Target", "N")
    )

    preds <- legacy_unnest(mn_id_eval_1$Predictions[[1]])
    expect_equal(
      preds$Target,
      c(
        "cl_1", "cl_1", "cl_2", "cl_2", "cl_3", "cl_3", "cl_4", "cl_5",
        "cl_5"
      )
    )
    expect_equal(
      preds$`Predicted Class`,
      c(
        "cl_2", "cl_1", "cl_1", "cl_4", "cl_2", "cl_2", "cl_4", "cl_1",
        "cl_4"
      )
    )
    expect_equal(
      preds$id,
      structure(
        c(1L, 2L, 3L, 4L, 5L, 6L, 8L, 9L, 10L),
        .Label = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
        class = "factor"
      )
    )
    expect_equal(preds$id_method, rep("mean", 9))
    expect_equal(
      preds$cl_1,
      c(
        0.158794620741017, 0.261808068888474, 0.261808068888474, 0.148847581202078,
        0.148847581202078, 0.148847581202078, 0.148847581202078, 0.40460967519169,
        0.158794620741017
      )
    )
    expect_equal(
      preds$cl_2,
      c(
        0.261808068888474, 0.158794620741017, 0.158794620741017, 0.148847581202078,
        0.40460967519169, 0.40460967519169, 0.148847581202078, 0.148847581202078,
        0.158794620741017
      )
    )
    expect_equal(
      preds$cl_3,
      c(
        0.158794620741017, 0.261808068888474, 0.158794620741017, 0.148847581202078,
        0.148847581202078, 0.148847581202078, 0.148847581202078, 0.148847581202078,
        0.158794620741017
      )
    )
    expect_equal(
      preds$cl_4,
      c(
        0.158794620741017, 0.158794620741017, 0.158794620741017, 0.40460967519169,
        0.148847581202078, 0.148847581202078, 0.40460967519169, 0.148847581202078,
        0.261808068888474
      )
    )
    expect_equal(
      preds$cl_5,
      c(
        0.261808068888474, 0.158794620741017, 0.261808068888474, 0.148847581202078,
        0.148847581202078, 0.148847581202078, 0.148847581202078, 0.148847581202078,
        0.261808068888474
      )
    )
  }

  # Test grouping vars
  data_2 <- data_classes %>%
    dplyr::mutate(fold_ = 1) %>%
    dplyr::bind_rows(data_classes %>% dplyr::mutate(fold_ = 2))

  mn_id_eval_2 <- evaluate(
    data = data_2 %>% dplyr::group_by(fold_),
    target_col = "cl_char",
    prediction_cols = "pred_cl_char",
    id_col = "id",
    id_method = "majority",
    type = "multinomial",
    apply_softmax = FALSE
  )

  expect_equal(mn_id_eval_2$fold_, c(1, 2))
  expect_equal(dplyr::bind_rows(mn_id_eval_2$`Class Level Results`)$fold_, rep(1:2, each = 5))
  expect_equal(
    colnames(mn_id_eval_2),
    c(
      "fold_", "Overall Accuracy", "Balanced Accuracy", "F1", "Sensitivity",
      "Specificity", "Pos Pred Value", "Neg Pred Value",
      "Kappa", "MCC", "Detection Rate", "Detection Prevalence",
      "Prevalence", "Predictions", "Confusion Matrix", "Class Level Results"
    )
  )
  expect_equal(
    colnames(mn_id_eval_2$`Class Level Results`[[1]]),
    c(
      "fold_", "Class", "Balanced Accuracy", "F1", "Sensitivity",
      "Specificity", "Pos Pred Value", "Neg Pred Value",
      "Kappa", "MCC", "Detection Rate", "Detection Prevalence",
      "Prevalence", "Support", "Confusion Matrix"
    )
  )
  expect_equal(
    mn_id_eval_2$`Class Level Results`[[1]]$`Confusion Matrix`[[1]][, -1],
    mn_id_eval_2$`Class Level Results`[[2]]$`Confusion Matrix`[[1]][, -1]
  )

  # What happens when a class is not in the targets but has a probability column?

  data_3 <- data_classes %>%
    dplyr::mutate(
      cl = as.factor(rep(1:4, each = 5)),
      cl_char = paste0("cl_", cl)
    )

  # Testing multinomial
  expect_warning(mb_eval <- evaluate(
    data = data_3,
    target_col = "cl_char",
    prediction_cols = "pred_cl_char",
    apply_softmax = FALSE,
    type = "multinomial",
    metrics = list("AUC" = TRUE)
  ), "The following classes were not found in 'response': cl_5.",
  fixed = TRUE
  )

  expect_equal(mb_eval$`Overall Accuracy`, 0.3)
  expect_equal(mb_eval$`Balanced Accuracy`, NaN)
  expect_equal(mb_eval$F1, NaN)
  expect_equal(mb_eval$Sensitivity, NaN)
  expect_equal(mb_eval$Specificity, 0.8266667, tolerance = 1e-4)
  expect_equal(mb_eval$`Pos Pred Value`, 0.31, tolerance = 1e-4)
  expect_equal(mb_eval$`Neg Pred Value`, 0.825555555555556, tolerance = 1e-4)
  expect_equal(mb_eval$AUC, 0.5666667, tolerance = 1e-4)
  expect_equal(mb_eval$Kappa, 0.1133333, tolerance = 1e-4)
  expect_equal(mb_eval$MCC, 0.11849, tolerance = 1e-4)
  expect_equal(mb_eval$`Detection Rate`, 0.06, tolerance = 1e-4)
  expect_equal(mb_eval$`Detection Prevalence`, 0.2, tolerance = 1e-4)
  expect_equal(mb_eval$Prevalence, 0.2, tolerance = 1e-4)
  expect_true("cl_5" %ni% mb_eval$Predictions[[1]]$Target)
  expect_equal(
    mb_eval$`Confusion Matrix`[[1]]$Target,
    rep(paste0("cl_", 1:5), each = 5)
  )
  expect_equal(
    mb_eval$`Confusion Matrix`[[1]]$Prediction,
    rep(paste0("cl_", 1:5), 5)
  )
  expect_equal(
    mb_eval$`Confusion Matrix`[[1]]$N,
    c(
      1L, 1L, 1L, 0L, 2L, 2L, 1L, 0L, 2L, 0L, 1L, 2L, 1L, 0L, 1L,
      1L, 0L, 0L, 3L, 1L, 0L, 0L, 0L, 0L, 0L
    )
  )


  expect_equal(
    mb_eval$`Class Level Results`[[1]]$Class,
    c("cl_1", "cl_2", "cl_3", "cl_4", "cl_5")
  )
  expect_equal(
    mb_eval$`Class Level Results`[[1]]$`Balanced Accuracy`,
    c(
      0.466666666666667, 0.5, 0.566666666666667,
      0.733333333333333, NaN
    )
  )
  expect_equal(
    mb_eval$`Class Level Results`[[1]]$F1,
    c(0.2, 0.222222222222222, 0.285714285714286, 0.6, NaN)
  )
  expect_equal(
    mb_eval$`Class Level Results`[[1]]$Sensitivity,
    c(0.2, 0.2, 0.2, 0.6, NA)
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$Specificity,
    c(
      0.733333333333333, 0.8, 0.933333333333333,
      0.866666666666667, 0.8
    ),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Pos Pred Value`,
    c(0.2, 0.25, 0.5, 0.6, 0),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Neg Pred Value`,
    c(
      0.733333333333333, 0.75, 0.777777777777778,
      0.866666666666667, 1
    ),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$Kappa,
    c(
      -0.0666666666666667, -3.17206578464331e-16, 0.166666666666666,
      0.466666666666667, 0
    ),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$MCC,
    c(
      -0.0666666666666667, 0, 0.192450089729875,
      0.466666666666667, 0
    ),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Detection Rate`,
    c(0.05, 0.05, 0.05, 0.15, 0),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Detection Prevalence`,
    c(0.25, 0.2, 0.1, 0.25, 0.2),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$Prevalence,
    c(0.25, 0.25, 0.25, 0.25, 0),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$Support,
    c(5L, 5L, 5L, 5L, NaN),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$N,
    c(11, 4, 4, 1),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Confusion Matrix`[[5]]$N,
    c(16, 4, 0, 0),
    tolerance = 1e-4
  )


  # What if a target class is never predicted?
  mb_eval <- evaluate(
    data = data_3,
    target_col = "pred_cl_char",
    prediction_cols = "cl_char",
    apply_softmax = FALSE,
    type = "multinomial",
    metrics = list("AUC" = TRUE)
  )

  expect_equal(mb_eval$`Overall Accuracy`, 0.3, tolerance = 1e-4)
  expect_equal(mb_eval$`Balanced Accuracy`, 0.5677778, tolerance = 1e-4)
  expect_equal(mb_eval$F1, NaN)
  expect_equal(mb_eval$Sensitivity, 0.31)
  expect_equal(mb_eval$Specificity, 0.8255556, tolerance = 1e-4)
  expect_equal(mb_eval$`Pos Pred Value`, NaN, tolerance = 1e-4)
  expect_equal(mb_eval$`Neg Pred Value`, 0.8266667, tolerance = 1e-4)
  expect_equal(mb_eval$AUC, 0.56875, tolerance = 1e-4)
  expect_equal(mb_eval$Kappa, 0.1133333, tolerance = 1e-4)
  expect_equal(mb_eval$MCC, 0.11849, tolerance = 1e-4)
  expect_equal(mb_eval$`Detection Rate`, 0.06, tolerance = 1e-4)
  expect_equal(mb_eval$`Detection Prevalence`, 0.2, tolerance = 1e-4)
  expect_equal(mb_eval$Prevalence, 0.2, tolerance = 1e-4)
  expect_true("cl_5" %in% mb_eval$Predictions[[1]]$Target)
  expect_equal(
    mb_eval$`Confusion Matrix`[[1]]$Target,
    rep(paste0("cl_", 1:5), each = 5)
  )
  expect_equal(
    mb_eval$`Confusion Matrix`[[1]]$Prediction,
    rep(paste0("cl_", 1:5), 5)
  )
  expect_equal(
    mb_eval$`Confusion Matrix`[[1]]$N,
    c(
      1L, 2L, 1L, 1L, 0L, 1L, 1L, 2L, 0L, 0L, 1L, 0L, 1L, 0L, 0L,
      0L, 2L, 0L, 3L, 0L, 2L, 0L, 1L, 1L, 0L
    )
  )


  expect_equal(
    mb_eval$`Class Level Results`[[1]]$Class,
    c("cl_1", "cl_2", "cl_3", "cl_4", "cl_5")
  )
  expect_equal(
    mb_eval$`Class Level Results`[[1]]$`Balanced Accuracy`,
    c(
      0.466666666666667, 0.5, 0.638888888888889,
      0.733333333333333, 0.5
    )
  )
  expect_equal(
    mb_eval$`Class Level Results`[[1]]$F1,
    c(0.2, 0.222222222222222, 0.285714285714286, 0.6, NaN)
  )
  expect_equal(
    mb_eval$`Class Level Results`[[1]]$Sensitivity,
    c(0.2, 0.25, 0.5, 0.6, 0)
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$Specificity,
    c(
      0.733333333333333, 0.75, 0.777777777777778,
      0.866666666666667, 1
    ),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Pos Pred Value`,
    c(0.2, 0.2, 0.2, 0.6, NaN),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Neg Pred Value`,
    c(
      0.733333333333333, 0.8, 0.933333333333333, 0.866666666666667,
      0.8
    ),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$Kappa,
    c(
      -0.0666666666666667, -3.17206578464331e-16, 0.166666666666666,
      0.466666666666667, 0
    ),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$MCC,
    c(
      -0.0666666666666667, 0, 0.192450089729875,
      0.466666666666667, 0
    ),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Detection Rate`,
    c(0.05, 0.05, 0.05, 0.15, 0),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Detection Prevalence`,
    c(0.25, 0.25, 0.25, 0.25, 0),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$Prevalence,
    c(0.25, 0.2, 0.1, 0.25, 0.2),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$Support,
    c(5L, 4L, 2L, 5L, 4),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Confusion Matrix`[[1]]$N,
    c(11, 4, 4, 1),
    tolerance = 1e-4
  )
  expect_equal(mb_eval$`Class Level Results`[[1]]$`Confusion Matrix`[[5]]$N,
    c(16, 0, 4, 0),
    tolerance = 1e-4
  )

  # TODO test that group_by and evaluate work correctly together
})

test_that("nested tibbles are correctly added to grouped multinomial results in evaluate()", {
  xpectr::set_test_seed(1)
  random_probabilities <- multiclass_probability_tibble(
    num_classes = 3,
    num_observations = 60,
    apply_softmax = FALSE # Test with as well
  )
  expect_equal(sum(random_probabilities), 92.56257, tolerance = 1e-5)

  random_classes <- argmax(random_probabilities)

  data_ <- random_probabilities %>%
    dplyr::mutate(
      cl = as.factor(rep(1:3, each = 20)),
      cl_char = paste0("cl_", cl),
      pred_cl = random_classes
    )

  data_ <- data_ %>%
    dplyr::rename_at(dplyr::vars(paste0("class_", 1:3)), .funs = ~ paste0("cl_", 1:3)) %>%
    dplyr::mutate(pred_cl_char = paste0("cl_", pred_cl))

  # add grouping vars
  data_[["gk1_char"]] <- as.character(rep(1:3, each = 20))
  data_[["gk2_int"]] <- rep(rep(1:5, each = 4), 3)

  data_ <- data_ %>%
    dplyr::bind_rows(data_ %>%
      dplyr::mutate(
        gk1_char = "10",
        gk2_int = 10
      ))

  data_ <- data_ %>%
    dplyr::sample_frac()

  mn_eval_1 <-
    data_ %>%
    dplyr::group_by(.data$gk1_char, .data$gk2_int) %>%
    evaluate(
      target_col = "cl_char",
      prediction_cols = paste0("cl_", 1:3),
      type = "multinomial",
      apply_softmax = TRUE
    ) %>%
    .[, c("gk1_char", "gk2_int", "Predictions", "Confusion Matrix", "Class Level Results")]

  clr_grkeys <- plyr::ldply(mn_eval_1$`Class Level Results`, function(clr) {
    tibble::tibble(
      "gk1_char" = unique(clr[["gk1_char"]]),
      "gk2_int" = unique(clr[["gk2_int"]])
    )
  })

  expect_equal(mn_eval_1$gk1_char, clr_grkeys$gk1_char)
  expect_equal(mn_eval_1$gk2_int, clr_grkeys$gk2_int)

  clr_cnfm_grkeys <- plyr::ldply(mn_eval_1$`Class Level Results`, function(clr) {
    bn_confmat <- dplyr::bind_rows(clr[["Confusion Matrix"]])
    tibble::tibble(
      "gk1_char" = unique(bn_confmat[["gk1_char"]]),
      "gk2_int" = unique(bn_confmat[["gk2_int"]])
    )
  })

  expect_equal(mn_eval_1$gk1_char, clr_cnfm_grkeys$gk1_char)
  expect_equal(mn_eval_1$gk2_int, clr_cnfm_grkeys$gk2_int)

  pred_grkeys <- plyr::ldply(mn_eval_1$Predictions, function(clr) {
    tibble::tibble(
      "gk1_char" = unique(clr[["gk1_char"]]),
      "gk2_int" = unique(clr[["gk2_int"]])
    )
  })

  expect_equal(mn_eval_1$gk1_char, pred_grkeys$gk1_char)
  expect_equal(mn_eval_1$gk2_int, pred_grkeys$gk2_int)

  cnfm_grkeys <- plyr::ldply(mn_eval_1$`Confusion Matrix`, function(clr) {
    tibble::tibble(
      "gk1_char" = unique(clr[["gk1_char"]]),
      "gk2_int" = unique(clr[["gk2_int"]])
    )
  })

  expect_equal(mn_eval_1$gk1_char, cnfm_grkeys$gk1_char)
  expect_equal(mn_eval_1$gk2_int, cnfm_grkeys$gk2_int)
})

test_that("nested tibbles are correctly added to grouped binomial results in evaluate()", {
  xpectr::set_test_seed(1)
  random_probabilities <- multiclass_probability_tibble(
    num_classes = 1,
    num_observations = 60,
    apply_softmax = FALSE # Test with as well
  )
  expect_equal(sum(random_probabilities), 30.72525, tolerance = 1e-5)

  random_classes <- ifelse(random_probabilities$class_1 > 0.5, 1, 0)

  data_ <- random_probabilities %>%
    dplyr::mutate(
      pred_cl = random_classes,
      cl = as.factor(rep(c(0, 1), each = 30))
    )

  data_ <- data_ %>%
    dplyr::sample_frac()

  # add grouping vars
  data_[["gk1_char"]] <- as.character(rep(1:3, each = 20))
  data_[["gk2_int"]] <- rep(rep(1:2, each = 10), 3)

  data_ <- data_ %>%
    dplyr::bind_rows(data_ %>%
      dplyr::mutate(
        gk1_char = "10",
        gk2_int = 10
      ))

  data_ <- data_ %>%
    dplyr::sample_frac()

  bn_eval_1 <-
    data_ %>%
    dplyr::group_by(.data$gk1_char, .data$gk2_int) %>%
    evaluate(
      target_col = "cl",
      prediction_cols = "class_1",
      type = "binomial",
    ) %>%
    .[, c("gk1_char", "gk2_int", "Predictions", "Confusion Matrix")]

  pred_grkeys <- plyr::ldply(bn_eval_1$Predictions, function(clr) {
    tibble::tibble(
      "gk1_char" = unique(clr[["gk1_char"]]),
      "gk2_int" = unique(clr[["gk2_int"]])
    )
  })

  expect_equal(bn_eval_1$gk1_char, pred_grkeys$gk1_char)
  expect_equal(bn_eval_1$gk2_int, pred_grkeys$gk2_int)

  cnfm_grkeys <- plyr::ldply(bn_eval_1$`Confusion Matrix`, function(clr) {
    tibble::tibble(
      "gk1_char" = unique(clr[["gk1_char"]]),
      "gk2_int" = unique(clr[["gk2_int"]])
    )
  })

  expect_equal(bn_eval_1$gk1_char, cnfm_grkeys$gk1_char)
  expect_equal(bn_eval_1$gk2_int, cnfm_grkeys$gk2_int)
})

test_that("nested tibbles are correctly added to grouped gaussian results in evaluate()", {
  xpectr::set_test_seed(1)
  random_probabilities <- multiclass_probability_tibble(
    num_classes = 2,
    num_observations = 60,
    apply_softmax = FALSE # Test with as well
  )
  expect_equal(sum(random_probabilities), 61.59519, tolerance = 1e-5)

  data_ <- random_probabilities

  # add grouping vars
  data_[["gk1_char"]] <- as.character(rep(1:3, each = 20))
  data_[["gk2_int"]] <- rep(rep(1:2, each = 10), 3)

  data_ <- data_ %>%
    dplyr::bind_rows(data_ %>%
      dplyr::mutate(
        gk1_char = "10",
        gk2_int = 10
      ))

  data_ <- data_ %>%
    dplyr::sample_frac()

  gs_eval_1 <-
    data_ %>%
    dplyr::group_by(.data$gk1_char, .data$gk2_int) %>%
    evaluate(
      target_col = "class_1",
      prediction_cols = "class_2",
      type = "gaussian",
    ) %>%
    .[, c("gk1_char", "gk2_int", "Predictions")]

  pred_grkeys <- plyr::ldply(gs_eval_1$Predictions, function(clr) {
    tibble::tibble(
      "gk1_char" = unique(clr[["gk1_char"]]),
      "gk2_int" = unique(clr[["gk2_int"]])
    )
  })

  expect_equal(gs_eval_1$gk1_char, pred_grkeys$gk1_char)
  expect_equal(gs_eval_1$gk2_int, pred_grkeys$gk2_int)
})

test_that("specific multinomial predictions yield correct results in evaluate()", {
  data <- tibble::tibble(
    "target" = c(
      "1", "2", "2", "3", "1", "3", "3", "2", "2", "1", "1", "1",
      "2", "1", "2", "3", "3", "3", "2", "1", "1", "3", "3", "1", "3",
      "1", "2", "2", "3", "1", "3", "3", "2", "2", "1", "1", "1", "2",
      "1", "2", "3", "3", "3", "2", "1", "1", "3", "3", "1", "3", "1",
      "2", "2", "3", "1", "3", "3", "2", "2", "1", "1", "1", "2", "1",
      "2", "3", "3", "3", "2", "1", "1", "3", "3", "1", "3", "1", "2",
      "2", "3", "1", "3", "3", "2", "2", "1", "1", "1", "2", "1", "2",
      "3", "3", "3", "2", "1", "1", "3", "3", "1", "3", "1", "2", "2",
      "3", "1", "3", "3", "2", "2", "1", "1", "1", "2", "1", "2", "3",
      "3", "3", "2", "1", "1", "3", "3", "1", "3", "1", "2", "2", "3",
      "1", "3", "3", "2", "2", "1", "1", "1", "2", "1", "2", "3", "3",
      "3", "2", "1", "1", "3", "3", "1", "3", "1", "2", "2", "3", "1",
      "3", "3", "2", "2", "1", "1", "1", "2", "1", "2", "3", "3", "3",
      "2", "1", "1", "3", "3", "1", "3", "1", "2", "2", "3", "1", "3",
      "3", "2", "2", "1", "1", "1", "2", "1", "2", "3", "3", "3", "2",
      "1", "1", "3", "3", "1", "3", "1", "2", "2", "3", "1", "3", "3",
      "2", "2", "1", "1", "1", "2", "1", "2", "3", "3", "3", "2", "1",
      "1", "3", "3", "1", "3", "1", "2", "2", "3", "1", "3", "3", "2",
      "2", "1", "1", "1", "2", "1", "2", "3", "3", "3", "2", "1", "1",
      "3", "3", "1", "3"
    ),
    "1" = c(
      0.362824302965311, 0.360903717826462, 0.285822146515266, 0.37477371815703,
      0.381700937144022, 0.273724077239358, 0.341941761553692, 0.304958692149421,
      0.296272111439159, 0.42404336282095, 0.293116571086624, 0.435758286831625,
      0.28272698563188, 0.224804797891925, 0.198163481343272, 0.222945305755905,
      0.300594211265473, 0.3278586388128, 0.414855759724269, 0.33840681107911,
      0.407904832070239, 0.329062099428566, 0.347623656119297, 0.436145034077774,
      0.335017655729982, 0.331364482540279, 0.283536882101457, 0.281143446843595,
      0.425995399790343, 0.261537001684637, 0.237313955938272, 0.23261151589263,
      0.385748271928449, 0.334411930525475, 0.401249217427923, 0.420235849517611,
      0.425289710559998, 0.262465444718002, 0.336697104880243, 0.231858151646129,
      0.210092588546093, 0.47741912516683, 0.244565586519055, 0.286947978160956,
      0.417209987822813, 0.409276670773936, 0.354307298336307, 0.276564090289706,
      0.330862775865476, 0.44695225847507, 0.317576338796576, 0.382847300413345,
      0.224081474342672, 0.308365835600141, 0.302015399930661, 0.313332459936172,
      0.251934064646274, 0.23915184388103, 0.277538091890923, 0.471891254622384,
      0.40754972988395, 0.345372198209239, 0.33466402327811, 0.462461145964111,
      0.266579885476856, 0.352899631920573, 0.421312945065338, 0.200478726356666,
      0.309794998292503, 0.380501567619241, 0.370761060364264, 0.286594888362904,
      0.361067815387182, 0.245947918320429, 0.389260130880553, 0.321714471097802,
      0.331089372813525, 0.256210536793289, 0.285625126260833, 0.27568485403864,
      0.274822151212381, 0.389484563107126, 0.212979233666992, 0.296561920614094,
      0.200225006332582, 0.242554965054235, 0.401234678799669, 0.330009524801572,
      0.348663184860405, 0.411214822720279, 0.307151655834041, 0.204995757155556,
      0.351402327921561, 0.374094844465352, 0.248711786514304, 0.336708510840726,
      0.408292124109811, 0.357562258085327, 0.34181138284562, 0.323252856286123,
      0.447544862966328, 0.429915527750429, 0.363601070683015, 0.333552425710284,
      0.398225671426714, 0.221696833964976, 0.271265251552504, 0.481294916218702,
      0.328149707350169, 0.450855293083027, 0.237104558776676, 0.374383211394644,
      0.443948913175648, 0.494051191743219, 0.256273986260961, 0.489484795353784,
      0.329464982964633, 0.220032486448317, 0.408557119092641, 0.319230850919545,
      0.355237403525459, 0.23455484947749, 0.382020061725786, 0.267110221515588,
      0.449602783872994, 0.202164855975727, 0.236445026776993, 0.319748604954957,
      0.293320401651886, 0.287711207487059, 0.358419477510523, 0.346556723204159,
      0.206074092549024, 0.22717751457188, 0.397646700390622, 0.446688182141175,
      0.25620775941098, 0.501804532497241, 0.408063047760274, 0.443251289188255,
      0.249444482951462, 0.383829461406706, 0.526313287884649, 0.452849242524878,
      0.253013631607335, 0.334191437619488, 0.210413611589952, 0.23421023559276,
      0.321507174883409, 0.509517153778936, 0.315671789411075, 0.255620649439559,
      0.239331881508991, 0.322754669547894, 0.435042212296461, 0.251412865189562,
      0.282387104005381, 0.245973141871465, 0.341257410449456, 0.277527088361948,
      0.399220690874693, 0.439926753580167, 0.345819178673885, 0.305382501740128,
      0.418275547531307, 0.357628745014896, 0.294197703135495, 0.438696916274104,
      0.316517159370203, 0.357792733303972, 0.28831445694109, 0.327860691995961,
      0.22281288859293, 0.314065020723387, 0.459321941470143, 0.387693577991577,
      0.363268586267414, 0.303950826699894, 0.372655080723904, 0.339529039157154,
      0.224621024989129, 0.296482850978123, 0.314332611351849, 0.234267899937577,
      0.437859055024521, 0.269731707168111, 0.331690324355338, 0.280473038011755,
      0.399824753263675, 0.383719422449624, 0.330850303521819, 0.225277208971775,
      0.359553396121816, 0.328615974470287, 0.231945476475709, 0.214223736176299,
      0.20582242433687, 0.375727645283227, 0.235524783117192, 0.286268463291008,
      0.229673192858615, 0.283772599798294, 0.219611172339202, 0.295826542146669,
      0.285141592656138, 0.192558055771125, 0.355721326100929, 0.409545410065343,
      0.331835694337497, 0.230954644322117, 0.340582165139984, 0.294251623668547,
      0.298768624646445, 0.291985965689547, 0.300172335564108, 0.318523203157749,
      0.237566842343213, 0.37261470439223, 0.331390765035546, 0.364342409833174,
      0.397916841690445, 0.217399738068907, 0.264193767365295, 0.208427577921687,
      0.420006306243977, 0.292308232373888, 0.281443833338, 0.30947189334135,
      0.257203806304276, 0.295150877610198, 0.439630183471502, 0.321730635029117,
      0.383315062595601, 0.225583239939362, 0.309872283286329, 0.255887220229597,
      0.316939046835727, 0.449868348167623, 0.304614105760487, 0.248742717566446,
      0.305293159179038, 0.222558427746317, 0.244342581208601, 0.503534783305527,
      0.286383868562386, 0.428804177911175, 0.295404759872436, 0.30687255200497,
      0.445596166797302, 0.334706962656109
    ),
    "2" = c(
      0.385780580440307, 0.342604539124775, 0.423080439134247, 0.225175445397002,
      0.312989908016585, 0.475217543730522, 0.224916968457944, 0.30476258690942,
      0.482054751251862, 0.214841434476723, 0.344321198446437, 0.333138708845406,
      0.292804142075078, 0.31480368331023, 0.453026287138725, 0.4719140606185,
      0.338078757628025, 0.296611945042962, 0.25365258932653, 0.305362051955673,
      0.240571861737913, 0.333693522274202, 0.27392009695986, 0.28042156343828,
      0.377909676645575, 0.279959262495121, 0.2122887561223, 0.470196696415608,
      0.239862567097843, 0.370221172919109, 0.25443866213747, 0.477173210501813,
      0.259642635838842, 0.322066433258374, 0.353453695410528, 0.1765751153703,
      0.285504011914052, 0.424456315084771, 0.288460995364288, 0.35958455461334,
      0.478761722406255, 0.250631249168976, 0.445446891169545, 0.369385615626211,
      0.349831286201674, 0.234976115878086, 0.378701320138235, 0.431201840261207,
      0.350278234492975, 0.262288382369503, 0.307058606700166, 0.320533642024935,
      0.43236586844692, 0.292897655407014, 0.272621596850078, 0.270996516799443,
      0.410085989322878, 0.444842557489891, 0.388276130257391, 0.235166674305724,
      0.285112014266039, 0.437273128065374, 0.454306056457757, 0.193441477963145,
      0.420457813840421, 0.20658635492773, 0.266523286854865, 0.398245797218399,
      0.397335280211415, 0.214226033921459, 0.334049449261309, 0.465337959942035,
      0.233723758733514, 0.31472699114585, 0.190099685465158, 0.336264164127875,
      0.228217420461845, 0.478951976081042, 0.445589110407979, 0.429453925922651,
      0.472930553187392, 0.332138049773757, 0.431357175765288, 0.227158863320801,
      0.414490874557254, 0.492841238109576, 0.234359938505408, 0.446090237254051,
      0.35319859961771, 0.345280017404155, 0.327026905651897, 0.315397998189469,
      0.341892635859671, 0.363505947910766, 0.404023366558927, 0.276862535515932,
      0.294742179813213, 0.294768056642531, 0.266684517736902, 0.311820705852887,
      0.274265040914617, 0.245816171241433, 0.440341510685916, 0.421104352741149,
      0.388918784994157, 0.421278084276545, 0.240879703622285, 0.289683431392311,
      0.339677067961078, 0.208333263474704, 0.437464387830416, 0.367535015967519,
      0.226639265821188, 0.311401754535334, 0.372083534671264, 0.250419612561257,
      0.299632515128755, 0.32824246642957, 0.297400161614639, 0.313227906367457,
      0.300459136297168, 0.303048382688999, 0.423041978939619, 0.21995462356869,
      0.220422428036373, 0.405812939731575, 0.295769536850216, 0.276168343895395,
      0.265750665287833, 0.253402549071951, 0.411972235353439, 0.351927605924981,
      0.485986422454677, 0.321551112338582, 0.206970975070247, 0.200850312668311,
      0.372875934652805, 0.234374628487236, 0.245897425556106, 0.253770902739585,
      0.468416469360073, 0.410120970528605, 0.238800068178443, 0.280904468157008,
      0.359143089366176, 0.407538360613741, 0.463482198596469, 0.346052750154407,
      0.287160871624961, 0.227514884062728, 0.240774210253124, 0.468578579358505,
      0.353317373812137, 0.349640083059674, 0.369950508544139, 0.380002971753307,
      0.289234489357681, 0.395516529035609, 0.467797098876064, 0.338410210733615,
      0.331006277488809, 0.266926876246406, 0.198909961692134, 0.477956034101944,
      0.304153252664601, 0.269291217911081, 0.42201242766336, 0.236603843824688,
      0.323467921547914, 0.352132946072107, 0.369703528563403, 0.329596532065509,
      0.327226373739197, 0.462085224932779, 0.189039991684081, 0.314951443633383,
      0.284435887439821, 0.366590341963145, 0.395601714171088, 0.335225687954128,
      0.5311988845767, 0.346007208388757, 0.446522267443117, 0.343863071903928,
      0.292892871402239, 0.382830131827548, 0.385357786983514, 0.214142145338179,
      0.376626067248013, 0.389178684389126, 0.338344548396213, 0.403001958525644,
      0.290654812925342, 0.247101836432825, 0.370492901531161, 0.461281265139314,
      0.375012631382266, 0.363470643270294, 0.226067381809261, 0.313047833074042,
      0.479443190691509, 0.490484639496364, 0.391240933517263, 0.478047191943444,
      0.356438854507902, 0.343453444225452, 0.216202187035667, 0.237739153185802,
      0.301640124052444, 0.247512173039711, 0.309589249125308, 0.308972701838745,
      0.358423819908425, 0.304335319964878, 0.448013834885817, 0.378099789829676,
      0.541697788505534, 0.268360413749574, 0.331964563898707, 0.26728121083406,
      0.225911392328278, 0.533006780442077, 0.342769074491857, 0.347630476342447,
      0.345065351570056, 0.35598982747141, 0.453613405495365, 0.191197557332096,
      0.401269074847496, 0.438308839883431, 0.239209391932476, 0.29631107556132,
      0.178586958336824, 0.478858385133731, 0.475817691964852, 0.427658658905656,
      0.298338105256703, 0.272983994354953, 0.308124320381545, 0.471773233423636,
      0.373331904052012, 0.260774223314073, 0.326618966011962, 0.252950597054979,
      0.399516649309411, 0.258469593402028, 0.328387994212459, 0.263157498352965,
      0.180860493953652, 0.325672027376522
    ),
    "3" = c(
      0.251395116594382, 0.296491743048763, 0.291097414350488, 0.400050836445967,
      0.305309154839393, 0.25105837903012, 0.433141269988364, 0.390278720941159,
      0.221673137308979, 0.361115202702327, 0.362562230466938, 0.231103004322969,
      0.424468872293042, 0.460391518797845, 0.348810231518003, 0.305140633625595,
      0.361327031106502, 0.375529416144238, 0.331491650949201, 0.356231136965217,
      0.351523306191848, 0.337244378297231, 0.378456246920842, 0.283433402483946,
      0.287072667624444, 0.3886762549646, 0.504174361776243, 0.248659856740797,
      0.334142033111814, 0.368241825396254, 0.508247381924258, 0.290215273605557,
      0.354609092232709, 0.343521636216152, 0.245297087161549, 0.403189035112089,
      0.28920627752595, 0.313078240197227, 0.374841899755469, 0.408557293740532,
      0.311145689047652, 0.271949625664194, 0.3099875223114, 0.343666406212833,
      0.232958725975514, 0.355747213347978, 0.266991381525458, 0.292234069449087,
      0.318858989641549, 0.290759359155428, 0.375365054503257, 0.29661905756172,
      0.343552657210408, 0.398736508992845, 0.425363003219261, 0.415671023264385,
      0.337979946030848, 0.316005598629079, 0.334185777851686, 0.292942071071892,
      0.30733825585001, 0.217354673725387, 0.211029920264134, 0.344097376072744,
      0.312962300682723, 0.440514013151697, 0.312163768079797, 0.401275476424934,
      0.292869721496081, 0.4052723984593, 0.295189490374428, 0.24806715169506,
      0.405208425879304, 0.439325090533721, 0.420640183654289, 0.342021364774323,
      0.440693206724629, 0.264837487125669, 0.268785763331188, 0.294861220038709,
      0.252247295600227, 0.278377387119117, 0.35566359056772, 0.476279216065105,
      0.385284119110164, 0.264603796836189, 0.364405382694923, 0.223900237944377,
      0.298138215521885, 0.243505159875566, 0.365821438514061, 0.479606244654974,
      0.306705036218769, 0.262399207623883, 0.347264846926769, 0.386428953643342,
      0.296965696076976, 0.347669685272142, 0.391504099417478, 0.364926437860989,
      0.278190096119055, 0.324268301008138, 0.196057418631069, 0.245343221548567,
      0.212855543579129, 0.357025081758479, 0.487855044825211, 0.229021652388988,
      0.332173224688753, 0.340811443442268, 0.325431053392908, 0.258081772637838,
      0.329411821003164, 0.194547053721447, 0.371642479067775, 0.260095592084959,
      0.370902501906612, 0.451725047122112, 0.29404271929272, 0.367541242712998,
      0.344303460177374, 0.462396767833511, 0.194937959334594, 0.512935154915721,
      0.329974788090632, 0.392022204292698, 0.467785436372791, 0.404083051149648,
      0.440928933060281, 0.45888624344099, 0.229608287136038, 0.30151567087086,
      0.307939484996299, 0.451271373089538, 0.395382324539131, 0.352461505190514,
      0.370916305936214, 0.263820839015524, 0.34603952668362, 0.30297780807216,
      0.282139047688465, 0.206049568064689, 0.234886643936908, 0.266246289318114,
      0.387843279026488, 0.258270201766771, 0.326104189813578, 0.419737014252832,
      0.39133195349163, 0.262967962158336, 0.443554000335801, 0.275800771201936,
      0.407350744678872, 0.327605247392432, 0.1950072791594, 0.36858416305713,
      0.428378406636938, 0.358510329092926, 0.190945490674481, 0.384062700904437,
      0.269773031636498, 0.293146370173427, 0.455270859633981, 0.216661464157928,
      0.277571199804092, 0.373080037074023, 0.283789869201145, 0.324699239901208,
      0.360014919081883, 0.290074320623921, 0.341982014495507, 0.34254277593853,
      0.449960737667873, 0.223849754343834, 0.351638066845776, 0.29735497837504,
      0.352295526292765, 0.329458831336961, 0.231743205105008, 0.325245272888717,
      0.24418009043417, 0.35750994063312, 0.239145121205034, 0.421869028158495,
      0.269248073573241, 0.347438161004341, 0.282951888661148, 0.505384816650066,
      0.223549179488312, 0.22710189316125, 0.330805148081968, 0.371720832502581,
      0.349791790952842, 0.424282189096888, 0.39756162199313, 0.324494998684386,
      0.419164944280864, 0.260801711446479, 0.538407835073546, 0.400683703634949,
      0.290883616449876, 0.225742760705341, 0.389147894143535, 0.226126265909887,
      0.35841955283596, 0.463988500003423, 0.428076486863404, 0.352715436748856,
      0.366524181610059, 0.521533182638172, 0.349828585734708, 0.396775674492708,
      0.34280755544513, 0.403678714345574, 0.251813829550075, 0.303377007012575,
      0.220735369151253, 0.359024881858196, 0.336644671065747, 0.368376379332766,
      0.376171765981278, 0.249593481489016, 0.393037158142848, 0.443941945735865,
      0.234928342185967, 0.351701940154702, 0.264942761166635, 0.499330549326554,
      0.341527118848228, 0.266540282506371, 0.321160424596023, 0.381958289409563,
      0.438097979067575, 0.295558374926908, 0.214310024748819, 0.316454120864747,
      0.38472284790757, 0.277147657477424, 0.387261573857968, 0.279484049009918,
      0.32137493676895, 0.51666734893961, 0.429038452779437, 0.243514619639494,
      0.314099482128203, 0.312726228686797, 0.376207245915106, 0.429969949642065,
      0.373543339249046, 0.339621009967369
    ),
    ".groups" = rep(1:10, each = 25)
  )

  evals <- evaluate(
    data = data %>% dplyr::group_by(.data$.groups),
    target_col = "target",
    prediction_cols = c("1", "2", "3"),
    metrics = list("AUC" = TRUE),
    type = "multinomial"
  )

  expect_equal(
    colnames(evals),
    c(
      ".groups", "Overall Accuracy", "Balanced Accuracy", "F1", "Sensitivity",
      "Specificity", "Pos Pred Value", "Neg Pred Value", "AUC",
      "Kappa", "MCC", "Detection Rate", "Detection Prevalence",
      "Prevalence", "Predictions", "ROC", "Confusion Matrix", "Class Level Results"
    )
  )

  expect_equal(
    evals$`Overall Accuracy`,
    c(0.56, 0.36, 0.64, 0.28, 0.52, 0.24, 0.44, 0.4, 0.32, 0.44)
  )
  expect_equal(evals$`Balanced Accuracy`,
    c(
      0.665178571428571, 0.523974867724868, 0.73776455026455, 0.466104497354497,
      0.636243386243386, 0.427744708994709, 0.580687830687831, 0.552910052910053,
      0.498015873015873, 0.581845238095238
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$F1,
    c(
      0.551190476190476, 0.347276688453159, 0.638304093567251, 0.276960784313726,
      0.511111111111111, 0.237745098039216, 0.439814814814815, 0.401960784313726,
      0.313186813186813, 0.423411469851098
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$Sensitivity,
    c(
      0.55026455026455, 0.365079365079365, 0.656084656084656, 0.291005291005291,
      0.513227513227513, 0.232804232804233, 0.439153439153439, 0.402116402116402,
      0.338624338624339, 0.439153439153439
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$Specificity,
    c(
      0.780092592592593, 0.68287037037037, 0.819444444444444, 0.641203703703704,
      0.759259259259259, 0.622685185185185, 0.722222222222222, 0.703703703703704,
      0.657407407407407, 0.724537037037037
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$`Pos Pred Value`,
    c(
      0.562770562770563, 0.340740740740741, 0.644444444444444, 0.272619047619048,
      0.531746031746032, 0.24537037037037, 0.44973544973545, 0.41547619047619,
      0.314814814814815, 0.433333333333333
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$`Neg Pred Value`,
    c(
      0.78042328042328, 0.687426900584795, 0.824780701754386, 0.645315904139434,
      0.761283550757235, 0.620098039215686, 0.719907407407407, 0.700871459694989,
      0.656669719169719, 0.727777777777778
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$AUC,
    c(
      0.666960611405056, 0.529100529100529, 0.766901822457378, 0.496766607877719,
      0.563786008230453, 0.397119341563786, 0.6222810111699, 0.481187536743092,
      0.468841857730747, 0.583774250440917
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$Kappa,
    c(
      0.332217431748538, 0.0411382701447752, 0.462128481918942, -0.0736440767694057,
      0.274254384977631, -0.1375345264999, 0.163202892673696, 0.108837675055412,
      -0.0101585873493696, 0.160109639592244
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$MCC,
    c(
      0.33636907200676, 0.0385278775986594, 0.47105139539729, -0.0747673723899526,
      0.281866012204808, -0.139292933974385, 0.1652388967936, 0.110830712271453,
      -0.0151691930693147, 0.162094868501065
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$`Detection Rate`,
    c(
      0.186666666666667, 0.12, 0.213333333333333, 0.0933333333333333,
      0.173333333333333, 0.08, 0.146666666666667, 0.133333333333333,
      0.106666666666667, 0.146666666666667
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$`Detection Prevalence`,
    c(
      0.333333333333333, 0.333333333333333, 0.333333333333333, 0.333333333333333,
      0.333333333333333, 0.333333333333333, 0.333333333333333, 0.333333333333333,
      0.333333333333333, 0.333333333333333
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$Prevalence,
    c(
      0.333333333333333, 0.333333333333333, 0.333333333333333, 0.333333333333333,
      0.333333333333333, 0.333333333333333, 0.333333333333333, 0.333333333333333,
      0.333333333333333, 0.333333333333333
    ),
    tolerance = 1e-5
  )
  expect_equal(
    evals$.groups,
    1:10
  )
  expect_equal(names(evals$ROC[[1]]$rocs), c("1/2", "1/3", "2/3"))
  expect_equal(evals$ROC[[1]]$rocs$`1/2`[[1]]$sensitivities,
    c(
      1, 1, 1, 1, 0.857142857142857, 0.857142857142857, 0.857142857142857,
      0.857142857142857, 0.714285714285714, 0.714285714285714, 0.571428571428571,
      0.428571428571429, 0.428571428571429, 0.285714285714286, 0.142857142857143,
      0.142857142857143, 0
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$ROC[[1]]$rocs$`1/2`[[2]]$sensitivities,
    c(
      1, 1, 1, 1, 0.888888888888889, 0.777777777777778, 0.777777777777778,
      0.666666666666667, 0.555555555555556, 0.444444444444444, 0.333333333333333,
      0.333333333333333, 0.333333333333333, 0.222222222222222, 0.222222222222222,
      0.111111111111111, 0
    ),
    tolerance = 1e-5
  )

  expect_equal(evals$ROC[[1]]$rocs$`1/2`[[1]]$specificities,
    c(
      0, 0.111111111111111, 0.222222222222222, 0.333333333333333,
      0.333333333333333, 0.444444444444444, 0.555555555555556, 0.666666666666667,
      0.666666666666667, 0.777777777777778, 0.777777777777778, 0.777777777777778,
      0.888888888888889, 0.888888888888889, 0.888888888888889, 1, 1
    ),
    tolerance = 1e-5
  )
  expect_equal(evals$ROC[[1]]$rocs$`1/2`[[2]]$specificities,
    c(
      0, 0.142857142857143, 0.285714285714286, 0.428571428571429,
      0.428571428571429, 0.428571428571429, 0.571428571428571, 0.571428571428571,
      0.571428571428571, 0.571428571428571, 0.571428571428571, 0.714285714285714,
      0.857142857142857, 0.857142857142857, 1, 1, 1
    ),
    tolerance = 1e-5
  )


  # class level
  expect_equal(evals$`Class Level Results`[[1]]$Class, as.character(c(1, 2, 3)))
  expect_equal(evals$`Class Level Results`[[1]]$`Balanced Accuracy`,
    c(0.715277777777778, 0.603174603174603, 0.677083333333333),
    tolerance = 1e-5
  )
  expect_equal(evals$`Class Level Results`[[1]]$F1,
    c(0.625, 0.428571428571429, 0.6),
    tolerance = 1e-5
  )
  expect_equal(
    evals$`Class Level Results`[[1]]$Support,
    c(9, 7, 9)
  )
  expect_equal(
    evals$`Class Level Results`[[1]]$`Confusion Matrix`[[1]],
    structure(list(
      .groups = c(1L, 1L, 1L, 1L),
      Class = c(1L, 1L, 1L, 1L),
      Prediction = c("0", "1", "0", "1"),
      Target = c("0", "0", "1", "1"),
      Pos_0 = c("TP", "FN", "FP", "TN"),
      Pos_1 = c("TN", "FP", "FN", "TP"),
      N = c(14L, 2L, 4L, 5L)
    ),
    class = c("tbl_df", "tbl", "data.frame"), row.names = c(NA, -4L)
    )
  )
})

# TODO Add test that majority vote id_method works when not all classes are predicted most by one of the ids

test_that("arguments throw proper errors and warnings in evaluate()", {
  xpectr::set_test_seed(1)
  random_probabilities <- multiclass_probability_tibble(
    num_classes = 5,
    num_observations = 20,
    apply_softmax = FALSE # Test with as well
  )
  data_ <- random_probabilities %>%
    dplyr::mutate(
      cl = as.factor(rep(1:5, each = 4)),
      cl_char = paste0("cl_", cl)
    ) %>%
    dplyr::rename_at(dplyr::vars(paste0("class_", 1:5)), .funs = ~ paste0("cl_", 1:5))

  # Testing 'metrics'

  expect_error(
    xpectr::strip_msg(evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multinomial",
    apply_softmax = TRUE,
    metrics = "none"
  )),
    xpectr::strip(paste0(
    "1 assertions failed:\n * Variable 'metrics': Must be of typ",
    "e 'list', not 'character'."
  )),
  fixed = TRUE
  )

  expect_error(
    xpectr::strip_msg(evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multinomial",
    apply_softmax = TRUE,
    metrics = list(TRUE, FALSE)
  )),
    xpectr::strip("1 assertions failed:\n * Variable 'metrics': Must have names."),
    fixed = TRUE
  )

  # TODO add more

  # Testing gaussian
  expect_error(evaluate(
    data = data_,
    target_col = "cl_1",
    prediction_cols = "cl_2",
    type = "gaussian",
    id_col = "cl",
    id_method = "mean"
  ), paste0(
    "The targets must be constant within the IDs with the current ID method. ",
    "These IDs had more than one unique value in the target column: 1, 2, 3, 4, 5."
  ),
  fixed = TRUE
  )

  # Only one class in target column for binomial
  data_3 <- data.frame(
    "target" = c(1, 1, 1, 1, 1),
    "prediction" = c(0.1, 0.2, 0.7, 0.8, 0.9)
  )
  expect_error(evaluate(
    data = data_3,
    target_col = "target",
    prediction_cols = "prediction",
    cutoff = 0.5,
    type = "binomial"
  ), "found less than 2 levels in the target column.",
  fixed = TRUE
  )


  # Test that pROC::roc returns the expected error
  # when there's only observations for one level in the target col ("response")

  expect_error(pROC::roc(data.frame(
    "target" = c(1, 1, 1),
    preds = c(0.01, 0.01, 1 - 0.02)
  ),
  response = "target",
  predictor = "preds",
  levels = c(0, 1)
  ),
  "No control observation.",
  fixed = TRUE
  )
})

test_that("binomial evaluation works in evaluate()", {
  xpectr::set_test_seed(1)
  random_probabilities <- multiclass_probability_tibble(
    num_classes = 1,
    num_observations = 20,
    apply_softmax = FALSE # Test with as well
  )
  expect_equal(sum(random_probabilities), 11.10334, tolerance = 1e-5)

  data_ <- random_probabilities %>%
    dplyr::rename(prediction = class_1) %>%
    dplyr::mutate(
      cl = as.factor(rep(1:2, each = 10)),
      cl_char = paste0("cl_", cl),
      predicted_class = ifelse(prediction > 0.5,
        "cl_2",
        "cl_1"
      ),
      inv_prediction = 1 - prediction,
      inv_predicted_class = ifelse(inv_prediction > 0.5,
        "cl_2",
        "cl_1"
      ),
    )

  bn_eval_1 <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "prediction",
    type = "binomial",
    apply_softmax = TRUE,
    metrics = list("Accuracy" = TRUE)
  )
  bn_eval_1_inv <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "inv_prediction",
    type = "binomial",
    apply_softmax = TRUE,
    metrics = list("Accuracy" = TRUE)
  )

  expect_equal(
    bn_eval_1$Accuracy,
    mean(data_$cl_char == data_$predicted_class)
  )
  expect_equal(
    bn_eval_1$Accuracy,
    0.45
  )
  expect_equal(
    bn_eval_1$`Balanced Accuracy`,
    0.45
  )
  expect_equal(
    bn_eval_1_inv$Accuracy,
    mean(data_$cl_char == data_$inv_predicted_class)
  )
  expect_equal(
    bn_eval_1_inv$Accuracy,
    0.55
  )
  expect_equal(
    bn_eval_1_inv$`Balanced Accuracy`,
    0.55
  )

  expect_equal(bn_eval_1$F1,
    0.4761905,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$Sensitivity,
    0.5,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$Specificity,
    0.4,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$`Pos Pred Value`,
    0.4545455,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$`Neg Pred Value`,
    0.4444444,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$AUC,
    0.53,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$AUC,
    bn_eval_1$ROC[[1]]$auc[[1]],
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$`Lower CI`,
    0.2573215,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$`Upper CI`,
    0.8026785,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$Kappa,
    -0.1,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$MCC,
    -0.1005038,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$`Detection Rate`,
    0.25,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$`Detection Prevalence`,
    0.55,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1$Prevalence,
    0.5,
    tolerance = 1e-4
  )

  expect_equal(bn_eval_1_inv$F1,
    0.5263158,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$Sensitivity,
    0.5,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$Specificity,
    0.6,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$`Pos Pred Value`,
    0.5555556,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$`Neg Pred Value`,
    0.5454545,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$AUC,
    0.47,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$AUC,
    bn_eval_1_inv$ROC[[1]]$auc[[1]],
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$`Lower CI`,
    0.1973215,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$`Upper CI`,
    0.7426785,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$Kappa,
    0.1,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$MCC,
    0.1005038,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$`Detection Rate`,
    0.25,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$`Detection Prevalence`,
    0.45,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_1_inv$Prevalence,
    0.5,
    tolerance = 1e-4
  )

  expect_true(!identical(bn_eval_1$ROC[[1]], bn_eval_1_inv$ROC[[1]]))

  expect_equal(
    bn_eval_1$ROC[[1]]$sensitivities,
    c(
      1, 1, 0.9, 0.9, 0.8, 0.8, 0.8, 0.7, 0.6, 0.5, 0.5, 0.5, 0.5,
      0.4, 0.3, 0.2, 0.1, 0.1, 0.1, 0.1, 0
    )
  )
  expect_equal(
    bn_eval_1$ROC[[1]]$specificities,
    c(
      0, 0.1, 0.1, 0.2, 0.2, 0.3, 0.4, 0.4, 0.4, 0.4, 0.5, 0.6, 0.7,
      0.7, 0.7, 0.7, 0.7, 0.8, 0.9, 1, 1
    )
  )
  expect_equal(
    bn_eval_1_inv$ROC[[1]]$sensitivities,
    c(
      1, 0.9, 0.9, 0.9, 0.9, 0.8, 0.7, 0.6, 0.5, 0.5, 0.5, 0.5, 0.4,
      0.3, 0.2, 0.2, 0.2, 0.1, 0.1, 0, 0
    )
  )
  expect_equal(
    bn_eval_1_inv$ROC[[1]]$specificities,
    c(
      0, 0, 0.1, 0.2, 0.3, 0.3, 0.3, 0.3, 0.3, 0.4, 0.5, 0.6, 0.6,
      0.6, 0.6, 0.7, 0.8, 0.8, 0.9, 0.9, 1
    )
  )

  bn_eval_2 <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "prediction",
    type = "binomial",
    apply_softmax = TRUE,
    positive = "cl_1",
    metrics = list("Accuracy" = TRUE)
  )

  expect_equal(
    bn_eval_2$Accuracy,
    mean(data_$cl_char == data_$predicted_class)
  )
  expect_equal(
    bn_eval_2$Accuracy,
    0.45
  )
  expect_equal(
    bn_eval_2$`Balanced Accuracy`,
    0.45
  )
  expect_equal(bn_eval_2$F1,
    0.4210526,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_2$Sensitivity,
    0.4,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_2$Specificity,
    0.5,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_2$`Pos Pred Value`,
    0.4444444,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_2$`Neg Pred Value`,
    0.4545455,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_2$AUC,
    0.53,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_2$`Lower CI`,
    0.2573215,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_2$`Upper CI`,
    0.8026785,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_2$Kappa,
    -0.1,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_2$MCC,
    -0.1005038,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_2$`Detection Rate`,
    0.2,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_2$`Detection Prevalence`,
    0.45,
    tolerance = 1e-4
  )
  expect_equal(bn_eval_2$Prevalence,
    0.5,
    tolerance = 1e-4
  )

  # not including predictions
  bn_eval_2_no_preds <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "prediction",
    type = "binomial",
    apply_softmax = TRUE,
    positive = "cl_1",
    metrics = list("Accuracy" = TRUE),
    include_predictions = FALSE
  )

  expect_equal(
    colnames(bn_eval_2_no_preds),
    c(
      "Balanced Accuracy", "Accuracy", "F1", "Sensitivity",
      "Specificity", "Pos Pred Value", "Neg Pred Value", "AUC",
      "Lower CI", "Upper CI", "Kappa", "MCC",
      "Detection Rate", "Detection Prevalence", "Prevalence",
      "ROC", "Confusion Matrix"
    )
  )
  expect_identical(bn_eval_2_no_preds, bn_eval_2 %>% dplyr::select(-dplyr::one_of("Predictions")))


  # TODO Create actual expected tests, where you curate a dataset, an aggregated version (all methods)
  # and make sure the results are identical in all settings.


  # ID level
  data_ <- data_ %>%
    dplyr::mutate(id = factor(rep(1:10, each = 2)))

  bn_eval_3 <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "prediction",
    id_col = "id",
    id_method = "mean",
    type = "binomial",
    apply_softmax = TRUE
  )

  expect_equal(
    colnames(bn_eval_3),
    c(
      "Balanced Accuracy", "F1", "Sensitivity",
      "Specificity", "Pos Pred Value", "Neg Pred Value", "AUC",
      "Lower CI", "Upper CI", "Kappa", "MCC",
      "Detection Rate", "Detection Prevalence", "Prevalence",
      "Predictions", "ROC", "Confusion Matrix"
    )
  )
  expect_equal(bn_eval_3$`Balanced Accuracy`, 0.6)
  expect_equal(bn_eval_3$F1, 0.6666667, tolerance = 1e-4)
  expect_equal(bn_eval_3$Sensitivity, 0.8, tolerance = 1e-4)
  expect_equal(bn_eval_3$Specificity, 0.4, tolerance = 1e-4)
  expect_equal(bn_eval_3$`Pos Pred Value`, 0.5714286, tolerance = 1e-4)
  expect_equal(bn_eval_3$`Neg Pred Value`, 0.6666667, tolerance = 1e-4)
  expect_equal(bn_eval_3$AUC, 0.52, tolerance = 1e-4)
  expect_equal(bn_eval_3$`Lower CI`, 0.1051538, tolerance = 1e-4)
  expect_equal(bn_eval_3$`Upper CI`, 0.9348462, tolerance = 1e-4)
  expect_equal(bn_eval_3$Kappa, 0.2, tolerance = 1e-4)
  expect_equal(bn_eval_3$MCC, 0.2182179, tolerance = 1e-4)
  expect_equal(bn_eval_3$`Detection Rate`, 0.4, tolerance = 1e-4)
  expect_equal(bn_eval_3$`Detection Prevalence`, 0.7, tolerance = 1e-4)
  expect_equal(bn_eval_3$Prevalence, 0.5, tolerance = 1e-4)
  expect_equal(length(bn_eval_3$Predictions), 1, tolerance = 1e-4)
  expect_equal(bn_eval_3$Predictions[[1]]$Target,
    c(
      "cl_1", "cl_1", "cl_1", "cl_1", "cl_1",
      "cl_2", "cl_2", "cl_2", "cl_2", "cl_2"
    ),
    tolerance = 1e-4
  )
  expect_equal(bn_eval_3$Predictions[[1]]$Prediction,
    c(
      0.3188163, 0.7405306, 0.5500358, 0.8027365, 0.3454502,
      0.1912657, 0.5355633, 0.6337703, 0.8547623, 0.5787402
    ),
    tolerance = 1e-4
  )
  expect_equal(bn_eval_3$Predictions[[1]]$`Predicted Class`,
    c(
      "cl_1", "cl_2", "cl_2", "cl_2", "cl_1",
      "cl_1", "cl_2", "cl_2", "cl_2", "cl_2"
    ),
    tolerance = 1e-4
  )
  expect_equal(bn_eval_3$Predictions[[1]]$id,
    factor(1:10),
    tolerance = 1e-4
  )
  expect_equal(bn_eval_3$Predictions[[1]]$id_method,
    rep("mean", 10),
    tolerance = 1e-4
  )

  bn_eval_3_no_preds <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "prediction",
    id_col = "id",
    id_method = "mean",
    type = "binomial",
    apply_softmax = TRUE,
    include_predictions = FALSE
  )

  expect_equal(
    colnames(bn_eval_3_no_preds),
    c(
      "Balanced Accuracy", "F1", "Sensitivity",
      "Specificity", "Pos Pred Value", "Neg Pred Value", "AUC",
      "Lower CI", "Upper CI", "Kappa", "MCC",
      "Detection Rate", "Detection Prevalence", "Prevalence",
      "ROC", "Confusion Matrix"
    )
  )

  # TODO ADD TESTS HERE!

  # Majority vote
  bn_eval_4 <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "prediction",
    id_col = "id",
    id_method = "majority",
    type = "binomial",
    apply_softmax = FALSE
  )


  expect_equal(
    colnames(bn_eval_4),
    c(
      "Balanced Accuracy", "F1", "Sensitivity",
      "Specificity", "Pos Pred Value", "Neg Pred Value", "AUC",
      "Lower CI", "Upper CI", "Kappa", "MCC",
      "Detection Rate", "Detection Prevalence", "Prevalence",
      "Predictions", "ROC", "Confusion Matrix"
    )
  )
  expect_equal(bn_eval_4$`Balanced Accuracy`, 0.5)
  expect_equal(bn_eval_4$F1, 0.6153846, tolerance = 1e-4)
  expect_equal(bn_eval_4$Sensitivity, 0.8, tolerance = 1e-4)
  expect_equal(bn_eval_4$Specificity, 0.2, tolerance = 1e-4)
  expect_equal(bn_eval_4$`Pos Pred Value`, 0.5, tolerance = 1e-4)
  expect_equal(bn_eval_4$`Neg Pred Value`, 0.5, tolerance = 1e-4)
  expect_equal(bn_eval_4$AUC, 0.42, tolerance = 1e-4)
  expect_equal(bn_eval_4$`Lower CI`, 0.05437346, tolerance = 1e-4)
  expect_equal(bn_eval_4$`Upper CI`, 0.7856265, tolerance = 1e-4)
  expect_equal(bn_eval_4$Kappa, 0, tolerance = 1e-4)
  expect_equal(bn_eval_4$MCC, 0, tolerance = 1e-4)
  expect_equal(bn_eval_4$`Detection Rate`, 0.4, tolerance = 1e-4)
  expect_equal(bn_eval_4$`Detection Prevalence`, 0.8, tolerance = 1e-4)
  expect_equal(bn_eval_4$Prevalence, 0.5, tolerance = 1e-4)
  expect_equal(length(bn_eval_4$Predictions), 1, tolerance = 1e-4)
  expect_equal(bn_eval_4$Predictions[[1]]$Target,
    c(
      "cl_1", "cl_1", "cl_1", "cl_1", "cl_1",
      "cl_2", "cl_2", "cl_2", "cl_2", "cl_2"
    ),
    tolerance = 1e-4
  )
  expect_equal(bn_eval_4$Predictions[[1]]$Prediction,
    c(
      1e-40, 1e+00, 5e-01, 1e+00, 5e-01,
      1e-40, 5e-01, 5e-01, 1e+00, 5e-01
    ),
    tolerance = 1e-4
  )
  expect_equal(bn_eval_4$Predictions[[1]]$`Predicted Class`,
    c(
      "cl_1", "cl_2", "cl_2", "cl_2", "cl_2",
      "cl_1", "cl_2", "cl_2", "cl_2", "cl_2"
    ),
    tolerance = 1e-4
  )
  expect_equal(bn_eval_4$Predictions[[1]]$id,
    factor(1:10),
    tolerance = 1e-4
  )
  expect_equal(bn_eval_4$Predictions[[1]]$id_method,
    rep("majority", 10),
    tolerance = 1e-4
  )

  data_2 <- data_ %>%
    dplyr::mutate(fold_ = 1) %>%
    dplyr::bind_rows(data_ %>% dplyr::mutate(fold_ = 2))

  bn_eval_5 <- evaluate(
    data = data_2 %>% dplyr::group_by(fold_),
    target_col = "cl_char",
    prediction_cols = "prediction",
    id_col = "id",
    id_method = "majority",
    type = "binomial",
    apply_softmax = FALSE
  )

  # TODO Add tests here that grouped dataframes work in binomial!


  # Errors

  expect_error(evaluate(
    data = data_ %>% dplyr::mutate(
      cl = ifelse(dplyr::row_number() == 5, 3, cl),
      cl_char = ifelse(dplyr::row_number() == 5, "cl_3", cl_char)
    ),
    target_col = "cl_char",
    prediction_cols = "prediction",
    type = "binomial",
    apply_softmax = TRUE,
    metrics = list("Accuracy" = TRUE)
  ),
  "The target column must maximally contain 2 levels.",
  fixed = TRUE
  )
})

test_that("softmax works in multiclass_probability_tibble()", {

  # Test softmax was applied correctly in multiclass_probability_tibble
  xpectr::set_test_seed(1)
  random_probabilities_1 <- multiclass_probability_tibble(
    num_classes = 3,
    num_observations = 20,
    apply_softmax = TRUE
  )
  xpectr::set_test_seed(1)
  random_probabilities_2 <- multiclass_probability_tibble(
    num_classes = 3,
    num_observations = 20,
    apply_softmax = FALSE
  ) %>% softmax()

  expect_equal(sum(random_probabilities_1), sum(random_probabilities_2))
  expect_equal(sum(random_probabilities_1), 20) # due to softmax, each row sums to 1

  expect_equal(sum(softmax_row(c(1, 2, 3, 4))), 1)
  expect_equal(as.vector(t(softmax_row(c(1, 2, 3, 4)))),
    c(0.03205860, 0.08714432, 0.23688282, 0.64391426),
    tolerance = 1e-4
  )
  expect_equal(colnames(softmax_row(c(1, 2, 3, 4))), c("V1", "V2", "V3", "V4"))
})

test_that("probability nesting works in multinomial evaluate", {
  xpectr::set_test_seed(1)
  random_probabilities_1 <- multiclass_probability_tibble(
    num_classes = 3,
    num_observations = 20,
    apply_softmax = TRUE
  )

  system.time({
    manually_nested_probs <- random_probabilities_1 %>%
      dplyr::mutate(ind = 1:20) %>%
      dplyr::group_by(ind) %>%
      legacy_nest(1:3) %>%
      dplyr::pull(.data$data)
  })

  # Changed to basically do the same as above
  system.time({
    package_nested_probs <- random_probabilities_1 %>%
      nest_rowwise()
  })

  # group_nest adds an attribute
  attr(package_nested_probs, "ptype") <- NULL

  expect_true(identical(manually_nested_probs, package_nested_probs))

  unnested <- package_nested_probs %>%
    dplyr::bind_rows()

  expect_true(identical(random_probabilities_1, unnested))
})

test_that("gaussian evaluations are correct in evaluate()", {
  xpectr::set_test_seed(1)

  score_model_1 <- lm("score ~ diagnosis", data = participant.scores)
  score_model_2 <- lm("score ~ diagnosis+age", data = participant.scores)

  # summary(score_model_1)

  score_predictions <- stats::predict(score_model_1, participant.scores,
    type = "response", allow.new.levels = TRUE
  )

  eval_data <- participant.scores
  eval_data[["score_predictions"]] <- score_predictions

  expect_error(evaluate(eval_data,
    target_col = "score",
    prediction_cols = "score_predictions",
    models = list(score_model_1),
    type = "gaussian",
    metrics = "all"
  ),
  class = "lifecycle_error_deprecated"
  )

  e1 <- evaluate(eval_data,
    target_col = "score",
    prediction_cols = "score_predictions",
    type = "gaussian",
    metrics = "all"
  )

  expect_equal(
    colnames(e1),
    c(
      "RMSE", "MAE", "NRMSE", "RMSEIQR", "RMSESTD",
      "RMSLE", "MALE",
      "RAE", "RSE", "RRSE",
      "MAPE", "MSE", "TAE", "TSE",
      "Predictions"
    )
  )

  expect_equal(e1$RMSE, 16.16881, tolerance = 1e-4)
  expect_equal(e1$MAE, 13.47778, tolerance = 1e-4)
  expect_equal(e1$NRMSE, 0.2277298, tolerance = 1e-4)
  expect_equal(e1$RMSEIQR, 0.5774577, tolerance = 1e-4)
  expect_equal(e1$RMSESTD, 0.8380279, tolerance = 1e-4)
  expect_equal(e1$RMSLE, 0.4677011, tolerance = 1e-4)
  expect_equal(e1$MALE, 0.3768815, tolerance = 1e-4)
  expect_equal(e1$RAE, 0.8666762, tolerance = 1e-4)
  expect_equal(e1$RSE, 0.7265077, tolerance = 1e-4)
  expect_equal(e1$RRSE, 0.8523542, tolerance = 1e-4)
  expect_equal(e1$MAPE, 0.4836546, tolerance = 1e-4)
  expect_equal(e1$MSE, 261.4306, tolerance = 1e-4)
  expect_equal(e1$TAE, 404.3333, tolerance = 1e-4)
  expect_equal(e1$TSE, 7842.917, tolerance = 1e-4)
  expect_equal(e1$Predictions[[1]][["Target"]], eval_data$score)
  expect_equal(e1$Predictions[[1]][["Prediction"]], eval_data$score_predictions)

  # Not passing a model
  # This should remove the metrics that depend on the models to be passed
  e2 <- evaluate(eval_data,
    target_col = "score",
    prediction_cols = "score_predictions",
    type = "gaussian",
    metrics = "all"
  )

  expect_equal(
    colnames(e2),
    c(
      "RMSE", "MAE", "NRMSE", "RMSEIQR", "RMSESTD",
      "RMSLE", "MALE", "RAE", "RSE", "RRSE",
      "MAPE", "MSE", "TAE", "TSE", "Predictions"
    )
  )
  expect_equal(e2$RMSE, 16.16881, tolerance = 1e-4)
  expect_equal(e2$MAE, 13.47778, tolerance = 1e-4)
  expect_equal(e2$RMSLE, 0.4677011, tolerance = 1e-4)
  expect_equal(e2$MALE, 0.3768815, tolerance = 1e-4)
  expect_equal(e2$Predictions[[1]][["Target"]], eval_data$score)
  expect_equal(e2$Predictions[[1]][["Prediction"]], eval_data$score_predictions)

  # Grouped with multiple models

  eval_data_2 <- eval_data

  score_predictions_2 <- stats::predict(score_model_2, participant.scores,
    type = "response", allow.new.levels = TRUE
  )

  eval_data_2[["score_predictions"]] <- score_predictions_2

  e3 <- evaluate(eval_data_2,
    target_col = "score",
    prediction_cols = "score_predictions",
    type = "gaussian",
    metrics = "all"
  )

  expect_equal(
    colnames(e3),
    c(
      "RMSE", "MAE", "NRMSE", "RMSEIQR", "RMSESTD",
      "RMSLE", "MALE", "RAE", "RSE", "RRSE",
      "MAPE", "MSE", "TAE", "TSE",
      "Predictions"
    )
  )

  eval_data_3 <- dplyr::bind_rows(
    eval_data %>% dplyr::mutate(fold_ = 1),
    eval_data_2 %>% dplyr::mutate(fold_ = 2)
  ) %>%
    dplyr::group_by(fold_)

  # eval_data_3 %>% dplyr::group_keys()
  # eval_data_3 %>% dplyr::group_indices()

  e4 <- evaluate(eval_data_3,
    target_col = "score",
    prediction_cols = "score_predictions",
    type = "gaussian",
    metrics = "all"
  ) %>%
    dplyr::mutate(fold_ = as.factor(.data$fold_))

  expect_equal(
    colnames(e4),
    c(
      "fold_", "RMSE", "MAE", "NRMSE", "RMSEIQR", "RMSESTD",
      "RMSLE", "MALE", "RAE", "RSE", "RRSE", "MAPE", "MSE", "TAE", "TSE",
      "Predictions"
    )
  )

  e1_e3 <- dplyr::bind_rows(e1, e3) %>%
    dplyr::mutate(fold_ = factor(1:2)) %>%
    dplyr::select(.data$fold_, dplyr::everything())
  e1_e3$Predictions[[1]] <- e1_e3$Predictions[[1]] %>%
    dplyr::as_tibble() %>%
    tibble::add_column("fold_" = 1, .before = "Target")
  e1_e3$Predictions[[2]] <- e1_e3$Predictions[[2]] %>%
    dplyr::as_tibble() %>%
    tibble::add_column("fold_" = 2, .before = "Target")

  expect_true(length(setdiff(colnames(e4), colnames(e1_e3))) == 0)
  expect_identical(e4, e1_e3)

  expect_equal(e4$fold_, factor(c(1, 2)))
  expect_equal(e4$RMSE, c(16.16881, 16.12762), tolerance = 1e-4)
  expect_equal(e4$MAE, c(13.47778, 13.28942), tolerance = 1e-4)
  expect_equal(e4$NRMSE, c(0.227729778737206, 0.227149512023389), tolerance = 1e-4)
  expect_equal(e4$RMSEIQR, c(0.577457653226487, 0.575986262630736), tolerance = 1e-4)
  expect_equal(e4$RMSESTD, c(0.838027891023239, 0.835892554603436), tolerance = 1e-4)
  expect_equal(e4$RMSLE, c(0.4677011, 0.4666284), tolerance = 1e-4)
  expect_equal(e4$MALE, c(0.3768815, 0.3723774), tolerance = 1e-4)
  expect_equal(e4$RAE, c(0.866676193198057, 0.8545638890023), tolerance = 1e-4)
  expect_equal(e4$RSE, c(0.852354191878764, 0.850182351337433)^2, tolerance = 1e-4)
  expect_equal(e4$RRSE, c(0.852354191878764, 0.850182351337433), tolerance = 1e-4)
  expect_equal(e4$MAPE, c(0.483654620140199, 0.478764805777454), tolerance = 1e-4)
  expect_equal(e4$MSE, c(261.430555555556, 260.099976995629), tolerance = 1e-4)
  expect_equal(e4$TAE, c(404.333333333333, 398.68253968254), tolerance = 1e-4)
  expect_equal(e4$TSE, c(7842.91666666667, 7802.99930986888), tolerance = 1e-4)

  expect_equal(e4$Predictions[[1]]$Target,
    c(
      10, 24, 45, 24, 40, 67, 15, 30, 40, 35, 50, 78, 24, 54, 62,
      14, 25, 30, 11, 35, 41, 16, 32, 44, 33, 53, 66, 29, 55, 81
    ),
    tolerance = 1e-4
  )
  expect_equal(e4$Predictions[[2]]$Target,
    c(
      10, 24, 45, 24, 40, 67, 15, 30, 40, 35, 50, 78, 24, 54, 62,
      14, 25, 30, 11, 35, 41, 16, 32, 44, 33, 53, 66, 29, 55, 81
    ),
    tolerance = 1e-4
  )
  expect_equal(e4$Predictions[[1]]$Prediction,
    c(
      30.66667, 30.66667, 30.66667, 50.91667, 50.91667,
      50.91667, 30.66667, 30.66667, 30.66667, 50.91667,
      50.91667, 50.91667, 30.66667, 30.66667, 30.66667,
      30.66667, 30.66667, 30.66667, 30.66667, 30.66667,
      30.66667, 30.66667, 30.66667, 30.66667, 50.91667,
      50.91667, 50.91667, 50.91667, 50.91667, 50.91667
    ),
    tolerance = 1e-4
  )
  expect_equal(e4$Predictions[[2]]$Prediction,
    c(
      29.17288, 29.17288, 29.17288, 50.16977, 50.16977,
      50.16977, 30.33471, 30.33471, 30.33471, 49.83782,
      49.83782, 49.83782, 31.16460, 31.16460, 31.16460,
      30.99862, 30.99862, 30.99862, 32.99034, 32.99034,
      32.99034, 29.33885, 29.33885, 29.33885, 51.99551,
      51.99551, 51.99551, 51.66356, 51.66356, 51.66356
    ),
    tolerance = 1e-4
  )

  # ID evaluation
  age_model_1 <- lm("age ~ diagnosis", participant.scores)
  age_model_2 <- lm("age ~ diagnosis + score", participant.scores)
  age_predictions_1 <- stats::predict(age_model_1, participant.scores,
    type = "response", allow.new.levels = TRUE
  )
  age_predictions_2 <- stats::predict(age_model_2, participant.scores,
    type = "response", allow.new.levels = TRUE
  )

  id_eval_data_4 <- participant.scores %>%
    dplyr::mutate(
      fold_ = 1,
      predicted_age = age_predictions_1
    ) %>%
    dplyr::bind_rows(participant.scores %>%
      dplyr::mutate(
        fold_ = 2,
        predicted_age = age_predictions_2
      )) %>%
    dplyr::group_by(fold_)

  e5 <- evaluate(id_eval_data_4,
    target_col = "age",
    prediction_cols = "predicted_age",
    id_col = "participant",
    id_method = "mean",
    type = "gaussian",
    metrics = "all"
  )

  expect_equal(e5$fold_, c(1, 2))
  expect_equal(e5$RMSE, c(6.949820, 6.917232), tolerance = 1e-4)
  expect_equal(e5$MAE, c(6.0, 5.935604), tolerance = 1e-4)
  expect_equal(e5$NRMSE, c(0.302166093111201, 0.300749209434143), tolerance = 1e-4)
  expect_equal(e5$RMSEIQR, c(0.661887632529297, 0.658783982570027), tolerance = 1e-4)
  expect_equal(e5$RMSESTD, c(0.943424157020128, 0.939000357265295), tolerance = 1e-4)
  expect_equal(e5$RMSLE, c(0.234168121096768, 0.23292937565111), tolerance = 1e-4)
  expect_equal(e5$MALE, c(0.206142303617716, 0.203966962001996), tolerance = 1e-4)
  expect_equal(e5$RAE, c(1, 0.989267385645221), tolerance = 1e-4)
  expect_equal(e5$RSE, c(0.994456378602646, 0.98979328422339)^2, tolerance = 1e-4)
  expect_equal(e5$RRSE, c(0.994456378602646, 0.98979328422339), tolerance = 1e-4)
  expect_equal(e5$RRSE, c(0.994456378602646, 0.98979328422339), tolerance = 1e-4)
  expect_equal(e5$MAPE, c(0.222585143343264, 0.220163297924543), tolerance = 1e-4)
  expect_equal(e5$MSE, c(48.3, 47.8480960099135), tolerance = 1e-4)
  expect_equal(e5$TAE, c(60, 59.3560431387133), tolerance = 1e-4)
  expect_equal(e5$TSE, c(483, 478.480960099135), tolerance = 1e-4)

  expect_equal(length(e5$Predictions), 2, tolerance = 1e-4)
  expect_equal(colnames(e5$Predictions[[1]]),
    c("fold_", "Target", "Prediction", "participant", "id_method"),
    tolerance = 1e-4
  )
  expect_equal(colnames(e5$Predictions[[2]]),
    c("fold_", "Target", "Prediction", "participant", "id_method"),
    tolerance = 1e-4
  )
  expect_equal(e5$Predictions[[1]]$Target,
    c(20, 23, 27, 21, 32, 31, 43, 21, 34, 32),
    tolerance = 1e-4
  )
  expect_equal(e5$Predictions[[2]]$Target,
    c(20, 23, 27, 21, 32, 31, 43, 21, 34, 32),
    tolerance = 1e-4
  )
  expect_equal(e5$Predictions[[1]]$Prediction,
    c(29.0, 27.5, 29.0, 27.5, 29.0, 29.0, 29.0, 29.0, 27.5, 27.5),
    tolerance = 1e-4
  )
  expect_equal(e5$Predictions[[2]]$Prediction,
    c(
      28.86712, 27.27768, 28.92845, 27.60477, 29.49063,
      28.76490, 28.94889, 29.00000, 27.49233, 27.62521
    ),
    tolerance = 1e-4
  )
  expect_equal(e5$Predictions[[1]]$participant,
    factor(1:10),
    tolerance = 1e-4
  )
  expect_equal(e5$Predictions[[2]]$participant,
    factor(1:10),
    tolerance = 1e-4
  )
  expect_equal(e5$Predictions[[1]]$id_method,
    rep("mean", 10),
    tolerance = 1e-4
  )
  expect_equal(e5$Predictions[[2]]$id_method,
    rep("mean", 10),
    tolerance = 1e-4
  )

  # Not including predictions in the output
  e6 <- evaluate(id_eval_data_4,
    target_col = "age",
    prediction_cols = "predicted_age",
    id_col = "participant",
    id_method = "mean",
    type = "gaussian",
    metrics = "all",
    include_predictions = FALSE
  )

  expect_equal(
    colnames(e6),
    c(
      "fold_", "RMSE", "MAE", "NRMSE", "RMSEIQR", "RMSESTD",
      "RMSLE", "MALE", "RAE", "RSE", "RRSE", "MAPE", "MSE", "TAE", "TSE"
    )
  )
})

test_that("evaluate() treats dfs and tbls the same", {

  # Gaussian

  # Binomial

  # Multinomial
  xpectr::set_test_seed(1)
  random_probabilities <- multiclass_probability_tibble(
    num_classes = 5,
    num_observations = 20,
    apply_softmax = FALSE # Test with as well
  )
  expect_equal(sum(random_probabilities), 51.78471, tolerance = 1e-5)

  data_ <- random_probabilities %>%
    dplyr::mutate(
      cl = as.factor(rep(1:5, each = 4)),
      cl_char = paste0("cl_", cl)
    ) %>%
    dplyr::rename_at(dplyr::vars(paste0("class_", 1:5)), .funs = ~ paste0("cl_", 1:5))

  mn_eval_1_tbl <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multinomial",
    apply_softmax = TRUE
  )

  mn_eval_1_df <- evaluate(
    data = as.data.frame(data_),
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multinomial",
    apply_softmax = TRUE
  )

  mn_eval_1_dt <- evaluate( # TODO Need to test this for gaussian and binomial as well!!!
    data = as.data.table(data_),
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multinomial",
    apply_softmax = TRUE
  )

  expect_identical(
    mn_eval_1_df$`Class Level Results`[[1]]$`Confusion Matrix`,
    mn_eval_1_tbl$`Class Level Results`[[1]]$`Confusion Matrix`
  )
  expect_identical(
    mn_eval_1_df$`Confusion Matrix`,
    mn_eval_1_tbl$`Confusion Matrix`
  )
  expect_identical(mn_eval_1_tbl, mn_eval_1_df)

  # TODO Find out why the group_nest attribute is only added to DT ?
  attr(mn_eval_1_dt$Predictions[[1]]$Prediction, "ptype") <- NULL

  # There is a "attr(*, ".internal.selfref")=<externalptr> " attribute added to the
  # predictions list with the data.table.
  expect_identical(
    mn_eval_1_tbl$Predictions[[1]]$Prediction,
    mn_eval_1_dt$Predictions[[1]]$Prediction
  )
  mn_eval_1_dt$Predictions <- NULL
  mn_eval_1_tbl$Predictions <- NULL
  expect_identical(mn_eval_1_dt, mn_eval_1_tbl)
})

# test_that("profiling",{
#
#   # Load file with prepared predictions and hparams
#   load(file="")
#
#   evals <- predictions %>%
#     dplyr::group_by(results_folder, epoch) %>%
#     cvms:::evaluate(target_col = "target_string",
#                     prediction_cols = current_hparams %>%
#                       dplyr::arrange(class_indices_map_values) %>%
#                       dplyr::pull(class_names) %>%
#                       as.character(),
#                     type = "multinomial",
#                     apply_softmax = FALSE,
#                     parallel = TRUE)
#
#
# })

test_that("evaluate() works with wines dataset", {
  xpectr::set_test_seed(1)

  # Load wines dataset
  w <- wines
  varieties <- unique(as.character(w$Variety))

  ## Create All_x one-vs-all evaluations

  to_evaluate <- plyr::llply(varieties, function(vary) {
    d <- w
    d[["label"]] <- ifelse(as.character(d$Variety) == vary, 1, 0)
    d[["current_variety"]] <- vary
    d[["probability"]] <- matrix(0, nrow = 1, ncol = length(varieties)) %>%
      as.data.frame() %>%
      setNames(varieties) %>%
      dplyr::mutate_at(.vars = vary, .funs = ~1) %>%
      list()

    d
  }) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(prediction = 1)

  evaluations <- to_evaluate %>%
    dplyr::group_by(current_variety) %>%
    evaluate(
      target_col = "label",
      prediction_cols = "prediction",
      type = "binomial",
      metrics = list("Accuracy" = TRUE)
    ) %>%
    dplyr::arrange(current_variety)

  expect_equal(sort(evaluations$current_variety), sort(varieties))
  expect_equal(evaluations$`Balanced Accuracy`, rep(0.5, length(varieties)))
  expect_equal(evaluations$Accuracy,
    c(
      0.0271739130434783, 0.0217391304347826, 0.0380434782608696,
      0.0108695652173913, 0.00815217391304348, 0.0135869565217391,
      0.513586956521739, 0.0570652173913043, 0.201086956521739, 0.108695652173913
    ),
    tolerance = 1e-4
  )
  expect_equal(evaluations$F1,
    c(
      0.0529100529100529, 0.0425531914893617, 0.0732984293193717,
      0.021505376344086, 0.0161725067385445, 0.0268096514745308, 0.678635547576302,
      0.107969151670951, 0.334841628959276, 0.196078431372549
    ),
    tolerance = 1e-4
  )
  expect_equal(evaluations$Sensitivity, c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), tolerance = 1e-4)
  expect_equal(evaluations$Specificity, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), tolerance = 1e-4)
  expect_equal(evaluations$`Pos Pred Value`,
    c(
      0.0271739130434783, 0.0217391304347826, 0.0380434782608696,
      0.0108695652173913, 0.00815217391304348, 0.0135869565217391,
      0.513586956521739, 0.0570652173913043, 0.201086956521739, 0.108695652173913
    ),
    tolerance = 1e-4
  )
  expect_equal(evaluations$`Neg Pred Value`,
    c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN),
    tolerance = 1e-4
  )
  expect_equal(evaluations$AUC,
    c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    tolerance = 1e-4
  )
  expect_equal(evaluations$`Lower CI`,
    c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    tolerance = 1e-4
  )
  expect_equal(evaluations$`Upper CI`,
    c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    tolerance = 1e-4
  )
  expect_equal(evaluations$Kappa,
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    tolerance = 1e-4
  )
  expect_equal(evaluations$MCC,
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    tolerance = 1e-4
  )
  expect_equal(evaluations$`Detection Rate`,
    c(
      0.0271739130434783, 0.0217391304347826, 0.0380434782608696,
      0.0108695652173913, 0.00815217391304348, 0.0135869565217391,
      0.513586956521739, 0.0570652173913043, 0.201086956521739, 0.108695652173913
    ),
    tolerance = 1e-4
  )
  expect_equal(evaluations$`Detection Prevalence`,
    c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    tolerance = 1e-4
  )
  expect_equal(evaluations$Prevalence,
    c(
      0.0271739130434783, 0.0217391304347826, 0.0380434782608696,
      0.0108695652173913, 0.00815217391304348, 0.0135869565217391,
      0.513586956521739, 0.0570652173913043, 0.201086956521739, 0.108695652173913
    ),
    tolerance = 1e-4
  )
  expect_equal(evaluations$`Confusion Matrix`[[1]]$N,
    c(0L, 358L, 0L, 10L),
    tolerance = 1e-4
  )

  ## Create All_x multinomial evaluations

  mn_evaluations <- to_evaluate %>%
    legacy_unnest(probability) %>%
    dplyr::group_by(current_variety) %>%
    evaluate(
      target_col = "Variety",
      prediction_cols = varieties,
      type = "multinomial",
      metrics = list("Accuracy" = TRUE)
    ) %>%
    dplyr::arrange(current_variety)

  expect_equal(mn_evaluations$`Class Level Results`[[1]]$Accuracy,
    c(
      0.486413043478261, 0.798913043478261, 0.891304347826087, 0.942934782608696,
      0.96195652173913, 0.0271739130434783, 0.978260869565217, 0.986413043478261,
      0.989130434782609, 0.991847826086957
    ),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$`Class Level Results`[[1]]$F1,
    c(
      NaN, NaN, NaN, NaN, NaN, 0.0529100529100529, NaN, NaN, NaN,
      NaN
    ),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$`Overall Accuracy`,
    c(
      0.0271739130434783, 0.0217391304347826, 0.0380434782608696,
      0.0108695652173913, 0.00815217391304348, 0.0135869565217391,
      0.513586956521739, 0.0570652173913043, 0.201086956521739, 0.108695652173913
    ),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$`Balanced Accuracy`,
    c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$Accuracy,
    c(
      0.805434782608696, 0.804347826086957, 0.807608695652174, 0.802173913043478,
      0.801630434782609, 0.802717391304348, 0.902717391304348, 0.811413043478261,
      0.840217391304348, 0.821739130434783
    ),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$F1,
    c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$Sensitivity,
    c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$Specificity,
    c(0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9, 0.9),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$`Pos Pred Value`,
    c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$`Neg Pred Value`,
    c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$Kappa,
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$MCC,
    c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$`Detection Rate`,
    c(
      0.00271739130434783, 0.00217391304347826, 0.00380434782608696,
      0.00108695652173913, 0.000815217391304348, 0.00135869565217391,
      0.0513586956521739, 0.00570652173913043, 0.0201086956521739,
      0.0108695652173913
    ),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$`Detection Prevalence`,
    c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$Prevalence,
    c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1),
    tolerance = 1e-4
  )
  expect_equal(mn_evaluations$`Confusion Matrix`[[1]]$N,
    c(
      10L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 8L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 0L, 14L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
      4L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 3L, 0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 5L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 189L,
      0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 21L, 0L, 0L, 0L, 0L, 0L,
      0L, 0L, 0L, 0L, 74L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 40L,
      0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L
    ),
    tolerance = 1e-4
  )


  # Check that these All_x evaluations are the same in baseline()

  xpectr::set_test_seed(1)
  suppressWarnings(bsl <- baseline(
    w,
    dependent_col = "Variety",
    n = 1,
    family = "multinomial",
    metrics = list("Accuracy" = TRUE)
  ))

  all_x_baseline_results <- bsl$summarized_metrics[bsl$summarized_metrics$Measure %in% paste0("All_", varieties), ]
  expect_equal(all_x_baseline_results$`Overall Accuracy`, evaluations$Accuracy, tolerance = 1e-4)
  expect_equal(mn_evaluations$`Overall Accuracy`, evaluations$Accuracy, tolerance = 1e-4)
  expect_equal(all_x_baseline_results$`Balanced Accuracy`, mn_evaluations$`Balanced Accuracy`, tolerance = 1e-4)
  expect_equal(all_x_baseline_results$Accuracy, mn_evaluations$Accuracy, tolerance = 1e-4)
  expect_equal(all_x_baseline_results$F1, mn_evaluations$F1, tolerance = 1e-4)
  expect_equal(all_x_baseline_results$Sensitivity, mn_evaluations$Sensitivity, tolerance = 1e-4)
  expect_equal(all_x_baseline_results$Specificity, mn_evaluations$Specificity, tolerance = 1e-4)
  expect_equal(all_x_baseline_results$`Pos Pred Value`, mn_evaluations$`Pos Pred Value`, tolerance = 1e-4)
  expect_equal(all_x_baseline_results$`Neg Pred Value`, mn_evaluations$`Neg Pred Value`, tolerance = 1e-4)
  expect_equal(all_x_baseline_results$Kappa, mn_evaluations$Kappa, tolerance = 1e-4)
  expect_equal(all_x_baseline_results$MCC, mn_evaluations$MCC, tolerance = 1e-4)
  expect_equal(all_x_baseline_results$`Detection Rate`, mn_evaluations$`Detection Rate`, tolerance = 1e-4)
  expect_equal(all_x_baseline_results$`Detection Prevalence`, mn_evaluations$`Detection Prevalence`, tolerance = 1e-4)
  expect_equal(all_x_baseline_results$Prevalence, mn_evaluations$Prevalence, tolerance = 1e-4)
})


test_that("evaluate() is agnostic about the order of the input data", {
  dat <- data.frame(
    "target" = c(2, 1, 2, 1, 2, 1, 1, 1, 2, 2, 2, 1, 2, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1),
    "prediction" = c(
      0.7, 0.3, 0.1, 0.1, 0.9, 0.3, 0.8, 0.7, 0.9, 0.1, 0.9, 0.1,
      0.2, 0.4, 0.2, 0.6, 0.1, 0.6, 0.1, 0.2, 0.1, 0.8, 0.3
    ),
    ".group" = paste0("cl_", c(1, 3, 2, 2, 3, 1, 3, 1, 2, 3, 2, 1, 2, 1, 1, 3, 3, 2, 1, 3, 2, 1, 1))
  )

  eval_1 <- dat %>%
    dplyr::group_by(.data$.group) %>%
    evaluate(target_col = "target", prediction_cols = "prediction", type = "binomial")

  eval_2 <- dat %>%
    dplyr::arrange(.data$.group) %>%
    dplyr::group_by(.data$.group) %>%
    evaluate(target_col = "target", prediction_cols = "prediction", type = "binomial")

  eval_3 <- dat %>%
    dplyr::arrange(dplyr::desc(.data$.group)) %>%
    dplyr::group_by(.data$.group) %>%
    evaluate(target_col = "target", prediction_cols = "prediction", type = "binomial")

  expect_identical(eval_1[, 1:15], eval_2[, 1:15])
  expect_identical(eval_1[, 1:15], eval_3[, 1:15])

  eval_group_1 <- dat %>%
    dplyr::filter(.data$.group == "cl_1") %>%
    evaluate(target_col = "target", prediction_cols = "prediction", type = "binomial")

  eval_group_2 <- dat %>%
    dplyr::filter(.data$.group == "cl_2") %>%
    evaluate(target_col = "target", prediction_cols = "prediction", type = "binomial")

  eval_group_3 <- dat %>%
    dplyr::filter(.data$.group == "cl_3") %>%
    evaluate(target_col = "target", prediction_cols = "prediction", type = "binomial")

  eval_groups <- dplyr::bind_rows(
    eval_group_1, eval_group_2, eval_group_3
  )

  expect_equal(eval_1[, 2:15], eval_groups[, 1:14])
})
