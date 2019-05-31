library(cvms)
context("baseline()")


test_that("binomial evaluation are correct in baseline()",{

  set.seed(1)
  binom_baseline <- baseline(test_data = participant.scores,
                    dependent_col = "diagnosis",
                    n = 10,
                    family = "binomial",
                    parallel = FALSE)

  binom_baseline_summ <- binom_baseline$summarized_metrics
  binom_baseline_reval <- binom_baseline$random_evaluations

  # Summarized results
  expect_equal(binom_baseline_summ$Measure, c("Mean", "Median", "SD", "IQR", "Max",
                                              "Min", "NAs", "INFs", "All_0", "All_1"))
  expect_equal(binom_baseline_summ$`Balanced Accuracy`,
               c(0.49305556, 0.472, 0.09402822, 0.111, 0.6805, 0.3472,
                 0.0, 0.0, 0.50, 0.50), tolerance=1e-3)

  expect_equal(binom_baseline_summ$F1,
               c(0.5080706, 0.5294118, 0.1486011, 0.1303922, 0.6875000,
                 0.1600000, 0.0000000, 0.0000000, NA, 0.7500000), tolerance=1e-3)

  expect_equal(binom_baseline_summ$AUC,
               c(0.54907407, 0.54398148, 0.08210109, 0.09143519,
                 0.69907407, 0.43055556, 0.0, 0.0, 0.50, 0.50), tolerance=1e-3)

  expect_equal(binom_baseline_summ$`Pos Pred Value`,
               c(0.5751821, 0.5729167, 0.1299251, 0.1009191,
                 0.7857143, 0.2857143, 0.0, 0.0, NaN, 0.60), tolerance=1e-3)

  expect_equal(binom_baseline_summ$`Neg Pred Value`,
               c(0.39934657, 0.37301587, 0.07978087, 0.11401099,
                 0.5625, 0.30434783, 0.0, 0.0, 0.40, NaN), tolerance=1e-3)

  # The random evaluations
  expect_equal(binom_baseline_reval$`Balanced Accuracy`,
               c(0.6805556, 0.5277778, 0.4861111, 0.4305556, 0.5555556,
                 0.4583333, 0.4583333, 0.5694444, 0.4166667, 0.3472222), tolerance=1e-3)

  expect_equal(binom_baseline_reval$F1,
               c(0.6875, 0.5882353, 0.4666667, 0.4848485, 0.6285714,
                 0.5294118, 0.5294118, 0.6060606, 0.40, 0.160), tolerance=1e-3)

  expect_equal(binom_baseline_reval$AUC,
               c(0.6990741, 0.5925926, 0.6527778, 0.500, 0.5555556,
                 0.4305556, 0.5601852, 0.5324074, 0.4907407, 0.4768519),
               tolerance=1e-3)

  expect_equal(binom_baseline_reval$MCC,
               c(0.35460407, 0.05455447, -0.0277, -0.13608276, 0.10984701,
                 -0.08183171, -0.08183171,  0.13608276, -0.166,-0.35391920),
               tolerance=1e-3)

  expect_equal(binom_baseline_reval$Dependent,
               rep("diagnosis", 10))

  expect_equal(binom_baseline_reval$Family,
               rep("binomial", 10))

  expect_equal(length(binom_baseline_reval$Predictions), 10)

  all_predictions <- dplyr::bind_rows(binom_baseline_reval$Predictions)
  expect_equal(sum(as.numeric(all_predictions$Fold)), 300)
  expect_equal(sum(all_predictions$Target), 180)
  expect_equal(sum(as.numeric(all_predictions$`Predicted Class`)), 140)
  expect_equal(sum(all_predictions$Prediction), 147.0334, tolerance=1e-3)

  all_confmats <- dplyr::bind_rows(binom_baseline_reval$`Confusion Matrix`)
  expect_equal(sum(as.numeric(all_confmats$Prediction)), 20, tolerance=1e-3)
  expect_equal(sum(as.numeric(all_confmats$Reference)), 20, tolerance=1e-3)
  expect_equal(sum(all_confmats$N), 300, tolerance=1e-3)
  expect_equal(sum(all_confmats$`Fold Column`), 220, tolerance=1e-3) # Should be changed to Evaluation


})

test_that("gaussian evaluation are correct in baseline()",{

  set.seed(1)
  dat <- groupdata2::partition(participant.scores, p = 0.6, list_out = TRUE)
  train_data <- dat[[1]]
  test_data <- dat[[2]]
  gaussian_baseline <- baseline(test_data = test_data,
                             train_data = train_data,
                             dependent_col = "diagnosis",
                             n = 10,
                             family = "gaussian",
                             parallel = FALSE)

  gaussian_baseline_summ <- gaussian_baseline$summarized_metrics
  gaussian_baseline_reval <- gaussian_baseline$random_evaluations

  # Summarized results
  expect_equal(gaussian_baseline_summ$Measure, c("Mean", "Median", "SD", "IQR", "Max",
                                              "Min", "NAs", "INFs", "All_rows"))
  expect_equal(gaussian_baseline_summ$RMSE,
               c(0.53817815, 0.53278138, 0.03113600, 0.02189922,
                 0.60092521, 0.50507627, 0.0, 0.0, 0.52704628), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$MAE,
               c(0.5, 0.5, 0.0, 0.0, 0.5, 0.5, 0.0, 0.0, 0.5), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$AIC,
               c(16.483477, 16.772293, 5.093491, 6.335927, 24.033488,
                 9.026478, 0.0, 0.0, 28.008394), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$`Training Rows`,
               c(9.60, 10.0, 3.062316, 4.50, 14.0, 5.0, 0.0, 0.0, 18.0), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$r2m,
               rep(0.0, 9), tolerance=1e-3)
  expect_equal(gaussian_baseline_summ$r2c,
               rep(0.0, 9), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$AICc,
               c(18.931616, 18.486579, 3.681980, 4.335927,
                 25.124397, 13.182776, 0.0, 0.0, 28.808394), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$BIC,
               c(16.901920, 17.377463, 5.787432, 7.375648,
                 25.311603, 8.245354, 0.0, 0.0, 29.789138), tolerance=1e-3)

  # The random evaluations
  expect_equal(gaussian_baseline_reval$RMSE,
               c(0.6009252, 0.5270463, 0.5385165, 0.5131409, 0.5050763,
                 0.5270463, 0.5385165, 0.5830952, 0.5385165, 0.5099020), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$MAE,
               rep(0.5, 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$AIC,
               c(9.182776, 12.002798, 16.772293, 22.159151, 24.033488,
                 20.005596, 16.772293, 9.026478, 16.772293, 18.107607), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$`Training Rows`,
               c(6, 6, 10, 13, 14, 12, 10, 5, 10, 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Family,
               rep("gaussian", 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Dependent,
               rep("diagnosis", 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Fixed,
               rep("1", 10), tolerance=1e-3)

  all_predictions <- dplyr::bind_rows(gaussian_baseline_reval$Predictions)
  expect_equal(sum(as.numeric(all_predictions$Fold)), 120)
  expect_equal(sum(all_predictions$Target), 60)
  expect_equal(sum(all_predictions$Prediction), 82.24176, tolerance=1e-3)

  all_coeffs <- dplyr::bind_rows(gaussian_baseline_reval$Coefficients)
  expect_equal(all_coeffs$estimate,
               c(0.8333333, 0.6666667, 0.70, 0.6153846, 0.5714286,
                 0.6666667, 0.70, 0.80, 0.70, 0.60), tolerance=1e-3)
  expect_equal(all_coeffs$std.error,
               c(0.1666667, 0.2108185, 0.1527525, 0.1404417, 0.1372527,
                 0.1421338, 0.1527525, 0.20, 0.1527525, 0.1632993), tolerance=1e-3)
  expect_equal(all_coeffs$p.value,
               c(0.0041047160, 0.0250310158, 0.0013229506, 0.0008935055, 0.0011131538,
                 0.0006603135, 0.0013229506, 0.0161300899, 0.0013229506, 0.0051210728), tolerance=1e-3)
})
