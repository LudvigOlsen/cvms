library(cvms)
context("baseline()")


test_that("binomial evaluation are correct in baseline()",{

  set_seed_for_R_compatibility(1)
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

  set_seed_for_R_compatibility(2)
  # set.seed(1)
  dat <- groupdata2::partition(participant.scores, p = 0.6, list_out = TRUE)
  train_data <- dat[[1]]
  test_data <- dat[[2]]
  suppressWarnings(gaussian_baseline <- baseline(test_data = test_data,
                             train_data = train_data,
                             dependent_col = "diagnosis",
                             n = 10,
                             family = "gaussian",
                             parallel = FALSE))

  gaussian_baseline_summ <- gaussian_baseline$summarized_metrics
  gaussian_baseline_reval <- gaussian_baseline$random_evaluations

  # Summarized results
  expect_equal(gaussian_baseline_summ$Measure, c("Mean", "Median", "SD", "IQR", "Max",
                                              "Min", "NAs", "INFs", "All_rows"))
  expect_equal(gaussian_baseline_summ$RMSE,
               c(0.61635445,0.62449980,0.04249801,0.06481080,0.67314560,
                 0.54241837,0.00000000,0.00000000,0.58001703), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$MAE,
               c(0.54700397,0.55000000,0.01239385,0.01835317,0.56250000,
                 0.52380952,0.00000000,0.00000000,0.53703704), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$AIC,
               c(14.010994,13.396725,5.303117,7.897265,
                 23.129949,8.999233,0.0,0.0, 26.167375), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$`Training Rows`,
               c(9.50,9.0,3.341656,5.0,14.0,5.0,0.0,0.0,18.0), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$r2m,
               rep(0.0, 9), tolerance=1e-3)
  expect_equal(gaussian_baseline_summ$r2c,
               rep(0.0, 9), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$AICc,
               c(16.507272,15.719689,4.269571,4.773214,24.220858,
                 11.399233,0.0,0.0, 26.967375), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$BIC,
               c(14.395589,13.645220,5.965933,8.954378,24.408063,
                 8.245354,0.0,0.0,27.948118), tolerance=1e-3)

  # The random evaluations
  expect_equal(gaussian_baseline_reval$RMSE,
               c(0.6244998,0.6454972,0.5758756,0.6454972,0.6611164,
                 0.5424184,0.6244998,0.5758756,0.5951190,0.6731456), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$MAE,
               c(0.5500000,0.5555556,0.5357143,0.5555556,0.5595238,
                 0.5238095,0.5500000,0.5357143,0.5416667,0.5625000), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$AIC,
               c(9.026478,14.365552,12.740493,9.182776,9.164714,
                 23.129949,14.052956,21.480986,17.966808,8.999233), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$`Training Rows`,
               c(5,12,7,6,7,14,10,14,12,8), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Family,
               rep("gaussian", 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Dependent,
               rep("diagnosis", 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Fixed,
               rep("1", 10), tolerance=1e-3)

  all_predictions <- dplyr::bind_rows(gaussian_baseline_reval$Predictions)
  expect_equal(sum(as.numeric(all_predictions$Fold)), 120)
  expect_equal(sum(all_predictions$Target), 50)
  expect_equal(sum(all_predictions$Prediction), 93.84286, tolerance=1e-3)

  all_coeffs <- dplyr::bind_rows(gaussian_baseline_reval$Coefficients)
  expect_equal(all_coeffs$estimate,
               c(0.8,0.8333333,0.7142857,0.8333333,0.8571429,0.6428571,
                 0.8,0.7142857,0.75,0.875), tolerance=1e-3)
  expect_equal(all_coeffs$std.error,
               c(0.2,0.1123666,0.1844278,0.1666667,0.1428571,0.1328944,
                 0.1333333,0.1252940,0.1305582,0.125), tolerance=1e-3)
  expect_equal(all_coeffs$p.value,
               c(1.613009e-02,1.332505e-05,8.237354e-03,4.104716e-03,
                 9.645352e-04,3.244011e-04,2.024993e-04,7.282309e-05,
                 1.294017e-04,2.115549e-04), tolerance=1e-3)
})


# TODO Create baseline test where both classes are 50% 50% , 100% 0%, 0% 100%, 30/70 etc.
# Do we get what we expect?

test_that("baseline() throws expected errors",{

  # Binomial

  set_seed_for_R_compatibility(1)

  # cutoff

  expect_error(baseline(test_data = participant.scores,
                        dependent_col = "diagnosis",
                        n = 10,
                        family = "binomial",
                        cutoff = 1.1),
               "'cutoff' must be between 0.0 and 1.0.", fixed=T)
  expect_error(baseline(test_data = participant.scores,
                        dependent_col = "diagnosis",
                        n = 10,
                        family = "binomial",
                        cutoff = NA),
               "'cutoff' must be numeric.", fixed=T)
  expect_error(baseline(test_data = participant.scores,
                        dependent_col = "diagnosis",
                        n = 10,
                        family = "binomial",
                        cutoff = NULL),
               "'cutoff' was NULL. Must be numeric between 0.0 and 1.0.", fixed=T)
  expect_error(baseline(test_data = participant.scores,
                        dependent_col = "diagnosis",
                        n = 10,
                        family = "binomial",
                        cutoff = c(0,1)),
               "'cutoff' must have length 1.", fixed=T)

  # positive
  expect_error(baseline(test_data = participant.scores,
                        dependent_col = "diagnosis",
                        n = 10,
                        family = "binomial",
                        positive = NULL),
               "'positive' was NULL. Must be either whole number or character.", fixed=T)
  expect_error(baseline(test_data = participant.scores,
                        dependent_col = "diagnosis",
                        n = 10,
                        family = "binomial",
                        positive = NA),
               "'positive' must be either a whole number or character.", fixed=T)
  expect_error(baseline(test_data = participant.scores,
                        dependent_col = "diagnosis",
                        n = 10,
                        family = "binomial",
                        positive = 3),
               "When 'positive' is numeric, it must be either 0 or 1.", fixed=T)
  expect_error(baseline(test_data = participant.scores,
                        dependent_col = "diagnosis",
                        n = 10,
                        family = "binomial",
                        positive = -1),
               "When 'positive' is numeric, it must be either 0 or 1.", fixed=T)
  expect_error(baseline(test_data = participant.scores,
                        dependent_col = "diagnosis",
                        n = 10,
                        family = "binomial",
                        positive = c("0","1")),
               "'positive' must have length 1.", fixed=T)
  expect_error(baseline(test_data = participant.scores,
                        dependent_col = "diagnosis",
                        n = 10,
                        family = "binomial",
                        positive = c(0,1)),
               "'positive' must have length 1.", fixed=T)


})
