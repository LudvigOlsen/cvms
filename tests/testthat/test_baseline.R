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

  set_seed_for_R_compatibility(1)
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
               c(0.53976549,0.52003413,0.05536050,0.06101522,0.64549722,0.49315036,0.00000000,0.00000000,0.49378858), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$MAE,
               c(0.47052503,0.46611722,0.03613427,0.03597884,0.55000000,0.41666667,0.00000000,0.00000000,0.48148148), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$AIC,
               c(-21.81094,12.39809,113.07636,7.15748,21.48099,-343.36287,0.00000,0.00000,29.21690), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$`Training Rows`,
               c(7.9000,7.0000,3.3483,4.0000,14.0000,5.0000,0.0000,0.0000,18.0000), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$r2m,
               rep(0.0, 9), tolerance=1e-3)
  expect_equal(gaussian_baseline_summ$r2c,
               rep(0.0, 9), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$AICc,
               c(-18.181848,16.379556,112.194238,3.451304,22.571895,-337.362866, 0.000000, 0.000000,30.016902), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$BIC,
               c(-21.825333,12.090622,113.376366, 8.096223,22.759101,-344.143990, 0.000000, 0.000000,30.997645), tolerance=1e-3)

  # The random evaluations
  expect_equal(gaussian_baseline_reval$RMSE,
               c(0.4932883,0.5101020,0.5639390,0.6454972,0.4931504,0.5049069,0.5299662,0.6244998,0.5385165,0.4937886), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$MAE,
               c(0.4833333,0.4642857,0.4404762,0.4166667,0.4880952,0.4679487,0.4537037,0.5500000,0.4500000,0.4907407), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$AIC,
               c(11.053804,21.480986, 9.164714,-343.362866,14.016744,20.789465,13.742367, 9.026478, 9.026478,16.952442), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$`Training Rows`,
               c(5,14,7,5,7,13,9,5,5,9), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Family,
               rep("gaussian", 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Dependent,
               rep("diagnosis", 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Fixed,
               rep("1", 10), tolerance=1e-3)

  all_predictions <- dplyr::bind_rows(gaussian_baseline_reval$Predictions)
  expect_equal(sum(as.numeric(all_predictions$Fold)), 120)
  expect_equal(sum(all_predictions$Target), 70)
  expect_equal(sum(all_predictions$Prediction), 81.22198, tolerance=1e-3)

  all_coeffs <- dplyr::bind_rows(gaussian_baseline_reval$Coefficients)
  expect_equal(all_coeffs$estimate,
               c(0.6000000,0.7142857,0.8571429,1.0000000,0.5714286,
                 0.6923077,0.7777778,0.2000000,0.8000000,0.5555556), tolerance=1e-3)
  expect_equal(all_coeffs$std.error,
               c(2.449490e-01,1.252940e-01,1.428571e-01,9.930137e-17,
                 2.020305e-01,1.332347e-01,1.469862e-01,2.000000e-01,
                 2.000000e-01, 1.756821e-01), tolerance=1e-3)
  expect_equal(all_coeffs$p.value,
               c(7.048400e-02,7.282309e-05,9.645352e-04,5.834077e-64,
                 3.001975e-02,2.233108e-04,7.359017e-04,3.739010e-01,
                 1.613009e-02,1.334906e-02), tolerance=1e-3)
})


# Create baseline test where both classes are 50% 50% , 100% 0%, 0% 100%, 30/70 etc.
# Do we get what we expect?
