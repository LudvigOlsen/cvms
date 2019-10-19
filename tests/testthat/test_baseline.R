library(cvms)
context("baseline()")


test_that("binomial evaluations are correct in baseline()",{

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
  expect_equal(colnames(all_predictions),
               c("Repetition", "Target", "Prediction", "Predicted Class"))
  expect_equal(sum(as.numeric(all_predictions$Repetition)),
               sum(rep(1:10, each = 30)))
  expect_equal(sum(all_predictions$Target), 180)
  expect_equal(sum(as.numeric(all_predictions$`Predicted Class`)), 140)
  expect_equal(sum(all_predictions$Prediction), 147.0334, tolerance=1e-3)

  all_confmats <- dplyr::bind_rows(binom_baseline_reval$`Confusion Matrix`)
  expect_equal(sum(as.numeric(all_confmats$Prediction)), 20, tolerance=1e-3)
  expect_equal(sum(as.numeric(all_confmats$Target)), 20, tolerance=1e-3)
  expect_equal(sum(all_confmats$N), 300, tolerance=1e-3)
  expect_equal(sum(all_confmats$Repetition), 220, tolerance=1e-3)

})

test_that("gaussian evaluations are correct in baseline()",{

  set_seed_for_R_compatibility(2)
  # set.seed(1)
  dat <- groupdata2::partition(participant.scores, p = 0.6, list_out = TRUE)
  train_data <- dat[[1]]
  test_data <- dat[[2]]
  suppressWarnings(gaussian_baseline <- baseline(test_data = test_data,
                             train_data = train_data,
                             dependent_col = "score",
                             n = 10,
                             family = "gaussian",
                             parallel = FALSE))

  gaussian_baseline_summ <- gaussian_baseline$summarized_metrics
  gaussian_baseline_reval <- gaussian_baseline$random_evaluations

  # Summarized results
  expect_equal(gaussian_baseline_summ$Measure, c("Mean", "Median", "SD", "IQR", "Max",
                                              "Min", "NAs", "INFs", "All_rows"))
  expect_equal(gaussian_baseline_summ$RMSE,
               c(23.548673,22.631836,2.151282,1.774305,
                 27.814396,20.671713,0.0,0.0,23.108507), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$MAE,
               c(20.601647,20.021429,1.559585,1.191468,
                 23.645833,18.214286,0.0,0.0,20.370370), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$AIC,
               c(80.88782,73.28142,31.21325,46.54117,
                 125.03483,40.63481,0.0,0.0,156.22826), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$`Training Rows`,
               c(9.50,9.0,3.341656,5.0,14.0,5.0,0.0,0.0,18.0), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$r2m,
               rep(0.0, 9))
  expect_equal(gaussian_baseline_summ$r2c,
               rep(0.0, 9))

  expect_equal(gaussian_baseline_summ$AICc,
               c(83.38409,75.63857,29.84876,45.02450,
                 126.12574,46.63481,0.0,0.0,157.02826), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$BIC,
               c(81.27241,73.52992,31.93310,47.55239,
                 126.31295,39.85369,0.0,0.0,158.0090), tolerance=1e-3)

  # The random evaluations
  expect_equal(gaussian_baseline_reval$RMSE,
               c(26.47565,22.39047,20.67171,24.70268,23.08974,
                 22.57248,22.69119,22.51107,22.56734,27.81440), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$MAE,
               c(22.63333,19.83333,18.21429,21.38889,20.35714,
                 19.97619,20.06667,19.92857,19.97222,23.64583), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$AIC,
               c(40.63481,96.54162,63.62098,46.39313,57.93568,
                 123.76021,82.94187,125.03483,108.86128,63.15374), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$`Training Rows`,
               c(5,12,7,6,7,14,10,14,12,8), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Family,
               rep("gaussian", 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Dependent,
               rep("score", 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Fixed,
               rep("1", 10), tolerance=1e-3)

  all_predictions <- dplyr::bind_rows(gaussian_baseline_reval$Predictions)
  expect_equal(colnames(all_predictions),
               c("Repetition", "Target", "Prediction"))
  expect_equal(sum(as.numeric(all_predictions$Repetition)),
               sum(rep(1:10, each = 12)))
  expect_equal(sum(all_predictions$Target), 5440)
  expect_equal(sum(all_predictions$Prediction), 4085.043, tolerance=1e-3)

  all_coeffs <- dplyr::bind_rows(gaussian_baseline_reval$Coefficients)
  expect_equal(colnames(all_coeffs),
               c("Repetition", "term", "estimate", "std.error", "statistic",
                 "p.value"))
  expect_equal(all_coeffs$Repetition, 1:10)
  expect_equal(all_coeffs$estimate,
               c(28.40000,36.00000,41.71429,31.33333,34.42857,
                 35.57143,35.30000,35.71429,35.58333,26.37500), tolerance=1e-3)
  expect_equal(all_coeffs$std.error,
               c(4.718050,3.448759,6.985408,3.702852,4.654016,
                 4.834303,4.176788,5.059458,5.762284,3.688484), tolerance=1e-3)
  expect_equal(all_coeffs$p.value,
               c(3.836776e-03,4.806737e-07,9.888062e-04,3.785030e-04,
                 3.133274e-04,5.518161e-06,1.423837e-05,8.556549e-06,
                 6.954980e-05, 1.852572e-04), tolerance=1e-3)
})

test_that("gaussian evaluations of random effects models are correct in baseline()",{

  set_seed_for_R_compatibility(2)
  # set.seed(1)
  dat <- groupdata2::partition(participant.scores, p = 0.6, list_out = TRUE)
  train_data <- dat[[1]]
  test_data <- dat[[2]]
  suppressWarnings(gaussian_baseline_random_effects <- baseline(
    test_data = test_data,
    train_data = train_data,
    dependent_col = "score",
    random_effects = "( 1 | session )",
    n = 10,
    family = "gaussian",
    REML = TRUE, # Usually FALSE, but TRUE to avoid changing the tests
    parallel = FALSE))

  gaussian_baseline_summ <- gaussian_baseline_random_effects$summarized_metrics
  gaussian_baseline_reval <- gaussian_baseline_random_effects$random_evaluations

  # Summarized results
  expect_equal(gaussian_baseline_summ$Measure, c("Mean", "Median", "SD", "IQR", "Max",
                                                 "Min", "NAs", "INFs", "All_rows"))
  expect_equal(gaussian_baseline_summ$RMSE,
               c(19.600875,19.568833,3.010504,5.332666,
                 23.631064,15.792418,0.0,0.0,18.066222), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$MAE,
               c(17.231620,17.259500,2.284422,3.206596,
                 20.680264,14.117023,0.0,0.0,16.173775), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$AIC,
               c(74.72007,68.53657,29.06934,44.68871,
                 115.91591,37.04749,0.0,0.0,146.98744), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$`Training Rows`,
               c(9.50,9.0,3.341656,5.0,14.0,5.0,0.0,0.0,18.0), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$r2m,
               rep(0.0, 9), tolerance=1e-3)
  expect_equal(gaussian_baseline_summ$r2c,
               c(0.5689284,0.5931160,0.1248541,0.1792077,
                 0.7194042,0.3450947,0.0,0.0,0.5092992), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$AICc,
               c(82.00007,74.53657,24.19749,39.18656,
                 118.31591,54.54786,0.0,0.0,148.70173), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$BIC,
               c(75.2969607911245, 68.9093092222065, 30.1532384455176, 46.2055462747716,
                 117.833083604798, 35.875805418616, 0, 0, 149.658560139426), tolerance=1e-3)

  # The random evaluations
  expect_equal(gaussian_baseline_reval$RMSE,
               c(23.3023673893311, 20.2831211351759, 18.4653958494823, 23.6310640067372,
                 19.6708765288482, 15.7924182217171, 19.4667891710134, 16.5250914306427,
                 15.8422216462345, 23.0294037571561), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$MAE,
               c(19.9254197349807, 18.1852507167284, 17.0487074806514, 20.6802643236492,
                 17.0551245625476, 14.2061291677316, 17.4638756853949, 14.8688972771474,
                 14.1170229613728, 18.7655103243966), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$AIC,
               c(37.0474916813137, 92.0001975699801, 59.3199306240758, 42.5478613478296,
                 51.7112998206263, 112.962482318462, 77.7532020941892, 115.915911615952,
                 99.9577092947807, 57.9846047242755), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$`Training Rows`,
               c(5,12,7,6,7,14,10,14,12,8), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Family,
               rep("gaussian", 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Dependent,
               rep("score", 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Fixed,
               rep("1", 10), tolerance=1e-3)
  expect_equal(gaussian_baseline_reval$Random,
               rep("(1|session)", 10), tolerance=1e-3)

  all_predictions <- dplyr::bind_rows(gaussian_baseline_reval$Predictions)
  expect_equal(colnames(all_predictions),
               c("Repetition", "Target", "Prediction"))
  expect_equal(sum(as.numeric(all_predictions$Repetition)),
               sum(rep(1:10, each = 12)))
  expect_equal(sum(all_predictions$Target), 5440)
  expect_equal(sum(all_predictions$Prediction), 4030.024, tolerance=1e-3)

  all_coeffs <- dplyr::bind_rows(gaussian_baseline_reval$Coefficients)
  expect_equal(colnames(all_coeffs),
               c("Repetition", "term", "estimate", "std.error", "statistic",
                 "p.value"))
  expect_equal(all_coeffs$Repetition, 1:10)
  expect_equal(all_coeffs$estimate,
               c(29.4482246187172, 34.039741354335, 40.9321844272457, 29.7091048995076,
                 31.6380887585632, 37.7282592375317, 34.1782347165533, 35.9506799949463,
                 36.3068983754988, 27.2041030958495), tolerance=1e-3)
  expect_equal(all_coeffs$std.error,
               c(7.15564449282046, 5.53120659588499, 8.97275372070671, 4.96123227269955,
                 7.05688044109226, 10.2739018309621, 6.56119887646549, 9.73185796300383,
                 10.7798478381932, 5.86883171896131), tolerance=1e-3)
  expect_equal(all_coeffs$p.value,
               rep(NA, 10), tolerance=1e-3)

})

test_that("gaussian evaluations of random effects models are correct with REML FALSE in baseline()",{

  set_seed_for_R_compatibility(2)
  # set.seed(1)
  dat <- groupdata2::partition(participant.scores, p = 0.6, list_out = TRUE)
  train_data <- dat[[1]]
  test_data <- dat[[2]]
  suppressWarnings(gaussian_baseline_random_effects <- baseline(
    test_data = test_data,
    train_data = train_data,
    dependent_col = "score",
    random_effects = "( 1 | session )",
    n = 10,
    family = "gaussian",
    REML = FALSE,
    parallel = FALSE))

  gaussian_baseline_summ <- gaussian_baseline_random_effects$summarized_metrics
  gaussian_baseline_reval <- gaussian_baseline_random_effects$random_evaluations

  # Summarized results
  expect_equal(gaussian_baseline_summ$Measure, c("Mean", "Median", "SD", "IQR", "Max",
                                                 "Min", "NAs", "INFs", "All_rows"))
  expect_equal(gaussian_baseline_summ$RMSE,
               c(19.9461643038004, 19.8243992085347, 3.16669902505845, 5.45212618245324,
                 24.498950920267, 15.8922827970791, 0, 0, 18.2579833481909), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$MAE,
               c(17.5688071767949, 17.5664123038964, 2.36581521783252, 3.32573044194396,
                 20.960459513901, 14.4129406656979, 0, 0, 16.3907116308631), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$AIC,
               c(80.3465725153843, 74.2525560821578, 29.4337002888526, 45.288244994372,
                 122.097050951734, 42.4261074987794, 0, 0, 152.799138513963), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$`Training Rows`,
               c(9.50,9.0,3.341656,5.0,14.0,5.0,0.0,0.0,18.0), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$r2m,
               rep(0.0, 9), tolerance=1e-3)
  expect_equal(gaussian_baseline_summ$r2c,
               c(0.43613229083168, 0.474672230635666, 0.149845112686991, 0.23376511982053,
                 0.624606985762511, 0.200718929950735, 0, 0, 0.397952267974043
               ), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$AICc,
               c(87.6265725153843, 80.2525560821578, 24.5752651300548, 39.9130171335876,
                 124.497050951734, 59.3905521226883, 0, 0, 154.513424228249), tolerance=1e-3)

  expect_equal(gaussian_baseline_summ$BIC,
               c(80.9234641973603, 74.6252989452318, 30.5156235782933, 46.8050859521017,
                 124.01422294058, 41.2544212360817, 0, 0, 155.470253787652), tolerance=1e-3)

  # The random evaluations
  expect_equal(gaussian_baseline_reval$RMSE,
               c(24.498950920267, 20.5418159984439, 18.992695846541, 23.6559983524106,
                 19.8835818741749, 15.8922827970791, 19.7652165428946, 16.7141367039409,
                 16.0496991056741, 23.4672648965776), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$MAE,
               c(20.960459513901, 18.4479900242695, 17.3968470787666, 20.6756807573846,
                 17.3014038386473, 14.4129406656979, 17.7359775290262, 15.1341822802114,
                 14.4362959391151, 19.1862941409292), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$AIC,
               c(42.4261074987794, 97.0575891839717, 65.3436873296684, 47.3905521226883,
                 57.2591370177335, 119.245364275572, 83.1614248346471, 122.097050951734,
                 106.339403446441, 63.1454084926068), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$`Training Rows`,
               c(5,12,7,6,7,14,10,14,12,8), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Family,
               rep("gaussian", 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Dependent,
               rep("score", 10), tolerance=1e-3)

  expect_equal(gaussian_baseline_reval$Fixed,
               rep("1", 10), tolerance=1e-3)
  expect_equal(gaussian_baseline_reval$Random,
               rep("(1|session)", 10), tolerance=1e-3)

  all_predictions <- dplyr::bind_rows(gaussian_baseline_reval$Predictions)
  expect_equal(colnames(all_predictions),
               c("Repetition", "Target", "Prediction"))
  expect_equal(sum(as.numeric(all_predictions$Repetition)),
               sum(rep(1:10, each = 12)))
  expect_equal(sum(all_predictions$Target), 5440)
  expect_equal(sum(all_predictions$Prediction), 4042.804, tolerance=1e-3)

  all_coeffs <- dplyr::bind_rows(gaussian_baseline_reval$Coefficients)
  expect_equal(colnames(all_coeffs),
               c("Repetition", "term", "estimate", "std.error", "statistic",
                 "p.value"))
  expect_equal(all_coeffs$Repetition, 1:10)
  expect_equal(all_coeffs$estimate,
               c(29.0475640591351, 34.3203425052484, 41.2416418477926, 29.944216654405,
                 31.7864586462034, 37.6658072225419, 34.3377960494584, 35.9622550910097,
                 36.2791461299582, 27.1152376447763), tolerance=1e-3)
  expect_equal(all_coeffs$std.error,
               c(5.0066788799575, 4.58125952668684, 7.415753419446, 4.1185058577398,
                 5.86297959144672, 8.4064394096518, 5.47687440034061, 8.01519324303267,
                 8.84195558284918, 4.79166575929533), tolerance=1e-3)
  expect_equal(all_coeffs$p.value,
               rep(NA, 10), tolerance=1e-3)
})



# TODO Create baseline test where both classes are 50% 50% , 100% 0%, 0% 100%, 30/70 etc.
# Do we get what we expect?


test_that("multinomial evaluations are correct in baseline()",{

  set_seed_for_R_compatibility(1)
  targets_1 <- dplyr::sample_n(tibble::enframe(rep(1:3, each=10), value = "targets_3"), 25) %>%
    dplyr::select(-name)
  targets_2 <- dplyr::sample_n(tibble::enframe(rep(1:4, each=10), value = "targets_4"), 25) %>%
    dplyr::select(-name)
  targets_3 <- dplyr::sample_n(tibble::enframe(rep(1:5, each=10), value = "targets_5"), 25) %>%
    dplyr::select(-name)

  different_targets <- targets_1 %>%
    dplyr::bind_cols(targets_2, targets_3)

  multinom_baseline <- baseline(test_data = different_targets,
                                dependent_col = "targets_3",
                                n = 10,
                                family = "multinomial",
                                parallel = FALSE)

  multinom_baseline_summ <- multinom_baseline$summarized_metrics
  multinom_baseline_class <- tidyr::unnest(multinom_baseline$summarized_class_level_results, .data$Results)
  multinom_baseline_random_eval_summ <- multinom_baseline$random_evaluations
  multinom_baseline_random_eval_class <- dplyr::bind_rows(multinom_baseline$random_evaluations$`Class Level Results`)

  # Summarized results
  if (TRUE){
  # # Summarized results
  expect_equal(colnames(multinom_baseline_summ),
               c("Measure","Overall Accuracy","Balanced Accuracy","F1","Sensitivity",
                 "Specificity","Pos Pred Value","Neg Pred Value","AUC","Lower CI",
                 "Upper CI","Kappa","MCC","Detection Rate","Detection Prevalence",
                 "Prevalence"))
  expect_equal(multinom_baseline_summ$Measure,
               c("Mean", "Median", "SD", "IQR", "Max",
                 "Min", "NAs", "INFs", "All_1", "All_2", "All_3"))
  expect_equal(multinom_baseline_summ$`Balanced Accuracy`,
               c(0.567046957671958, 0.566798941798942, 0.0945059449777177, 0.118138227513228,
                 0.845238095238095, 0.349206349206349, 0, 0, 0.5, 0.5, 0.5), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$F1,
               c(0.414096213384139, 0.412686127082412, 0.126458068440281, 0.171577755033637,
                 0.75, 0.125, 0, 0, NA, NA, NA), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$Sensitivity,
               c(0.422751322751323, 0.420634920634921, 0.126356719688141, 0.149470899470899,
                 0.857142857142857, 0.111111111111111, 0, 0, 0.333333333333333,
                 0.333333333333333, 0.333333333333333), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$Specificity,
               c(0.711342592592593, 0.712962962962963, 0.0628566830255579, 0.0868055555555556,
                  0.875, 0.4375, 0, 0, 0.666666666666667, 0.666666666666667, 0.666666666666667
               ), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$`Pos Pred Value`,
               c(0.421105098605099, 0.424404761904762, 0.130757940506255, 0.18994708994709,
                 0.714285714285714, 0.111111111111111, 0, 0, NaN, NaN, NaN), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$`Neg Pred Value`,
               c(0.712455474092471, 0.710389433551198, 0.0639629682811759, 0.0885480929888824,
                 0.9375, 0.538461538461539, 0, 0, NaN, NaN, NaN), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$AUC,
               c(0.556944444444444, 0.547123015873016, 0.107095761199707, 0.127066798941799,
                 0.849206349206349, 0.263888888888889, 0, 0, 0.5, 0.5, 0.5), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$`Lower CI`,
               c(0.308305137872545, 0.298496257102124, 0.12183675196496, 0.147565240013357,
                 0.697404693102006, 0.0289092878990155, 0, 0, 0.5, 0.5, 0.5), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$`Upper CI`,
               c(0.805550150839321, 0.795749774643908, 0.0943969906850918, 0.106568357870241,
                 1, 0.498868489878762, 0, 0, 0.5, 0.5, 0.5), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$Kappa,
               c(0.132055158549256, 0.134473657323828, 0.187086327373864, 0.243825884877481,
                 0.635036496350365, -0.277372262773723, 0, 0, 0, 0, 0), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$MCC,
               c(0.133674933533998, 0.136462790386259, 0.191009426580045, 0.254454158754327,
                 0.645881334621692, -0.282109088685337, 0, 0, 0, 0, 0), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$`Detection Rate`,
               c(0.14, 0.14, 0.0422806613120899, 0.0566666666666667, 0.24, 0.04,
                 0, 0, 0.12, 0.0933333333333333, 0.12), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$`Detection Prevalence`,
               c(0.333333333333333, 0.333333333333333, 3.70074341541719e-17,
                 5.55111512312578e-17, 0.48, 0.16, 0, 0, 0.333333333333333, 0.333333333333333,
                 0.333333333333333), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$Prevalence,
               c(0.333333333333333, 0.333333333333333, 0, 0, 0.36, 0.28, 0,
                 0, 0.333333333333333, 0.333333333333333, 0.333333333333333), tolerance=1e-3)
  }

  # Summarized class level results
  if (TRUE){
    expect_equal(colnames(multinom_baseline_class),
                 c("Class","Measure","Balanced Accuracy","F1","Sensitivity",
                   "Specificity","Pos Pred Value","Neg Pred Value","AUC","Lower CI",
                   "Upper CI","Kappa","MCC","Detection Rate","Detection Prevalence",
                   "Prevalence"))
    expect_equal(multinom_baseline_class$Class,
                 as.character(rep(1:3, each=10)))
    expect_equal(multinom_baseline_class$Measure,
                 rep(c("Mean","Median","SD","IQR","Max","Min","NAs","INFs","All_0","All_1"), 3))
    expect_equal(sum(multinom_baseline_class$`Balanced Accuracy`), 10.58557, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$F1, na.rm = TRUE), 7.506455, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$Sensitivity), 9.202117, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$Specificity), 12.08067, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$`Pos Pred Value`, na.rm=T), 7.107812, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$`Neg Pred Value`, na.rm=T), 11.06547, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$AUC), 10.73728, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$`Lower CI`), 7.982493, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$`Upper CI`), 13.49186, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$Kappa), 3.139021, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$MCC), 3.177878, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$`Detection Rate`), 3.067856, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$`Detection Prevalence`), 7.413783, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$Prevalence), 6)

  }

  # Random evaluations results
  if (TRUE){
    expect_equal(colnames(multinom_baseline_random_eval_summ),
                 c("Repetition", "Overall Accuracy", "Balanced Accuracy", "F1", "Sensitivity",
                   "Specificity", "Pos Pred Value", "Neg Pred Value", "AUC","Lower CI",
                   "Upper CI", "Kappa", "MCC", "Detection Rate", "Detection Prevalence",
                   "Prevalence", "Predictions", "Confusion Matrix", "Class Level Results",
                   "Family", "Dependent" ))
    expect_equal(colnames(multinom_baseline_random_eval_summ$`Confusion Matrix`[[1]]),
                 c("Prediction", "Target", "N"))
    expect_equal(colnames(multinom_baseline_random_eval_summ$`Confusion Matrix`[[2]]),
                 c("Prediction", "Target", "N"))
    expect_equal(colnames(multinom_baseline_random_eval_summ$Predictions[[1]]),
                 c("Target", "Prediction", "Predicted Class"))

    expect_equal(multinom_baseline_random_eval_summ$Repetition,
                 1:10)
    expect_equal(multinom_baseline_random_eval_summ$`Overall Accuracy`,
                 c(0.56,0.36,0.64,0.28,0.52,0.24,0.44,0.40,0.32,0.44), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$`Balanced Accuracy`,
                 c(0.6651786,0.5239749,0.7377646,0.4661045,0.6362434,
                   0.4277447,0.5806878,0.5529101,0.4980159,0.5818452), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$F1,
                 c(0.5511905,0.3472767,0.6383041,0.2769608,0.5111111,
                   0.2377451,0.4398148,0.4019608,0.3131868,0.4234115), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$Sensitivity,
                 c(0.5502646,0.3650794,0.6560847,0.2910053,0.5132275,
                   0.2328042,0.4391534,0.4021164,0.3386243,0.4391534), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$Specificity,
                 c(0.7800926,0.6828704,0.8194444,0.6412037,0.7592593,
                   0.6226852,0.7222222,0.7037037,0.6574074,0.7245370), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$`Pos Pred Value`,
                 c(0.5627706,0.3407407,0.6444444,0.2726190,0.5317460,
                   0.2453704,0.4497354,0.4154762,0.3148148,0.4333333), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$`Neg Pred Value`,
                 c(0.7804233,0.6874269,0.8247807,0.6453159,0.7612836,
                   0.6200980,0.7199074,0.7008715,0.6566697,0.7277778), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$AUC,
                 c(0.6676587,0.5274471,0.7599206,0.4910714,0.5667989,
                   0.3961640,0.6240079,0.4857804,0.4659392,0.5846561), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$`Lower CI`,
                 c(0.4219246,0.2858758,0.5616756,0.2237593,0.3111167,
                   0.1519624,0.3799845,0.2168618,0.2052386,0.3246521), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$`Upper CI`,
                 c(0.9133929,0.7690184,0.9578297,0.7583836,0.8224811,
                   0.6403656,0.8680314,0.7546990,0.7266397,0.8446601), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$Kappa,
                 c(0.33221743,0.04113827,0.46212848, -0.07364408,
                   0.27425438, -0.13753453,0.16320289,0.10883768,
                   -0.01015859, 0.16010964), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$MCC,
                 c(0.33636907,0.03852788,0.47105140, -0.07476737,
                   0.28186601, -0.13929293,0.16523890,0.11083071,
                   -0.01516919, 0.16209487), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$`Detection Rate`,
                 c(0.18666667,0.12000000,0.21333333,0.09333333,
                   0.17333333,0.08000000,0.14666667,0.13333333,
                   0.10666667,0.14666667), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$`Detection Prevalence`,
                 c(0.3333333,0.3333333,0.3333333,0.3333333,0.3333333,
                   0.3333333,0.3333333,0.3333333,0.3333333,0.3333333), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$Prevalence,
                 c(0.3333333,0.3333333,0.3333333,0.3333333,0.3333333,
                   0.3333333,0.3333333,0.3333333,0.3333333,0.3333333), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$Family,
                 rep("multinomial", 10))
    expect_equal(multinom_baseline_random_eval_summ$Dependent,
                 rep("targets_3", 10))

    expect_equal(length(multinom_baseline_random_eval_summ$Predictions),
                 10)
    expect_equal(length(multinom_baseline_random_eval_summ$`Confusion Matrix`),
                 10)
    expect_equal(multinom_baseline_random_eval_summ$Predictions[[1]]$Target,
                 as.character(c(1,2,2,3,1,3,3,2,2,1,1,1,2,1,2,3,3,3,2,1,1,3,3,1,3)))
    expect_equal(multinom_baseline_random_eval_summ$Predictions[[1]]$`Predicted Class`,
                 c("2","1","2","3","1","2","3","3","2","1","3","1",
                   "3","3","2","2","3","3","1","3","1","3","3","1","2"))
    preds <- dplyr::bind_rows(multinom_baseline_random_eval_summ$Predictions[[1]]$Prediction)
    expect_equal(preds$`1`,
                 c(0.3628243,0.3609037,0.2858221,0.3747737,0.3817009,
                   0.2737241,0.3419418,0.3049587,0.2962721,0.4240434,
                   0.2931166,0.4357583,0.2827270,0.2248048,0.1981635,
                   0.2229453,0.3005942,0.3278586,0.4148558,0.3384068,
                   0.4079048,0.3290621,0.3476237,0.4361450,0.3350177), tolerance = 1e-4)
    expect_equal(preds$`2`,
                 c(0.3857806,0.3426045,0.4230804,0.2251754,0.3129899,
                   0.4752175,0.2249170,0.3047626,0.4820548,0.2148414,
                   0.3443212,0.3331387,0.2928041,0.3148037,0.4530263,
                   0.4719141,0.3380788,0.2966119,0.2536526,0.3053621,
                   0.2405719,0.3336935,0.2739201,0.2804216,0.3779097), tolerance = 1e-4)
    expect_equal(preds$`3`,
                 c(0.2513951,0.2964917,0.2910974,0.4000508,0.3053092,
                   0.2510584,0.4331413,0.3902787,0.2216731,0.3611152,
                   0.3625622,0.2311030,0.4244689,0.4603915,0.3488102,
                   0.3051406,0.3613270,0.3755294,0.3314917,0.3562311,
                   0.3515233,0.3372444,0.3784562,0.2834334,0.2870727), tolerance = 1e-4)

    expect_equal(multinom_baseline_random_eval_summ$`Confusion Matrix`[[1]]$Prediction,
                 c("1","2","3","1","2","3","1","2","3"))
    expect_equal(multinom_baseline_random_eval_summ$`Confusion Matrix`[[1]]$Target,
                 c("1","1","1","2","2","2","3","3","3"))
    expect_equal(multinom_baseline_random_eval_summ$`Confusion Matrix`[[1]]$N,
                 c(5,1,3,2,3,2,0,3,6))
  }

  # Random evaluations class level results
  if (TRUE){
    expect_equal(colnames(multinom_baseline_random_eval_class),
                 c("Repetition","Class","Balanced Accuracy","F1","Sensitivity",
                   "Specificity","Pos Pred Value","Neg Pred Value","AUC","Lower CI",
                   "Upper CI", "Kappa","MCC","Detection Rate","Detection Prevalence",
                   "Prevalence","Support","ROC","Confusion Matrix","Family",
                   "Dependent"))
    expect_equal(length(multinom_baseline_random_eval_class$`Confusion Matrix`),
                 30)
    expect_equal(colnames(multinom_baseline_random_eval_class$`Confusion Matrix`[[1]]),
                 c("Prediction","Target","Pos_0","Pos_1","N"))
    expect_equal(multinom_baseline_random_eval_class$`Confusion Matrix`[[1]]$Prediction,
                 c("0","1","0","1"))
    expect_equal(multinom_baseline_random_eval_class$`Confusion Matrix`[[1]]$Target,
                 c("0","0","1","1"))
    expect_equal(multinom_baseline_random_eval_class$`Confusion Matrix`[[1]]$Pos_0,
                 c("TP", "FN", "FP", "TN"))
    expect_equal(multinom_baseline_random_eval_class$`Confusion Matrix`[[1]]$Pos_1,
                 c("TN", "FP", "FN", "TP"))
    expect_equal(multinom_baseline_random_eval_class$`Confusion Matrix`[[1]]$N,
                 c(14, 2, 4, 5))
    expect_equal(multinom_baseline_random_eval_class$Repetition,
                 rep(1:10, each=3))
    expect_equal(multinom_baseline_random_eval_class$Class,
                 as.character(rep(1:3, 10)))
    expect_equal(sum(multinom_baseline_random_eval_class$`Balanced Accuracy`),
                 17.01141, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$F1),
                 12.42289, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$Sensitivity),
                 12.68254, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$Specificity),
                 21.34028, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$`Pos Pred Value`),
                 12.63315, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$`Neg Pred Value`),
                 21.37366, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$AUC),
                 16.70833, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$`Lower CI`),
                 9.249154, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$`Upper CI`),
                 24.1665, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$Kappa),
                 3.961655, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$MCC),
                 4.010248, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$`Detection Rate`),
                 4.2, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$`Detection Prevalence`),
                 10, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$Prevalence),
                 10, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$Support),
                 250, tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_class$Family,
                 rep("binomial", 30), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_class$Dependent,
                 rep("targets_3", 30), tolerance = 1e-4)

    expect_equal(length(multinom_baseline_random_eval_class$ROC),
                 30)
    expect_equal(colnames(multinom_baseline_random_eval_class$ROC[[1]]),
                 c("Sensitivities", "Specificities"))
    expect_equal(multinom_baseline_random_eval_class$ROC[[1]]$Sensitivities,
                 c(1.0000000,1.0000000,1.0000000,0.8888889,0.8888889,
                   0.8888889,0.8888889,0.7777778,0.7777778,0.7777778,
                   0.7777778,0.7777778,0.7777778,0.7777778,0.6666667,
                   0.6666667,0.6666667,0.6666667,0.5555556,0.5555556,
                   0.4444444,0.3333333,0.3333333,0.2222222,0.1111111,
                   0.0000000), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_class$ROC[[1]]$Specificities,
                 c(0.0000,0.0625,0.1250,0.1250,0.1875,0.2500,0.3125,
                   0.3125,0.3750,0.4375,0.5000,0.5625,0.6250,0.6875,
                   0.6875,0.7500,0.8125,0.8750,0.8750,0.9375,0.9375,
                   0.9375,1.0000,1.0000,1.0000,1.0000), tolerance = 1e-4)

  }


  ## Custom random generator fn

  # Multinomial with custom random generator function
  # that creates very "certain" predictions
  # (once softmax is applied)

  rcertain <- function(n){
    (runif(n, min = 1, max = 100)^1.4)/100
  }

  set_seed_for_R_compatibility(1)

  multinom_baseline_certain <- baseline(test_data = different_targets,
           dependent_col = "targets_3",
           n = 10, family = "multinomial",
           random_generator_fn = rcertain)

  expect_equal(names(multinom_baseline_certain),
               c("summarized_metrics", "summarized_class_level_results",
                 "random_evaluations"))

  multinom_baseline_summ <- multinom_baseline_certain$summarized_metrics
  multinom_baseline_class <- tidyr::unnest(multinom_baseline_certain$summarized_class_level_results, .data$Results)
  multinom_baseline_random_eval_summ <- multinom_baseline_certain$random_evaluations
  multinom_baseline_random_eval_class <- dplyr::bind_rows(multinom_baseline_certain$random_evaluations$`Class Level Results`)

  # Summarized results
  if (TRUE){
    # # Summarized results
    expect_equal(colnames(multinom_baseline_summ),
                 c("Measure","Overall Accuracy","Balanced Accuracy","F1","Sensitivity",
                   "Specificity","Pos Pred Value","Neg Pred Value","AUC","Lower CI",
                   "Upper CI","Kappa","MCC","Detection Rate","Detection Prevalence",
                   "Prevalence"))
    expect_equal(multinom_baseline_summ$Measure,
                 c("Mean", "Median", "SD", "IQR", "Max",
                   "Min", "NAs", "INFs", "All_1", "All_2", "All_3"))
    expect_equal(multinom_baseline_summ$`Balanced Accuracy`,
                 c(0.562285052910053, 0.552331349206349, 0.102909289295557, 0.16025958994709,
                   0.845238095238095, 0.349206349206349, 0, 0, 0.5, 0.5, 0.5), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$F1,
                 c(0.410443406496038, 0.393545751633987, 0.135473485139399, 0.213743453255837,
                   0.75, 0.125, 0, 0, NA, NA, NA), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$Sensitivity,
                 c(0.416931216931217, 0.402116402116402, 0.137153997558857, 0.207671957671958,
                   0.857142857142857, 0.111111111111111, 0, 0, 0.333333333333333,
                   0.333333333333333, 0.333333333333333), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$Specificity,
                 c(0.707638888888889, 0.702546296296296, 0.0688421238509424, 0.112847222222222,
                   0.888888888888889, 0.5, 0, 0, 0.666666666666667, 0.666666666666667,
                   0.666666666666667), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$`Pos Pred Value`,
                 c(0.416391293891294, 0.395238095238095, 0.141112097555744, 0.232296176046176,
                   0.714285714285714, 0.111111111111111, 0, 0, NaN, NaN, NaN), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$`Neg Pred Value`,
                 c(0.709738237625235, 0.703667153996101, 0.0702194379364757, 0.112253271754046,
                   0.9375, 0.533333333333333, 0, 0, NaN, NaN, NaN), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$AUC,
                 c(0.560416666666667, 0.549768518518518, 0.125187856135687, 0.167410714285714,
                   0.888888888888889, 0.229166666666667, 0, 0, 0.5, 0.5, 0.5), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$`Lower CI`,
                 c(0.318165773799738, 0.306044413847538, 0.140135976122912, 0.195062153327983,
                   0.756377867839226, 0.0261943929564497, 0, 0, 0.5, 0.5, 0.5), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$`Upper CI`,
                 c(0.801928332929399, 0.793492623189499, 0.111440524054069, 0.141679900329526,
                   1, 0.432138940376884, 0, 0, 0.5, 0.5, 0.5), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$Kappa,
                 c(0.122510558981315, 0.102170581409235, 0.204023225460463, 0.328600213517171,
                   0.635036496350365, -0.277372262773723, 0, 0, 0, 0, 0), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$MCC,
                 c(0.124977459958719, 0.10188338719613, 0.207768661322695, 0.336587028842194,
                   0.645881334621692, -0.282109088685337, 0, 0, 0, 0, 0), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$`Detection Rate`,
                 c(0.138666666666667, 0.133333333333333, 0.0462734758571391, 0.0766666666666667,
                   0.28, 0.04, 0, 0, 0.12, 0.0933333333333333, 0.12), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$`Detection Prevalence`,
                 c(0.333333333333333, 0.333333333333333, 3.70074341541719e-17,
                   5.55111512312578e-17, 0.48, 0.16, 0, 0, 0.333333333333333, 0.333333333333333,
                   0.333333333333333), tolerance=1e-3)
    expect_equal(multinom_baseline_summ$Prevalence,
                 c(0.333333333333333, 0.333333333333333, 0, 0, 0.36, 0.28, 0,
                   0, 0.333333333333333, 0.333333333333333, 0.333333333333333), tolerance=1e-3)
  }

  # Summarized class level results
  if (TRUE){
    expect_equal(colnames(multinom_baseline_class),
                 c("Class","Measure","Balanced Accuracy","F1","Sensitivity",
                   "Specificity","Pos Pred Value","Neg Pred Value","AUC","Lower CI",
                   "Upper CI","Kappa","MCC","Detection Rate","Detection Prevalence",
                   "Prevalence"))
    expect_equal(multinom_baseline_class$Class,
                 as.character(rep(1:3, each=10)))
    expect_equal(multinom_baseline_class$Measure,
                 rep(c("Mean","Median","SD","IQR","Max","Min","NAs","INFs","All_0","All_1"), 3))
    expect_equal(sum(multinom_baseline_class$`Balanced Accuracy`), 10.68149, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$F1, na.rm = TRUE), 7.599136, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$Sensitivity), 9.372747, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$Specificity), 12.17256, tolerance = 1e-4)
    # Finish:
    # expect_equal(sum(multinom_baseline_class$`Pos Pred Value`, na.rm=T), 7.107812, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_class$`Neg Pred Value`, na.rm=T), 11.06547, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_class$AUC), 10.73728, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_class$`Lower CI`), 7.982493, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_class$`Upper CI`), 13.49186, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_class$Kappa), 3.139021, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_class$MCC), 3.177878, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_class$`Detection Rate`), 3.067856, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_class$`Detection Prevalence`), 7.413783, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_class$Prevalence), 6)

  }

  # Random evaluations results
  if (TRUE){
    expect_equal(colnames(multinom_baseline_random_eval_summ),
                 c("Repetition", "Overall Accuracy", "Balanced Accuracy", "F1", "Sensitivity",
                   "Specificity", "Pos Pred Value", "Neg Pred Value", "AUC","Lower CI",
                   "Upper CI", "Kappa", "MCC", "Detection Rate", "Detection Prevalence",
                   "Prevalence", "Predictions", "Confusion Matrix", "Class Level Results",
                   "Family", "Dependent" ))
    expect_equal(colnames(multinom_baseline_random_eval_summ$`Confusion Matrix`[[1]]),
                 c("Repetition", "Prediction", "Target", "N"))
    expect_equal(colnames(multinom_baseline_random_eval_summ$`Confusion Matrix`[[2]]),
                 c("Repetition", "Prediction", "Target", "N"))
    expect_equal(colnames(multinom_baseline_random_eval_summ$Predictions[[1]]),
                 c("Repetition", "Target", "Prediction", "Predicted Class"))

    expect_equal(multinom_baseline_random_eval_summ$Repetition,
                 1:10)
    expect_equal(multinom_baseline_random_eval_summ$`Overall Accuracy`,
                 c(0.28, 0.52, 0.32, 0.56, 0.36, 0.64, 0.28, 0.52, 0.24, 0.44), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$`Balanced Accuracy`,
                 c(0.458498677248677, 0.628637566137566, 0.498015873015873, 0.665178571428571,
                   0.523974867724868, 0.73776455026455, 0.466104497354497, 0.636243386243386,
                   0.427744708994709, 0.580687830687831), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$F1,
                 c(0.283060592658116, 0.494896331738437, 0.324074074074074, 0.551190476190476,
                   0.347276688453159, 0.638304093567251, 0.276960784313726, 0.511111111111111,
                   0.237745098039216, 0.439814814814815), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$Sensitivity,
    #              c(0.5502646,0.3650794,0.6560847,0.2910053,0.5132275,
    #                0.2328042,0.4391534,0.4021164,0.3386243,0.4391534), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$Specificity,
    #              c(0.7800926,0.6828704,0.8194444,0.6412037,0.7592593,
    #                0.6226852,0.7222222,0.7037037,0.6574074,0.7245370), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$`Pos Pred Value`,
    #              c(0.5627706,0.3407407,0.6444444,0.2726190,0.5317460,
    #                0.2453704,0.4497354,0.4154762,0.3148148,0.4333333), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$`Neg Pred Value`,
    #              c(0.7804233,0.6874269,0.8247807,0.6453159,0.7612836,
    #                0.6200980,0.7199074,0.7008715,0.6566697,0.7277778), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$AUC,
    #              c(0.6676587,0.5274471,0.7599206,0.4910714,0.5667989,
    #                0.3961640,0.6240079,0.4857804,0.4659392,0.5846561), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$`Lower CI`,
    #              c(0.4219246,0.2858758,0.5616756,0.2237593,0.3111167,
    #                0.1519624,0.3799845,0.2168618,0.2052386,0.3246521), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$`Upper CI`,
    #              c(0.9133929,0.7690184,0.9578297,0.7583836,0.8224811,
    #                0.6403656,0.8680314,0.7546990,0.7266397,0.8446601), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$Kappa,
    #              c(0.33221743,0.04113827,0.46212848, -0.07364408,
    #                0.27425438, -0.13753453,0.16320289,0.10883768,
    #                -0.01015859, 0.16010964), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$MCC,
    #              c(0.33636907,0.03852788,0.47105140, -0.07476737,
    #                0.28186601, -0.13929293,0.16523890,0.11083071,
    #                -0.01516919, 0.16209487), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$`Detection Rate`,
    #              c(0.18666667,0.12000000,0.21333333,0.09333333,
    #                0.17333333,0.08000000,0.14666667,0.13333333,
    #                0.10666667,0.14666667), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$`Detection Prevalence`,
    #              c(0.3333333,0.3333333,0.3333333,0.3333333,0.3333333,
    #                0.3333333,0.3333333,0.3333333,0.3333333,0.3333333), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$Prevalence,
    #              c(0.3333333,0.3333333,0.3333333,0.3333333,0.3333333,
    #                0.3333333,0.3333333,0.3333333,0.3333333,0.3333333), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_summ$Family,
    #              rep("multinomial", 10))
    # expect_equal(multinom_baseline_random_eval_summ$Dependent,
    #              rep("targets_3", 10))
    #
    # expect_equal(length(multinom_baseline_random_eval_summ$Predictions),
    #              10)
    # expect_equal(length(multinom_baseline_random_eval_summ$`Confusion Matrix`),
    #              10)
    # expect_equal(multinom_baseline_random_eval_summ$Predictions[[1]]$Target,
    #              as.character(c(1,2,2,3,1,3,3,2,2,1,1,1,2,1,2,3,3,3,2,1,1,3,3,1,3)))
    # expect_equal(multinom_baseline_random_eval_summ$Predictions[[1]]$`Predicted Class`,
    #              c("2","1","2","3","1","2","3","3","2","1","3","1",
    #                "3","3","2","2","3","3","1","3","1","3","3","1","2"))
    # preds <- dplyr::bind_rows(multinom_baseline_random_eval_summ$Predictions[[1]]$Prediction)
    # expect_equal(preds$`1`,
    #              c(0.3628243,0.3609037,0.2858221,0.3747737,0.3817009,
    #                0.2737241,0.3419418,0.3049587,0.2962721,0.4240434,
    #                0.2931166,0.4357583,0.2827270,0.2248048,0.1981635,
    #                0.2229453,0.3005942,0.3278586,0.4148558,0.3384068,
    #                0.4079048,0.3290621,0.3476237,0.4361450,0.3350177), tolerance = 1e-4)
    # expect_equal(preds$`2`,
    #              c(0.3857806,0.3426045,0.4230804,0.2251754,0.3129899,
    #                0.4752175,0.2249170,0.3047626,0.4820548,0.2148414,
    #                0.3443212,0.3331387,0.2928041,0.3148037,0.4530263,
    #                0.4719141,0.3380788,0.2966119,0.2536526,0.3053621,
    #                0.2405719,0.3336935,0.2739201,0.2804216,0.3779097), tolerance = 1e-4)
    # expect_equal(preds$`3`,
    #              c(0.2513951,0.2964917,0.2910974,0.4000508,0.3053092,
    #                0.2510584,0.4331413,0.3902787,0.2216731,0.3611152,
    #                0.3625622,0.2311030,0.4244689,0.4603915,0.3488102,
    #                0.3051406,0.3613270,0.3755294,0.3314917,0.3562311,
    #                0.3515233,0.3372444,0.3784562,0.2834334,0.2870727), tolerance = 1e-4)
    #
    # expect_equal(multinom_baseline_random_eval_summ$`Confusion Matrix`[[1]]$Prediction,
    #              c("1","2","3","1","2","3","1","2","3"))
    # expect_equal(multinom_baseline_random_eval_summ$`Confusion Matrix`[[1]]$Target,
    #              c("1","1","1","2","2","2","3","3","3"))
    # expect_equal(multinom_baseline_random_eval_summ$`Confusion Matrix`[[1]]$N,
    #              c(5,1,3,2,3,2,0,3,6))
  }

  # Random evaluations class level results
  if (TRUE){
    expect_equal(colnames(multinom_baseline_random_eval_class),
                 c("Repetition","Class","Balanced Accuracy","F1","Sensitivity",
                   "Specificity","Pos Pred Value","Neg Pred Value","AUC","Lower CI",
                   "Upper CI", "Kappa","MCC","Detection Rate","Detection Prevalence",
                   "Prevalence","Support","ROC","Confusion Matrix","Family",
                   "Dependent"))
    expect_equal(length(multinom_baseline_random_eval_class$`Confusion Matrix`),
                 30)
    expect_equal(colnames(multinom_baseline_random_eval_class$`Confusion Matrix`[[1]]),
                 c("Prediction","Target","Pos_0","Pos_1","N"))
    expect_equal(multinom_baseline_random_eval_class$`Confusion Matrix`[[1]]$Prediction,
                 c("0","1","0","1"))
    expect_equal(multinom_baseline_random_eval_class$`Confusion Matrix`[[1]]$Target,
                 c("0","0","1","1"))
    expect_equal(multinom_baseline_random_eval_class$`Confusion Matrix`[[1]]$Pos_0,
                 c("TP", "FN", "FP", "TN"))
    expect_equal(multinom_baseline_random_eval_class$`Confusion Matrix`[[1]]$Pos_1,
                 c("TN", "FP", "FN", "TP"))
    expect_equal(multinom_baseline_random_eval_class$`Confusion Matrix`[[1]]$N,
                 c(8, 8, 7, 2))
    expect_equal(multinom_baseline_random_eval_class$Repetition,
                 rep(1:10, each=3))
    expect_equal(multinom_baseline_random_eval_class$Class,
                 as.character(rep(1:3, 10)))
    expect_equal(sum(multinom_baseline_random_eval_class$`Balanced Accuracy`),
                 16.86855, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$F1),
                 12.3133, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$Sensitivity),
    #              12.68254, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$Specificity),
    #              21.34028, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$`Pos Pred Value`),
    #              12.63315, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$`Neg Pred Value`),
    #              21.37366, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$AUC),
    #              16.70833, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$`Lower CI`),
    #              9.249154, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$`Upper CI`),
    #              24.1665, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$Kappa),
    #              3.961655, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$MCC),
    #              4.010248, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$`Detection Rate`),
    #              4.2, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$`Detection Prevalence`),
    #              10, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$Prevalence),
    #              10, tolerance = 1e-4)
    # expect_equal(sum(multinom_baseline_random_eval_class$Support),
    #              250, tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_class$Family,
    #              rep("binomial", 30), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_class$Dependent,
    #              rep("targets_3", 30), tolerance = 1e-4)
    #
    # expect_equal(length(multinom_baseline_random_eval_class$ROC),
    #              30)
    # expect_equal(colnames(multinom_baseline_random_eval_class$ROC[[1]]),
    #              c("Sensitivities", "Specificities"))
    # expect_equal(multinom_baseline_random_eval_class$ROC[[1]]$Sensitivities,
    #              c(1.0000000,1.0000000,1.0000000,0.8888889,0.8888889,
    #                0.8888889,0.8888889,0.7777778,0.7777778,0.7777778,
    #                0.7777778,0.7777778,0.7777778,0.7777778,0.6666667,
    #                0.6666667,0.6666667,0.6666667,0.5555556,0.5555556,
    #                0.4444444,0.3333333,0.3333333,0.2222222,0.1111111,
    #                0.0000000), tolerance = 1e-4)
    # expect_equal(multinom_baseline_random_eval_class$ROC[[1]]$Specificities,
    #              c(0.0000,0.0625,0.1250,0.1250,0.1875,0.2500,0.3125,
    #                0.3125,0.3750,0.4375,0.5000,0.5625,0.6250,0.6875,
    #                0.6875,0.7500,0.8125,0.8750,0.8750,0.9375,0.9375,
    #                0.9375,1.0000,1.0000,1.0000,1.0000), tolerance = 1e-4)

  }


})

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
               "When 'positive' is numeric, it must be either 1 or 2.", fixed=T)
  expect_error(baseline(test_data = participant.scores,
                        dependent_col = "diagnosis",
                        n = 10,
                        family = "binomial",
                        positive = -1),
               "When 'positive' is numeric, it must be either 1 or 2.", fixed=T)
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

  expect_message(baseline(train_data = participant.scores,
                          test_data = participant.scores,
                          dependent_col = "diagnosis",
                          n = 2,
                          family = "binomial"),
                 "'train_data' was not used for binomial version of baseline().",
                 fixed=T
                 )

  expect_message(baseline(test_data = participant.scores,
                          dependent_col = "diagnosis",
                          n = 2,
                          family = "binomial",
                          random_generator_fn = rnorm),
                 paste0("'random_generator_fn' was not default function. ",
                        "Note that the 'random_generator_fn' is not used in ",
                        "the binomial version of baseline()."),
                 fixed=T
  )

  expect_message(suppressWarnings(baseline(
    test_data = participant.scores,
    train_data = participant.scores,
    dependent_col = "score",
    random_generator_fn = rnorm,
    n = 10,
    family = "gaussian",
    parallel = FALSE)),
    paste0("'random_generator_fn' was not default function. ",
           "Note that the 'random_generator_fn' is not used in ",
           "the Gaussian version of baseline()."))

  expect_error(baseline(test_data = participant.scores,
                          dependent_col = "score",
                          n = 10,
                          family = "gaussian"),
                 "train_data must be passed for Gaussian baseline.",
                 fixed=T
  )

  expect_error(baseline(
    train_data = participant.scores,
    test_data = participant.scores,
    dependent_col = "xxx",
    n = 10,
    family = "gaussian"),
    "could not find these variables in the training data: xxx",
    fixed=T
  )
  expect_error(baseline(
    train_data = participant.scores,
    test_data = dplyr::select(participant.scores, -.data$score),
    dependent_col = "score",
    n = 10,
    family = "gaussian"),
    "could not find these variables in the test data: score",
    fixed=T
  )

  expect_equal(subtract_inf_count_from_na_count(tibble::tibble("Measure" = c("Mean","NAs","INFs","Lol"),
                                                  "Accuracy" = c(1,3,4,2),
                                                  "Balanced Accuracy" = c(3,2,5,2))),
               tibble::tibble("Measure" = c("Mean","NAs","INFs","Lol"),
                              "Accuracy" = c(1,-1,4,2),
                              "Balanced Accuracy" = c(3,-3,5,2)))

  expect_error(create_multinomial_baseline_evaluations(data.frame(),
                                                       dependent_col = "",
                                                       na.rm = NULL,
                                                       random_generator_fn = runif,
                                                       parallel_ = FALSE),
               "'na.rm' must be logical scalar (TRUE/FALSE).",
               fixed = TRUE)

  expect_error(create_gaussian_baseline_evaluations(as.data.frame(matrix(1,11,1)),
                                       data.frame(),
                                       dependent_col = "",
                                       na.rm = NULL),
               "'na.rm' must be logical scalar (TRUE/FALSE).",
               fixed = TRUE)


  set_seed_for_R_compatibility(1)
  dat <- participant.scores
  dat$diagnosis <- c(head(dat$diagnosis, 20),rep(2,10))
  expect_error(baseline(test_data = dat,
                          dependent_col = "diagnosis",
                          n = 2,
                          family = "binomial"),
                 "The dependent column must maximally contain 2 levels. Did you specify the correct family?",
                 fixed = T)
  dat$diagnosis <- as.character(dat$diagnosis)
  expect_error(baseline(test_data = dat,
                          dependent_col = "diagnosis",
                          n = 2,
                          family = "binomial"),
                 "The dependent column must maximally contain 2 levels.",
                 fixed = T)
})
