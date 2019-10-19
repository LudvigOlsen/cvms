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

    all_preds <- dplyr::bind_rows(
      dplyr::bind_rows(multinom_baseline_random_eval_summ$Predictions)$Prediction)

    expect_equal(all_preds$`1`,
                 c(0.362824302965311, 0.360903717826462, 0.285822146515266, 0.37477371815703,
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
                   0.445596166797302, 0.334706962656109))

    expect_equal(all_preds$`2`,
                 c(0.385780580440307, 0.342604539124775, 0.423080439134247, 0.225175445397002,
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
                   0.180860493953652, 0.325672027376522))

    expect_equal(all_preds$`3`,
                 c(0.251395116594382, 0.296491743048763, 0.291097414350488, 0.400050836445967,
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
                   0.373543339249046, 0.339621009967369))

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
    expect_equal(sum(multinom_baseline_class$`Pos Pred Value`, na.rm=T), 7.221436, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$`Neg Pred Value`, na.rm=T), 11.1646, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$AUC), 10.749, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$`Lower CI`), 8.135684, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_class$`Upper CI`), 13.37502, tolerance = 1e-4)
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
    expect_equal(multinom_baseline_random_eval_summ$AUC,
                 c(0.408730158730159, 0.666997354497355, 0.489748677248677, 0.670304232804233,
                   0.531415343915344, 0.782407407407407, 0.485780423280423, 0.568121693121693,
                   0.384920634920635, 0.615740740740741), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$`Lower CI`,
                 c(0.169789620535037, 0.426575816291112, 0.216849594782662, 0.430502594677966,
                   0.296773926922773, 0.592859761469038, 0.221079289048498, 0.315314900772302,
                   0.139762995662908, 0.372149237835082), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_summ$`Upper CI`,
                 c(0.647670696925281, 0.90715992997448, 0.762647759714692, 0.9101058709305,
                   0.766056760907914, 0.964821750032927, 0.750481557512349, 0.820928485471084,
                   0.630078274178362, 0.8593322436464), tolerance = 1e-4)
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
    preds <- dplyr::bind_rows(multinom_baseline_random_eval_summ$Predictions[[1]]$Prediction)
    expect_equal(preds$`1`,
                 c(0.0292644714588426, 0.0141597631269381, 0.298438639380654,
                   0.80940820303688, 0.0937005066362035, 0.35235101836276, 0.975413098798697,
                   0.175578647077284, 0.262771970923691, 0.00356192768281427, 0.0101813747120091,
                   0.124070740002606, 0.914437937270295, 0.0345747630499071, 0.443603716122321,
                   0.754396177250108, 0.622702608502644, 0.855071475511674, 0.149160434731838,
                   0.732327772925981, 0.704499634048166, 0.0395430319094823, 0.771078117207197,
                   0.311489940634356, 0.240482208017168), tolerance = 1e-4)
    expect_equal(preds$`2`,
                 c(0.79920110839287, 0.864873705929184, 0.149529614257696, 0.0748305440084275,
                   0.472042382499711, 0.00209808185776226, 0.00727516403195308,
                   0.0559580981939634, 0.0479065485802701, 0.953560037716267, 0.0652172908723352,
                   0.252080524884308, 0.0636526992714326, 0.593734385511715, 0.263207821862484,
                   0.116691007284114, 0.0136173006382093, 0.119886570376107, 0.735943777020592,
                   0.0149628131301268, 0.00263470364120449, 0.0254377163472207,
                   0.125594895133848, 0.373604359020064, 0.284343108476632), tolerance = 1e-4)
    expect_equal(preds$`3`,
                 c(0.171534420148287, 0.120966530943878, 0.55203174636165, 0.115761252954693,
                   0.434257110864085, 0.645550899779478, 0.0173117371693497, 0.768463254728752,
                   0.689321480496038, 0.0428780346009194, 0.924601334415656, 0.623848735113086,
                   0.0219093634582727, 0.371690851438378, 0.293188462015195, 0.128912815465778,
                   0.363680090859147, 0.0250419541122186, 0.11489578824757, 0.252709413943892,
                   0.29286566231063, 0.935019251743297, 0.103326987658955, 0.31490570034558,
                   0.4751746835062), tolerance = 1e-4)
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
    expect_equal(sum(multinom_baseline_random_eval_class$AUC),
                 16.8125, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$`Lower CI`),
                 9.544973, tolerance = 1e-4)
    expect_equal(sum(multinom_baseline_random_eval_class$`Upper CI`),
                 24.05785, tolerance = 1e-4)
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
    expect_equal(colnames(multinom_baseline_random_eval_class$ROC[[1]]),
                 c("Sensitivities", "Specificities"))
    expect_equal(multinom_baseline_random_eval_class$ROC[[1]]$Sensitivities,
                 c(1, 0.888888888888889, 0.777777777777778, 0.777777777777778,
                   0.666666666666667, 0.555555555555556, 0.555555555555556, 0.444444444444444,
                   0.333333333333333, 0.333333333333333, 0.333333333333333, 0.333333333333333,
                   0.333333333333333, 0.333333333333333, 0.222222222222222, 0.222222222222222,
                   0.222222222222222, 0.222222222222222, 0.111111111111111, 0, 0,
                   0, 0, 0, 0, 0), tolerance = 1e-4)
    expect_equal(multinom_baseline_random_eval_class$ROC[[1]]$Specificities,
                 c(0, 0, 0, 0.0625, 0.0625, 0.0625, 0.125, 0.125, 0.125, 0.1875,
                   0.25, 0.3125, 0.375, 0.4375, 0.4375, 0.5, 0.5625, 0.625, 0.625,
                   0.625, 0.6875, 0.75, 0.8125, 0.875, 0.9375, 1), tolerance = 1e-4)

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
