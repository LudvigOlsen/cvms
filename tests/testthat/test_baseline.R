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
  expect_equal(sum(as.numeric(all_confmats$Target)), 20, tolerance=1e-3)
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


test_that("multinomial evaluation are correct in baseline()",{

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

  multinom_baseline_summ <- multinom_baseline$Summarized$Results
  multinom_baseline_class <- multinom_baseline$Summarized$`Class Level Results`
  multinom_baseline_random_eval_summ <- multinom_baseline$`Random Evaluations`$Results
  multinom_baseline_random_eval_class <- multinom_baseline$`Random Evaluations`$`Class Level Results`


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
               c(0.56704696,0.56679894,0.09450594,0.11813823,0.73776455,
                 0.42774471,0.00000000,0.00000000,0.50000000,0.50000000,
                 0.50000000), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$F1,
               c( 0.4140962,0.4126861,0.1264581,0.1715778,0.6383041,0.2377451,
                  0.0000000,0.0000000,NA,NA,NA), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$Sensitivity,
               c(0.4227513,0.4206349,0.1263567,0.1494709,0.6560847,0.2328042,
                 0.0000000,0.0000000,0.3333333,0.3333333,0.3333333), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$Specificity,
               c(0.71134259,0.71296296,0.06285668,0.08680556,0.81944444,0.62268519,
                 0.00000000,0.00000000,0.66666667,0.66666667,0.66666667), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$`Pos Pred Value`,
               c(0.4211051,0.4244048,0.1307579,0.1899471,0.6444444,0.2453704,
                 0.0000000,0.0000000, NaN,NaN, NaN), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$`Neg Pred Value`,
               c(0.71245547,0.71038943,0.06396297,0.08854809,0.82478070,
                 0.62009804,0.00000000,0.00000000,NaN,NaN,NaN), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$AUC,
               c(0.5569444,0.5471230,0.1070958,0.1270668,0.7599206,0.3961640,
                 0.0000000,0.0000000,0.5000000,0.5000000,0.5000000), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$`Lower CI`,
               c(0.3083051,0.2984963,0.1218368,0.1475652,0.5616756,
                 0.1519624,0.0000000,0.0000000,0.5000000,0.5000000,0.5000000), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$`Upper CI`,
               c(0.80555015,0.79574977,0.09439699,0.10656836,0.95782969,
                 0.64036563,0.00000000,0.00000000,0.50000000,0.50000000,
                 0.50000000), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$Kappa,
               c(0.1320552,0.1344737,0.1870863,0.2438259,0.4621285,
                 -0.1375345,0.0000000,0.0000000,0.0000000,0.0000000,
                 0.0000000), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$MCC,
               c(0.1336749,0.1364628,0.1910094,0.2544542,0.4710514,-0.1392929,
                 0.0000000,0.0000000,0.0000000,0.0000000,0.0000000), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$`Detection Rate`,
               c( 0.14000000, 0.14000000, 0.04228066, 0.05666667, 0.21333333,
                  0.08000000, 0.00000000, 0.00000000, 0.12000000, 0.09333333,
                  0.12000000), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$`Detection Prevalence`,
               c(3.333333e-01,3.333333e-01,3.700743e-17,5.551115e-17,3.333333e-01,
                 3.333333e-01,0.000000e+00,0.000000e+00,3.333333e-01,
                 3.333333e-01,3.333333e-01), tolerance=1e-3)
  expect_equal(multinom_baseline_summ$Prevalence,
               c(0.3333333,0.3333333,0.0000000,0.0000000,0.3333333,0.3333333,
                 0.0000000,0.0000000,0.3333333,0.3333333,0.3333333), tolerance=1e-3)
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
                   "Prevalence", "Predictions", "Confusion Matrix", "Family", "Dependent" ))
    expect_equal(colnames(multinom_baseline_random_eval_summ$`Confusion Matrix`[[1]]),
                 c("Prediction", "Target", "N"))
    expect_equal(colnames(multinom_baseline_random_eval_summ$`Confusion Matrix`[[2]]),
                 c("Prediction", "Target", "N"))
    expect_equal(colnames(multinom_baseline_random_eval_summ$Predictions[[1]]),
                 c("Target", "Prediction", "Predicted Class"))

    expect_equal(multinom_baseline_random_eval_summ$Repetition,
                 as.character(1:10))
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
                 as.character(rep(1:10, each=3)))
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
                 "train_data was not used for binomial baseline.",
                 fixed=T
                 )
  expect_error(baseline(test_data = participant.scores,
                          dependent_col = "score",
                          n = 10,
                          family = "gaussian"),
                 "train_data must be passed for Gaussian baseline.",
                 fixed=T
  )

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
