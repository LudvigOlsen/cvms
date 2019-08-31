library(cvms)
context("evaluate()")


test_that("multinomial evaluations are correct in evaluate()",{

  set_seed_for_R_compatibility(1)
  random_probabilities <- multiclass_probability_tibble(
    num_classes = 5,
    num_observations = 20,
    apply_softmax = FALSE # Test with as well
  )
  expect_equal(sum(random_probabilities), 51.78471, tolerance = 1e-5)

  data_ <- random_probabilities %>%
    dplyr::mutate(cl = as.factor(rep(1:5, each = 4)),
                  cl_char = paste0("cl_", cl))

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
    dplyr::rename_at(dplyr::vars(paste0("class_", 1:5)), .funs = ~paste0("cl_", 1:5))

  mn_eval_1 <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multinomial",
    apply_softmax = TRUE
  )

  # TODO Add more tests
  expect_equal(mn_eval_1$Results$`Overall Accuracy`, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$`Balanced Accuracy`, 0.5, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$`Balanced Accuracy`,
               mean(mn_eval_1$`Class Level Results`$`Balanced Accuracy`))
  expect_equal(mn_eval_1$Results$F1, NaN)
  expect_equal(mn_eval_1$Results$Sensitivity, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$Specificity, 0.8, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$`Pos Pred Value`, 0.23, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$`Neg Pred Value`, 0.7991667, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$AUC, 0.49375)
  expect_equal(mn_eval_1$Results$`Lower CI`, 0.2235814, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$`Upper CI`, 0.7558714, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$Kappa, 0.008653846, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$MCC, 0.0125, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$`Detection Rate`, 0.04, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$`Detection Prevalence`, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$Prevalence, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$Class,
               c("cl_1","cl_2","cl_3","cl_4","cl_5"))
  expect_equal(mn_eval_1$`Class Level Results`$`Balanced Accuracy`,
               c(0.50000,0.37500,0.59375,0.50000,0.53125), tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$F1,
               c(0.2222222, NaN, 0.3333333, 0.2222222, 0.2500000), tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$Sensitivity,
               c(0.25, 0.0, 0.25, 0.25, 0.25), tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$Specificity,
               c(0.7500,0.7500,0.9375,0.7500,0.8125), tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$`Pos Pred Value`,
               c(0.20, 0.00, 0.50, 0.20, 0.25), tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$`Neg Pred Value`,
               c(0.80, 0.750, 0.8333333, 0.80, 0.81250), tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$AUC,
               c(0.421875, 0.250000, 0.890625, 0.390625, 0.515625), tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$`Lower CI`,
               c(0.04381511, 0.01016288, 0.74101384,0.05608427, 0.26683090), tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$Kappa,
               c(-3.172066e-16, -2.500000e-01, 2.307692e-01, -3.172066e-16, 6.250000e-02),
               tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$MCC,
               c(0.0000, -0.2500, 0.2500, 0.0000, 0.0625), tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$`Detection Rate`,
               c(0.05,0.00, 0.05, 0.05, 0.05), tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$`Detection Prevalence`,
               c(0.25, 0.20, 0.10, 0.25, 0.20), tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$Prevalence,
               c(0.2,0.2,0.2,0.2,0.2), tolerance = 1e-4)
  expect_equal(mn_eval_1$`Class Level Results`$Support,
               c(4,4,4,4,4))
  expect_equal(mn_eval_1$`Class Level Results`$`Confusion Matrix`[[1]]$Prediction,
               as.character(c(0,1,0,1)))
  expect_equal(mn_eval_1$`Class Level Results`$`Confusion Matrix`[[1]]$Target,
               as.character(c(0,0,1,1)))
  expect_equal(mn_eval_1$`Class Level Results`$`Confusion Matrix`[[1]]$Pos_0,
               c("TP", "FN", "FP", "TN"))
  expect_equal(mn_eval_1$`Class Level Results`$`Confusion Matrix`[[1]]$Pos_1,
               c("TN", "FP", "FN", "TP"))
  expect_equal(mn_eval_1$`Class Level Results`$`Confusion Matrix`[[1]]$N,
               c(12,4,3,1))
  expect_equal(mn_eval_1$`Class Level Results`$`Confusion Matrix`[[2]]$Prediction,
               as.character(c(0,1,0,1)))
  expect_equal(mn_eval_1$`Class Level Results`$`Confusion Matrix`[[2]]$Target,
               as.character(c(0,0,1,1)))
  expect_equal(mn_eval_1$`Class Level Results`$`Confusion Matrix`[[2]]$Pos_0,
               c("TP", "FN", "FP", "TN"))
  expect_equal(mn_eval_1$`Class Level Results`$`Confusion Matrix`[[2]]$Pos_1,
               c("TN", "FP", "FN", "TP"))
  expect_equal(mn_eval_1$`Class Level Results`$`Confusion Matrix`[[2]]$N,
               c(12,4,4,0))
  expect_equal(mn_eval_1$`Class Level Results`$ROC[[1]]$Sensitivities,
               c(1.00,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.50,0.50,0.25,
                 0.25,0.25,0.25,0.25,0.25,0.25,0.25,0.00,0.00,0.00))
  expect_equal(mn_eval_1$`Class Level Results`$ROC[[2]]$Sensitivities,
               c(1.00,0.75,0.75,0.75,0.75,0.75,0.75,0.50,0.25,0.25,0.00,
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00))
  expect_equal(mn_eval_1$`Class Level Results`$ROC[[1]]$Specificities,
               c(0.0000,0.0000,0.0625,0.1250,0.1875,0.2500,0.3125,0.3750,
                 0.3750,0.4375,0.4375,0.5000,0.5625,0.6250,0.6875,0.7500,
                 0.8125,0.8750,0.8750,0.9375,1.0000))
  expect_equal(mn_eval_1$`Class Level Results`$ROC[[2]]$Specificities,
               c(0.0000,0.0000,0.0625,0.1250,0.1875,0.2500,0.3125,0.3125,0.3125,0.3750,0.3750,0.4375,0.5000,0.5625,0.6250,0.6875,0.7500,0.8125,0.8750,0.9375,1.0000))
  expect_equal(colnames(mn_eval_1$`Class Level Results`$ROC[[1]]),
               c("Sensitivities", "Specificities"))
  expect_equal(colnames(mn_eval_1$`Class Level Results`$`Confusion Matrix`[[1]]),
               c("Prediction", "Target", "Pos_0", "Pos_1", "N"))
  expect_equal(colnames(mn_eval_1$Results$`Confusion Matrix`[[1]]),
               c("Prediction", "Target", "N"))


  # Test Weighted metrics, and metrics == "all"

  set_seed_for_R_compatibility(1)
  mn_eval_2 <- evaluate(
    data = data_ %>% dplyr::sample_n(17),
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multinomial",
    apply_softmax = TRUE,
    metrics = "all"
  )

  # Create manual weighted mean function
  manual_weighted_mean <- function(x,w){
    sum(x * w)/sum(w)
  }

  # Test manual weighted mean function
  expect_equal(manual_weighted_mean(x = c(0.2,0.8),w = c(1,1)), 0.5, tolerance = 1e-4)
  expect_equal(manual_weighted_mean(x = c(0.3,0.7),w = c(1,1)), 0.5, tolerance = 1e-4)
  expect_equal(manual_weighted_mean(x = c(0.5,0.5),w = c(1,1)), 0.5, tolerance = 1e-4)
  expect_equal(manual_weighted_mean(x = c(0.2),w = c(4)), 0.2)
  expect_equal(manual_weighted_mean(x = c(0.2,0.8),w = c(4,1)), 0.32, tolerance = 1e-4)

  expect_equal(mn_eval_2$Results$`Overall Accuracy`, 0.2352941, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Balanced Accuracy`, 0.5380586, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Balanced Accuracy`,
               mean(mn_eval_2$`Class Level Results`$`Balanced Accuracy`))
  expect_equal(mn_eval_2$Results$`Weighted Balanced Accuracy`, 0.5236264, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Balanced Accuracy`,
               manual_weighted_mean(x = mn_eval_2$`Class Level Results`$`Balanced Accuracy`,
                                    w = mn_eval_2$`Class Level Results`$Support))
  expect_equal(mn_eval_2$Results$Accuracy, 0.6941176, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Accuracy`, 0.6851211, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$F1, NaN)
  expect_equal(mn_eval_2$Results$`Weighted F1`, NaN)
  expect_equal(mn_eval_2$Results$Sensitivity, 0.2666667, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Sensitivity`, 0.2352941, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$Specificity, 0.8094505, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Specificity`, 0.8119586, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Pos Pred Value`, 0.2666667, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Pos Pred Value`, 0.2696078, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Neg Pred Value`, 0.8094505, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Neg Pred Value`, 0.7939237, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$AUC, 0.5344689, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted AUC`, 0.5301875, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Lower CI`, 0.2431864, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Lower CI`, 0.239267, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Upper CI`, 0.8172027, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Upper CI`, 0.8110507, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$Kappa, 0.06428773, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Kappa`, 0.04481687, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$MCC, 0.07240413, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted MCC`, 0.05207999, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Detection Rate`, 0.04705882, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Detection Rate`, 0.0449827, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Detection Prevalence`, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Detection Prevalence`, 0.1937716, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$Prevalence, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Prevalence`, 0.2110727, tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$Class,
               c("cl_1","cl_2","cl_3","cl_4","cl_5"))
  expect_equal(mn_eval_2$`Class Level Results`$`Balanced Accuracy`,
               c(0.5480769,0.3461538,0.5865385,0.5595238,0.6500000), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$F1,
               c(0.2857143, NaN, 0.3333333, 0.2857143, 0.3333333), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$Sensitivity,
               c(0.250, 0.00, 0.250, 0.3333333, 0.500), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$Specificity,
               c(0.8461538, 0.6923077, 0.9230769, 0.7857143, 0.80), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$`Pos Pred Value`,
               c(0.3333333, 0.0, 0.50, 0.25, 0.25), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$`Neg Pred Value`,
               c(0.7857143, 0.6923077, 0.80, 0.8461538, 0.9230769), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$AUC,
               c(0.4807692, 0.250, 0.8653846, 0.4761905, 0.60), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$`Lower CI`,
               c(0.0798065, 0.0, 0.6833168, 0.1094280, 0.3433805), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$Kappa,
               c(0.1052632, -0.3076923,  0.2093023,  0.1052632,  0.2093023), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$MCC,
               c(0.1069901, -0.3076923, 0.2278664, 0.1069901, 0.2278664), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$`Detection Rate`,
               c(0.05882353,0.00, 0.05882353, 0.05882353, 0.05882353), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$`Detection Prevalence`,
               c(0.1764706,0.2352941,0.1176471,0.2352941,0.2352941), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$Prevalence,
               c(0.2352941,0.2352941,0.2352941,0.1764706,0.1176471), tolerance = 1e-4)
  expect_equal(mn_eval_2$`Class Level Results`$Support,
               c(4,4,4,3,2))

  expect_equal(mn_eval_2$`Class Level Results`$`Confusion Matrix`[[1]]$Prediction,
               as.character(c(0,1,0,1)))
  expect_equal(mn_eval_2$`Class Level Results`$`Confusion Matrix`[[1]]$Target,
               as.character(c(0,0,1,1)))
  expect_equal(mn_eval_2$`Class Level Results`$`Confusion Matrix`[[1]]$Pos_0,
               c("TP", "FN", "FP", "TN"))
  expect_equal(mn_eval_2$`Class Level Results`$`Confusion Matrix`[[1]]$Pos_1,
               c("TN", "FP", "FN", "TP"))
  expect_equal(mn_eval_2$`Class Level Results`$`Confusion Matrix`[[1]]$N,
               c(11,2,3,1))
  expect_equal(mn_eval_2$`Class Level Results`$`Confusion Matrix`[[2]]$Prediction,
               as.character(c(0,1,0,1)))
  expect_equal(mn_eval_2$`Class Level Results`$`Confusion Matrix`[[2]]$Target,
               as.character(c(0,0,1,1)))
  expect_equal(mn_eval_2$`Class Level Results`$`Confusion Matrix`[[2]]$Pos_0,
               c("TP", "FN", "FP", "TN"))
  expect_equal(mn_eval_2$`Class Level Results`$`Confusion Matrix`[[2]]$Pos_1,
               c("TN", "FP", "FN", "TP"))
  expect_equal(mn_eval_2$`Class Level Results`$`Confusion Matrix`[[2]]$N,
               c(9,4,4,0))
  expect_equal(mn_eval_2$`Class Level Results`$ROC[[1]]$Sensitivities,
               c(1.00,0.75,0.75,0.75,0.75,0.75,0.75,0.75,0.50,0.50,
                 0.25,0.25,0.25,0.25,0.25,0.25,0.00,0.00))
  expect_equal(mn_eval_2$`Class Level Results`$ROC[[2]]$Sensitivities,
               c(1.00,0.75,0.75,0.75,0.75,0.75,0.50,0.25,0.25,0.00,
                 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00))
  expect_equal(mn_eval_2$`Class Level Results`$ROC[[1]]$Specificities,
               c(0.00000000,0.00000000,0.07692308,0.15384615,0.23076923,
                 0.30769231,0.38461538,0.46153846,0.46153846,0.53846154,
                 0.53846154,0.61538462,0.69230769,0.76923077,0.84615385,
                 0.92307692,0.92307692,1.00000000))
  expect_equal(mn_eval_2$`Class Level Results`$ROC[[2]]$Specificities,
               c(0.00000000,0.00000000,0.07692308,0.15384615,0.23076923,
                 0.30769231,0.30769231,0.30769231,0.38461538,0.38461538,
                 0.46153846,0.53846154,0.61538462,0.69230769,0.76923077,
                 0.84615385,0.92307692,1.00000000))
  expect_equal(colnames(mn_eval_2$`Class Level Results`$ROC[[1]]),
               c("Sensitivities", "Specificities"))
  expect_equal(colnames(mn_eval_2$`Class Level Results`$`Confusion Matrix`[[1]]),
               c("Prediction", "Target", "Pos_0", "Pos_1", "N"))
  expect_equal(colnames(mn_eval_2$Results$`Confusion Matrix`[[1]]),
               c("Prediction", "Target", "N"))

  # Enabling and disabling a few metrics

  set_seed_for_R_compatibility(1)
  mn_eval_3 <- evaluate(
    data = data_ %>% dplyr::sample_n(17),
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multiclass_classification",
    apply_softmax = TRUE,
    metrics = list("Accuracy" = TRUE, "Overall Accuracy" = FALSE, "F1" = FALSE)
  )

  expect_true("Accuracy" %in% colnames(mn_eval_3$Results))
  expect_equal(mn_eval_3$Results$Accuracy, 0.6941176, tolerance = 1e-4)
  expect_true("Accuracy" %in% colnames(mn_eval_3$`Class Level Results`))
  expect_equal(mn_eval_3$`Class Level Results`$Accuracy,
               c(0.7058824, 0.5294118, 0.7647059, 0.7058824, 0.7647059), tolerance = 1e-4)
  expect_true("Overall Accuracy" %ni% colnames(mn_eval_3$Results))
  expect_true("F1" %ni% colnames(mn_eval_3$Results))
  expect_true("F1" %ni% colnames(mn_eval_3$`Class Level Results`))


  # TODO

  # ID level
  data_ <- data_ %>%
    dplyr::mutate(id = factor(rep(1:10, each=2)))

  set_seed_for_R_compatibility(9)
  suppressWarnings(
    mn_id_eval_1 <- evaluate(
    data = data_ %>% dplyr::sample_n(13),
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    id_col = "id",
    id_method = "mean",
    type = "multiclass_classification",
    apply_softmax = TRUE,
    metrics = "all"
  ))

  expect_equal(mn_id_eval_1$Results$`Overall Accuracy`, 0.222222, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Balanced Accuracy`, 0.5535714, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Balanced Accuracy`,
               mean(mn_id_eval_1$`Class Level Results`$`Balanced Accuracy`))
  expect_equal(mn_id_eval_1$Results$`Weighted Balanced Accuracy`, 0.5178571, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Weighted Balanced Accuracy`,
               manual_weighted_mean(x = mn_id_eval_1$`Class Level Results`$`Balanced Accuracy`,
                                    w = mn_id_eval_1$`Class Level Results`$Support))
  expect_equal(mn_id_eval_1$Results$Accuracy, 0.688888889, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Weighted Accuracy`, 0.6790123, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$F1, NaN)
  expect_equal(mn_id_eval_1$Results$`Weighted F1`, NaN)
  expect_equal(mn_id_eval_1$Results$Sensitivity, 0.3, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Weighted Sensitivity`, 0.222222, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$Specificity, 0.8071429, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Weighted Specificity`, 0.8134921, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Pos Pred Value`, NaN)
  expect_equal(mn_id_eval_1$Results$`Weighted Pos Pred Value`, NaN)
  expect_equal(mn_id_eval_1$Results$`Neg Pred Value`, 0.8126984, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Weighted Neg Pred Value`, 0.7918871, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$AUC, 0.6142857, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Weighted AUC`, 0.5714286, tolerance = 1e-4)
  expect_true(is.na(mn_id_eval_1$Results$`Lower CI`))
  expect_true(is.na(mn_id_eval_1$Results$`Weighted Lower CI`))
  expect_true(is.na(mn_id_eval_1$Results$`Upper CI`))
  expect_true(is.na(mn_id_eval_1$Results$`Weighted Upper CI`))
  expect_equal(mn_id_eval_1$Results$Kappa, 0.03714286, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Weighted Kappa`, -0.003174603, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$MCC, 0.05714286, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Weighted MCC`, 0.007936508, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Detection Rate`, 0.04444444, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Weighted Detection Rate`, 0.03703704, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Detection Prevalence`, 0.2, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Weighted Detection Prevalence`, 0.1851852, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$Prevalence, 0.2, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Weighted Prevalence`, 0.2098765, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$Class,
               c("cl_1","cl_2","cl_3","cl_4","cl_5"))
  expect_equal(mn_id_eval_1$`Class Level Results`$`Balanced Accuracy`,
               c(0.6785714, 0.3571429, 0.3571429, 0.8750000, 0.5000000), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$F1,
               c(0.5, NaN, NaN, 0.5, NA), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$Sensitivity,
               c(0.5,0.0,0.0,1.0,0.0), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$Specificity,
               c(0.8571429,0.7142857,0.7142857,0.7500000,1.0000000), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$`Pos Pred Value`,
               c(0.5000000,0.0000000,0.0000000,0.3333333,NaN), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$`Neg Pred Value`,
               c(0.8571429, 0.7142857, 0.7142857, 1.0000000, 0.7777778), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$AUC,
               c(0.4285714,0.2142857,0.7857143,1.0000000,0.6428571), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$`Lower CI`,
               c(0.0,0.0000000,0.3213953,NA,0.1380892), tolerance = 1e-4) # TODO How did the NA happen?
  expect_equal(mn_id_eval_1$`Class Level Results`$`Upper CI`,
               c(1.0000000,0.5375959,1.0000000,NA,1.0000000), tolerance = 1e-4) # TODO How did the NA happen?
  expect_equal(mn_id_eval_1$`Class Level Results`$Kappa,
               c(0.3571429,-0.2857143,-0.2857143,0.4000000,0.0000000), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$MCC,
               c(0.3571429, -0.2857143, -0.2857143, 0.5000000, 0.0000000), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$`Detection Rate`,
               c(0.1111111, 0.0000000, 0.0000000, 0.1111111, 0.0000000), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$`Detection Prevalence`,
               c(0.2222222,0.2222222,0.2222222,0.3333333,0.0000000), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$Prevalence,
               c(0.2222222,0.2222222,0.2222222,0.1111111,0.2222222), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$`Class Level Results`$Support,
               c(2,2,2,1,2))
  expect_equal(mn_id_eval_1$`Class Level Results`$`Confusion Matrix`[[1]]$Prediction,
               c("0", "1", "0", "1"))
  expect_equal(mn_id_eval_1$`Class Level Results`$`Confusion Matrix`[[1]]$Target,
               c("0", "0", "1", "1"))
  expect_equal(mn_id_eval_1$`Class Level Results`$`Confusion Matrix`[[1]]$Pos_0,
               c("TP", "FN", "FP", "TN"))
  expect_equal(mn_id_eval_1$`Class Level Results`$`Confusion Matrix`[[1]]$Pos_1,
               c("TN", "FP", "FN", "TP"))
  expect_equal(mn_id_eval_1$`Class Level Results`$`Confusion Matrix`[[1]]$N,
               c(6, 1, 1, 1))
  expect_equal(colnames(mn_id_eval_1$`Class Level Results`$ROC[[1]]),
               c("Sensitivities", "Specificities"))
  expect_equal(colnames(mn_id_eval_1$`Class Level Results`$`Confusion Matrix`[[1]]),
               c("Prediction", "Target", "Pos_0", "Pos_1", "N"))
  expect_equal(colnames(mn_id_eval_1$Results$`Confusion Matrix`[[1]]),
               c("Prediction", "Target", "N"))



  # Test grouping vars
  data_2 <- data_ %>% dplyr::mutate(fold_ = 1) %>% dplyr::bind_rows(data_ %>% dplyr::mutate(fold_ = 2))

  mn_id_eval_2 <- evaluate(
    data = data_2 %>% dplyr::group_by(fold_),
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    id_col = "id",
    id_method = "majority",
    type = "multiclass_classification",
    apply_softmax = TRUE
  )

  expect_equal(mn_id_eval_2$Results$fold_, c(1,2))
  expect_equal(mn_id_eval_2$`Class Level Results`$fold_,rep(1:2, each=5))
  expect_equal(colnames(mn_id_eval_2$Results),
               c("fold_","Overall Accuracy","Balanced Accuracy","F1","Sensitivity",
                 "Specificity","Pos Pred Value","Neg Pred Value","AUC","Lower CI",
                 "Upper CI","Kappa","MCC","Detection Rate","Detection Prevalence",
                 "Prevalence","Predictions","Confusion Matrix"))
  expect_equal(colnames(mn_id_eval_2$`Class Level Results`),
               c("fold_","Class","Balanced Accuracy","F1","Sensitivity",
                 "Specificity","Pos Pred Value","Neg Pred Value","AUC","Lower CI",
                 "Upper CI","Kappa","MCC","Detection Rate","Detection Prevalence",
                 "Prevalence","Support","ROC","Confusion Matrix"))
  expect_equal(mn_id_eval_2$`Class Level Results`$`Confusion Matrix`[[1]],
               mn_id_eval_2$`Class Level Results`$`Confusion Matrix`[[6]])





  # TODO test that group_by and evaluate work correctly together

})

# TODO Add test that majority vote id_method works when not all classes are predicted most by one of the ids

test_that("arguments throw proper errors and warnings in evaluate()",{

  set_seed_for_R_compatibility(1)
  random_probabilities <- multiclass_probability_tibble(
    num_classes = 5,
    num_observations = 20,
    apply_softmax = FALSE # Test with as well
  )
  data_ <- random_probabilities %>%
    dplyr::mutate(cl = as.factor(rep(1:5, each = 4)),
                  cl_char = paste0("cl_", cl)) %>%
    dplyr::rename_at(dplyr::vars(paste0("class_", 1:5)), .funs = ~paste0("cl_", 1:5))

  # Testing 'metrics'

  expect_error(evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multiclass_classification",
    apply_softmax = TRUE,
    metrics = "none"
  ), "'metrics' must be either a list or the string 'all'.",
  fixed=TRUE)

  expect_error(evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multiclass_classification",
    apply_softmax = TRUE,
    metrics = list(TRUE,FALSE)
  ), "when 'metrics' is a non-empty list, it must be a named list.",
  fixed=TRUE)

  # TODO add more

  # Testing gaussian
  expect_error(evaluate(
    data = data_,
    target_col = "cl_1",
    prediction_cols = "cl_2",
    type = "gaussian",
    id_col = "cl",
    id_method = "mean"
    ), paste0("The targets must be constant within the IDs with the current ID method. ",
              "These IDs had more than one unique value in the target column: 1, 2, 3, 4, 5."),
    fixed=TRUE)


})

test_that("binomial evaluation works in evaluate()",{

  set_seed_for_R_compatibility(1)
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
                               "cl_1"),
      inv_prediction = 1 - prediction,
      inv_predicted_class = ifelse(inv_prediction > 0.5,
                                   "cl_2",
                                   "cl_1"),
    )

  bn_eval_1 <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "prediction",
    type = "binomial_classification",
    apply_softmax = TRUE,
    metrics = list("Accuracy" = TRUE)
  )
  bn_eval_1_inv <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "inv_prediction",
    type = "binomial_classification",
    apply_softmax = TRUE,
    metrics = list("Accuracy" = TRUE)
  )

  expect_equal(bn_eval_1$Accuracy,
               mean(data_$cl_char == data_$predicted_class))
  expect_equal(bn_eval_1$Accuracy,
               0.45)
  expect_equal(bn_eval_1$`Balanced Accuracy`,
               0.45)
  expect_equal(bn_eval_1_inv$Accuracy,
               mean(data_$cl_char == data_$inv_predicted_class))
  expect_equal(bn_eval_1_inv$Accuracy,
               0.55)
  expect_equal(bn_eval_1_inv$`Balanced Accuracy`,
               0.55)

  expect_equal(bn_eval_1$F1,
               0.4761905, tolerance = 1e-4)
  expect_equal(bn_eval_1$Sensitivity,
               0.5, tolerance = 1e-4)
  expect_equal(bn_eval_1$Specificity,
               0.4, tolerance = 1e-4)
  expect_equal(bn_eval_1$`Pos Pred Value`,
               0.4545455, tolerance = 1e-4)
  expect_equal(bn_eval_1$`Neg Pred Value`,
               0.4444444, tolerance = 1e-4)
  expect_equal(bn_eval_1$AUC,
               0.53, tolerance = 1e-4)
  expect_equal(bn_eval_1$`Lower CI`,
               0.2573215, tolerance = 1e-4)
  expect_equal(bn_eval_1$`Upper CI`,
               0.8026785, tolerance = 1e-4)
  expect_equal(bn_eval_1$Kappa,
               -0.1, tolerance = 1e-4)
  expect_equal(bn_eval_1$MCC,
               -0.1005038, tolerance = 1e-4)
  expect_equal(bn_eval_1$`Detection Rate`,
               0.25, tolerance = 1e-4)
  expect_equal(bn_eval_1$`Detection Prevalence`,
               0.55, tolerance = 1e-4)
  expect_equal(bn_eval_1$Prevalence,
               0.5, tolerance = 1e-4)

  expect_equal(bn_eval_1_inv$F1,
               0.5263158, tolerance = 1e-4)
  expect_equal(bn_eval_1_inv$Sensitivity,
               0.5, tolerance = 1e-4)
  expect_equal(bn_eval_1_inv$Specificity,
               0.6, tolerance = 1e-4)
  expect_equal(bn_eval_1_inv$`Pos Pred Value`,
               0.5555556, tolerance = 1e-4)
  expect_equal(bn_eval_1_inv$`Neg Pred Value`,
               0.5454545, tolerance = 1e-4)
  expect_equal(bn_eval_1_inv$AUC,
               0.47, tolerance = 1e-4)
  expect_equal(bn_eval_1_inv$`Lower CI`,
               0.1973215, tolerance = 1e-4)
  expect_equal(bn_eval_1_inv$`Upper CI`,
               0.7426785, tolerance = 1e-4)
  expect_equal(bn_eval_1_inv$Kappa,
               0.1, tolerance = 1e-4)
  expect_equal(bn_eval_1_inv$MCC,
               0.1005038, tolerance = 1e-4)
  expect_equal(bn_eval_1_inv$`Detection Rate`,
               0.25, tolerance = 1e-4)
  expect_equal(bn_eval_1_inv$`Detection Prevalence`,
               0.45, tolerance = 1e-4)
  expect_equal(bn_eval_1_inv$Prevalence,
               0.5, tolerance = 1e-4)

  bn_eval_2 <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "prediction",
    type = "binomial_classification",
    apply_softmax = TRUE,
    positive = "cl_1",
    metrics = list("Accuracy" = TRUE)
  )

  expect_equal(bn_eval_2$Accuracy,
               mean(data_$cl_char == data_$predicted_class))
  expect_equal(bn_eval_2$Accuracy,
               0.45)
  expect_equal(bn_eval_2$`Balanced Accuracy`,
               0.45)
  expect_equal(bn_eval_2$F1,
               0.4210526, tolerance = 1e-4)
  expect_equal(bn_eval_2$Sensitivity,
               0.4, tolerance = 1e-4)
  expect_equal(bn_eval_2$Specificity,
               0.5, tolerance = 1e-4)
  expect_equal(bn_eval_2$`Pos Pred Value`,
               0.4444444, tolerance = 1e-4)
  expect_equal(bn_eval_2$`Neg Pred Value`,
               0.4545455, tolerance = 1e-4)
  expect_equal(bn_eval_2$AUC,
               0.53, tolerance = 1e-4)
  expect_equal(bn_eval_2$`Lower CI`,
               0.2573215, tolerance = 1e-4)
  expect_equal(bn_eval_2$`Upper CI`,
               0.8026785, tolerance = 1e-4)
  expect_equal(bn_eval_2$Kappa,
               -0.1, tolerance = 1e-4)
  expect_equal(bn_eval_2$MCC,
               -0.1005038, tolerance = 1e-4)
  expect_equal(bn_eval_2$`Detection Rate`,
               0.2, tolerance = 1e-4)
  expect_equal(bn_eval_2$`Detection Prevalence`,
               0.45, tolerance = 1e-4)
  expect_equal(bn_eval_2$Prevalence,
               0.5, tolerance = 1e-4)

  # not including predictions
  bn_eval_2_no_preds <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "prediction",
    type = "binomial_classification",
    apply_softmax = TRUE,
    positive = "cl_1",
    metrics = list("Accuracy" = TRUE),
    include_predictions = FALSE
  )

  expect_equal(colnames(bn_eval_2_no_preds),
               c("Balanced Accuracy","Accuracy","F1","Sensitivity",
                 "Specificity","Pos Pred Value","Neg Pred Value","AUC",
                 "Lower CI","Upper CI","Kappa","MCC",
                 "Detection Rate","Detection Prevalence","Prevalence",
                 "ROC","Confusion Matrix"))
  expect_identical(bn_eval_2_no_preds, bn_eval_2 %>% dplyr::select(-dplyr::one_of("Predictions")))


  # TODO Create actual expected tests, where you curate a dataset, an aggregated version (all methods)
  # and make sure the results are identical in all settings.


  # ID level
  data_ <- data_ %>%
    dplyr::mutate(id = factor(rep(1:10, each=2)))

  bn_eval_3 <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "prediction",
    id_col = "id",
    id_method = "mean",
    type = "binomial_classification",
    apply_softmax = TRUE
  )

  expect_equal(colnames(bn_eval_3),
               c("Balanced Accuracy","F1","Sensitivity",
                 "Specificity","Pos Pred Value","Neg Pred Value","AUC",
                 "Lower CI","Upper CI","Kappa","MCC",
                 "Detection Rate","Detection Prevalence","Prevalence",
                 "Predictions","ROC","Confusion Matrix"))
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
               c("cl_1","cl_1","cl_1","cl_1","cl_1",
                 "cl_2","cl_2","cl_2","cl_2","cl_2"), tolerance = 1e-4)
  expect_equal(bn_eval_3$Predictions[[1]]$Prediction,
               c( 0.3188163,0.7405306,0.5500358,0.8027365,0.3454502,
                  0.1912657,0.5355633,0.6337703,0.8547623,0.5787402), tolerance = 1e-4)
  expect_equal(bn_eval_3$Predictions[[1]]$`Predicted Class`,
               c("cl_1","cl_2","cl_2","cl_2","cl_1",
                 "cl_1","cl_2","cl_2","cl_2","cl_2"), tolerance = 1e-4)
  expect_equal(bn_eval_3$Predictions[[1]]$id,
               factor(1:10), tolerance = 1e-4)
  expect_equal(bn_eval_3$Predictions[[1]]$id_method,
               rep("mean",10), tolerance = 1e-4)

  bn_eval_3_no_preds <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "prediction",
    id_col = "id",
    id_method = "mean",
    type = "binomial_classification",
    apply_softmax = TRUE,
    include_predictions = FALSE
  )

  expect_equal(colnames(bn_eval_3_no_preds),
               c("Balanced Accuracy","F1","Sensitivity",
                 "Specificity","Pos Pred Value","Neg Pred Value","AUC",
                 "Lower CI","Upper CI","Kappa","MCC",
                 "Detection Rate","Detection Prevalence","Prevalence",
                 "ROC","Confusion Matrix"))

  # TODO ADD TESTS HERE!

  # Majority vote
  bn_eval_4 <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "prediction",
    id_col = "id",
    id_method = "majority",
    type = "binomial_classification",
    apply_softmax = FALSE
  )


  expect_equal(colnames(bn_eval_4),
               c("Balanced Accuracy","F1","Sensitivity",
                 "Specificity","Pos Pred Value","Neg Pred Value","AUC",
                 "Lower CI","Upper CI","Kappa","MCC",
                 "Detection Rate","Detection Prevalence","Prevalence",
                 "Predictions","ROC","Confusion Matrix"))
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
               c("cl_1","cl_1","cl_1","cl_1","cl_1",
                 "cl_2","cl_2","cl_2","cl_2","cl_2"), tolerance = 1e-4)
  expect_equal(bn_eval_4$Predictions[[1]]$Prediction,
               c(1e-40,1e+00,5e-01,1e+00,5e-01,
                 1e-40,5e-01,5e-01,1e+00,5e-01), tolerance = 1e-4)
  expect_equal(bn_eval_4$Predictions[[1]]$`Predicted Class`,
               c("cl_1","cl_2","cl_2","cl_2","cl_2",
                 "cl_1","cl_2","cl_2","cl_2","cl_2"), tolerance = 1e-4)
  expect_equal(bn_eval_4$Predictions[[1]]$id,
               factor(1:10), tolerance = 1e-4)
  expect_equal(bn_eval_4$Predictions[[1]]$id_method,
               rep("majority",10), tolerance = 1e-4)

  data_2 <- data_ %>% dplyr::mutate(fold_ = 1) %>%
    dplyr::bind_rows(data_ %>% dplyr::mutate(fold_ = 2))

  bn_eval_5 <- evaluate(
    data = data_2 %>% dplyr::group_by(fold_),
    target_col = "cl_char",
    prediction_cols = "prediction",
    id_col = "id",
    id_method = "majority",
    type = "binomial_classification",
    apply_softmax = FALSE
  )

  # TODO Add tests here that grouped dataframes work in binomial!


})

test_that("softmax works in multiclass_probability_tibble()",{

  # Test softmax was applied correctly in multiclass_probability_tibble
  set_seed_for_R_compatibility(1)
  random_probabilities_1 <- multiclass_probability_tibble(
    num_classes = 3,
    num_observations = 20,
    apply_softmax = TRUE
  )
  set_seed_for_R_compatibility(1)
  random_probabilities_2 <- multiclass_probability_tibble(
    num_classes = 3,
    num_observations = 20,
    apply_softmax = FALSE
  ) %>% softmax()

  expect_equal(sum(random_probabilities_1),sum(random_probabilities_2))
  expect_equal(sum(random_probabilities_1), 20) # due to softmax, each row sums to 1

  expect_equal(sum(softmax_row(c(1,2,3,4))), 1)
  expect_equal(as.vector(t(softmax_row(c(1,2,3,4)))),
               c(0.03205860,0.08714432,0.23688282,0.64391426), tolerance = 1e-4)
  expect_equal(colnames(softmax_row(c(1,2,3,4))), c("V1","V2","V3","V4"))

})

test_that("probability nesting works in multinomial evaluate",{

  set_seed_for_R_compatibility(1)
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
    nest_probabilities_rowwise()
  })

  expect_true(identical(manually_nested_probs,package_nested_probs))

  unnested <- package_nested_probs %>%
    dplyr::bind_rows()

  expect_true(identical(random_probabilities_1,unnested))

})

test_that("gaussian evaluations are correct in evaluate()",{

  set_seed_for_R_compatibility(1)

  score_model_1 <- lm("score ~ diagnosis", data = participant.scores)
  score_model_2 <- lm("score ~ diagnosis+age", data = participant.scores)

  # summary(score_model_1)

  score_predictions <- stats::predict(score_model_1, participant.scores,
                                      type="response", allow.new.levels = TRUE)

  eval_data <- participant.scores
  eval_data[["score_predictions"]] <- score_predictions

  e1 <- evaluate(eval_data, target_col = "score",
                 prediction_cols = "score_predictions",
                 models = list(score_model_1),
                 type = "gaussian",
                 metrics = "all")

  expect_equal(e1$RMSE, 16.16881, tolerance = 1e-4)
  expect_equal(e1$MAE, 13.47778, tolerance = 1e-4)
  expect_equal(e1$r2m, 0.2665756, tolerance = 1e-4)
  expect_equal(e1$r2c, 0.2665756, tolerance = 1e-4)
  expect_equal(e1$AIC, 258.1214, tolerance = 1e-4)
  expect_equal(e1$AICc, 259.0444, tolerance = 1e-4)
  expect_equal(e1$BIC, 262.325, tolerance = 1e-4)
  expect_equal(e1$Predictions[[1]][["Target"]], eval_data$score)
  expect_equal(e1$Predictions[[1]][["Prediction"]], eval_data$score_predictions)
  expect_equal(e1$Coefficients[[1]]$term, c("(Intercept)","diagnosis"))
  expect_equal(e1$Coefficients[[1]]$estimate, c(50.91667, -20.25000), tolerance = 1e-4)
  expect_equal(e1$Coefficients[[1]]$std.error, c(4.831357, 6.237256), tolerance = 1e-4)
  expect_equal(e1$Coefficients[[1]]$statistic, c(10.53879, -3.24662), tolerance = 1e-4)
  expect_equal(e1$Coefficients[[1]]$p.value, c(2.984260e-11, 3.025349e-03), tolerance = 1e-4)

  # Not passing a model
  # This should remove the metrics that depend on the models to be passed
  e2 <- evaluate(eval_data, target_col = "score",
                 prediction_cols = "score_predictions",
                 models = NULL,
                 type = "gaussian",
                 metrics = "all")

  expect_equal(colnames(e2), c("RMSE","MAE","Predictions"))
  expect_equal(e2$RMSE, 16.16881, tolerance = 1e-4)
  expect_equal(e2$MAE, 13.47778, tolerance = 1e-4)
  expect_equal(e2$Predictions[[1]][["Target"]], eval_data$score)
  expect_equal(e2$Predictions[[1]][["Prediction"]], eval_data$score_predictions)

  # Grouped with multiple models

  eval_data_2 <- eval_data

  score_predictions_2 <- stats::predict(score_model_2, participant.scores,
                                        type = "response",  allow.new.levels = TRUE)

  eval_data_2[["score_predictions"]] <- score_predictions_2

  e3 <- evaluate(eval_data_2, target_col = "score",
                 prediction_cols = "score_predictions",
                 models = list(score_model_2),
                 type = "gaussian",
                 metrics = "all")


  eval_data_3 <- dplyr::bind_rows(eval_data %>% dplyr::mutate(fold_ = 1),
                                  eval_data_2 %>% dplyr::mutate(fold_ = 2)) %>%
    dplyr::group_by(fold_)

  # eval_data_3 %>% dplyr::group_keys()
  # eval_data_3 %>% dplyr::group_indices()

  e4 <- evaluate(eval_data_3, target_col = "score",
                 prediction_cols = "score_predictions",
                 models = list(score_model_1,
                               score_model_2),
                 type = "gaussian",
                 metrics = "all") %>%
    dplyr::mutate(fold_ = as.factor(.data$fold_))

  e1_e3 <- dplyr::bind_rows(e1, e3) %>%
    dplyr::mutate(fold_ = factor(1:2)) %>%
    dplyr::select(.data$fold_, dplyr::everything())

  expect_true(length(setdiff(colnames(e4), colnames(e1_e3))) == 0)
  expect_identical(e4, e1_e3)

  expect_equal(e4$fold_, factor(c(1,2)))
  expect_equal(e4$RMSE, c(16.16881, 16.12762), tolerance = 1e-4)
  expect_equal(e4$MAE, c(13.47778, 13.28942), tolerance = 1e-4)
  expect_equal(e4$r2m, c(0.2665756, 0.2631030), tolerance = 1e-4)
  expect_equal(e4$r2c, c(0.2665756, 0.2631030), tolerance = 1e-4)
  expect_equal(e4$AIC, c(258.1214, 259.9683), tolerance = 1e-4)
  expect_equal(e4$AICc, c(259.0444, 261.5683), tolerance = 1e-4)
  expect_equal(e4$BIC, c(262.3250, 265.5731), tolerance = 1e-4)
  expect_equal(e4$Predictions[[1]]$Target,
               c(10,24,45,24,40,67,15,30,40,35,50,78,24,54,62,
                 14,25,30,11,35,41,16,32,44,33,53,66,29,55,81), tolerance = 1e-4)
  expect_equal(e4$Predictions[[2]]$Target,
               c(10,24,45,24,40,67,15,30,40,35,50,78,24,54,62,
                 14,25,30,11,35,41,16,32,44,33,53,66,29,55,81), tolerance = 1e-4)
  expect_equal(e4$Predictions[[1]]$Prediction,
               c(30.66667,30.66667,30.66667,50.91667,50.91667,
                 50.91667,30.66667,30.66667,30.66667,50.91667,
                 50.91667,50.91667,30.66667,30.66667,30.66667,
                 30.66667,30.66667,30.66667,30.66667,30.66667,
                 30.66667,30.66667,30.66667,30.66667,50.91667,
                 50.91667,50.91667,50.91667,50.91667,50.91667), tolerance = 1e-4)
  expect_equal(e4$Predictions[[2]]$Prediction,
               c(29.17288,29.17288,29.17288,50.16977,50.16977,
                 50.16977,30.33471,30.33471,30.33471,49.83782,
                 49.83782,49.83782,31.16460,31.16460,31.16460,
                 30.99862,30.99862,30.99862,32.99034,32.99034,
                 32.99034,29.33885,29.33885,29.33885,51.99551,
                 51.99551,51.99551,51.66356,51.66356,51.66356), tolerance = 1e-4)
  expect_equal(colnames(e4$Coefficients[[1]]),
               colnames(e4$Coefficients[[2]]), tolerance = 1e-4)
  expect_equal(colnames(e4$Coefficients[[1]]),
               c("term","estimate","std.error","statistic","p.value"))
  binded_coefficients <- dplyr::bind_rows(e4$Coefficients)
  expect_equal(binded_coefficients$term,
               c("(Intercept)", "diagnosis", "(Intercept)",
                 "diagnosis", "age"), tolerance = 1e-4)
  expect_equal(binded_coefficients$estimate,
               c(50.9166667,-20.2500000, 46.3523119,
                 -20.4989648, 0.1659765), tolerance = 1e-4)
  expect_equal(binded_coefficients$std.error,
               c(4.8313574,6.2372555,13.2255733, 6.3708432,
                 0.4465959), tolerance = 1e-4)
  expect_equal(binded_coefficients$statistic,
               c(10.5387913,-3.2466202,3.5047488,
                 -3.2176219,0.3716482), tolerance = 1e-4)
  expect_equal(binded_coefficients$p.value,
               c(2.984260e-11,3.025349e-03,1.613661e-03,
                 3.348279e-03,7.130556e-01), tolerance = 1e-4)

  # Errors

  expect_error(evaluate(eval_data, target_col = "score",
                        prediction_cols = "score_predictions",
                        models = list(),
                        type = "gaussian",
                        metrics = "all"),
               paste0(
                 "'models' must be either NULL or an unnamed list with fitted model object(s). ",
                 "'models' had length 0."),
               fixed = TRUE)

  expect_error(evaluate(eval_data, target_col = "score",
           prediction_cols = "score_predictions",
           models = score_model_1,
           type = "gaussian",
           metrics = "all"),
           paste0("'models' must be provided as an unnamed list with fitted model object(s).",
           " Did you pass the model object without putting it in a list?"),
           fixed = TRUE)

  expect_error(evaluate(eval_data_3, target_col = "score",
                        prediction_cols = "score_predictions",
                        models = list(score_model_1),
                        type = "gaussian",
                        metrics = "all"),
               paste0("When the dataframe is grouped, please provide ",
                      "a fitted model object per group or set models to NULL."),
               fixed = TRUE)


  # ID evaluation
  age_model_1 <- lm("age ~ diagnosis", participant.scores)
  age_model_2 <- lm("age ~ diagnosis + score", participant.scores)
  age_predictions_1 <- stats::predict(age_model_1, participant.scores,
                                      type = "response",  allow.new.levels = TRUE)
  age_predictions_2 <- stats::predict(age_model_2, participant.scores,
                                      type = "response",  allow.new.levels = TRUE)

  id_eval_data_4 <- participant.scores %>%
    dplyr::mutate(fold_ = 1,
                  predicted_age = age_predictions_1) %>%
    dplyr::bind_rows(participant.scores %>%
                       dplyr::mutate(fold_ = 2,
                                     predicted_age = age_predictions_2)) %>%
    dplyr::group_by(fold_)

  e5 <- evaluate(id_eval_data_4, target_col = "age",
                 prediction_cols = "predicted_age",
                 id_col = "participant",
                 id_method = "mean",
                 type = "gaussian",
                 metrics = "all")

  expect_equal(e5$fold_, c(1,2))
  expect_equal(e5$RMSE, c(6.949820, 6.917232), tolerance = 1e-4)
  expect_equal(e5$MAE, c(6.0, 5.935604), tolerance = 1e-4)
  expect_equal(length(e5$Predictions), 2, tolerance = 1e-4)
  expect_equal(colnames(e5$Predictions[[1]]),
               c("Target", "Prediction", "participant", "id_method"), tolerance = 1e-4)
  expect_equal(colnames(e5$Predictions[[2]]),
               c("Target", "Prediction", "participant", "id_method"), tolerance = 1e-4)
  expect_equal(e5$Predictions[[1]]$Target,
               c(20, 23, 27, 21, 32, 31, 43, 21, 34, 32), tolerance = 1e-4)
  expect_equal(e5$Predictions[[2]]$Target,
               c(20, 23, 27, 21, 32, 31, 43, 21, 34, 32), tolerance = 1e-4)
  expect_equal(e5$Predictions[[1]]$Prediction,
               c(29.0,27.5,29.0,27.5,29.0,29.0,29.0,29.0,27.5,27.5), tolerance = 1e-4)
  expect_equal(e5$Predictions[[2]]$Prediction,
               c(28.86712,27.27768,28.92845,27.60477,29.49063,
                 28.76490,28.94889,29.00000,27.49233,27.62521), tolerance = 1e-4)
  expect_equal(e5$Predictions[[1]]$participant,
               factor(1:10), tolerance = 1e-4)
  expect_equal(e5$Predictions[[2]]$participant,
               factor(1:10), tolerance = 1e-4)
  expect_equal(e5$Predictions[[1]]$id_method,
               rep("mean", 10), tolerance = 1e-4)
  expect_equal(e5$Predictions[[2]]$id_method,
               rep("mean", 10), tolerance = 1e-4)

  # Not including predictions in the output
  e6 <- evaluate(id_eval_data_4, target_col = "age",
                 prediction_cols = "predicted_age",
                 id_col = "participant",
                 id_method = "mean",
                 type = "gaussian",
                 metrics = "all",
                 include_predictions = FALSE)

  expect_equal(colnames(e6), c("fold_","RMSE","MAE"))

  # Errors

  expect_error(evaluate(id_eval_data_4, target_col = "age",
                        prediction_cols = "predicted_age",
                        models = list(age_model_1,
                                      age_model_2),
                        id_col = "participant",
                        id_method = "mean",
                        type = "gaussian",
                        metrics = "all"),
               "When aggregating by ID, 'models' should be NULL.",
               fixed = T)

})

test_that("evaluate() treats dfs and tbls the same",{

  # Gaussian

  # Binomial

  # Multinomial
  set_seed_for_R_compatibility(1)
  random_probabilities <- multiclass_probability_tibble(
    num_classes = 5,
    num_observations = 20,
    apply_softmax = FALSE # Test with as well
  )
  expect_equal(sum(random_probabilities), 51.78471, tolerance = 1e-5)

  data_ <- random_probabilities %>%
    dplyr::mutate(cl = as.factor(rep(1:5, each = 4)),
                  cl_char = paste0("cl_", cl)) %>%
    dplyr::rename_at(dplyr::vars(paste0("class_", 1:5)), .funs = ~paste0("cl_", 1:5))

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

  mn_eval_1_dt <- evaluate(
    data = as.data.table(data_),
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multinomial",
    apply_softmax = TRUE
  )

  expect_identical(mn_eval_1_tbl, mn_eval_1_df)

  # There is a "attr(*, ".internal.selfref")=<externalptr> " attribute added to the
  # predictions list with the data.table.
  expect_identical(mn_eval_1_tbl$Results$Predictions[[1]]$Prediction,
                   mn_eval_1_dt$Results$Predictions[[1]]$Prediction)
  mn_eval_1_dt$Results$Predictions <- NULL
  mn_eval_1_tbl$Results$Predictions <- NULL
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
