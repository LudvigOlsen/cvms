library(cvms)
context("evaluate()")


test_that("multinomial evaluations are correct in evaluate()",{

  set_seed_for_R_compatibility(1)
  random_probabilities <- create_multinomial_probability_tibble(
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
      type = "multiclass_classification",
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
    type = "multiclass_classification",
    apply_softmax = TRUE
  )

  # TODO Add more tests
  expect_equal(mn_eval_1$Results$`Overall Accuracy`, 0.2, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$`Balanced Accuracy`, 0.5, tolerance = 1e-4)
  expect_equal(mn_eval_1$Results$`Balanced Accuracy`,
               mean(mn_eval_1$Class_level_results$`Balanced Accuracy`))
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
  expect_equal(mn_eval_1$Class_level_results$Class,
               c("cl_1","cl_2","cl_3","cl_4","cl_5"))
  expect_equal(mn_eval_1$Class_level_results$`Balanced Accuracy`,
               c(0.50000,0.37500,0.59375,0.50000,0.53125), tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$F1,
               c(0.2222222, NaN, 0.3333333, 0.2222222, 0.2500000), tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$Sensitivity,
               c(0.25, 0.0, 0.25, 0.25, 0.25), tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$Specificity,
               c(0.7500,0.7500,0.9375,0.7500,0.8125), tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$`Pos Pred Value`,
               c(0.20, 0.00, 0.50, 0.20, 0.25), tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$`Neg Pred Value`,
               c(0.80, 0.750, 0.8333333, 0.80, 0.81250), tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$AUC,
               c(0.421875, 0.250000, 0.890625, 0.390625, 0.515625), tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$`Lower CI`,
               c(0.04381511, 0.01016288, 0.74101384,0.05608427, 0.26683090), tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$Kappa,
               c(-3.172066e-16, -2.500000e-01, 2.307692e-01, -3.172066e-16, 6.250000e-02),
               tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$MCC,
               c(0.0000, -0.2500, 0.2500, 0.0000, 0.0625), tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$`Detection Rate`,
               c(0.05,0.00, 0.05, 0.05, 0.05), tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$`Detection Prevalence`,
               c(0.25, 0.20, 0.10, 0.25, 0.20), tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$Prevalence,
               c(0.2,0.2,0.2,0.2,0.2), tolerance = 1e-4)
  expect_equal(mn_eval_1$Class_level_results$Support,
               c(4,4,4,4,4))

  # Test Weighted metrics, and metrics == "all"

  set_seed_for_R_compatibility(1)
  mn_eval_2 <- evaluate(
    data = data_ %>% dplyr::sample_n(17),
    target_col = "cl_char",
    prediction_cols = paste0("cl_", 1:5),
    type = "multiclass_classification",
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
               mean(mn_eval_2$Class_level_results$`Balanced Accuracy`))
  expect_equal(mn_eval_2$Results$`Weighted Balanced Accuracy`, 0.5236264, tolerance = 1e-4)
  expect_equal(mn_eval_2$Results$`Weighted Balanced Accuracy`,
               manual_weighted_mean(x = mn_eval_2$Class_level_results$`Balanced Accuracy`,
                                    w = mn_eval_2$Class_level_results$Support))
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
  expect_equal(mn_eval_2$Class_level_results$Class,
               c("cl_1","cl_2","cl_3","cl_4","cl_5"))
  expect_equal(mn_eval_2$Class_level_results$`Balanced Accuracy`,
               c(0.5480769,0.3461538,0.5865385,0.5595238,0.6500000), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$F1,
               c(0.2857143, NaN, 0.3333333, 0.2857143, 0.3333333), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$Sensitivity,
               c(0.250, 0.00, 0.250, 0.3333333, 0.500), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$Specificity,
               c(0.8461538, 0.6923077, 0.9230769, 0.7857143, 0.80), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$`Pos Pred Value`,
               c(0.3333333, 0.0, 0.50, 0.25, 0.25), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$`Neg Pred Value`,
               c(0.7857143, 0.6923077, 0.80, 0.8461538, 0.9230769), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$AUC,
               c(0.4807692, 0.250, 0.8653846, 0.4761905, 0.60), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$`Lower CI`,
               c(0.0798065, 0.0, 0.6833168, 0.1094280, 0.3433805), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$Kappa,
               c(0.1052632, -0.3076923,  0.2093023,  0.1052632,  0.2093023), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$MCC,
               c(0.1069901, -0.3076923, 0.2278664, 0.1069901, 0.2278664), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$`Detection Rate`,
               c(0.05882353,0.00, 0.05882353, 0.05882353, 0.05882353), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$`Detection Prevalence`,
               c(0.1764706,0.2352941,0.1176471,0.2352941,0.2352941), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$Prevalence,
               c(0.2352941,0.2352941,0.2352941,0.1764706,0.1176471), tolerance = 1e-4)
  expect_equal(mn_eval_2$Class_level_results$Support,
               c(4,4,4,3,2))

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
  expect_true("Accuracy" %in% colnames(mn_eval_3$Class_level_results))
  expect_equal(mn_eval_3$Class_level_results$Accuracy,
               c(0.7058824, 0.5294118, 0.7647059, 0.7058824, 0.7647059), tolerance = 1e-4)
  expect_true("Overall Accuracy" %ni% colnames(mn_eval_3$Results))
  expect_true("F1" %ni% colnames(mn_eval_3$Results))
  expect_true("F1" %ni% colnames(mn_eval_3$Class_level_results))


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
               mean(mn_id_eval_1$Class_level_results$`Balanced Accuracy`))
  expect_equal(mn_id_eval_1$Results$`Weighted Balanced Accuracy`, 0.5178571, tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Results$`Weighted Balanced Accuracy`,
               manual_weighted_mean(x = mn_id_eval_1$Class_level_results$`Balanced Accuracy`,
                                    w = mn_id_eval_1$Class_level_results$Support))
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
  expect_equal(mn_id_eval_1$Class_level_results$Class,
               c("cl_1","cl_2","cl_3","cl_4","cl_5"))
  expect_equal(mn_id_eval_1$Class_level_results$`Balanced Accuracy`,
               c(0.6785714, 0.3571429, 0.3571429, 0.8750000, 0.5000000), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Class_level_results$F1,
               c(0.5, NaN, NaN, 0.5, NA), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Class_level_results$Sensitivity,
               c(0.5,0.0,0.0,1.0,0.0), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Class_level_results$Specificity,
               c(0.8571429,0.7142857,0.7142857,0.7500000,1.0000000), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Class_level_results$`Pos Pred Value`,
               c(0.5000000,0.0000000,0.0000000,0.3333333,NaN), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Class_level_results$`Neg Pred Value`,
               c(0.8571429, 0.7142857, 0.7142857, 1.0000000, 0.7777778), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Class_level_results$AUC,
               c(0.4285714,0.2142857,0.7857143,1.0000000,0.6428571), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Class_level_results$`Lower CI`,
               c(0.0,0.0000000,0.3213953,NA,0.1380892), tolerance = 1e-4) # TODO How did the NA happen?
  expect_equal(mn_id_eval_1$Class_level_results$`Upper CI`,
               c(1.0000000,0.5375959,1.0000000,NA,1.0000000), tolerance = 1e-4) # TODO How did the NA happen?
  expect_equal(mn_id_eval_1$Class_level_results$Kappa,
               c(0.3571429,-0.2857143,-0.2857143,0.4000000,0.0000000), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Class_level_results$MCC,
               c(0.3571429, -0.2857143, -0.2857143, 0.5000000, 0.0000000), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Class_level_results$`Detection Rate`,
               c(0.1111111, 0.0000000, 0.0000000, 0.1111111, 0.0000000), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Class_level_results$`Detection Prevalence`,
               c(0.2222222,0.2222222,0.2222222,0.3333333,0.0000000), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Class_level_results$Prevalence,
               c(0.2222222,0.2222222,0.2222222,0.1111111,0.2222222), tolerance = 1e-4)
  expect_equal(mn_id_eval_1$Class_level_results$Support,
               c(2,2,2,1,2))



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

  # TODO test that group_by and evaluate work correctly together

})

# TODO Add test that majority vote id_method works when not all classes are predicted most by one of the ids

test_that("arguments throw proper errors and warnings in evaluate()",{

  set_seed_for_R_compatibility(1)
  random_probabilities <- create_multinomial_probability_tibble(
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

})

test_that("binomial evaluation works in evaluate()",{

  set_seed_for_R_compatibility(1)
  random_probabilities <- create_multinomial_probability_tibble(
    num_classes = 1,
    num_observations = 20,
    apply_softmax = FALSE # Test with as well
  )
  expect_equal(sum(random_probabilities), 11.10334, tolerance = 1e-5)

  data_ <- random_probabilities %>%
    dplyr::mutate(cl = as.factor(rep(1:2, each = 10)),
                  cl_char = paste0("cl_", cl))

  bn_eval_1 <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "class_1",
    type = "binomial_classification",
    apply_softmax = TRUE
  )

  # Create actual expected tests, where you curate a dataset, an aggregated version (all methods)
  # and make sure the results are identical in all settings.


  # ID level
  data_ <- data_ %>%
    dplyr::mutate(id = factor(rep(1:10, each=2)))

  bn_eval_2 <- evaluate(
    data = data_,
    target_col = "cl_char",
    prediction_cols = "class_1",
    id_col = "id",
    id_method = "mean",
    type = "binomial_classification",
    apply_softmax = TRUE
  )

  data_2 <- data_ %>% dplyr::mutate(fold_ = 1) %>% dplyr::bind_rows(data_ %>% dplyr::mutate(fold_ = 2))

  bn_eval_3 <- evaluate(
    data = data_2 %>% dplyr::group_by(fold_),
    target_col = "cl_char",
    prediction_cols = "class_1",
    id_col = "id",
    id_method = "majority",
    type = "binomial_classification",
    apply_softmax = FALSE
  )


})

test_that("softmax works in create_multinomial_probability_tibble()",{

  # Test softmax was applied correctly in create_multinomial_probability_tibble
  set_seed_for_R_compatibility(1)
  random_probabilities_1 <- create_multinomial_probability_tibble(
    num_classes = 3,
    num_observations = 20,
    apply_softmax = TRUE
  )
  set_seed_for_R_compatibility(1)
  random_probabilities_2 <- create_multinomial_probability_tibble(
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
  random_probabilities_1 <- create_multinomial_probability_tibble(
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
