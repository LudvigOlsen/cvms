library(cvms)
context("set_metrics()")

test_that("metrics are correctly set in set_metrics()", {

  # multinomial
  expect_equal(
    set_metrics("multinomial",
      metrics_list = list("all" = FALSE, "Accuracy" = TRUE)
    ),
    "Accuracy"
  )
  expect_equal(
    set_metrics("multinomial",
      metrics_list = list("Accuracy" = TRUE, "all" = FALSE)
    ),
    "Accuracy"
  )
  expect_equal(
    set_metrics("multinomial",
      metrics_list = list("all" = FALSE, "Accuracy" = FALSE)
    ),
    character()
  )
  expect_equal(
    set_metrics("multinomial",
      metrics_list = list("all" = FALSE, "Weighted F1" = TRUE)
    ),
    "Weighted F1"
  )
  expect_error(set_metrics("multinomial",
    metrics_list = list("all" = FALSE, "RMSE" = TRUE)
  ),
  "'metrics_list' contained unknown metric names: RMSE.",
  fixed = TRUE
  )
  expect_error(set_metrics("multinomial",
    metrics_list = list(
      "all" = FALSE, "Accuracy" = TRUE,
      "Accuracy" = FALSE
    )
  ),
  "'metrics' cannot contain duplicate names.",
  fixed = TRUE
  )
  expect_equal(
    set_metrics("multinomial",
      metrics_list = list("all" = FALSE)
    ),
    character()
  )
  all_1 <- set_metrics("multinomial",
    metrics_list = list("all" = TRUE)
  )
  all_2 <- set_metrics("multinomial",
    metrics_list = "all"
  )
  expect_equal(all_1, all_2)
  expect_equal(
    all_1,
    c(
      "Overall Accuracy", "Balanced Accuracy", "Weighted Balanced Accuracy",
      "Accuracy", "Weighted Accuracy", "F1", "Weighted F1", "Sensitivity",
      "Weighted Sensitivity", "Specificity", "Weighted Specificity",
      "Pos Pred Value", "Weighted Pos Pred Value", "Neg Pred Value",
      "Weighted Neg Pred Value", "AUC", "Kappa", "Weighted Kappa",
      "MCC", "Weighted MCC", "Detection Rate", "Weighted Detection Rate",
      "Detection Prevalence", "Weighted Detection Prevalence", "Prevalence",
      "Weighted Prevalence", "False Neg Rate", "Weighted False Neg Rate",
      "False Pos Rate", "Weighted False Pos Rate", "False Discovery Rate",
      "Weighted False Discovery Rate", "False Omission Rate", "Weighted False Omission Rate",
      "Threat Score", "Weighted Threat Score"
    )
  )


  expect_equal(
    set_metrics("multinomial",
      metrics_list = list(
        "all" = TRUE, "Accuracy" = FALSE, "Overall Accuracy" = FALSE,
        "F1" = TRUE, "Weighted Balanced Accuracy" = FALSE
      )
    ),
    c(
      "Balanced Accuracy", "Weighted Accuracy", "F1", "Weighted F1",
      "Sensitivity", "Weighted Sensitivity", "Specificity", "Weighted Specificity",
      "Pos Pred Value", "Weighted Pos Pred Value", "Neg Pred Value",
      "Weighted Neg Pred Value", "AUC", "Kappa", "Weighted Kappa",
      "MCC", "Weighted MCC", "Detection Rate", "Weighted Detection Rate",
      "Detection Prevalence", "Weighted Detection Prevalence", "Prevalence",
      "Weighted Prevalence", "False Neg Rate", "Weighted False Neg Rate",
      "False Pos Rate", "Weighted False Pos Rate", "False Discovery Rate",
      "Weighted False Discovery Rate", "False Omission Rate", "Weighted False Omission Rate",
      "Threat Score", "Weighted Threat Score"
    )
  )


  # binomial
  expect_equal(
    set_metrics("binomial",
      metrics_list = list("all" = FALSE, "Accuracy" = TRUE)
    ),
    "Accuracy"
  )
  expect_equal(
    set_metrics("binomial",
      metrics_list = list("Accuracy" = TRUE, "all" = FALSE, "F1" = TRUE)
    ),
    c("Accuracy", "F1")
  )
  expect_equal(
    set_metrics("binomial",
      metrics_list = list("all" = FALSE, "Accuracy" = FALSE)
    ),
    character()
  )
  expect_error(set_metrics("binomial",
    metrics_list = list("all" = FALSE, "Weighted F1" = TRUE)
  ),
  "'metrics_list' contained unknown metric names: Weighted F1.",
  fixed = TRUE
  )
  expect_error(set_metrics("binomial",
    metrics_list = list("all" = FALSE, "Weighted F1" = FALSE)
  ),
  "'metrics_list' contained unknown metric names: Weighted F1.",
  fixed = TRUE
  )
  expect_error(set_metrics("binomial",
    metrics_list = list(
      "all" = FALSE, "Accuracy" = TRUE,
      "Accuracy" = FALSE
    )
  ),
  "'metrics' cannot contain duplicate names.",
  fixed = TRUE
  )
  expect_equal(
    set_metrics("binomial",
      metrics_list = list("all" = FALSE)
    ),
    character()
  )

  all_1 <- set_metrics("binomial",
    metrics_list = list("all" = TRUE)
  )
  all_2 <- set_metrics("binomial",
    metrics_list = "all"
  )
  expect_equal(all_1, all_2)
  expect_equal(
    all_1,
    c(
      "Balanced Accuracy", "Accuracy", "F1", "Sensitivity", "Specificity",
      "Pos Pred Value", "Neg Pred Value", "AUC", "Lower CI", "Upper CI",
      "Kappa", "MCC", "Detection Rate", "Detection Prevalence", "Prevalence",
      "False Neg Rate", "False Pos Rate", "False Discovery Rate", "False Omission Rate",
      "Threat Score"
    )
  )

  expect_equal(
    set_metrics("binomial",
      metrics_list = list(
        "all" = TRUE, "Accuracy" = FALSE, "Sensitivity" = FALSE,
        "AUC" = TRUE
      )
    ),
    c(
      "Balanced Accuracy", "F1", "Specificity", "Pos Pred Value",
      "Neg Pred Value", "AUC", "Lower CI", "Upper CI", "Kappa", "MCC",
      "Detection Rate", "Detection Prevalence", "Prevalence", "False Neg Rate",
      "False Pos Rate", "False Discovery Rate", "False Omission Rate",
      "Threat Score"
    )
  )


  # gaussian
  expect_equal(
    set_metrics("gaussian",
      metrics_list = list("all" = FALSE, "RMSE" = TRUE)
    ),
    "RMSE"
  )
  expect_equal(
    set_metrics("gaussian",
      metrics_list = list("RMSE" = TRUE, "all" = FALSE, "MAE" = TRUE)
    ),
    c("RMSE", "MAE")
  )
  expect_equal(
    set_metrics("gaussian",
      metrics_list = list("all" = FALSE, "RMSE" = FALSE)
    ),
    character()
  )
  expect_error(set_metrics("gaussian",
    metrics_list = list("all" = FALSE, "Weighted F1" = TRUE)
  ),
  "'metrics_list' contained unknown metric names: Weighted F1.",
  fixed = TRUE
  )
  expect_error(set_metrics("gaussian",
    metrics_list = list("all" = FALSE, "Weighted F1" = FALSE)
  ),
  "'metrics_list' contained unknown metric names: Weighted F1.",
  fixed = TRUE
  )
  expect_error(set_metrics("gaussian",
    metrics_list = list(
      "all" = FALSE, "RMSE" = TRUE,
      "RMSE" = FALSE
    )
  ),
  "'metrics' cannot contain duplicate names.",
  fixed = TRUE
  )
  expect_equal(
    set_metrics("gaussian",
      metrics_list = list("all" = FALSE)
    ),
    character()
  )

  all_1 <- set_metrics("gaussian",
    metrics_list = list("all" = TRUE)
  )
  all_2 <- set_metrics("gaussian",
    metrics_list = "all"
  )
  expect_equal(all_1, all_2)
  expect_equal(
    all_1,
    c(
      "RMSE", "MAE", "NRMSE", "RMSEIQR", "RMSESTD", "RMSLE", "MALE",
      "RAE", "RSE", "RRSE", "MAPE", "MSE", "TAE", "TSE"
    )
  )

  expect_equal(
    set_metrics("gaussian",
      metrics_list = list(
        "all" = TRUE, "RMSE" = FALSE, "RMSLE" = FALSE,
        "MAE" = TRUE
      )
    ),
    c(
      "MAE", "NRMSE", "RMSEIQR", "RMSESTD", "MALE", "RAE", "RSE",
      "RRSE", "MAPE", "MSE", "TAE", "TSE"
    )
  )
})
