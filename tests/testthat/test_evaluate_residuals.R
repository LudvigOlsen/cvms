library(cvms)
context("evaluate_residuals()")

test_that("evaluate_residuals() have expected output",{

  set_seed_for_R_compatibility(1)

  df <- data.frame("t" = runif(20), "p" = runif(20), "grp" = rep(1:5,4))

  ungrouped_res <- evaluate_residuals(
    df, "p", "t",
    metrics = "all"
  )

  expect_equal(colnames(ungrouped_res),
               c("RMSE", "MAE", "NRMSE", "RMSEIQR", "RMSESTD",
                 "RMSLE", "MALE", "RAE", "RSE" ,"RRSE",
                 "MAPE", "MSE", "TAE", "TSE"))
  expect_equal(ungrouped_res$RMSE, 0.4383415, tolerance = 1e-5)
  expect_equal(ungrouped_res$MAE,  0.3493256, tolerance = 1e-5)
  expect_equal(ungrouped_res$NRMSE,  0.4712743, tolerance = 1e-5)
  expect_equal(ungrouped_res$RMSEIQR,  1.028314, tolerance = 1e-5)
  expect_equal(ungrouped_res$RMSESTD,  1.532031, tolerance = 1e-5)
  expect_equal(ungrouped_res$RMSLE,  0.2955423, tolerance = 1e-5)
  expect_equal(ungrouped_res$MALE,  0.2355741, tolerance = 1e-5)
  expect_equal(ungrouped_res$RAE,  1.425217, tolerance = 1e-5)
  expect_equal(ungrouped_res$RSE,  2.470652, tolerance = 1e-5)
  expect_equal(ungrouped_res$RRSE,  1.571831, tolerance = 1e-5)
  expect_equal(ungrouped_res$MAPE,  0.9233602, tolerance = 1e-5)
  expect_equal(ungrouped_res$MSE,  0.1921433, tolerance = 1e-5)
  expect_equal(ungrouped_res$TAE,  6.986512, tolerance = 1e-5)
  expect_equal(ungrouped_res$TSE,  3.842867, tolerance = 1e-5)

  grouped_res <- evaluate_residuals(
    df %>% dplyr::group_by(grp),
    "p", "t",
    metrics = "all"
  )

  expect_equal(colnames(grouped_res),
               c("grp", "RMSE", "MAE", "NRMSE", "RMSEIQR", "RMSESTD",
                 "RMSLE", "MALE", "RAE", "RSE",
                 "RRSE", "MAPE", "MSE", "TAE", "TSE"))
  expect_equal(grouped_res$RMSE,
               c(0.451565647616432, 0.519060010936869, 0.475013011667194, 0.454888553611741,
                 0.234138573611605), tolerance = 1e-5)
  expect_equal(grouped_res$MAE,
               c(0.407086299266666, 0.397724184789695, 0.358668561675586, 0.391197844815906,
                 0.191951038548723), tolerance = 1e-5)
  expect_equal(grouped_res$NRMSE,
               c(0.652160302469174, 0.675755108193839, 1.1335399485195, 0.861249796972752,
                 0.327165018103644), tolerance = 1e-5)
  expect_equal(grouped_res$RMSEIQR,
               c(1.30041734700364, 1.15052496357359, 3.81745140187305, 1.44042831439479,
                 0.386983932227589), tolerance = 1e-5)
  expect_equal(grouped_res$RMSESTD,
               c(1.43814486544896, 1.50905226589782, 2.60289783098973, 1.81516488970619,
                 0.624379332243931), tolerance = 1e-5)
  expect_equal(grouped_res$RMSLE,
               c(0.2886413430748, 0.366211402088592, 0.314237354222149, 0.304503365190871,
                 0.166835911010467), tolerance = 1e-5)
  expect_equal(grouped_res$MALE,
               c(0.263279042673231, 0.281636667412819, 0.235196273801541, 0.260569609060969,
                 0.137188700334378), tolerance = 1e-5)
  expect_equal(grouped_res$RAE,
               c(1.76112392170928, 1.42859014846979, 2.71964745212324, 2.02383072091832,
                 0.598062880532025), tolerance = 1e-5)
  expect_equal(grouped_res$RSE,
               c(2.75768087202294, 3.03631832161513, 9.03343615809475, 4.39309810242944,
                 0.51979940071117), tolerance = 1e-5)
  expect_equal(grouped_res$RRSE,
               c(1.66062665040127, 1.74250346387464, 3.00556752679003, 2.09597187539085,
                 0.720971151094945), tolerance = 1e-5)
  expect_equal(grouped_res$MAPE,
               c(1.19356123357169, 0.979598260014201, 0.432928785595697, 0.665919069119316,
                 1.34479362791513), tolerance = 1e-5)
  expect_equal(grouped_res$MSE,
               c(0.203911534107247, 0.269423294953783, 0.225637361253137, 0.206923596206982,
                 0.0548208716528769), tolerance = 1e-5)
  expect_equal(grouped_res$TAE,
               c(1.62834519706666, 1.59089673915878, 1.43467424670234, 1.56479137926362,
                 0.767804154194891), tolerance = 1e-5)
  expect_equal(grouped_res$TSE,
               c(0.815646136428989, 1.07769317981513, 0.90254944501255, 0.827694384827926,
                 0.219283486611508), tolerance = 1e-5)

})


test_that("call_evaluate_residuals() creates NA results correcly",{

  set_seed_for_R_compatibility(1)

  df <- data.frame("t" = runif(20), "p" = runif(20), "grp" = rep(1:5,4))

  na_tibble <- tibble::tibble(
    "grp" = 1:5,
    "RMSE" = NA,
    "MAE" = NA,
    "TSE" = NA,
    "NRMSE" = NA,
    "RMSEIQR" = NA
  )

  expect_equal(
    call_evaluate_residuals(
      df, "p", "t",
      metrics = c("RMSE", "MAE", "TSE", "NRMSE", "RMSEIQR"),
      return_nas = TRUE
    ),
    na_tibble[1,2:6]
  )

  expect_equal(
    call_evaluate_residuals(
      df %>% dplyr::group_by(grp),
      "p", "t",
      metrics = c("RMSE", "MAE", "TSE", "NRMSE", "RMSEIQR"),
      return_nas = TRUE
    ),
    na_tibble
  )

})


