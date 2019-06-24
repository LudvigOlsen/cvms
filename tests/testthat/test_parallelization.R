library(cvms)
context("parallelization")

# Does not actually make checks regarding the parallelization (currently)

# Has been commented as doParallel might not be available on all platforms
# Uncomment to run and comment before CRAN submission


test_that("parallelization works with cross_validate()",{

  testthat::skip(message = "Skipping parallel tests")

  # library(doParallel)
  #
  # # Load data and fold it
  # set.seed(1)
  # dat <- groupdata2::fold(participant.scores, k = 5)
  #
  # wrks <- tryCatch({foreach::getDoParWorkers()}, error = function(e) {
  #   print(e)
  #   NULL
  # })
  #
  # if (!is.null(wrks) && wrks > 1){
  #   doParallel::registerDoParallel(wrks)
  #
  # } else {
  #
  #   # Register more cores than available
  #   # This might be slower than sequential
  #   doParallel::registerDoParallel(4)
  # }
  #
  #
  #   par_time <- system.time({cv_par <- cross_validate(dat,
  #                                                     models = rep(c("diagnosis~score","diagnosis~age"), 5),
  #                                                     fold_cols = '.folds', family='binomial',
  #                                                     REML = FALSE, model_verbose=FALSE,
  #                                                     positive=1, parallel = TRUE)})
  #
  #   non_par_time <- system.time({cv_not_par <- cross_validate(dat,
  #                                                             models = rep(c("diagnosis~score","diagnosis~age"), 5),
  #                                                             fold_cols = '.folds', family='binomial',
  #                                                             REML = FALSE, model_verbose=FALSE,
  #                                                             positive=1, parallel = FALSE)})
  #
  #   # expect_true(non_par_time[[3]] > par_time[[3]])
  #
  #   expect_equal(cv_par$`Balanced Accuracy`, cv_not_par$`Balanced Accuracy`)
  #   expect_equal(cv_par$AUC, cv_not_par$AUC)
  #   expect_equal(cv_par$Fixed, cv_not_par$Fixed)

})


test_that("parallelization works with validate()",{

  testthat::skip(message = "Skipping parallel tests")

  # library(doParallel)
  #
  # # Load data and fold it
  # set.seed(1)
  # dat <- groupdata2::partition(participant.scores, p = 0.7, list_out = FALSE)
  #
  # wrks <- tryCatch({foreach::getDoParWorkers()}, error = function(e) {
  #   print(e)
  #   NULL
  # })
  #
  # if (!is.null(wrks) && wrks > 1){
  #   doParallel::registerDoParallel(wrks)
  # } else {
  #
  #   # Register more cores than available
  #   # This might be slower than sequential
  #   doParallel::registerDoParallel(4)
  # }
  #
  #
  #   par_time <- system.time({v_par <- validate(dat,
  #                                              models = rep(c("diagnosis~score","diagnosis~age"), 5),
  #                                              partitions_col = '.partitions', family = 'binomial',
  #                                              REML = FALSE, model_verbose = FALSE,
  #                                              positive = 1, parallel = TRUE)})
  #
  #   non_par_time <- system.time({v_not_par <- validate(dat,
  #                                                       models = rep(c("diagnosis~score","diagnosis~age"), 5),
  #                                                       partitions_col = '.partitions', family = 'binomial',
  #                                                       REML = FALSE, model_verbose = FALSE,
  #                                                       positive = 1, parallel = FALSE)})
  #
  #   # expect_true(non_par_time[[3]] > par_time[[3]])
  #
  #   expect_equal(v_par$`Balanced Accuracy`, v_not_par$`Balanced Accuracy`)
  #   expect_equal(v_par$AUC, v_not_par$AUC)
  #   expect_equal(v_par$Fixed, v_not_par$Fixed)
  #

})

test_that("parallelization works with binomial baseline()",{

  # testthat::skip(message = "Skipping parallel tests")
  #
  # library(doParallel)
  #
  # wrks <- tryCatch({foreach::getDoParWorkers()}, error = function(e) {
  #   print(e)
  #   NULL
  # })
  #
  # if (!is.null(wrks) && wrks > 1){
  #   doParallel::registerDoParallel(wrks)
  # } else {
  #
  #   # Register more cores than available
  #   # This might be slower than sequential
  #   doParallel::registerDoParallel(4)
  # }
  #
  #
  # set.seed(1)
  # par_time <- system.time({b_par <- baseline(test_data = participant.scores,
  #                                            # train_data = train_data,
  #                                            dependent_col = "diagnosis",
  #                                            n = 10,
  #                                            family = "binomial",
  #                                            parallel = TRUE)})
  #
  # set.seed(1)
  # non_par_time <- system.time({b_not_par <- baseline(test_data = participant.scores,
  #                                                # train_data = train_data,
  #                                                dependent_col = "diagnosis",
  #                                                n = 10,
  #                                                family = "binomial",
  #                                                parallel = FALSE)})
  #
  # expect_equal(b_par$summarized_metrics$`Balanced Accuracy`, b_not_par$summarized_metrics$`Balanced Accuracy`)
  # expect_equal(b_par$summarized_metrics$AUC, b_not_par$summarized_metrics$AUC)

})

test_that("parallelization works with gaussian baseline()",{

  testthat::skip(message = "Skipping parallel tests")

  # library(doParallel)
  #
  # # Load data and fold it
  # set.seed(1)
  # dat <- groupdata2::partition(participant.scores, p = 0.6, list_out = TRUE)
  # train_data <- dat[[1]]
  # test_data <- dat[[2]]
  #
  # wrks <- tryCatch({foreach::getDoParWorkers()}, error = function(e) {
  #   print(e)
  #   NULL
  # })
  #
  # if (!is.null(wrks) && wrks > 1){
  #   doParallel::registerDoParallel(wrks)
  # } else {
  #
  #   # Register more cores than available
  #   # This might be slower than sequential
  #   doParallel::registerDoParallel(4)
  # }
  #
  #
  # set.seed(1)
  # par_time <- system.time({b_par <- baseline(test_data = test_data,
  #                                            train_data = train_data,
  #                                            dependent_col = "score",
  #                                            n = 10,
  #                                            family = "gaussian",
  #                                            parallel = TRUE)})
  #
  # set.seed(1)
  # non_par_time <- system.time({b_not_par <- baseline(test_data = test_data,
  #                                                    train_data = train_data,
  #                                                    dependent_col = "score",
  #                                                    n = 10,
  #                                                    family = "gaussian",
  #                                                    parallel = FALSE)})
  #
  # expect_equal(b_par$summarized_metrics$MAE,b_not_par$summarized_metrics$MAE)
  # expect_equal(b_par$summarized_metrics$RMSE,b_not_par$summarized_metrics$RMSE)
  # expect_equal(b_par$summarized_metrics$`Training Rows`, b_not_par$summarized_metrics$`Training Rows`)

})
