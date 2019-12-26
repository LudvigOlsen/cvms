library(cvms)
context("cv_plot()")


test_that("cv_plot() works",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Cross-validate the data
  CVbinom <- cross_validate(dat, "diagnosis~score",
                            fold_cols = '.folds',
                            family='binomial', REML = FALSE,
                            verbose=FALSE)

  CVgauss <- cross_validate(dat, "score~diagnosis",
                            fold_cols = '.folds',
                            family='gaussian', REML = FALSE,
                            verbose=FALSE)

  # If anyone can suggest a good way to test ggplot objects, feel free!
  # expect_is(cv_plot(CVbinom, type = 'ROC'), 'ggplot')
  expect_is(cv_plot(CVgauss, type = 'RMSE'), 'ggplot')
  expect_is(cv_plot(CVgauss, type = 'r2'), 'ggplot')
  expect_is(cv_plot(CVgauss, type = 'IC'), 'ggplot')
  expect_is(cv_plot(CVgauss, type = 'coefficients'), 'ggplot')

})


test_that("plot_confusion_matrix() returns expected plots",{

  # Note: These are just initial tests
  # There's probably a high number of errors it won't catch

  # TODO Check out https://github.com/r-lib/vdiffr
  # It may make testing easier and better

  targets <- c(0,1,0,1,0,1,0,1,0,1,0,1)
  predictions <- c(1,1,0,0,0,1,1,1,0,1,0,0)

  # Create confusion matrix with default metrics
  cm <- confusion_matrix(targets, predictions)
  cm[["Confusion Matrix"]]

  expect_equal(cm[["Confusion Matrix"]][[1]]$N, c(4L, 2L, 2L, 4L))

  p1 <- plot_confusion_matrix(cm[["Confusion Matrix"]][[1]])
  expect_equal(p1$data$Target, structure(c(1L, 1L, 2L, 2L), .Label = c("0", "1"), class = "factor"))
  expect_equal(p1$data$Prediction, structure(c(1L, 2L, 1L, 2L), .Label = c("0", "1"), class = "factor"))
  expect_equal(p1$data$N, c(4L, 2L, 2L, 4L))
  expect_equal(p1$data$N_text, as.character(c(4L, 2L, 2L, 4L)))
  expect_equal(p1$data$Normalized, c(33.3333333333333, 16.6666666666667, 16.6666666666667, 33.3333333333333))
  expect_equal(p1$data$Normalized_text, c("33.33%", "16.67%", "16.67%", "33.33%"))
  expect_equal(p1$data$Class_N, c(6,6,6,6))
  expect_equal(p1$data$Class_Percentage, c(66.6666666666667, 33.3333333333333, 33.3333333333333, 66.6666666666667))
  expect_equal(p1$data$Class_Percentage_text, c("-- 66.67% --", "-- 33.33% --", "-- 33.33% --", "-- 66.67% --"))
  expect_equal(p1$data$Prediction_N, c(6,6,6,6))
  expect_equal(p1$data$Prediction_Percentage, c(66.6666666666667, 33.3333333333333, 33.3333333333333, 66.6666666666667))
  expect_equal(p1$data$Prediction_Percentage_text, c("-- 66.67% --", "-- 33.33% --", "-- 33.33% --", "-- 66.67% --"))


  expect_equal(length(p1$layers), 5)
  expect_equal(sapply(p1$layers, function(x) class(x$geom)[1]),
               c("GeomTile", "GeomText", "GeomText", "GeomText", "GeomText"))
  expect_equal(p1$labels,
               list(x = "Target", y = "Prediction",
                    fill = "N", label = "N"))

  expect_equal(p1$scales$scales[[1]]$limits,
               c(2.0, 4.8))
  p1_darkest <- plot_confusion_matrix(cm[["Confusion Matrix"]][[1]], darkness = 1.0)
  expect_equal(p1_darkest$scales$scales[[1]]$limits,
               c(2.0, 4.0))

  expect_equal(p1$mapping,
               structure(list(x = ~.data$Target, y = ~.data$Prediction, fill = ~.data$N),
                         class = "uneval"))

  expect_true(!p1$guides$fill[[1]])

  expect_error(plot_confusion_matrix(cm[["Confusion Matrix"]][[1]], darkness = 1.1),
               "'darkness' must be between 0 and 1", fixed=TRUE)
  expect_error(plot_confusion_matrix(cm[["Confusion Matrix"]][[1]], darkness = -.1),
               "'darkness' must be between 0 and 1", fixed=TRUE)
})

test_that("plot_confusion_matrix() with multiclass conf mat returns expected plots",{

  # Note: These are just initial tests
  # There's probably a high number of errors it won't catch

  # TODO Check out https://github.com/r-lib/vdiffr
  # It may make testing easier and better

  targets <- c(0,1,0,1,0,1,0,1,0,1,0,1,2,2,2,2,2,2)
  predictions <- c(1,1,0,0,0,1,1,1,0,1,0,0,1,0,2,2,1,2)

  # Create confusion matrix with default metrics
  cm <- confusion_matrix(targets, predictions)
  conf_mat <- cm[["Confusion Matrix"]][[1]]
  conf_mat[["N"]] <- round(conf_mat[["N"]] * 100 + runif(n = nrow(conf_mat), 0, 100))

  p1 <- plot_confusion_matrix(conf_mat)

})
