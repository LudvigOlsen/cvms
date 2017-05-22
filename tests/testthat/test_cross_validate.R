library(cvms)
context("cross_validate()")

test_that("gaussian models with cross_validate()",{

  # Load data and fold it
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Cross-validate the data
  CVed <- cross_validate(dat, "score~diagnosis", folds_col = '.folds', family='gaussian', REML = FALSE,
                         do.plot=TRUE, which_plot = "all",
                         model_verbose=TRUE)

  #print(CVed)


  # data, model, folds_col = '.folds', family='gaussian', REML=FALSE,
  # cutoff=0.5, positive=1, do.plot=FALSE,
  # ){



})

