library(cvms)
context("cross_validate()")

# NOTICE:
# Numbers tested are the results I got and not "what should be"
# This will allow me to see if something changes, but it shouldn't give false confidence.


test_that("binomial models work with cross_validate()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  CVbinomlist <- cross_validate(dat,
                                models = c("diagnosis~score", "diagnosis~age"),
                                fold_cols = '.folds', family = 'binomial',
                                REML = FALSE, model_verbose = FALSE,
                                positive = 1 )

  expect_equal(CVbinomlist$AUC, c(0.7615741, 0.1666667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.58511535, 0.01748744), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9380328, 0.3158459), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4927536, -0.3636364), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8888889,0.6666667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7777778, 0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7619048, 0.5), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3,0.2), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111,0.3333333), tolerance=1e-3)
  expect_equal(CVbinomlist$MCC, c(0.5048268, -0.4082483), tolerance=1e-3)
  expect_equal(CVbinomlist$Folds, c(4,4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1,1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlist$Family, c('binomial','binomial'))
  expect_equal(CVbinomlist$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlist$Fixed, c('score','age'))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(colnames(CVbinomlist$ROC[[1]]), c("Sensitivities","Specificities"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlist$ROC[[1]]),29)
  expect_equal(CVbinomlist$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))

})

test_that("binomial models checks that dependent variable is numeric with cross_validate()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(7)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant') %>%
    dplyr::mutate(diagnosis = factor(diagnosis))

  # dat %>% str()

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = '.folds', family='binomial',
                                REML = FALSE, model_verbose=FALSE,
                                positive=1)

  expect_equal(CVbinomlist$AUC, c(0.7476852, 0.4583333), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.5621978, 0.2309283), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9331726, 0.6857384), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4285714, 0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8333333, 1.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7, NaN), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.75, 0.60), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.6363636, NA), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3333,0.0), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7083333, 0.5000000), tolerance=1e-3)
  expect_equal(CVbinomlist$MCC, c(0.4330127, 0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Folds, c(4,4))
  expect_equal(CVbinomlist$`Fold Columns`, c(1,1))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlist$Family, c('binomial','binomial'))
  expect_equal(CVbinomlist$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlist$Fixed, c('score','age'))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(colnames(CVbinomlist$ROC[[1]]), c("Sensitivities","Specificities"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlist$ROC[[1]]),30)
  expect_equal(CVbinomlist$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))


})

test_that("binomial models work with cross_validate()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(20)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')


  CVbinomlistrand <- cross_validate(dat,
                                    models = c("diagnosis~score + (1|session)","diagnosis~age"),
                                    fold_cols = '.folds',
                                    family='binomial',
                                    REML = FALSE,
                                    link = NULL,
                                    model_verbose=FALSE,
                                    positive=1)

  expect_equal(CVbinomlistrand$AUC, c(0.8472222, 0.4166667), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Lower CI`, c(0.6952664, 0.1964321), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Upper CI`, c(0.9991780, 0.6369012), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Kappa, c(0.58333333, -0.08695652), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Sensitivity, c(0.75, 0.25), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Specificity, c(0.8333333,0.6666667), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Pos Pred Value`, c(0.750, 0.3333333), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Neg Pred Value`, c(0.8333333, 0.5714286), tolerance=1e-3)
  expect_equal(CVbinomlistrand$F1, c(0.750, 0.2857143), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Detection Rate`, c(0.3, 0.1), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Detection Prevalence`, c(0.4,0.3), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Balanced Accuracy`, c(0.7916667, 0.4583333), tolerance=1e-3)
  expect_equal(CVbinomlistrand$MCC, c(0.58333333, -0.08908708), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Folds, c(4,4))
  expect_equal(CVbinomlistrand$`Fold Columns`, c(1,1))
  expect_equal(CVbinomlistrand$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlistrand$Family, c('binomial','binomial'))
  expect_equal(CVbinomlistrand$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlistrand$Fixed, c('score','age'))
  expect_equal(CVbinomlistrand$Random, c('(1|session)',NA))


  # Enter sub tibbles
  expect_is(CVbinomlistrand$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlistrand$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlistrand$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(colnames(CVbinomlistrand$ROC[[1]]), c("Sensitivities","Specificities"))
  expect_equal(nrow(CVbinomlistrand$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlistrand$ROC[[1]]),27) # Why?

  expect_is(CVbinomlistrand$Predictions[[2]], "tbl_df")
  expect_is(CVbinomlistrand$ROC[[2]], "tbl_df")
  expect_equal(colnames(CVbinomlistrand$Predictions[[2]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(colnames(CVbinomlistrand$ROC[[2]]), c("Sensitivities","Specificities"))
  expect_equal(nrow(CVbinomlistrand$Predictions[[2]]),30)
  expect_equal(nrow(CVbinomlistrand$ROC[[2]]),11) # Why?

  expect_equal(CVbinomlistrand$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))

})

test_that("gaussian model with cross_validate()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Cross-validate the data
  CVed <- cross_validate(dat, "score~diagnosis",
                          fold_cols = '.folds',
                          family='gaussian', link = NULL,
                          REML = FALSE,
                          model_verbose=FALSE)

  expect_equal(CVed$RMSE, 17.16817, tolerance=1e-3)
  expect_equal(CVed$MAE, 14.26914, tolerance=1e-3)
  expect_equal(CVed$r2m, 0.2640793, tolerance=1e-3)
  expect_equal(CVed$r2c, 0.2640793, tolerance=1e-3)
  expect_equal(CVed$AIC, 194.6904, tolerance=1e-3)
  expect_equal(CVed$AICc, 195.9963, tolerance=1e-3)
  expect_equal(CVed$BIC, 198.0243, tolerance=1e-3)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Fold Columns`, 1)
  expect_equal(CVed$`Convergence Warnings`, 0)
  expect_equal(CVed$Family, 'gaussian')
  expect_equal(CVed$Dependent, 'score')
  expect_equal(CVed$Fixed, 'diagnosis')
  expect_equal(CVed$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))

})

test_that("gaussian mixed models with cross_validate()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Cross-validate the data
  CVed <- cross_validate(dat, c("score~diagnosis + (1|session)","score~age + (1|session)"),
                         fold_cols = '.folds',
                         family='gaussian', link = NULL,
                         REML = FALSE,
                         model_verbose=FALSE)

  expect_equal(CVed$RMSE, c(9.65949, 15.20226), tolerance=1e-3)
  expect_equal(CVed$MAE, c(7.145933, 13.577082), tolerance=1e-3)
  expect_equal(CVed$r2m, c(0.28219291, 0.01319592), tolerance=1e-3)
  expect_equal(CVed$r2c, c(0.8043140, 0.5016056), tolerance=1e-3)
  expect_equal(CVed$AIC, c(175.9497, 194.6358), tolerance=1e-3)
  expect_equal(CVed$AICc, c(178.2523, 196.9384), tolerance=1e-3)
  expect_equal(CVed$BIC, c(180.3948, 199.0809), tolerance=1e-3)
  expect_equal(CVed$Folds, c(4,4))
  expect_equal(CVed$`Fold Columns`, c(1,1))
  expect_equal(CVed$`Convergence Warnings`, c(0,0))
  expect_equal(CVed$Family, c('gaussian','gaussian'))
  expect_equal(CVed$Dependent, c('score','score'))
  expect_equal(CVed$Fixed, c('diagnosis', 'age'))
  expect_equal(CVed$Random, c('(1|session)', '(1|session)'))
  expect_equal(CVed$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))


})

test_that("binomial models work with control specified in cross_validate()",{

  # skip_test_if_old_R_version()

  skip("testing different optimizers is too difficult given platform differences")

  # Load data and fold it
  set_seed_for_R_compatibility(7)
  dat <- groupdata2::fold(participant.scores, k = 3,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  tryCatch({
    cross_validate(
    dat,
    models = c("diagnosis~score + (1|session)"),
    fold_cols = '.folds',
    family = 'binomial',
    REML = FALSE,
    link = NULL,
    control = lme4::glmerControl(optimizer = "bobyqa"),
    model_verbose = FALSE,
    positive = 1
  )}, warning = function(w){
    expect_true(grepl("unable to evaluate scaled gradient", as.character(w), ignore.case = TRUE))
  })


  cv_Nelder_Mead <- cross_validate(
    dat %>% dplyr::bind_rows(dat,dat,dat,dat),
    models = c("diagnosis~score + age + (1|session)"),
    fold_cols = '.folds',
    family = 'binomial',
    REML = FALSE,
    link = NULL,
    control = lme4::glmerControl(optimizer = "Nelder_Mead"),
    model_verbose = FALSE,
    positive = 1
  )

  cv_bobyqa <- cross_validate(
    dat %>% dplyr::bind_rows(dat,dat,dat,dat),
    models = c("diagnosis~score + age + (1|session)"),
    fold_cols = '.folds',
    family = 'binomial',
    REML = FALSE,
    link = NULL,
    control = lme4::glmerControl(optimizer = "bobyqa"),
    model_verbose = FALSE,
    positive = 1
  )

  # Gather the results from the two different optimizers
  cv <- cv_Nelder_Mead %>%
    dplyr::bind_rows(cv_bobyqa)

  # Appears to be different on linux
  expect_true(cv$`Balanced Accuracy`[[1]] != cv$`Balanced Accuracy`[[2]]) # , c(0.736111111111111, 0.777777777777778))
  expect_true(cv$AUC[[1]] != cv$AUC[[2]]) # c(0.824074074074074, 0.875))
  expect_true(cv$F1[[1]] != cv$F1[[2]]) # c(0.666666666666667, 0.727272727272727))
  expect_equal(cv$Fixed, c("score+age", "score+age"))
  expect_equal(cv$Random, c("(1|session)", "(1|session)"))
  expect_equal(cv$Dependent, c("diagnosis", "diagnosis"))

})

test_that("binomial models gives warnings with control specified in cross_validate()",{

  testthat::skip("mac and ubuntu give different warnings")
  # Tested on both platforms on travis as well
  # Local test should run on mac as is

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(7)
  dat <- groupdata2::fold(participant.scores, k = 3,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Singular fit message

  cv_process <- tryCatch({
    purrr::map(.x = 1, .f = purrr::quietly(function(.x){
      set_seed_for_R_compatibility(2)
      cross_validate(dat,
                     models = c("diagnosis ~ score + age + (1|session) + (1|age)"),
                     fold_cols = '.folds',
                     family = 'binomial',
                     REML = FALSE,
                     link = NULL,
                     control = lme4::glmerControl(optimizer = "bobyqa",
                                                  optCtrl = list(maxfun = 100)),
                     model_verbose = FALSE)
    }))
  })

  expect_equal(cv_process[[1]]$messages,
               "\n--------------------------------------------------\ncross_validate(): Boundary (Singular) Fit Message:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n1\nboundary (singular) fit: see ?isSingular\n",
               fixed=TRUE)

  ### NOTE: The warnings are different between mac and linux
  # So we cannot check the below :/
  expect_equal(cv_process[[1]]$warnings,
               c("\n-------------------------------------\ncross_validate(): Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n1\nmaxfun < 10 * length(par)^2 is not recommended.",
                 "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n1\nconvergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded",
                 "\n-------------------------------------\ncross_validate(): Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n2\nmaxfun < 10 * length(par)^2 is not recommended.",
                 "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n2\nconvergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded",
                 "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n2\nModel failed to converge with max|grad| = 0.0405867 (tol = 0.001, component 1)",
                 "\n-------------------------------------\ncross_validate(): Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n3\nmaxfun < 10 * length(par)^2 is not recommended.",
                 "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n3\nconvergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded",
                 "\n-------------------------------------\ncross_validate(): Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n3\nunable to evaluate scaled gradient",
                 "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\ndiagnosis ~ score + age + (1|session) + (1|age)\nFor fold column:\n.folds\nIn fold:\n3\nModel failed to converge: degenerate  Hessian with 1 negative eigenvalues"
               ),
               fixed=TRUE)


  # set_seed_for_R_compatibility(2)
  # cv_messages <- suppressMessages(cross_validate(dat,
  #                               models = c("diagnosis ~ score + age + (1|session) + (1|age)"),
  #                               fold_cols = '.folds',
  #                               family='binomial',
  #                               REML = FALSE,
  #                               link = NULL,
  #                               control = lme4::glmerControl(optimizer="bobyqa",
  #                                                            optCtrl=list(maxfun=100))))
  # expect_equal(cv_messages$`Singular Fit Messages`, 2)


})

test_that("gaussian models work with control specified in cross_validate()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(2)
  dat <- groupdata2::fold(participant.scores, k = 3,
                          cat_col = 'diagnosis',
                          id_col = 'participant')


  CVgausslistrand <- cross_validate(dat,
                                    models = c("score~diagnosis + (1|session)"),
                                    fold_cols = '.folds',
                                    family='gaussian',
                                    REML = FALSE,
                                    link = NULL,
                                    control = lme4::lmerControl(optimizer="Nelder_Mead",
                                                                 optCtrl=list(maxfun=1000000)),
                                    model_verbose=FALSE)

  expect_equal(CVgausslistrand$RMSE, c(10.44299), tolerance=1e-3)
  expect_equal(CVgausslistrand$`Convergence Warnings`, c(0))

  expect_equal(evaluate_promise(cross_validate(dat,
                                    models = c("score~diagnosis + (1|session)"),
                                    fold_cols = '.folds',
                                    family='gaussian',
                                    REML = FALSE,
                                    link = NULL,
                                    control = lme4::lmerControl(optimizer="bobyqa",
                                                                optCtrl=list(maxfun=10)),
                                    model_verbose=FALSE))$warnings,
               c("\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\nscore~diagnosis + (1|session)\nFor fold column:\n.folds\nIn fold:\n1\nconvergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded",
                 "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\nscore~diagnosis + (1|session)\nFor fold column:\n.folds\nIn fold:\n1\nModel failed to converge with max|grad| = 0.429297 (tol = 0.002, component 1)",
                 "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\nscore~diagnosis + (1|session)\nFor fold column:\n.folds\nIn fold:\n2\nconvergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded",
                 "\n-------------------------------------\ncross_validate(): Convergence Warning:\nIn model:\nscore~diagnosis + (1|session)\nFor fold column:\n.folds\nIn fold:\n3\nconvergence code 1 from bobyqa: bobyqa -- maximum number of function evaluations exceeded"
               ),
               fixed = TRUE)


  # TODO When counting singular (boundary fit) messages, uncomment and change expected warning/message
  # # Warning because of too few iterations
  # expect_warning(cross_validate(dat,
  #                               models = c("age~diagnosis*score + (score|session) + (1|score)"),
  #                               fold_cols = '.folds',
  #                               family='gaussian',
  #                               REML = FALSE,
  #                               link = NULL,
  #                               control = lme4::lmerControl(optimizer="Nelder_Mead",
  #                                                            optCtrl=list(maxfun=100)),
  #                               model_verbose=FALSE), "cross_validate(): Convergence Warning:", fixed = TRUE)


})

test_that("model using dot in formula ( y ~ . ) works with cross_validate()",{

  # skip_test_if_old_R_version()

  # We wish to test if using the dot "y~." method in the model formula
  # correctly leaves out .folds column.

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant') %>%
    dplyr::select(-c(participant, session))

  # Expect no warnings
  # https://stackoverflow.com/questions/22003306/is-there-something-in-testthat-like-expect-no-warnings
  expect_warning(cross_validate(dat, models = c("diagnosis~."),
                                fold_cols = '.folds', family='binomial',
                                REML = FALSE, model_verbose=FALSE),
                 regexp = NA)

  # Expect no warnings
  # https://stackoverflow.com/questions/22003306/is-there-something-in-testthat-like-expect-no-warnings
  expect_warning(cross_validate(dat, models = c("score~."),
                                fold_cols = '.folds', family='gaussian',
                                REML = FALSE, model_verbose=FALSE),
                 regexp = NA)


})

test_that("binomial models work with repeated cross_validate()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(2)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant',
                          num_fold_cols=2)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE,
                                positive=1)

  expect_equal(CVbinomlist$AUC, c(0.750, 0.2291667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.5638021, 0.1037390), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9361979, 0.3645035), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4606625, -0.3043478), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333, 0.1250000), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8611111, 0.5833333), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7388889, 0.1666667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7559524, 0.5000000), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0500000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3166667, 0.3), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7222222, 0.3541667), tolerance=1e-3)
  expect_equal(CVbinomlist$MCC, c(0.4689197, -0.3118048), tolerance=1e-3)
  expect_equal(CVbinomlist$Folds, c(8,8))
  expect_equal(CVbinomlist$`Fold Columns`, c(2,2))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlist$Family, c('binomial','binomial'))
  expect_equal(CVbinomlist$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlist$Fixed, c('score','age'))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$Results[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]], "tbl_df")
  expect_is(CVbinomlist$`Confusion Matrix`[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(colnames(CVbinomlist$Results[[1]]), c("Fold Column","Balanced Accuracy","F1",
                                                     "Sensitivity","Specificity","Pos Pred Value",
                                                     "Neg Pred Value","AUC","Lower CI","Upper CI",
                                                     "Kappa","MCC","Detection Rate","Detection Prevalence",
                                                     "Prevalence"))
  expect_equal(colnames(CVbinomlist$ROC[[1]]), c("Fold Column","Sensitivities","Specificities"))
  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Target","Pos_0","Pos_1","N"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]),60)
  expect_equal(nrow(CVbinomlist$ROC[[1]]),60)
  expect_equal(colnames(CVbinomlist$Coefficients[[1]]),
               c("term","estimate","std.error","statistic","p.value","Fold","Fold Column"))
  expect_equal(colnames(CVbinomlist$Coefficients[[2]]),
               c("term","estimate","std.error","statistic","p.value","Fold","Fold Column"))
  expect_equal(CVbinomlist$Coefficients[[1]]$p.value,
               c(0.01294663,0.01931905,0.04833047,0.05703608,0.01900735,
                 0.03169528,0.04338194,0.05071058,0.01639887,0.02528367,
                 0.04118387,0.04851447,0.01412963,0.02178988,0.04010255,0.04752187))
  expect_equal(CVbinomlist$Coefficients[[2]]$p.value,
               c(0.62478703,0.84891972,0.18479764,0.14117487,0.09443436,
                 0.12527121,0.13628239,0.10142803,0.66873537,0.89953532,0.21621108,
                 0.18124974,0.67252682,0.45631539,0.87784992,0.77268412))
  expect_equal(CVbinomlist$Coefficients[[1]]$estimate,
               c(5.26664582,-0.12300571,2.56877552,-0.05531371,3.03166220,
                 -0.06342121,2.80320088,-0.06211859,3.61808397,-0.07362534,
                 2.70911710,-0.06030082,4.53407887,-0.10779184,2.62880059,-0.05770449))
  expect_equal(CVbinomlist$Coefficients[[2]]$estimate,
               c(0.818615019,-0.010725093,-2.665418817,0.106774730,4.483382847,
                 -0.141769999,-3.054394678,0.116528272,0.714444780,-0.007070299,
                 -3.594942567,0.123832276,-0.701387195,0.044667112,-0.340316825,
                 0.024469156))
  expect_equal(CVbinomlist$Coefficients[[1]]$std.error,
               c(2.11917509,0.05258190,1.30100740,0.02906606,1.29260651,0.02952251,
                 1.38771363,0.03179176,1.50758829,0.03291193,1.32690970,0.03056558,
                 1.84766496,0.04698838,1.28066063,0.02911987))
  expect_equal(CVbinomlist$Coefficients[[2]]$std.error,
               c(1.67379503,0.05630060,2.00992729,0.07256528,2.68071734,0.09247766,
                 2.05023360,0.07114190,1.66971492,0.05600309,2.90696307,0.09262526,
                 1.65937069,0.05996167,2.21422931,0.08470771))
  expect_equal(CVbinomlist$Coefficients[[1]]$statistic,
               c(2.485234,-2.339317,1.974451,-1.903034,2.345387,-2.148233,
                 2.020014,-1.953921,2.399915,-2.237041,2.041674,-1.972834,
                 2.453951,-2.294011,2.052691,-1.981619), tolerance = 1e-6)
  expect_equal(CVbinomlist$Coefficients[[2]]$statistic,
               c(0.4890772,-0.1904970,-1.3261270,1.4714300,1.6724564,-1.5330188,
                 -1.4897789,1.6379696,0.4278843,-0.1262484,-1.2366661,1.3369170,
                 -0.4226826,0.7449278,-0.1536954,0.2888657), tolerance = 1e-6)
  expect_equal(CVbinomlist$Coefficients[[1]]$Fold, c(1,1,2,2,3,3,4,4,1,1,2,2,3,3,4,4))
  expect_equal(CVbinomlist$Coefficients[[2]]$Fold, c(1,1,2,2,3,3,4,4,1,1,2,2,3,3,4,4))
  expect_equal(CVbinomlist$Coefficients[[1]]$`Fold Column`, rep(c(".folds_1",".folds_2"), each=8))
  expect_equal(CVbinomlist$Coefficients[[2]]$`Fold Column`, rep(c(".folds_1",".folds_2"), each=8))

  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$`Fold Column`, rep(c(".folds_1",".folds_2"), each=4))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$Prediction, as.character(c(0,1,0,1,0,1,0,1)))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$Target, as.character(c(0,0,1,1,0,0,1,1)))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$Pos_0, rep(c("TP","FN","FP","TN"),2))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$Pos_1, rep(c("TN","FP","FN","TP"),2))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7,5,2,16,7,5,3,15))

  expect_equal(CVbinomlist$Results[[1]]$`Fold Column`,c(".folds_1",".folds_2"))
  expect_equal(CVbinomlist$Results[[1]]$`Balanced Accuracy`,c(0.7361111, 0.7083333), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$F1,c(0.6666667, 0.6363636), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$Sensitivity,c(0.5833333, 0.5833333), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$Specificity,c(0.8888889, 0.8333333), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Pos Pred Value`,c(0.7777778, 0.7), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Neg Pred Value`,c(0.7619048, 0.75), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$AUC,c(0.7453704, 0.7546296), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Lower CI`,c(0.5545666, 0.5730375), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Upper CI`,c(0.9361741, 0.9362218), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$Kappa,c(0.4927536, 0.4285714), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Detection Rate`,c(0.2333333, 0.2333333), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Detection Prevalence`,c(0.3000000, 0.33333), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$Prevalence,c(0.4, 0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$MCC,c(0.5048268, 0.4330127), tolerance=1e-3)

  expect_equal(CVbinomlist$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))
})

test_that("binomial models work with positive as.character in cross_validate()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(2)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant',
                          num_fold_cols=2) %>%
  dplyr::mutate(diagnosis = factor(ifelse(diagnosis==0, "E","B")))

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE,
                                positive=1)

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Target","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16,2,5,7,15,3,5,7))
  expect_equal(CVbinomlist$F1, c(0.8049933, 0.5384615), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive="E")

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Target","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16,2,5,7,15,3,5,7))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive="B")

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Target","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16,2,5,7,15,3,5,7))
  expect_equal(CVbinomlist$F1, c(0.8049933, 0.5384615), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive=1)

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Target","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16,2,5,7,15,3,5,7))
  expect_equal(CVbinomlist$F1, c(0.8049933, 0.5384615), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive=2)

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Target","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16,2,5,7,15,3,5,7))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance=1e-3)

  expect_error(cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive="C"),
               "When 'positive' is a character, it must correspond to a factor level in the dependent variable.\n'positive' is C and levels are B and E.")

  # Interchanging the level names

  set_seed_for_R_compatibility(2)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant',
                          num_fold_cols=2) %>%
    dplyr::mutate(diagnosis = factor(ifelse(diagnosis==0, "B","E")))

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE,
                                positive=1)

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Target","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7,5,2,16,7,5,3,15))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive="E")

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Target","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7,5,2,16,7,5,3,15))
  expect_equal(CVbinomlist$F1, c(0.8049933,0.5384615), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive="B")

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Target","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7,5,2,16,7,5,3,15))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive=1)

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Target","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7,5,2,16,7,5,3,15))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive=2)

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Target","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7,5,2,16,7,5,3,15))
  expect_equal(CVbinomlist$F1, c(0.8049933,0.5384615), tolerance=1e-3)


  expect_equal(CVbinomlist$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))

  })

test_that("gaussian models work with repeated cross_validate()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant',
                          num_fold_cols=2)

  CVgausslist <- cross_validate(dat, models = c("score~diagnosis","score~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='gaussian',
                                REML = FALSE, model_verbose=FALSE)


  expect_equal(CVgausslist$RMSE, c(16.69535, 20.07280), tolerance=1e-3)
  expect_equal(CVgausslist$MAE, c(13.92099, 16.60674), tolerance=1e-3)
  expect_equal(CVgausslist$r2m, c(0.268698382, 0.008717078), tolerance=1e-3)
  expect_equal(CVgausslist$r2c, c(0.268698382, 0.008717078), tolerance=1e-3)
  expect_equal(CVgausslist$AIC, c(194.6793, 201.9189), tolerance=1e-3)
  expect_equal(CVgausslist$AICc, c(195.9852, 203.2248), tolerance=1e-3)
  expect_equal(CVgausslist$BIC, c(198.0132, 205.2527), tolerance=1e-3)
  expect_equal(CVgausslist$Folds, c(8,8))
  expect_equal(CVgausslist$`Fold Columns`, c(2,2))
  expect_equal(CVgausslist$`Convergence Warnings`, c(0,0))
  expect_equal(CVgausslist$Family, c('gaussian','gaussian'))
  expect_equal(CVgausslist$Dependent, c('score','score'))
  expect_equal(CVgausslist$Fixed, c('diagnosis', 'age'))

  # Enter sub tibbles
  expect_is(CVgausslist$Results[[1]], "tbl_df")
  expect_is(CVgausslist$Coefficients[[1]], "tbl_df")
  expect_equal(colnames(CVgausslist$Results[[1]]),
               c("Fold Column","Fold","RMSE","MAE","r2m","r2c","AIC","AICc","BIC"))
  expect_equal(CVgausslist$Results[[1]]$`Fold Column`,
               rep(c(".folds_1",".folds_2"), each=4))
  expect_equal(colnames(CVgausslist$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction"))
  expect_equal(unique(CVgausslist$Predictions[[1]]$`Fold Column`), c(".folds_1",".folds_2"))
  expect_equal(colnames(CVgausslist$Coefficients[[1]]),
               c("term","estimate","std.error","statistic","p.value","Fold","Fold Column"))
  expect_equal(CVgausslist$Coefficients[[1]]$p.value,
               c(8.230130e-09,1.290208e-02,1.346678e-08,1.073278e-02,2.434650e-09,
                 1.980414e-03,9.363412e-08,4.950473e-02,6.908574e-09,1.352584e-02,
                 9.209672e-08,3.018268e-02,1.700920e-08,1.994213e-02,2.078447e-09,1.105365e-03))
  expect_equal(CVgausslist$Coefficients[[2]]$p.value,
               c(0.009808606,0.637825565,0.258490506,0.636485814,0.010557346,
                 0.646231138,0.085798324,0.557360724,0.151732090,0.638915931,
                 0.093280236,0.851553515,0.017961529,0.953518671,0.021408386,0.658577591))
  expect_equal(CVgausslist$Coefficients[[1]]$estimate,
               c(49.55556,-18.88889,53.33333,-21.16667,51.00000,-23.53333,
                 49.77778,-16.61111,49.55556,-18.55556,49.77778,-18.52778,
                 51.00000,-18.80000,53.33333,-25.58333), tolerance = 1e-5)
  expect_equal(CVgausslist$Coefficients[[2]]$estimate,
               c(45.14324082,-0.25604297,29.32953312,0.42100977,43.24298347,
                 -0.25509419,30.55040872,0.33242507,28.88530466,0.34729296,
                 35.47615039,0.12093154,40.14314003,-0.03262612,46.87397730,
                 -0.27329111), tolerance = 1e-5)
  expect_equal(CVgausslist$Coefficients[[1]]$std.error,
               c(5.518360,6.980235,5.656682,7.483087,5.301864,6.706387,
                 5.984982,7.917387,5.463479,6.910816,5.978424,7.908712,
                 5.922223,7.491086,5.036852,6.663130), tolerance = 1e-6)
  expect_equal(CVgausslist$Coefficients[[2]]$std.error,
               c(15.9672206,0.5363914,25.1784502,0.8765252,15.4721646,
                 0.5481382,16.8586576,0.5565485,19.4513547,0.7299321,
                 20.0765470,0.6374769,15.6973841,0.5533890,18.6950224,
                 0.6087937), tolerance = 1e-6)
  expect_equal(CVgausslist$Coefficients[[1]]$statistic,
               c(8.980123,-2.706053,9.428378,-2.828601,9.619258,
                 -3.509093,8.317114,-2.098055,9.070329,-2.685002,
                 8.326238,-2.342705,8.611631,-2.509650,10.588623,
                 -3.839537), tolerance = 1e-6)
  expect_equal(CVgausslist$Coefficients[[2]]$statistic,
               c(2.82724476,-0.47734352,1.16486650,0.48031677,
                 2.79488905,-0.46538302,1.81214955,0.59729753,
                 1.48500220,0.47578805,1.76704442,0.18970340,
                 2.55731400,-0.05895694,2.50729719,-0.44890594), tolerance = 1e-6)

  expect_equal(CVgausslist$`Warnings and Messages`[[1]],
               structure(list(`Fold Column` = character(0), Fold = integer(0),
                              Type = character(0), Message = character(0)),
                         row.names = c(NA,0L), class = c("tbl_df", "tbl", "data.frame")))

})

test_that("that wrong model formulas are warned about in cross_validate()",{

  # skip_test_if_old_R_version()

  library(caret)
  library(groupdata2)

  set_seed_for_R_compatibility(1)

  data(Sacramento)

  full_data <- Sacramento
  full_data$in_sacramento <- factor( ifelse(full_data$city == "SACRAMENTO", 1, 0) )
  full_data <- fold(full_data, k=5, cat_col = "in_sacramento")

  # Create model formula without dependent variable
  # this should of course throw an error!
  model_formulas <- c("in_sacramento + beds + baths")

  expect_error(cross_validate(data=full_data, models=model_formulas,
                               fold_cols=".folds", family="gaussian"),
               "The model formula does not contain a dependent variable.")

  })

test_that("that singular fit messages are caught, counted and messaged about in cross_validate()",{

  # skip_test_if_old_R_version()

  library(groupdata2)

  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  expect_warning(expect_message(CVbinom <- cross_validate(dat, models = c("diagnosis~score+(1|participant)+(1|session)"),
                            family='binomial', REML = FALSE, model_verbose=FALSE,
                            positive=2), "Boundary \\(Singular\\) Fit Message"),
                 "cross_validate(): Convergence Warning:", fixed=TRUE)

  expect_equal(CVbinom$`Singular Fit Messages`, 3)

  # Can't expect the same warnings on mac and ubuntu
  # so we just check that the singular fit message is there
  expect_true(
    "boundary (singular) fit: see ?isSingular\n" %in%
      CVbinom$`Warnings and Messages`[[1]]$Message
  )
})

test_that("the expected errors are thrown by cross_validate()",{


  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          num_fold_cols = 3,
                          cat_col = 'diagnosis',
                          id_col = 'participant')
  dat[[".folds_3"]] <- as.character(dat[[".folds_3"]])

  expect_error(cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = paste0(".folds_",1:3), family='binomial',
                                REML = FALSE, model_verbose=FALSE,
                                positive=1),
               "At least one of the fold columns is not a factor.", fixed=TRUE)
  expect_error(cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                              fold_cols = paste0(".folds_",1), family='fdsfs',
                              REML = FALSE, model_verbose=FALSE,
                              positive=1),
               "Only 'gaussian' and 'binomial' families are currently allowed.", fixed=TRUE)

})

test_that("model_verbose reports the correct model functions in cross_validate()",{


  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          num_fold_cols = 3,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Test the list of verbose messages
  # glm()
  expect_equal(evaluate_promise(cross_validate(dat, models = c("diagnosis~score"),
                                               fold_cols = paste0(".folds_",1), family='binomial',
                                               REML = FALSE, model_verbose=TRUE,
                                               positive=1))$messages,
                 as.character(c(
                   "Updated model_specifics to { model_formula = , family = binomial, link = logit, control = (c(\"bobyqa\", \"Nelder_Mead\"), TRUE, FALSE, FALSE, 1e-05, 1e-07, TRUE, TRUE, list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), list(check.conv.grad = list(action = \"warning\", tol = 0.001, relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), list()), REML = FALSE, positive = 1, cutoff = 0.5, model_verbose = TRUE }. Note: If incorrect, remember to name arguments in model_specific.\n",
                   "Model function: Used glm()\n",
                   "Model function: Used glm()\n",
                   "Model function: Used glm()\n",
                   "Model function: Used glm()\n"
                 )))

  # glmer
  expect_equal(evaluate_promise(cross_validate(dat, models = c("diagnosis~score+(1|session)"),
                                               fold_cols = paste0(".folds_",1), family='binomial',
                                               REML = FALSE, model_verbose=TRUE,
                                               positive=1))$messages,
               as.character(c(
                 "Updated model_specifics to { model_formula = , family = binomial, link = logit, control = (c(\"bobyqa\", \"Nelder_Mead\"), TRUE, FALSE, FALSE, 1e-05, 1e-07, TRUE, TRUE, list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), list(check.conv.grad = list(action = \"warning\", tol = 0.001, relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), list()), REML = FALSE, positive = 1, cutoff = 0.5, model_verbose = TRUE }. Note: If incorrect, remember to name arguments in model_specific.\n",
                 "Model function: Used lme4::glmer()\n",
                 "Model function: Used lme4::glmer()\n",
                 "Model function: Used lme4::glmer()\n",
                 "Model function: Used lme4::glmer()\n"
               )))

  # lm
  expect_equal(evaluate_promise(cross_validate(dat, models = c("score~diagnosis"),
                                               fold_cols = paste0(".folds_",1), family='gaussian',
                                               REML = FALSE, model_verbose=TRUE,
                                               positive=1))$messages,
               as.character(c(
                 "Updated model_specifics to { model_formula = , family = gaussian, link = identity, control = (nloptwrap, TRUE, 1e-05, TRUE, FALSE, list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), list()), REML = FALSE, positive = 1, cutoff = 0.5, model_verbose = TRUE }. Note: If incorrect, remember to name arguments in model_specific.\n",
                 "Model function: Used lm()\n",
                 "Model function: Used lm()\n",
                 "Model function: Used lm()\n",
                 "Model function: Used lm()\n"
               )))
  # glm due to different link function
  expect_equal(evaluate_promise(cross_validate(dat, models = c("score~diagnosis"),
                                               fold_cols = paste0(".folds_",1), family='gaussian',
                                               link = "log",
                                               REML = FALSE, model_verbose=TRUE,
                                               positive=1))$messages,
               as.character(c(
                 "Updated model_specifics to { model_formula = , family = gaussian, link = log, control = (c(\"bobyqa\", \"Nelder_Mead\"), TRUE, FALSE, FALSE, 1e-05, 1e-07, TRUE, TRUE, list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), list(check.conv.grad = list(action = \"warning\", tol = 0.001, relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), list()), REML = FALSE, positive = 1, cutoff = 0.5, model_verbose = TRUE }. Note: If incorrect, remember to name arguments in model_specific.\n",
                 "Model function: Used glm()\n",
                 "Model function: Used glm()\n",
                 "Model function: Used glm()\n",
                 "Model function: Used glm()\n"
               )))

  # lmer
  expect_equal(evaluate_promise(cross_validate(dat, models = c("score~diagnosis+(1|session)"),
                                               fold_cols = paste0(".folds_",1), family = 'gaussian',
                                               REML = FALSE, model_verbose = TRUE,
                                               positive = 1))$messages,
               as.character(c(
                 "Updated model_specifics to { model_formula = , family = gaussian, link = identity, control = (nloptwrap, TRUE, 1e-05, TRUE, FALSE, list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\"), list(check.conv.grad = list(action = \"warning\", tol = 0.002, relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), list()), REML = FALSE, positive = 1, cutoff = 0.5, model_verbose = TRUE }. Note: If incorrect, remember to name arguments in model_specific.\n",
                 "Model function: Used lme4::lmer()\n",
                 "Model function: Used lme4::lmer()\n",
                 "Model function: Used lme4::lmer()\n",
                 "Model function: Used lme4::lmer()\n"
               )))

  # glmer due to different link function
  expect_equal(evaluate_promise(cross_validate(dat, models = c("score~diagnosis+(1|session)"),
                                               fold_cols = paste0(".folds_",1), family = 'gaussian',
                                               REML = FALSE, model_verbose = TRUE,
                                               link = "log",
                                               positive = 1))$messages,
               as.character(c(
                 "Updated model_specifics to { model_formula = , family = gaussian, link = log, control = (c(\"bobyqa\", \"Nelder_Mead\"), TRUE, FALSE, FALSE, 1e-05, 1e-07, TRUE, TRUE, list(check.nobs.vs.rankZ = \"ignore\", check.nobs.vs.nlev = \"stop\", check.nlev.gtreq.5 = \"ignore\", check.nlev.gtr.1 = \"stop\", check.nobs.vs.nRE = \"stop\", check.rankX = \"message+drop.cols\", check.scaleX = \"warning\", check.formula.LHS = \"stop\", check.response.not.const = \"stop\"), list(check.conv.grad = list(action = \"warning\", tol = 0.001, relTol = NULL), check.conv.singular = list(action = \"message\", tol = 1e-04), check.conv.hess = list(action = \"warning\", tol = 1e-06)), list()), REML = FALSE, positive = 1, cutoff = 0.5, model_verbose = TRUE }. Note: If incorrect, remember to name arguments in model_specific.\n",
                 "Model function: Used lme4::glmer()\n",
                 "Model function: Used lme4::glmer()\n",
                 "Model function: Used lme4::glmer()\n",
                 "Model function: Used lme4::glmer()\n"
               )))


})

test_that("binomial models with metrics list work with cross_validate()",{

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  CVbinomlist <- cross_validate(dat,
                                models = c("diagnosis~score", "diagnosis~age"),
                                fold_cols = '.folds', family = 'binomial',
                                REML = FALSE,
                                metrics = list("AUC" = FALSE,
                                               "Accuracy" = TRUE,
                                               "Prevalence" = FALSE),
                                model_verbose = FALSE,
                                positive = 1 )

  expect_equal(colnames(CVbinomlist),
               c("Balanced Accuracy", "Accuracy", "F1", "Sensitivity", "Specificity",
                 "Pos Pred Value", "Neg Pred Value", "Lower CI", "Upper CI", "Kappa",
                 "MCC", "Detection Rate", "Detection Prevalence", "Predictions",
                 "ROC", "Confusion Matrix", "Coefficients", "Folds", "Fold Columns",
                 "Convergence Warnings", "Singular Fit Messages", "Other Warnings",
                 "Warnings and Messages", "Family", "Link", "Dependent", "Fixed"
               ))

  expect_equal(CVbinomlist$Accuracy, c(0.766666666666667, 0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111,0.3333333), tolerance=1e-3)

  expect_error(cross_validate(dat,
                              models = c("diagnosis~score", "diagnosis~age"),
                              fold_cols = '.folds', family = 'binomial',
                              REML = FALSE,
                              metrics = list("AKG" = FALSE, # error here
                                             "Accuracy" = TRUE,
                                             "Prevalencer" = FALSE),
                              model_verbose = FALSE,
                              positive = 1 ),
               "'metrics_list' contained unknown metric names: AKG, Prevalencer.",
               fixed = TRUE)
  expect_error(cross_validate(dat,
                              models = c("diagnosis~score", "diagnosis~age"),
                              fold_cols = '.folds', family = 'binomial',
                              REML = FALSE,
                              metrics = list("AUC" = 1,
                                             "Accuracy" = TRUE,
                                             "Prevalence" = FALSE),
                              model_verbose = FALSE,
                              positive = 1 ),
               "The values in the 'metrics' list must be either TRUE or FALSE.",
               fixed = TRUE)

})

test_that("gaussian models with metrics list work with cross_validate()",{

  # skip_test_if_old_R_version()

  # Load data and fold it
  set_seed_for_R_compatibility(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Cross-validate the data
  CVed <- cross_validate(dat, "score~diagnosis",
                         fold_cols = '.folds',
                         family = 'gaussian', link = NULL,
                         REML = FALSE,
                         metrics = list("RMSE" = FALSE,
                                        "r2m" = TRUE),
                         model_verbose = FALSE)

  expect_equal(colnames(CVed),
               c("MAE", "r2m", "r2c", "AIC", "AICc", "BIC", "Predictions", "Results",
                 "Coefficients", "Folds", "Fold Columns", "Convergence Warnings",
                 "Singular Fit Messages", "Other Warnings", "Warnings and Messages",
                 "Family", "Link", "Dependent", "Fixed"))
  expect_equal(colnames(CVed$Results[[1]]),
               c("Fold Column", "Fold", "MAE", "r2m", "r2c", "AIC", "AICc",
                 "BIC"))

  # Cross-validate the data
  expect_error(cross_validate(dat, "score~diagnosis",
                         fold_cols = '.folds',
                         family = 'gaussian', link = NULL,
                         REML = FALSE,
                         metrics = list("Accuracy" = TRUE, # Should error in gaussian
                                        "r2m" = TRUE),
                         model_verbose = FALSE),
               "'metrics_list' contained unknown metric names: Accuracy.",
               fixed = TRUE)
})

