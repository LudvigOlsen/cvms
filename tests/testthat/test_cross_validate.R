library(cvms)
context("cross_validate()")

# NOTICE:
# Numbers tested are the results I got and not "what should be"
# This will allow me to see if something changes, but it shouldn't give false confidence.


test_that("binomial models work with cross_validate()",{


  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')


  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                folds_col = '.folds', family='binomial',
                                REML = FALSE, model_verbose=FALSE)

  expect_equal(CVbinomlist$AUC, c(0.7615741,0.8333333), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.5851154,0.6841541), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9380328,0.9825126), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4927536, -0.3636364), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8888889,0.6666667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7777778,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7619048,0.5000000), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3,0.2), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111,0.3333333), tolerance=1e-3)
  expect_equal(CVbinomlist$Folds, c(4,4))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlist$Family, c('binomial','binomial'))
  expect_equal(CVbinomlist$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlist$Fixed, c('score','age'))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("prediction","predicted_class","target"))
  expect_equal(colnames(CVbinomlist$ROC[[1]]), c("sensitivities","specificities"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlist$ROC[[1]]),29)


})

test_that("binomial models checks that dependent variable is numeric with cross_validate()",{


  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant') %>%
    dplyr::mutate(diagnosis = factor(diagnosis))

  # dat %>% str()

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                folds_col = '.folds', family='binomial',
                                REML = FALSE, model_verbose=FALSE)

  expect_equal(CVbinomlist$AUC, c(0.7615741,0.8333333), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.5851154,0.6841541), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9380328,0.9825126), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4927536, -0.3636364), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8888889,0.6666667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7777778,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7619048,0.5000000), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3,0.2), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111,0.3333333), tolerance=1e-3)
  expect_equal(CVbinomlist$Folds, c(4,4))
  expect_equal(CVbinomlist$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlist$Family, c('binomial','binomial'))
  expect_equal(CVbinomlist$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlist$Fixed, c('score','age'))

  # Enter sub tibbles
  expect_is(CVbinomlist$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlist$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlist$Predictions[[1]]), c("prediction","predicted_class","target"))
  expect_equal(colnames(CVbinomlist$ROC[[1]]), c("sensitivities","specificities"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlist$ROC[[1]]),29)


})

test_that("binomial models work with cross_validate()",{


  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')


  CVbinomlistrand <- cross_validate(dat,
                                    models = c("diagnosis~score + (1|session)","diagnosis~age + (1|session)"),
                                    folds_col = '.folds',
                                    family='binomial',
                                    REML = FALSE,
                                    link = NULL,
                                    model_verbose=FALSE)

  expect_equal(CVbinomlistrand$AUC, c(0.8611111,0.8333333), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Lower CI`, c(0.7103334,0.6841541), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Upper CI`, c(1.0000000,0.9825126), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Kappa, c(0.6575342, -0.3636364), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Sensitivity, c(0.8333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Specificity, c(0.8333333,0.6666667), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Pos Pred Value`, c(0.7692308,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Neg Pred Value`, c(0.8823529,0.5000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$F1, c(0.8, NA), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Detection Rate`, c(0.3333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Detection Prevalence`, c(0.4333333,0.2), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Balanced Accuracy`, c(0.8333333,0.3333333), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Folds, c(4,4))
  expect_equal(CVbinomlistrand$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlistrand$Family, c('binomial','binomial'))
  expect_equal(CVbinomlistrand$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlistrand$Fixed, c('score','age'))
  expect_equal(CVbinomlistrand$Random, c('1|session','1|session'))


  # Enter sub tibbles
  expect_is(CVbinomlistrand$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlistrand$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlistrand$Predictions[[1]]), c("prediction","predicted_class","target"))
  expect_equal(colnames(CVbinomlistrand$ROC[[1]]), c("sensitivities","specificities"))
  expect_equal(nrow(CVbinomlistrand$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlistrand$ROC[[1]]),27) # Why?

  expect_is(CVbinomlistrand$Predictions[[2]], "tbl_df")
  expect_is(CVbinomlistrand$ROC[[2]], "tbl_df")
  expect_equal(colnames(CVbinomlistrand$Predictions[[2]]), c("prediction","predicted_class","target"))
  expect_equal(colnames(CVbinomlistrand$ROC[[2]]), c("sensitivities","specificities"))
  expect_equal(nrow(CVbinomlistrand$Predictions[[2]]),30)
  expect_equal(nrow(CVbinomlistrand$ROC[[2]]),11) # Why?

})


test_that("gaussian model with cross_validate()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Cross-validate the data
  CVed <- cross_validate(dat, "score~diagnosis",
                          folds_col = '.folds',
                          family='gaussian', link = NULL,
                          REML = FALSE,
                          model_verbose=FALSE)

  expect_equal(CVed$RMSE, 17.16817, tolerance=1e-3)
  expect_equal(CVed$r2m, 0.2640793, tolerance=1e-3)
  expect_equal(CVed$r2c, 0.2640793, tolerance=1e-3)
  expect_equal(CVed$AIC, 194.6904, tolerance=1e-3)
  expect_equal(CVed$AICc, 195.9963, tolerance=1e-3)
  expect_equal(CVed$BIC, 198.0243, tolerance=1e-3)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Convergence Warnings`, 0)
  expect_equal(CVed$Family, 'gaussian')
  expect_equal(CVed$Dependent, 'score')
  expect_equal(CVed$Fixed, 'diagnosis')

})


test_that("gaussian mixed models with cross_validate()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Cross-validate the data
  CVed <- cross_validate(dat, c("score~diagnosis + (1|session)","score~age + (1|session)"),
                         folds_col = '.folds',
                         family='gaussian', link = NULL,
                         REML = FALSE,
                         model_verbose=FALSE)

  expect_equal(CVed$RMSE, c(9.659488, 15.202256), tolerance=1e-3)
  expect_equal(CVed$r2m, c(0.28218811, 0.01319593), tolerance=1e-3)
  expect_equal(CVed$r2c, c(0.8043153, 0.5016050), tolerance=1e-3)
  expect_equal(CVed$AIC, c(175.9497, 194.6358), tolerance=1e-3)
  expect_equal(CVed$AICc, c(178.2523, 196.9384), tolerance=1e-3)
  expect_equal(CVed$BIC, c(180.3948, 199.0809), tolerance=1e-3)
  expect_equal(CVed$Folds, c(4,4))
  expect_equal(CVed$`Convergence Warnings`, c(0,0))
  expect_equal(CVed$Family, c('gaussian','gaussian'))
  expect_equal(CVed$Dependent, c('score','score'))
  expect_equal(CVed$Fixed, c('diagnosis', 'age'))
  expect_equal(CVed$Random, c('1|session', '1|session'))


})


test_that("binomial models work with control specified in cross_validate()",{


  # Load data and fold it
  set.seed(2)
  dat <- groupdata2::fold(participant.scores, k = 3,
                          cat_col = 'diagnosis',
                          id_col = 'participant')


  CVbinomlistrand <- cross_validate(dat,
                                    models = c("diagnosis~score + (1|session)"),
                                    folds_col = '.folds',
                                    family='binomial',
                                    REML = FALSE,
                                    link = NULL,
                                    control = lme4::glmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=1000000)),
                                    model_verbose=FALSE)

  expect_equal(CVbinomlistrand$AUC, c(0.7847), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Convergence Warnings`, c(0))

  # Warning because of too few iterations
  expect_warning(cross_validate(dat,
                                models = c("diagnosis~score+age + (1|session) + (1|age)"),
                                folds_col = '.folds',
                                family='binomial',
                                REML = FALSE,
                                link = NULL,
                                control = lme4::glmerControl(optimizer="bobyqa",
                                                             optCtrl=list(maxfun=100)),
                                model_verbose=FALSE), "cross_validate(): Convergence Warning:", fixed = TRUE)

})

test_that("gaussian models work with control specified in cross_validate()",{


  # Load data and fold it
  set.seed(2)
  dat <- groupdata2::fold(participant.scores, k = 3,
                          cat_col = 'diagnosis',
                          id_col = 'participant')


  CVgausslistrand <- cross_validate(dat,
                                    models = c("score~diagnosis + (1|session)"),
                                    folds_col = '.folds',
                                    family='gaussian',
                                    REML = FALSE,
                                    link = NULL,
                                    control = lme4::lmerControl(optimizer="Nelder_Mead",
                                                                 optCtrl=list(maxfun=1000000)),
                                    model_verbose=FALSE)

  expect_equal(CVgausslistrand$RMSE, c(10.443), tolerance=1e-3)
  expect_equal(CVgausslistrand$`Convergence Warnings`, c(0))

  # TODO When counting singular (boundary fit) messages, uncomment and change expected warning/message
  # # Warning because of too few iterations
  # expect_warning(cross_validate(dat,
  #                               models = c("age~diagnosis*score + (score|session) + (1|score)"),
  #                               folds_col = '.folds',
  #                               family='gaussian',
  #                               REML = FALSE,
  #                               link = NULL,
  #                               control = lme4::lmerControl(optimizer="Nelder_Mead",
  #                                                            optCtrl=list(maxfun=100)),
  #                               model_verbose=FALSE), "cross_validate(): Convergence Warning:", fixed = TRUE)


})



test_that("model using dot in formula ( y ~ . ) works with cross_validate()",{

  # We wish to test if using the dot "y~." method in the model formula
  # correctly leaves out .folds column.

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant') %>%
    dplyr::select(-c(participant, session))

  # Expect no warnings
  # https://stackoverflow.com/questions/22003306/is-there-something-in-testthat-like-expect-no-warnings
  expect_warning(cross_validate(dat, models = c("diagnosis~."),
                                folds_col = '.folds', family='binomial',
                                REML = FALSE, model_verbose=FALSE),
                 regexp = NA)

  # Expect no warnings
  # https://stackoverflow.com/questions/22003306/is-there-something-in-testthat-like-expect-no-warnings
  expect_warning(cross_validate(dat, models = c("score~."),
                                folds_col = '.folds', family='gaussian',
                                REML = FALSE, model_verbose=FALSE),
                 regexp = NA)


})

test_that("models work with repeated cross_validate()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant',
                          num_fold_cols=2)


  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                folds_col = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE)

  CVgausslist <- cross_validate(dat, models = c("score~diagnosis","score~age"),
                                folds_col = c('.folds_1','.folds_2'), family='gaussian',
                                REML = FALSE, model_verbose=FALSE)

  CVgausslist$Results
})


