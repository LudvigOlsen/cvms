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
                                fold_cols = '.folds', family='binomial',
                                REML = FALSE, model_verbose=FALSE,
                                positive=1)

  expect_equal(CVbinomlist$AUC, c(0.76388889, 0.02083333), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.5855803, 0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9421975, 0.05157582), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4927536, -0.5217391), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8888889,0.5000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7777778, 0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7619048, 0.4285714), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3,0.3), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111,0.2500000), tolerance=1e-3)
  expect_equal(CVbinomlist$MCC, c(0.5048268, -0.5345225), tolerance=1e-3)
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
  expect_equal(nrow(CVbinomlist$ROC[[1]]),31)

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
                                fold_cols = '.folds', family='binomial',
                                REML = FALSE, model_verbose=FALSE,
                                positive=1)

  expect_equal(CVbinomlist$AUC, c(0.76388889, 0.02083333), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.5855803, 0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9421975, 0.05157582), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4927536, -0.5217391), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8888889,0.5000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7777778, 0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7619048, 0.4285714), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.3,0.3), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111,0.2500000), tolerance=1e-3)
  expect_equal(CVbinomlist$MCC, c(0.5048268, -0.5345225), tolerance=1e-3)
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
  expect_equal(nrow(CVbinomlist$ROC[[1]]),31)


})

test_that("binomial models work with cross_validate()",{


  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')


  CVbinomlistrand <- cross_validate(dat,
                                    models = c("diagnosis~score + (1|session)","diagnosis~age + (1|session)"),
                                    fold_cols = '.folds',
                                    family='binomial',
                                    REML = FALSE,
                                    link = NULL,
                                    model_verbose=FALSE,
                                    positive=1)

  expect_equal(CVbinomlistrand$AUC, c(0.8425926, 0.02083333), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Lower CI`, c(0.6892322, 0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Upper CI`, c(0.995953, 0.05157582), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Kappa, c(0.6575342, -0.5217391), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Sensitivity, c(0.8333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Specificity, c(0.8333333,0.5000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Pos Pred Value`, c(0.7692308,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Neg Pred Value`, c(0.8823529,0.4285714), tolerance=1e-3)
  expect_equal(CVbinomlistrand$F1, c(0.8, NA), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Detection Rate`, c(0.3333333,0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Detection Prevalence`, c(0.4333333,0.3), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Balanced Accuracy`, c(0.8333333,0.25), tolerance=1e-3)
  expect_equal(CVbinomlistrand$MCC, c(0.6590820, -0.5345225), tolerance=1e-3)
  expect_equal(CVbinomlistrand$Folds, c(4,4))
  expect_equal(CVbinomlistrand$`Fold Columns`, c(1,1))
  expect_equal(CVbinomlistrand$`Convergence Warnings`, c(0,0))
  expect_equal(CVbinomlistrand$Family, c('binomial','binomial'))
  expect_equal(CVbinomlistrand$Dependent, c('diagnosis','diagnosis'))
  expect_equal(CVbinomlistrand$Fixed, c('score','age'))
  expect_equal(CVbinomlistrand$Random, c('(1|session)','(1|session)'))


  # Enter sub tibbles
  expect_is(CVbinomlistrand$Predictions[[1]], "tbl_df")
  expect_is(CVbinomlistrand$ROC[[1]], "tbl_df")
  expect_equal(colnames(CVbinomlistrand$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(colnames(CVbinomlistrand$ROC[[1]]), c("Sensitivities","Specificities"))
  expect_equal(nrow(CVbinomlistrand$Predictions[[1]]),30)
  expect_equal(nrow(CVbinomlistrand$ROC[[1]]),25) # Why?

  expect_is(CVbinomlistrand$Predictions[[2]], "tbl_df")
  expect_is(CVbinomlistrand$ROC[[2]], "tbl_df")
  expect_equal(colnames(CVbinomlistrand$Predictions[[2]]), c("Fold Column","Fold","Target","Prediction","Predicted Class"))
  expect_equal(colnames(CVbinomlistrand$ROC[[2]]), c("Sensitivities","Specificities"))
  expect_equal(nrow(CVbinomlistrand$Predictions[[2]]),30)
  expect_equal(nrow(CVbinomlistrand$ROC[[2]]),10) # Why?

})


test_that("gaussian model with cross_validate()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  # Cross-validate the data
  CVed <- cross_validate(dat, "score~diagnosis",
                          fold_cols = '.folds',
                          family='gaussian', link = NULL,
                          REML = FALSE,
                          model_verbose=FALSE)

  expect_equal(CVed$RMSE, 16.5418, tolerance=1e-3)
  expect_equal(CVed$MAE, 13.82438, tolerance=1e-3)
  expect_equal(CVed$r2m, 0.2723672, tolerance=1e-3)
  expect_equal(CVed$r2c, 0.2723672, tolerance=1e-3)
  expect_equal(CVed$AIC, 194.6612, tolerance=1e-3)
  expect_equal(CVed$AICc, 195.967, tolerance=1e-3)
  expect_equal(CVed$BIC, 197.995, tolerance=1e-3)
  expect_equal(CVed$Folds, 4)
  expect_equal(CVed$`Fold Columns`, 1)
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
                         fold_cols = '.folds',
                         family='gaussian', link = NULL,
                         REML = FALSE,
                         model_verbose=FALSE)

  expect_equal(CVed$RMSE, c(8.764387, 17.231632), tolerance=1e-3)
  expect_equal(CVed$MAE, c(6.809691, 15.614625), tolerance=1e-3)
  expect_equal(CVed$r2m, c(0.29097372, 0.03803014), tolerance=1e-3)
  expect_equal(CVed$r2c, c(0.8080003, 0.5228147), tolerance=1e-3)
  expect_equal(CVed$AIC, c(175.6952, 193.8477), tolerance=1e-3)
  expect_equal(CVed$AICc, c(177.9978, 196.1504), tolerance=1e-3)
  expect_equal(CVed$BIC, c(180.1403, 198.2929), tolerance=1e-3)
  expect_equal(CVed$Folds, c(4,4))
  expect_equal(CVed$`Fold Columns`, c(1,1))
  expect_equal(CVed$`Convergence Warnings`, c(0,0))
  expect_equal(CVed$Family, c('gaussian','gaussian'))
  expect_equal(CVed$Dependent, c('score','score'))
  expect_equal(CVed$Fixed, c('diagnosis', 'age'))
  expect_equal(CVed$Random, c('(1|session)', '(1|session)'))


})


test_that("binomial models work with control specified in cross_validate()",{


  # Load data and fold it
  set.seed(2)
  dat <- groupdata2::fold(participant.scores, k = 3,
                          cat_col = 'diagnosis',
                          id_col = 'participant')


  CVbinomlistrand <- cross_validate(dat,
                                    models = c("diagnosis~score + (1|session)"),
                                    fold_cols = '.folds',
                                    family='binomial',
                                    REML = FALSE,
                                    link = NULL,
                                    control = lme4::glmerControl(optimizer="bobyqa",
                                                           optCtrl=list(maxfun=1000000)),
                                    model_verbose=FALSE,
                                    positive=1)

  expect_equal(CVbinomlistrand$AUC, c(0.8842593), tolerance=1e-3)
  expect_equal(CVbinomlistrand$`Convergence Warnings`, c(0))

  # Warning because of too few iterations
  expect_warning(cross_validate(dat,
                                models = c("diagnosis~score+age + (1|session) + (1|age)"),
                                fold_cols = '.folds',
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
                                    fold_cols = '.folds',
                                    family='gaussian',
                                    REML = FALSE,
                                    link = NULL,
                                    control = lme4::lmerControl(optimizer="Nelder_Mead",
                                                                 optCtrl=list(maxfun=1000000)),
                                    model_verbose=FALSE)

  expect_equal(CVgausslistrand$RMSE, c(9.604585), tolerance=1e-3)
  expect_equal(CVgausslistrand$`Convergence Warnings`, c(0))

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

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant',
                          num_fold_cols=2)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE,
                                positive=1)

  expect_equal(CVbinomlist$AUC, c(0.7708333, 0.07291667), tolerance=1e-3)
  expect_equal(CVbinomlist$`Lower CI`, c(0.5962955, 0.0000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Upper CI`, c(0.9453712, 0.1588227), tolerance=1e-3)
  expect_equal(CVbinomlist$Kappa, c(0.4927536, -0.3858696), tolerance=1e-3)
  expect_equal(CVbinomlist$Sensitivity, c(0.5833333, 0.1250000), tolerance=1e-3)
  expect_equal(CVbinomlist$Specificity, c(0.8888889, 0.5000000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Pos Pred Value`, c(0.7777778, 0.1250000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Neg Pred Value`, c(0.7619048, 0.4642857), tolerance=1e-3)
  expect_equal(CVbinomlist$F1, c(0.6666667, NA), tolerance=1e-3)
  expect_equal(CVbinomlist$Prevalence, c(0.4,0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Rate`, c(0.2333333,0.0500000), tolerance=1e-3)
  expect_equal(CVbinomlist$`Detection Prevalence`, c(0.30, 0.35), tolerance=1e-3)
  expect_equal(CVbinomlist$`Balanced Accuracy`, c(0.7361111, 0.3125000), tolerance=1e-3)
  expect_equal(CVbinomlist$MCC, c(0.5048268, -0.3922612), tolerance=1e-3)
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
  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Reference","Pos_0","Pos_1","N"))
  expect_equal(nrow(CVbinomlist$Predictions[[1]]),60)
  expect_equal(nrow(CVbinomlist$ROC[[1]]),60)
  expect_equal(colnames(CVbinomlist$Coefficients[[1]]),
               c("term","estimate","std.error","statistic","p.value","Fold","Fold Column"))

  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$`Fold Column`, rep(c(".folds_1",".folds_2"), each=4))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$Prediction, as.character(c(0,1,0,1,0,1,0,1)))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$Reference, as.character(c(0,0,1,1,0,0,1,1)))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$Pos_0, rep(c("TP","FN","FP","TN"),2))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$Pos_1, rep(c("TN","FP","FN","TP"),2))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7,5,2,16,7,5,2,16))

  expect_equal(CVbinomlist$Results[[1]]$`Fold Column`,c(".folds_1",".folds_2"))
  expect_equal(CVbinomlist$Results[[1]]$`Balanced Accuracy`,c(0.7361111, 0.7361111), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$F1,c(0.6666667, 0.6666667), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$Sensitivity,c(0.5833333, 0.5833333), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$Specificity,c(0.8888889, 0.8888889), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Pos Pred Value`,c(0.7777778, 0.7777778), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Neg Pred Value`,c(0.7619048, 0.7619048), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$AUC,c(0.7638889, 0.7777778), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Lower CI`,c(0.5855803, 0.6070107), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Upper CI`,c(0.9421975, 0.9485449), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$Kappa,c(0.4927536, 0.4927536), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Detection Rate`,c(0.2333333, 0.2333333), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$`Detection Prevalence`,c(0.3000000, 0.3000000), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$Prevalence,c(0.4, 0.4), tolerance=1e-3)
  expect_equal(CVbinomlist$Results[[1]]$MCC,c(0.5048268, 0.5048268), tolerance=1e-3)
})

test_that("binomial models work with positive as.character in cross_validate()",{

  # Load data and fold it
  set.seed(2)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant',
                          num_fold_cols=2) %>%
  dplyr::mutate(diagnosis = factor(ifelse(diagnosis==0, "E","B")))

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE,
                                positive=1)

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Reference","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16,2,5,7,15,3,5,7))
  expect_equal(CVbinomlist$F1, c(0.8049933, 0.5641026), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive="E")

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Reference","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16,2,5,7,15,3,5,7))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive="B")

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Reference","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16,2,5,7,15,3,5,7))
  expect_equal(CVbinomlist$F1, c(0.8049933, 0.5641026), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive=1)

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Reference","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16,2,5,7,15,3,5,7))
  expect_equal(CVbinomlist$F1, c(0.8049933, 0.5641026), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive=2)

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Reference","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(16,2,5,7,15,3,5,7))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance=1e-3)

  expect_error(cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive="C"),
               "When 'positive' is a character, it must correspond to a factor level in the dependent variable.\n'positive' is C and levels are B and E.")

  # Interchanging the level names

  set.seed(2)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant',
                          num_fold_cols=2) %>%
    dplyr::mutate(diagnosis = factor(ifelse(diagnosis==0, "B","E")))

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE,
                                positive=1)

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Reference","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7,5,2,16,7,5,3,15))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive="E")

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Reference","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7,5,2,16,7,5,3,15))
  expect_equal(CVbinomlist$F1, c(0.8049933,0.5641026), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive="B")

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Reference","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7,5,2,16,7,5,3,15))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive=1)

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Reference","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7,5,2,16,7,5,3,15))
  expect_equal(CVbinomlist$F1, c(0.6515152, NA), tolerance=1e-3)

  CVbinomlist <- cross_validate(dat, models = c("diagnosis~score","diagnosis~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='binomial',
                                REML = FALSE, model_verbose=FALSE, positive=2)

  expect_equal(colnames(CVbinomlist$`Confusion Matrix`[[1]]), c("Fold Column","Prediction","Reference","Pos_B","Pos_E","N"))
  expect_equal(CVbinomlist$`Confusion Matrix`[[1]]$N, c(7,5,2,16,7,5,3,15))
  expect_equal(CVbinomlist$F1, c(0.8049933,0.5641026), tolerance=1e-3)


  })



test_that("gaussian models work with repeated cross_validate()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant',
                          num_fold_cols=2)

  CVgausslist <- cross_validate(dat, models = c("score~diagnosis","score~age"),
                                fold_cols = c('.folds_1','.folds_2'), family='gaussian',
                                REML = FALSE, model_verbose=FALSE)


  expect_equal(CVgausslist$RMSE, c(16.6695, 22.6363), tolerance=1e-3)
  expect_equal(CVgausslist$MAE, c(13.83210, 18.96102), tolerance=1e-3)
  expect_equal(CVgausslist$r2m, c(0.27048240, 0.03761387), tolerance=1e-3)
  expect_equal(CVgausslist$r2c, c(0.27048240, 0.03761387), tolerance=1e-3)
  expect_equal(CVgausslist$AIC, c(194.7412, 201.2037), tolerance=1e-3)
  expect_equal(CVgausslist$AICc, c(196.0471, 202.5096), tolerance=1e-3)
  expect_equal(CVgausslist$BIC, c(198.0751, 204.5376), tolerance=1e-3)
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
  expect_equal(colnames(CVgausslist$Coefficients[[1]]),
               c("term","estimate","std.error","statistic","p.value","Fold","Fold Column"))
  expect_equal(colnames(CVgausslist$Predictions[[1]]), c("Fold Column","Fold","Target","Prediction"))
  expect_equal(unique(CVgausslist$Predictions[[1]]$`Fold Column`), c(".folds_1",".folds_2"))

})

test_that("that wrong model formulas are warned about in cross_validate()",{

  library(caret)
  library(groupdata2)

  set.seed(1)

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

  library(groupdata2)

  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  expect_message(CVbinom <- cross_validate(dat, models = c("diagnosis~score+(1|participant)+(1|session)"),
                            family='binomial', REML = FALSE, model_verbose=FALSE,
                            positive=2), "Boundary \\(Singular\\) Fit Message")

  expect_equal(CVbinom$`Singular Fit Messages`, 3)

})
