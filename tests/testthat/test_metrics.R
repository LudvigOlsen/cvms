library(cvms)
context("metrics")

# Diagnosis by score

test_that("Metrics work for glm in validate()",{

  set.seed(7)

  dat <- groupdata2::partition(participant.scores, p = 0.8,
                               cat_col = 'diagnosis',
                               id_col = 'participant',
                               list_out = FALSE)

  validated <- validate(train_data=dat, models="diagnosis~score",
                        partitions_col = '.partitions', family = 'binomial')
  same_model <- glm(diagnosis~score, data=dat[dat$.partitions==1,], family = 'binomial')

  test_data <- dat[dat$.partitions==2,]
  prob=predict(same_model, newdata=test_data, type=c("response"))

  test_data$prob=prob
  test_data <- test_data %>%
    dplyr::mutate(pred = dplyr::if_else(prob>0.5,1,0))

  # AUC
  g <- pROC::roc(diagnosis ~ prob, data = test_data)
  expect_equal(validated$Results$AUC,as.numeric(g$auc))

  auc2 <- AUC::auc(AUC::roc(test_data$prob, factor(test_data$diagnosis)))
  expect_equal(validated$Results$AUC,auc2)

  # Sensitivity
  sens <- caret::sensitivity(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                     positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$Sensitivity,sens)

  # Specificity
  spec <- caret::specificity(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                             positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$Specificity,spec)

  # posPredValue
  posPredValue_ <- caret::posPredValue(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                             positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$`Pos Pred Value`,posPredValue_)

  # negPredValue
  negPredValue_ <- caret::negPredValue(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                                       positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$`Neg Pred Value`,negPredValue_)

  rm(test_data)
})


test_that("Metrics work for glmer in validate()",{

  set.seed(7)

  dat <- groupdata2::partition(participant.scores, p = 0.8,
                               cat_col = 'diagnosis',
                               id_col = 'participant',
                               list_out = FALSE)

  validated <- validate(train_data=dat, models="diagnosis~score+(1|session)",
                        partitions_col = '.partitions', family = 'binomial')
  same_model <- lme4::glmer(diagnosis~score+(1|session), data=dat[dat$.partitions==1,], family = 'binomial')

  test_data <- dat[dat$.partitions==2,]
  prob=predict(same_model, newdata=test_data, type=c("response"))

  test_data$prob=prob
  test_data <- test_data %>%
    dplyr::mutate(pred = dplyr::if_else(prob>0.5,1,0))

  # AUC
  auc1 <- pROC::roc(diagnosis ~ prob, data = test_data, levels = c(0, 1), direction = "<")
  expect_equal(validated$Results$AUC,as.numeric(auc1$auc))

  auc2 <- AUC::auc(AUC::roc(test_data$prob, factor(test_data$diagnosis)))
  expect_equal(validated$Results$AUC,auc2)


  # Sensitivity
  sens <- caret::sensitivity(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                             positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$Sensitivity,sens)

  # Specificity
  spec <- caret::specificity(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                             positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$Specificity,spec)

  # posPredValue
  posPredValue_ <- caret::posPredValue(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                                       positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$`Pos Pred Value`,posPredValue_)

  # negPredValue
  negPredValue_ <- caret::negPredValue(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                                       positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$`Neg Pred Value`,negPredValue_)

})

# Diagnosis by age


test_that("Metrics work for glm in validate()",{

  set.seed(6)

  dat <- groupdata2::partition(participant.scores, p = 0.8,
                               cat_col = 'diagnosis',
                               id_col = 'participant',
                               list_out = FALSE)


  validated <- validate(train_data=dat, models="diagnosis~age",
                        partitions_col = '.partitions', family = 'binomial')
  same_model <- glm(diagnosis~age, data=dat[dat$.partitions==1,], family = 'binomial')

  test_data <- dat[dat$.partitions==2,]
  prob=predict(same_model, newdata=test_data, type=c("response"))

  test_data$prob=prob
  test_data <- test_data %>%
    dplyr::mutate(pred = dplyr::if_else(prob>0.5,1,0))

  # AUC
  g <- pROC::roc(diagnosis ~ prob, data = test_data)
  expect_equal(validated$Results$AUC,as.numeric(g$auc))

  auc2 <- AUC::auc(AUC::roc(test_data$prob, factor(test_data$diagnosis)))
  expect_equal(validated$Results$AUC,auc2)

  # Sensitivity
  sens <- caret::sensitivity(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                             positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$Sensitivity,sens)

  # Specificity
  spec <- caret::specificity(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                             positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$Specificity,spec)

  # posPredValue
  posPredValue_ <- caret::posPredValue(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                                       positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$`Pos Pred Value`,posPredValue_)

  # negPredValue
  negPredValue_ <- caret::negPredValue(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                                       positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$`Neg Pred Value`,negPredValue_)

  rm(test_data)
})


test_that("Metrics work for glmer in validate()",{

  set.seed(201)

  dat <- groupdata2::partition(participant.scores, p = 0.8,
                               cat_col = 'diagnosis',
                               id_col = 'participant',
                               list_out = FALSE)

  validated <- validate(train_data=dat, models="diagnosis~age+(1|session)",
                        partitions_col = '.partitions', family = 'binomial')
  same_model <- lme4::glmer(diagnosis~age+(1|session),
                            data=dat[dat$.partitions==1,], family = 'binomial')

  test_data <- dat[dat$.partitions==2,]
  prob=predict(same_model, newdata=test_data, type=c("response"))

  test_data$prob=prob
  test_data <- test_data %>%
    dplyr::mutate(pred = dplyr::if_else(prob>0.5,1,0))

  # AUC
  auc1 <- pROC::roc(diagnosis ~ prob, data = test_data)
  expect_equal(validated$Results$AUC,as.numeric(auc1$auc))

  auc2 <- AUC::auc(AUC::roc(test_data$prob, factor(test_data$diagnosis)))
  expect_equal(validated$Results$AUC,auc2)


  # Sensitivity
  sens <- caret::sensitivity(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                             positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$Sensitivity,sens)

  # Specificity
  spec <- caret::specificity(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                             positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$Specificity,spec)

  # posPredValue
  posPredValue_ <- caret::posPredValue(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                                       positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$`Pos Pred Value`,posPredValue_)

  # negPredValue
  negPredValue_ <- caret::negPredValue(as.factor(test_data$pred), as.factor(test_data$diagnosis),
                                       positive = levels(as.factor(test_data$diagnosis))[1])
  expect_equal(validated$Results$`Neg Pred Value`,negPredValue_)

})


test_that("Metrics work in cross_validate()",{

  #
  # In this test I printed the predictions within each training loop
  # and manually copied the predictions
  # I did this to ensure that cross_validate gathers the predictions correctly before
  # calculating its metrics. This is incredibly important.
  # Metrics are calculated and compared to the metrics I got from cross_validate.
  #

  target <- c(0,0,0,1,1,1,
              0,0,0,1,1,1,1,1,1,
              0,0,0,1,1,1,
              0,0,0,1,1,1,1,1,1)
  predictions_prob <- c(0.77379615,0.36952324,0.09125579,0.89205819,0.73620142,0.55282759,
                   0.8307928,0.6042899,0.1754574,0.9317034,0.8307928,0.5145979,0.9269098,0.6874739,0.5867096,
                   0.71867985,0.26746773,0.09346533,0.85976827,0.24884534,0.13205012,
                   0.6503171,0.4541755,0.1564246,0.8445872,0.7085838,0.5871876,0.8514956,0.7607141,0.7085838)
  predictions <- dplyr::if_else(predictions_prob>0.5,1,0)

  pred_df <- data.frame("obs"=target, "prob"=predictions_prob, "pred"=predictions)

  # AUC
  auc1 <- pROC::roc(obs ~ prob, data = pred_df)
  expect_equal(as.numeric(auc1$auc), 0.7615741, tolerance = 1e-3)

  auc2 <- AUC::auc(AUC::roc(pred_df$prob, factor(pred_df$obs)))
  expect_equal(auc2,0.7615741, tolerance = 1e-3)


  # Sensitivity
  sens <- caret::sensitivity(as.factor(pred_df$pred), as.factor(pred_df$obs),
                             positive = levels(as.factor(pred_df$obs))[1])
  expect_equal(sens,0.5833333, tolerance = 1e-3)

  # # Specificity
  spec <- caret::specificity(as.factor(pred_df$pred), as.factor(pred_df$obs),
                             positive = levels(as.factor(pred_df$obs))[1])
  expect_equal(spec,0.8888889, tolerance = 1e-3)

  # posPredValue
  posPredValue_ <- caret::posPredValue(as.factor(pred_df$pred), as.factor(pred_df$obs),
                                       positive = levels(as.factor(pred_df$obs))[1])
  expect_equal(posPredValue_,0.7777778, tolerance = 1e-3)

  # negPredValue
  negPredValue_ <- caret::negPredValue(as.factor(pred_df$pred), as.factor(pred_df$obs),
                                       positive = levels(as.factor(pred_df$obs))[1])
  expect_equal(negPredValue_,0.7619048, tolerance = 1e-3)

  # F1
  F1 <- (2 * posPredValue_ * sens) / (posPredValue_ + sens)
  expect_equal(F1,0.6666667, tolerance = 1e-3)

  # Confusion matrix
  confMat <- caret::confusionMatrix(factor(pred_df$pred, levels=c(0,1)),
                                    reference=factor(pred_df$obs, levels=c(0,1)))
  TP <- confMat$table[1]
  FP <- confMat$table[3]
  FN <- confMat$table[2]
  TN <- confMat$table[4]
  precision = TP / (TP + FP)
  recall = TP / (TP + FN)
  F1_2 <- 2 * precision * recall / (precision + recall)
  expect_equal(F1_2,0.6666667, tolerance = 1e-3)

  # Add tests for the following metrics
  # expect_equal(#LowerCI, 0.5851154)
  # expect_equal(#UpperCI, 0.9380328)
  # expect_equal(#kappa, 0.4927536)
  # expect_equal(#prevalence, 0.4)
  # expect_equal(#detectionrate, 0.2333333)
  # expect_equal(#detectionprevalence, 0.3)
  # expect_equal(#balanceACC, 0.7361111)

})
