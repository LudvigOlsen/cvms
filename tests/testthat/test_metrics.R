library(cvms)
context("metrics")

# Diagnosis by score

test_that("Metrics work for glm in validate()",{

  set.seed(7)

  dat <- groupdata2::partition(participant.scores, p = 0.8,
                               cat_col = 'diagnosis',
                               id_col = 'participant',
                               list_out = FALSE)


  validated <- validate(train_data=dat, model="diagnosis~score", partitions_col = '.partitions', family = 'binomial')
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

  validated <- validate(train_data=dat, model="diagnosis~score+(1|session)", partitions_col = '.partitions', family = 'binomial')
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


  validated <- validate(train_data=dat, model="diagnosis~age", partitions_col = '.partitions', family = 'binomial')
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

  validated <- validate(train_data=dat, model="diagnosis~age+(1|session)", partitions_col = '.partitions', family = 'binomial')
  same_model <- lme4::glmer(diagnosis~age+(1|session), data=dat[dat$.partitions==1,], family = 'binomial')

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
