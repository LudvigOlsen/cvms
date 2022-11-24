library(cvms)
context("metrics")

# Diagnosis by score

test_that("Metrics work for glm in validate()", {

  # skip_test_if_old_R_version()

  xpectr::set_test_seed(7)

  dat <- groupdata2::partition(participant.scores,
    p = 0.8,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  validated <- validate(
    train_data = dat, formulas = "diagnosis~score",
    partitions_col = ".partitions", family = "binomial",
    positive = 1
  )
  same_model <- glm(diagnosis ~ score, data = dat[dat$.partitions == 1, ], family = "binomial")

  train_data <- dat[dat$.partitions == 1, ]
  test_data <- dat[dat$.partitions == 2, ]
  prob <- predict(same_model, newdata = test_data, type = c("response"))

  test_data$prob <- prob
  test_data <- test_data %>%
    dplyr::mutate(pred = dplyr::if_else(prob > 0.5, 1, 0))

  # AUC
  g <- pROC::roc(diagnosis ~ prob, data = test_data, direction = "<", levels = c(0, 1))
  expect_equal(validated$AUC, as.numeric(g$auc))

  if (requireNamespace("AUC", quietly = TRUE)){
    auc2 <- AUC::auc(AUC::roc(test_data$prob, factor(test_data$diagnosis)))
    expect_equal(validated$AUC, auc2)
  }

  # Confusion Matrix Metrics

  conf_mat <- confusion_matrix(
    targets = test_data$diagnosis,
    predictions = test_data$pred,
    positive = levels(as.factor(test_data$diagnosis))[1],
    c_levels = levels(as.factor(train_data$diagnosis))
  )

  # Sensitivity
  expect_equal(validated$Sensitivity, conf_mat$Sensitivity)

  # Specificity
  expect_equal(validated$Specificity, conf_mat$Specificity)

  # posPredValue
  expect_equal(validated$`Pos Pred Value`, conf_mat$`Pos Pred Value`)

  # negPredValue
  expect_equal(validated$`Neg Pred Value`, conf_mat$`Neg Pred Value`)
})

test_that("Metrics work for glmer in validate()", {

  # skip_test_if_old_R_version()

  xpectr::set_test_seed(7)

  dat <- groupdata2::partition(participant.scores,
    p = 0.8,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  validated <- validate(
    train_data = dat, formulas = "diagnosis~score+(1|session)",
    partitions_col = ".partitions", family = "binomial",
    positive = 1
  )
  same_model <- lme4::glmer(diagnosis ~ score + (1 | session), data = dat[dat$.partitions == 1, ], family = "binomial")

  train_data <- dat[dat$.partitions == 1, ]
  test_data <- dat[dat$.partitions == 2, ]
  prob <- predict(same_model, newdata = test_data, type = c("response"))

  test_data$prob <- prob
  test_data <- test_data %>%
    dplyr::mutate(pred = dplyr::if_else(prob > 0.5, 1, 0))

  # AUC
  auc1 <- pROC::roc(diagnosis ~ prob, data = test_data, levels = c(0, 1), direction = "<")
  expect_equal(validated$AUC, as.numeric(auc1$auc))

  if (requireNamespace("AUC", quietly = TRUE)){
    auc2 <- AUC::auc(AUC::roc(test_data$prob, factor(test_data$diagnosis)))
    expect_equal(validated$AUC, auc2)
  }


  # Confusion Matrix metrics

  conf_mat <- confusion_matrix(
    targets = test_data$diagnosis,
    predictions = test_data$pred,
    positive = levels(as.factor(test_data$diagnosis))[1],
    c_levels = levels(as.factor(train_data$diagnosis))
  )

  # Sensitivity
  expect_equal(validated$Sensitivity, conf_mat$Sensitivity)

  # Specificity
  expect_equal(validated$Specificity, conf_mat$Specificity)

  # posPredValue
  expect_equal(validated$`Pos Pred Value`, conf_mat$`Pos Pred Value`)

  # negPredValue
  expect_equal(validated$`Neg Pred Value`, conf_mat$`Neg Pred Value`)
})

# Diagnosis by age

test_that("Metrics work for glm in validate()", {

  # skip_test_if_old_R_version()

  xpectr::set_test_seed(6)

  dat <- groupdata2::partition(participant.scores,
    p = 0.8,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  validated <- validate(
    train_data = dat, formulas = "diagnosis~age",
    partitions_col = ".partitions", family = "binomial",
    positive = 1
  )
  same_model <- glm(diagnosis ~ age, data = dat[dat$.partitions == 1, ], family = "binomial")

  train_data <- dat[dat$.partitions == 1, ]
  test_data <- dat[dat$.partitions == 2, ]
  prob <- predict(same_model, newdata = test_data, type = c("response"))

  test_data$prob <- prob
  test_data <- test_data %>%
    dplyr::mutate(pred = dplyr::if_else(prob > 0.5, 1, 0))

  # AUC
  g <- pROC::roc(diagnosis ~ prob,
    data = test_data,
    direction = "<", levels = c(0, 1)
  )
  expect_equal(validated$AUC, as.numeric(g$auc))

  if (requireNamespace("AUC", quietly = TRUE)){
    roc_ <- AUC::roc(test_data$prob, factor(test_data$diagnosis))
    auc2 <- AUC::auc(AUC::roc(test_data$prob, factor(test_data$diagnosis)))
    expect_equal(validated$AUC, auc2) # TODO What is the actual underlying error here?
  }

  # Confusion matrix metrics

  conf_mat <- confusion_matrix(
    targets = test_data$diagnosis,
    predictions = test_data$pred,
    positive = levels(as.factor(test_data$diagnosis))[1],
    c_levels = levels(as.factor(train_data$diagnosis))
  )

  # Sensitivity
  expect_equal(validated$Sensitivity, conf_mat$Sensitivity)

  # Specificity
  expect_equal(validated$Specificity, conf_mat$Specificity)

  # posPredValue
  expect_equal(validated$`Pos Pred Value`, conf_mat$`Pos Pred Value`)

  # negPredValue
  expect_equal(validated$`Neg Pred Value`, conf_mat$`Neg Pred Value`)
})

test_that("Metrics work for glmer in validate()", {

  # skip_test_if_old_R_version()

  xpectr::set_test_seed(201)

  dat <- groupdata2::partition(participant.scores,
    p = 0.8,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  validated <- validate(
    train_data = dat, formulas = "diagnosis~age+(1|session)",
    partitions_col = ".partitions", family = "binomial",
    positive = 1
  )
  same_model <- lme4::glmer(diagnosis ~ age + (1 | session),
    data = dat[dat$.partitions == 1, ], family = "binomial"
  )

  train_data <- dat[dat$.partitions == 1, ]
  test_data <- dat[dat$.partitions == 2, ]
  prob <- predict(same_model, newdata = test_data, type = c("response"))

  test_data$prob <- prob
  test_data <- test_data %>%
    dplyr::mutate(pred = dplyr::if_else(prob > 0.5, 1, 0))

  # AUC
  auc1 <- pROC::roc(diagnosis ~ prob, data = test_data, direction = "<", levels = c(0, 1))
  expect_equal(validated$AUC, as.numeric(auc1$auc))

  if (requireNamespace("AUC", quietly = TRUE)){
    auc2 <- AUC::auc(AUC::roc(
      test_data$prob,
      factor(test_data$diagnosis, levels = levels(as.factor(train_data$diagnosis)))
    ))
    expect_equal(validated$AUC, auc2)
  }

  # Confusion matrix metrics

  conf_mat <- confusion_matrix(
    targets = test_data$diagnosis,
    predictions = test_data$pred,
    positive = levels(as.factor(test_data$diagnosis))[1],
    c_levels = levels(as.factor(train_data$diagnosis))
  )

  # Sensitivity
  expect_equal(validated$Sensitivity, conf_mat$Sensitivity)

  # Specificity
  expect_equal(validated$Specificity, conf_mat$Specificity)

  # posPredValue
  expect_equal(validated$`Pos Pred Value`, conf_mat$`Pos Pred Value`)

  # negPredValue
  expect_equal(validated$`Neg Pred Value`, conf_mat$`Neg Pred Value`)
})

test_that("Metrics work when 0 is positive class for glmer in validate()", {

  # skip_test_if_old_R_version()

  # AUC approach was improved from this answer: https://stats.stackexchange.com/a/269577
  # Here I test that it works.

  # First we will check what should be the behavior, when changing positive to 0.
  participant.scores$perfect_predicted_probability <- c(
    0.8, 0.9, 0.7, 0.3, 0.2, 0.1,
    0.8, 0.7, 0.7, 0.1, 0.4, 0.3,
    0.8, 0.9, 0.7, 0.8, 0.7,
    0.7, 0.7, 0.9, 0.8, 0.8,
    0.7, 0.95, 0.3, 0.2, 0.1,
    0.4, 0.25, 0.2
  )

  participant.scores$few_false_negs_predicted_probability <- c(
    0.2, 0.3, 0.4, 0.3, 0.2, 0.1,
    0.8, 0.7, 0.7, 0.1, 0.4, 0.3,
    0.8, 0.9, 0.7, 0.8, 0.7,
    0.7, 0.7, 0.9, 0.8, 0.8,
    0.7, 0.95, 0.3, 0.2, 0.1,
    0.4, 0.25, 0.2
  )

  participant.scores$few_false_pos_predicted_probability <- c(
    0.8, 0.9, 0.7, 0.7, 0.9, 0.6,
    0.8, 0.7, 0.7, 0.1, 0.4, 0.3,
    0.8, 0.9, 0.7, 0.8, 0.7,
    0.7, 0.7, 0.9, 0.8, 0.8,
    0.7, 0.95, 0.3, 0.2, 0.1,
    0.4, 0.25, 0.2
  )

  participant.scores$worst_predicted_probability <- 1 - c(
    0.8, 0.9, 0.7, 0.3, 0.2, 0.1,
    0.8, 0.7, 0.7, 0.1, 0.4, 0.3,
    0.8, 0.9, 0.7, 0.8, 0.7,
    0.7, 0.7, 0.9, 0.8, 0.8,
    0.7, 0.95, 0.3, 0.2, 0.1,
    0.4, 0.25, 0.2
  )

  # AUC (positive = 1 vs positive = 0)

  # PERFECT

  # With AUC::
  if (requireNamespace("AUC", quietly = TRUE)){
    AUC_auc_perfect <- AUC::auc(AUC::roc(
      participant.scores$perfect_predicted_probability,
      factor(participant.scores$diagnosis)
    ))
    AUC_auc_perfect_pos0 <- AUC::auc(AUC::roc(
      1 - participant.scores$perfect_predicted_probability,
      factor(1 - participant.scores$diagnosis)
    ))

    expect_equal(AUC_auc_perfect, AUC_auc_perfect_pos0)

    # With pROC
    pROC_auc_perfect <- as.numeric(pROC::roc(
      response = participant.scores$diagnosis,
      predictor = participant.scores$perfect_predicted_probability,
      direction = "<", levels = c(0, 1)
    )$auc)

    pROC_auc_perfect_pos0 <- as.numeric(pROC::roc(
      response = 1 - participant.scores$diagnosis,
      predictor = 1 - participant.scores$perfect_predicted_probability,
      direction = ">", levels = c(1, 0)
    )$auc)

    expect_equal(pROC_auc_perfect, pROC_auc_perfect_pos0)
    expect_equal(pROC_auc_perfect, AUC_auc_perfect)
    expect_equal(AUC_auc_perfect_pos0, pROC_auc_perfect_pos0)

  }

  # FALSE NEGATIVES

  # With AUC

  if (requireNamespace("AUC", quietly = TRUE)){

    AUC_auc_false_negs <- AUC::auc(AUC::roc(
      participant.scores$few_false_negs_predicted_probability,
      factor(participant.scores$diagnosis)
    ))

    AUC_auc_false_negs_pos0 <- AUC::auc(AUC::roc(
      1 - participant.scores$few_false_negs_predicted_probability,
      factor(1 - participant.scores$diagnosis)
    ))

    expect_equal(AUC_auc_false_negs, AUC_auc_false_negs_pos0)


    # With pROC
    pROC_auc_false_negs <- as.numeric(pROC::roc(
      response = participant.scores$diagnosis,
      predictor = participant.scores$few_false_negs_predicted_probability,
      direction = "<", levels = c(0, 1)
    )$auc)

    pROC_auc_false_negs_pos0 <- as.numeric(pROC::roc(
      response = 1 - participant.scores$diagnosis,
      predictor = 1 - participant.scores$few_false_negs_predicted_probability,
      direction = ">", levels = c(1, 0)
    )$auc)

    expect_equal(pROC_auc_false_negs, pROC_auc_false_negs_pos0)
    expect_equal(pROC_auc_false_negs, AUC_auc_false_negs)
    expect_equal(AUC_auc_false_negs_pos0, pROC_auc_false_negs_pos0)

  }

  # FALSE POSITIVES

  # With AUC

  if (requireNamespace("AUC", quietly = TRUE)){

    AUC_auc_false_pos <- AUC::auc(AUC::roc(
      participant.scores$few_false_pos_predicted_probability,
      factor(participant.scores$diagnosis)
    ))

    AUC_auc_false_pos_pos0 <- AUC::auc(AUC::roc(
      1 - participant.scores$few_false_pos_predicted_probability,
      factor(1 - participant.scores$diagnosis)
    ))

    expect_equal(AUC_auc_false_pos, AUC_auc_false_pos_pos0)

    # With pROC
    pROC_auc_false_pos <- as.numeric(pROC::roc(
      response = participant.scores$diagnosis,
      predictor = participant.scores$few_false_pos_predicted_probability,
      direction = "<", levels = c(0, 1)
    )$auc)

    pROC_auc_false_pos_pos0 <- as.numeric(pROC::roc(
      response = 1 - participant.scores$diagnosis,
      predictor = 1 - participant.scores$few_false_pos_predicted_probability,
      direction = ">", levels = c(1, 0)
    )$auc)

    expect_equal(pROC_auc_false_pos, pROC_auc_false_pos_pos0)
    expect_equal(pROC_auc_false_pos, AUC_auc_false_pos)
    expect_equal(AUC_auc_false_pos_pos0, pROC_auc_false_pos_pos0)

  }

  # ALL WRONG

  # With AUC

  if (requireNamespace("AUC", quietly = TRUE)){

    AUC_auc_worst <- AUC::auc(AUC::roc(
      participant.scores$worst_predicted_probability,
      factor(participant.scores$diagnosis)
    ))

    AUC_auc_worst_pos0 <- AUC::auc(AUC::roc(
      1 - participant.scores$worst_predicted_probability,
      factor(1 - participant.scores$diagnosis)
    ))

    expect_equal(AUC_auc_worst, AUC_auc_worst_pos0)

  }

  # With pROC
  pROC_auc_worst <- as.numeric(pROC::roc(
    response = participant.scores$diagnosis,
    predictor = participant.scores$worst_predicted_probability,
    direction = "<", levels = c(0, 1)
  )$auc)

  pROC_auc_worst_pos0 <- as.numeric(pROC::roc(
    response = 1 - participant.scores$diagnosis,
    predictor = 1 - participant.scores$worst_predicted_probability,
    direction = ">", levels = c(1, 0)
  )$auc)

  expect_equal(pROC_auc_worst, pROC_auc_worst_pos0)
  expect_equal(pROC_auc_worst, AUC_auc_worst)
  expect_equal(AUC_auc_worst_pos0, pROC_auc_worst_pos0)

  xpectr::set_test_seed(201)

  dat <- groupdata2::partition(participant.scores,
    p = 0.8,
    cat_col = "diagnosis",
    id_col = "participant",
    list_out = FALSE
  )

  validated_pos1 <- validate(
    train_data = dat, formulas = "diagnosis~score",
    partitions_col = ".partitions", family = "binomial",
    positive = 2
  )

  validated_pos0 <- validate(
    train_data = dat, formulas = "diagnosis~score",
    partitions_col = ".partitions", family = "binomial",
    positive = 1
  )

  expect_equal(validated_pos1$AUC, validated_pos0$AUC)

  validated_pos1 <- validate(
    train_data = dat, formulas = "diagnosis~age",
    partitions_col = ".partitions", family = "binomial",
    positive = 2
  )

  validated_pos0 <- validate(
    train_data = dat, formulas = "diagnosis~age",
    partitions_col = ".partitions", family = "binomial",
    positive = 1
  )

  expect_equal(validated_pos1$AUC, validated_pos0$AUC)

  # If dependent variable is character factor

  dat$diagnosis_chr <- factor(ifelse(dat$diagnosis == 0, "a", "b"))

  validated_pos1_num <- validate(
    train_data = dat, formulas = "diagnosis_chr~age",
    partitions_col = ".partitions", family = "binomial",
    positive = 2
  )
  validated_pos1_chr <- validate(
    train_data = dat, formulas = "diagnosis_chr~age",
    partitions_col = ".partitions", family = "binomial",
    positive = "b"
  )

  expect_equal(validated_pos1_num$AUC, validated_pos1_chr$AUC)

  validated_pos0_num <- validate(
    train_data = dat, formulas = "diagnosis_chr~age",
    partitions_col = ".partitions", family = "binomial",
    positive = 1
  )
  validated_pos0_chr <- validate(
    train_data = dat, formulas = "diagnosis_chr~age",
    partitions_col = ".partitions", family = "binomial",
    positive = "a"
  )

  expect_equal(validated_pos0_num$AUC, validated_pos0_chr$AUC)

  expect_equal(validated_pos0_num$AUC, validated_pos1_num$AUC)
  expect_equal(validated_pos0_chr$AUC, validated_pos1_chr$AUC)
})

test_that("AUC works", {

  # skip_test_if_old_R_version()

  #
  # In this test I printed the predictions within each training loop
  # and manually copied the predictions
  # I did this to ensure that cross_validate gathers the predictions correctly before
  # calculating its metrics. This is incredibly important.
  # Metrics are calculated and compared to the metrics I got from cross_validate.
  #

  target <- c(
    0, 0, 0, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 1, 1, 1
  )
  predictions_prob <- c(
    0.77379615, 0.36952324, 0.09125579, 0.89205819,
    0.73620142, 0.55282759, 0.8307928, 0.6042899,
    0.1754574, 0.9317034, 0.8307928, 0.5145979,
    0.9269098, 0.6874739, 0.5867096, 0.71867985,
    0.26746773, 0.09346533, 0.85976827, 0.24884534,
    0.13205012, 0.6503171, 0.4541755, 0.1564246,
    0.8445872, 0.7085838, 0.5871876, 0.8514956,
    0.7607141, 0.7085838
  )
  predictions <- dplyr::if_else(predictions_prob > 0.5, 1, 0)

  pred_df <- data.frame("obs" = target, "prob" = predictions_prob, "pred" = predictions)

  # AUC
  auc1 <- pROC::roc(obs ~ prob, data = pred_df, direction = "<", levels = c(0, 1))
  expect_equal(as.numeric(auc1$auc), 0.7615741, tolerance = 1e-3)

  if (requireNamespace("AUC", quietly = TRUE)){
    auc2 <- AUC::auc(AUC::roc(pred_df$prob, factor(pred_df$obs)))
    expect_equal(auc2, 0.7615741, tolerance = 1e-3)
  }

})

test_that("Metrics work in confusion_matrix()", {

  # skip_test_if_old_R_version()

  #
  # In this test I printed the predictions within each training loop
  # and manually copied the predictions
  # I did this to ensure that cross_validate gathers the predictions correctly before
  # calculating its metrics. This is incredibly important.
  # Metrics are calculated and compared to the metrics I got from cross_validate.
  #

  target <- c(
    0, 0, 0, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 1, 1, 1,
    0, 0, 0, 1, 1, 1,
    0, 0, 0, 1, 1, 1, 1, 1, 1
  )
  predictions_prob <- c(
    0.77379615, 0.36952324, 0.09125579, 0.89205819,
    0.73620142, 0.55282759, 0.8307928, 0.6042899,
    0.1754574, 0.9317034, 0.8307928, 0.5145979,
    0.9269098, 0.6874739, 0.5867096, 0.71867985,
    0.26746773, 0.09346533, 0.85976827, 0.24884534,
    0.13205012, 0.6503171, 0.4541755, 0.1564246,
    0.8445872, 0.7085838, 0.5871876, 0.8514956,
    0.7607141, 0.7085838
  )
  predictions <- dplyr::if_else(predictions_prob > 0.5, 1, 0)

  pred_df <- data.frame("obs" = target, "prob" = predictions_prob, "pred" = predictions)

  # Confusion matrix metrics

  conf_mat <- confusion_matrix(
    targets = pred_df$obs,
    predictions = pred_df$pred,
    positive = levels(as.factor(pred_df$obs))[1],
    c_levels = levels(as.factor(pred_df$obs)),
    metrics = "all"
  )


  ## Testing 'conf_mat'                                                     ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(conf_mat),
    c("cfm_results", "cfm_binomial", "tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    conf_mat[["Positive Class"]],
    "0",
    fixed = TRUE)
  expect_equal(
    conf_mat[["Balanced Accuracy"]],
    0.73611,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["Accuracy"]],
    0.76667,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["F1"]],
    0.66667,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["Sensitivity"]],
    0.58333,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["Specificity"]],
    0.88889,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["Pos Pred Value"]],
    0.77778,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["Neg Pred Value"]],
    0.7619,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["Kappa"]],
    0.49275,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["MCC"]],
    0.50483,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["Detection Rate"]],
    0.23333,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["Detection Prevalence"]],
    0.3,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["Prevalence"]],
    0.4,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["False Neg Rate"]],
    0.41667,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["False Pos Rate"]],
    0.11111,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["False Discovery Rate"]],
    0.22222,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["False Omission Rate"]],
    0.2381,
    tolerance = 1e-4)
  expect_equal(
    conf_mat[["Threat Score"]],
    0.5,
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(conf_mat),
    c("Confusion Matrix", "Table", "Positive Class", "Balanced Accuracy",
      "Accuracy", "F1", "Sensitivity", "Specificity", "Pos Pred Value",
      "Neg Pred Value", "Kappa", "MCC", "Detection Rate", "Detection Prevalence",
      "Prevalence", "False Neg Rate", "False Pos Rate", "False Discovery Rate",
      "False Omission Rate", "Threat Score"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(conf_mat),
    c("list", "list", "character", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(conf_mat),
    c("list", "list", "character", "double", "double", "double", "double",
      "double", "double", "double", "double", "double", "double",
      "double", "double", "double", "double", "double", "double",
      "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(conf_mat),
    c(1L, 20L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(conf_mat)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'conf_mat'                                            ####

  # F1
  F1 <- (2 * conf_mat$`Pos Pred Value` * conf_mat$Sensitivity) / (conf_mat$`Pos Pred Value` + conf_mat$Sensitivity)
  expect_equal(F1, 0.6666667, tolerance = 1e-5)
  expect_equal(conf_mat$F1, F1, tolerance = 1e-5)

  confMatTable <- conf_mat[["Table"]][[1]]
  # Confusion matrix
  TP <- confMatTable[1] # Dependent on positive = 0 ?
  FP <- confMatTable[3]
  FN <- confMatTable[2]
  TN <- confMatTable[4]

  ppv <- TP / (TP + FP)
  expect_equal(ppv, conf_mat$`Pos Pred Value`[[1]], tolerance = 1e-5)
  expect_equal(ppv, 0.7777778, tolerance = 1e-5)

  npv <- TN / (TN + FN)
  expect_equal(npv, conf_mat$`Neg Pred Value`[[1]], tolerance = 1e-5)
  expect_equal(npv, 0.7619048, tolerance = 1e-5)

  sensitivity <- TP / (TP + FN)
  expect_equal(sensitivity, conf_mat$Sensitivity[[1]], tolerance = 1e-5)
  expect_equal(sensitivity, 0.5833333, tolerance = 1e-5)

  specificity <- TN / (TN + FP)
  expect_equal(specificity, conf_mat$Specificity[[1]], tolerance = 1e-5)
  expect_equal(specificity, 0.8888889, tolerance = 1e-5)

  acc <- (TP + TN) / (TP + TN + FP + FN)
  expect_equal(acc, conf_mat$Accuracy[[1]], tolerance = 1e-5)
  expect_equal(acc, 0.7666667, tolerance = 1e-5)

  F1_2 <- 2 * ppv * sensitivity / (ppv + sensitivity)
  expect_equal(F1_2, conf_mat$F1[[1]], tolerance = 1e-5)
  expect_equal(F1_2, 0.6666667, tolerance = 1e-5)

  bal_acc <- (sensitivity + specificity) / 2
  expect_equal(bal_acc, conf_mat$`Balanced Accuracy`[[1]], tolerance = 1e-5)
  expect_equal(bal_acc, 0.7361111, tolerance = 1e-5)

  bal_acc <- (sensitivity + specificity) / 2
  expect_equal(bal_acc, conf_mat$`Balanced Accuracy`[[1]], tolerance = 1e-5)
  expect_equal(bal_acc, 0.7361111, tolerance = 1e-5)

  p_observed <- TP + TN
  p_expected <- (((TP + FP)*(TP + FN))+((FN+TN)*(FP+TN))) / (TP + TN + FP + FN)
  kappa <- (p_observed - p_expected) / ((TP + TN + FP + FN)-p_expected)
  expect_equal(kappa, conf_mat$Kappa[[1]], tolerance = 1e-5)
  expect_equal(kappa, 0.4927536, tolerance = 1e-5)

  mcc <- ((TP*TN) - (FP*FN))/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
  expect_equal(mcc, conf_mat$MCC[[1]], tolerance = 1e-5)
  expect_equal(mcc, 0.5048268, tolerance = 1e-5)

  detection_rate <- TP / (TP + TN + FP + FN)
  expect_equal(detection_rate, conf_mat$`Detection Rate`[[1]], tolerance = 1e-5)
  expect_equal(detection_rate, 0.2333333, tolerance = 1e-5)

  detection_prevalence <- (TP + FP) / (TP + TN + FP + FN)
  expect_equal(detection_prevalence, conf_mat$`Detection Prevalence`[[1]], tolerance = 1e-5)
  expect_equal(detection_prevalence, 0.3, tolerance = 1e-5)

  prevalence <- (TP + FN) / (TP + TN + FP + FN)
  expect_equal(prevalence, conf_mat$Prevalence[[1]], tolerance = 1e-5)
  expect_equal(prevalence, 0.4, tolerance = 1e-5)

  fnr <- FN / (FN + TP)
  expect_equal(fnr, conf_mat$`False Neg Rate`[[1]], tolerance = 1e-5)
  expect_equal(fnr, 0.4166667, tolerance = 1e-5)

  fpr <- FP / (TN + FP)
  expect_equal(fpr, conf_mat$`False Pos Rate`[[1]], tolerance = 1e-5)
  expect_equal(fpr, 0.1111111, tolerance = 1e-5)

  fdr <- FP / (FP + TP)
  expect_equal(fdr, conf_mat$`False Discovery Rate`[[1]], tolerance = 1e-5)
  expect_equal(fdr, 0.22222, tolerance = 1e-5)

  f_omission_rate <- FN / (FN + TN)
  expect_equal(f_omission_rate, conf_mat$`False Omission Rate`[[1]], tolerance = 1e-5)
  expect_equal(f_omission_rate, 0.2380952, tolerance = 1e-5)

  threat_score <- TP / (TP + FN + FP)
  expect_equal(threat_score, conf_mat$`Threat Score`[[1]], tolerance = 1e-5)
  expect_equal(threat_score, 0.5, tolerance = 1e-5)

  # Test that MCC does not care about what class if positive
  expect_equal(
    mcc(list("TP" = TP, "FP" = FP, "FN" = FN, "TN" = TN)),
    mcc(list("TP" = TN, "FP" = FN, "FN" = FP, "TN" = TP))
  )


  # Multiclass MCC
  xpectr::set_test_seed(1)
  mc_preds <- factor(sample(c(1,2,3,4), 100, replace = TRUE))
  mc_targs_random <- factor(sample(c(1,2,3,4), 100, replace = TRUE))
  mc_targs_good <- factor(ifelse(runif(100) < 0.7, mc_preds, mc_targs_random))

  mcc_cmf <- confusion_matrix(targets = mc_targs_good, predictions = mc_preds, do_one_vs_all = FALSE)
  # yardstick_mcc <- yardstick::mcc_vec(mc_targs_good, mc_preds) # 0.759631087897275
  expect_equal(mcc_cmf$MCC, 0.759631087897275, tolerance = 1e-8)

  mcc_cmf <- confusion_matrix(targets = mc_targs_random, predictions = mc_preds, do_one_vs_all = FALSE)
  # yardstick_mcc <- yardstick::mcc_vec(mc_targs_random, mc_preds) # 0.0153721822602552
  expect_equal(mcc_cmf$MCC, 0.0153721822602552, tolerance = 1e-8)


})

test_that("evaluate_residuals() metrics work", {

  # skip_test_if_old_R_version()

  # Normal distribution
  xpectr::set_test_seed(6)
  targets <- rnorm(100)
  preds <- rnorm(100)
  df <- data.frame(t = targets, p = preds)
  results <- evaluate_residuals(df, prediction_col = "p", target_col = "t", metrics = "all")


  ## Testing 'results'                                                      ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(results),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    results[["RMSE"]],
    1.23924,
    tolerance = 1e-4)
  expect_equal(
    results[["MAE"]],
    0.98881,
    tolerance = 1e-4)
  expect_equal(
    results[["NRMSE(RNG)"]],
    0.27174,
    tolerance = 1e-4)
  expect_equal(
    results[["NRMSE(IQR)"]],
    0.96402,
    tolerance = 1e-4)
  expect_equal(
    results[["NRMSE(STD)"]],
    1.1991,
    tolerance = 1e-4)
  expect_equal(
    results[["NRMSE(AVG)"]],
    -121.41375,
    tolerance = 1e-4)
  expect_equal(
    results[["RMSLE"]],
    NaN,
    tolerance = 1e-4)
  expect_equal(
    results[["MALE"]],
    NaN,
    tolerance = 1e-4)
  expect_equal(
    results[["RAE"]],
    1.20507,
    tolerance = 1e-4)
  expect_equal(
    results[["RSE"]],
    1.45236,
    tolerance = 1e-4)
  expect_equal(
    results[["RRSE"]],
    1.20514,
    tolerance = 1e-4)
  expect_equal(
    results[["MAPE"]],
    4.7236,
    tolerance = 1e-4)
  expect_equal(
    results[["MSE"]],
    1.53572,
    tolerance = 1e-4)
  expect_equal(
    results[["TAE"]],
    98.88096,
    tolerance = 1e-4)
  expect_equal(
    results[["TSE"]],
    153.5717,
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(results),
    c("RMSE", "MAE", "NRMSE(RNG)", "NRMSE(IQR)", "NRMSE(STD)", "NRMSE(AVG)",
      "RSE", "RRSE", "RAE", "RMSLE", "MALE", "MAPE", "MSE", "TAE",
      "TSE"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(results),
    c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
      "numeric", "numeric", "numeric"),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(results),
    c("double", "double", "double", "double", "double", "double", "double",
      "double", "double", "double", "double", "double", "double",
      "double", "double"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(results),
    c(1L, 15L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(results)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'results'                                             ####

  # Manual calculation

  resids <- function(targets, preds, log=FALSE){
    if (isTRUE(log)) xpectr::suppress_mw(err <- log(preds + 1) - log(targets + 1))
    else err <- preds - targets
    err
  }

  centered_targets <- function(targets){
    targets - mean(targets)
  }

  rmse_ <- function(targets, preds, log=FALSE) {
    err <- resids(targets, preds, log)
    sqrt(mean(err^2))
  }

  mae_ <- function(targets, preds, log=FALSE) {
    err <- resids(targets, preds, log)
    mean(abs(err))
  }

  nrmse_ <- function(targets, preds, by){
    rms <- rmse_(targets, preds)
    if (by == "iqr") div <- IQR(targets)
    else if (by == "sd") div <- sd(targets)
    else if (by == "avg") div <- mean(targets)
    else if (by == "rng") div <- max(targets)-min(targets)
    rms / div
  }

  rae_ <- function(targets, preds){
    err <- sum(abs(resids(targets, preds)))
    cent <- sum(abs(centered_targets(targets)))
    err/cent
  }

  rse_ <- function(targets, preds){
    err <- sum(resids(targets, preds)^2)
    cent <- sum(centered_targets(targets)^2)
    err/cent
  }

  mape_ <- function(targets, preds){
    err <- resids(targets, preds)
    mean(abs(err/targets))
  }

  # RMSE
  expect_equal(results$RMSE, rmse_(targets, preds), tolerance = 1e-3)
  expect_equal(results$RMSE, 1.23924, tolerance = 1e-3)

  # RMSLE
  expect_equal(results$RMSLE, rmse_(targets, preds, log = TRUE), tolerance = 1e-3)
  expect_equal(results$RMSLE, NaN, tolerance = 1e-3)

  # MAE
  expect_equal(results$MAE, mae_(targets, preds), tolerance = 1e-3)
  expect_equal(results$MAE, 0.9888096, tolerance = 1e-3)

  # MALE
  expect_equal(results$MALE, mae_(targets, preds, log = TRUE), tolerance = 1e-3)
  expect_equal(results$MALE, NaN, tolerance = 1e-3)

  # NRMSE
  expect_equal(results$`NRMSE(RNG)`, nrmse_(targets, preds, by = "rng"), tolerance = 1e-3)
  expect_equal(results$`NRMSE(RNG)`, 0.271736645098678, tolerance = 1e-3)
  expect_equal(results$`NRMSE(IQR)`, nrmse_(targets, preds, by = "iqr"), tolerance = 1e-3)
  expect_equal(results$`NRMSE(IQR)`, 0.964022899327126, tolerance = 1e-3)
  expect_equal(results$`NRMSE(STD)`, nrmse_(targets, preds, by = "sd"), tolerance = 1e-3)
  expect_equal(results$`NRMSE(STD)`, 1.19909776380955, tolerance = 1e-3)
  expect_equal(results$`NRMSE(AVG)`, nrmse_(targets, preds, by = "avg"), tolerance = 1e-3)
  expect_equal(results$`NRMSE(AVG)`, -121.413747175841, tolerance = 1e-3)

  # RAE
  expect_equal(results$RAE, rae_(targets, preds), tolerance = 1e-3)
  expect_equal(results$RAE, 1.2050715889456, tolerance = 1e-3)

  # RSE
  expect_equal(results$RSE, rse_(targets, preds), tolerance = 1e-3)
  expect_equal(results$RSE, 1.45235903754855, tolerance = 1e-3)

  # RRSE
  expect_equal(results$RRSE, sqrt(rse_(targets, preds)), tolerance = 1e-3)
  expect_equal(results$RRSE, 1.20513859682136, tolerance = 1e-3)

  # MAPE
  expect_equal(results$MAPE, mape_(targets, preds), tolerance = 1e-3)
  expect_equal(results$MAPE, 4.72360030788065, tolerance = 1e-3)

  # MSE
  expect_equal(results$MSE, rmse_(targets, preds)^2, tolerance = 1e-3)
  expect_equal(results$MSE, 1.53571701341794, tolerance = 1e-3)

  # TAE
  expect_equal(results$TAE, sum(abs(resids(targets, preds))), tolerance = 1e-3)
  expect_equal(results$TAE, 98.880955884436, tolerance = 1e-3)

  # TSE
  expect_equal(results$TSE, sum(resids(targets, preds)^2), tolerance = 1e-3)
  expect_equal(results$TSE, 153.571701341794, tolerance = 1e-3)


  # Uniform distribution
  xpectr::set_test_seed(9)
  targets <- runif(100, min = 45, max = 97)
  preds <- runif(100, min = 54, max = 120)
  df <- data.frame(t = targets, p = preds)
  results <- evaluate_residuals(df, prediction_col = "p",
                                target_col = "t", metrics = "all")

  # RMSE
  expect_equal(results$RMSE, rmse_(targets, preds), tolerance = 1e-3)
  expect_equal(results$RMSE, 30.2487016310356, tolerance = 1e-3)

  # RMSLE
  expect_equal(results$RMSLE, rmse_(targets, preds, log = TRUE), tolerance = 1e-3)
  expect_equal(results$RMSLE, 0.381933438597387, tolerance = 1e-3)

  # MAE
  expect_equal(results$MAE, mae_(targets, preds), tolerance = 1e-3)
  expect_equal(results$MAE, 24.3477034755331, tolerance = 1e-3)

  # MALE
  expect_equal(results$MALE, mae_(targets, preds, log = TRUE), tolerance = 1e-3)
  expect_equal(results$MALE, 0.309460458578487, tolerance = 1e-3)

  # NRMSE
  expect_equal(results$`NRMSE(RNG)`, nrmse_(targets, preds, by = "rng"), tolerance = 1e-3)
  expect_equal(results$`NRMSE(RNG)`, 0.585117778308063, tolerance = 1e-3)
  expect_equal(results$`NRMSE(IQR)`, nrmse_(targets, preds, by = "iqr"), tolerance = 1e-3)
  expect_equal(results$`NRMSE(IQR)`, 1.05622503346585, tolerance = 1e-3)
  expect_equal(results$`NRMSE(STD)`, nrmse_(targets, preds, by = "sd"), tolerance = 1e-3)
  expect_equal(results$`NRMSE(STD)`, 1.90845876022934, tolerance = 1e-3)
  expect_equal(results$`NRMSE(AVG)`, nrmse_(targets, preds, by = "avg"), tolerance = 1e-3)
  expect_equal(results$`NRMSE(AVG)`, 0.428886643444635, tolerance = 1e-3)

  # RAE
  expect_equal(results$RAE, rae_(targets, preds), tolerance = 1e-3)
  expect_equal(results$RAE, 1.76907453491764, tolerance = 1e-3)

  # RSE
  expect_equal(results$RSE, rse_(targets, preds), tolerance = 1e-3)
  expect_equal(results$RSE, 3.67900488837992, tolerance = 1e-3)

  # RRSE
  expect_equal(results$RRSE, sqrt(rse_(targets, preds)), tolerance = 1e-3)
  expect_equal(results$RRSE, 1.9180732228932, tolerance = 1e-3)

  # MAPE
  expect_equal(results$MAPE, mape_(targets, preds), tolerance = 1e-3)
  expect_equal(results$MAPE, 0.390727375837037, tolerance = 1e-3)

  # MSE
  expect_equal(results$MSE, rmse_(targets, preds)^2, tolerance = 1e-3)
  expect_equal(results$MSE, 914.983950363419, tolerance = 1e-3)

  # TAE
  expect_equal(results$TAE, sum(abs(resids(targets, preds))), tolerance = 1e-3)
  expect_equal(results$TAE, 2434.77034755331, tolerance = 1e-3)

  # TSE
  expect_equal(results$TSE, sum(resids(targets, preds)^2), tolerance = 1e-3)
  expect_equal(results$TSE, 91498.3950363419, tolerance = 1e-3)


})
