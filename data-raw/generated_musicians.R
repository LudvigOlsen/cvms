

#   ____________________________________________________________________________
#   Creating dataset                                                        ####

create_multinomial_dataset <- function(n_participants = 60){

  if (!is_wholenumber_(n_participants/4))
    stop("'n_participants' must be divisible with 4 (and be a wholenumber).")

  xpectr::set_test_seed(43)
  d <- tibble::tibble(
    "ID" = 1:n_participants,
    "Age" = round(runif(n_participants ,min = 16, max = 67)),
    "Class" = factor(rep(c("A","B","C","D"), each=n_participants/4)),
    "Height" = round(c(rnorm(n_participants/4, mean = 164, sd = 8),
                 rnorm(n_participants/4, mean = 171, sd = 12),
                 rnorm(n_participants/4, mean = 175, sd = 11),
                 rnorm(n_participants/4, mean = 179, sd = 10))),
    "Drums" = round(c(runif(n_participants/4, 0.3, 1),
                      runif(n_participants/4, 0.0, 0.7),
                      runif(n_participants/4, 0.2, 1),
                      runif(n_participants/4, 0.0, 0.8))),
    "Bass" = round(c(runif(n_participants/4, 0.0, 1),
                     runif(n_participants/4, 0.1, 1),
                     runif(n_participants/4, 0.2, 1),
                     runif(n_participants/4, 0.3, 1))),
    "Guitar" = round(c(runif(n_participants/4, 0.0, 0.7),
                       runif(n_participants/4, 0.1, 1),
                       runif(n_participants/4, 0.3, 1),
                       runif(n_participants/4, 0.0, 0.8))),
    "Keys" = round(c(runif(n_participants/4, 0.3, 1),
                     runif(n_participants/4, 0.2, 1),
                     runif(n_participants/4, 0.0, 0.7),
                     runif(n_participants/4, 0.0, 0.65))),
    "Vocals" = round(c(runif(n_participants/4, 0.3, 1),
                       runif(n_participants/4, 0.0, 0.7),
                       runif(n_participants/4, 0.2, 1),
                       runif(n_participants/4, 0.0, 0.8)))
  )

  d

}


#   ____________________________________________________________________________
#   Creating predicted probabilities dataset                                ####


get_probabilities <- function(df){

  xpectr::set_test_seed(43)

  df_folded <-
    df %>% groupdata2::fold(
      k = 5,
      cat_col = "Class",
      num_col = "Height",
      num_fold_cols = 3
    )

  model_formula <- "Class ~ Height + Age + Drums + Bass + Guitar + Keys + Vocals"

  svm_model_fn <- model_functions("svm_multinomial")
  svm_predict_fn <- predict_functions("svm_multinomial")
  svm_cv <- cross_validate_fn(
    df_folded,
    model_formula,
    fold_cols = paste0(".folds_", 1:3),
    model_fn = svm_model_fn,
    predict_fn = svm_predict_fn,
    hyperparameters = list("kernel" = "linear", "cost" = 10),
    type = "multinomial"
  )
  svm_predictions <- dplyr::bind_rows(svm_cv$Predictions) %>%
    dplyr::mutate(Classifier = "e1071_svm")

  rf_model_fn <- function(train_data, formula, hyperparameters) {
    randomForest::randomForest(
      formula = formula,
      data = train_data
    )
  }
  rf_predict_fn <- predict_functions("randomForest_multinomial")
  rf_cv <- cross_validate_fn(
    df_folded,
    model_formula,
    fold_cols = paste0(".folds_", 1:3),
    model_fn = rf_model_fn,
    predict_fn = rf_predict_fn,
    type = "multinomial"
  )
  rf_predictions <- dplyr::bind_rows(rf_cv$Predictions) %>%
    dplyr::mutate(Classifier = "randomForest")

  nnet_model_fn <- function(train_data, formula, hyperparameters) {
    nnet::multinom(
      formula = formula,
      data = train_data
    )
  }
  nnet_predict_fn <- predict_functions("nnet_multinom")
  nnet_cv <- cross_validate_fn(
    df_folded,
    model_formula,
    fold_cols = paste0(".folds_", 1:3),
    model_fn = nnet_model_fn,
    predict_fn = nnet_predict_fn,
    type = "multinomial"
  )
  nnet_predictions <- dplyr::bind_rows(nnet_cv$Predictions) %>%
    dplyr::mutate(Classifier = "nnet_multinom")

  dplyr::bind_rows(svm_predictions, rf_predictions, nnet_predictions) %>%
    dplyr::as_tibble() %>%
    dplyr::rename(ID = Observation) %>%
    dplyr::mutate(ID = factor(ID)) %>%
    tidyr::unnest(cols = "Prediction") %>%
    position_first(col = "Classifier")

}


#   ____________________________________________________________________________
#   Creating and saving datasets                                            ####

musicians <- create_multinomial_dataset(60)
predicted.musicians <- get_probabilities(musicians)

# usethis::use_data(musicians)
# usethis::use_data(predicted.musicians, overwrite = TRUE)
