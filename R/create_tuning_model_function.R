
# TODO cross_validate_fn needs to pass fold() relevant columns as well?
create_tuning_model_fn <- function(
    type,
    model_fn,
    predict_fn,
    hyperparameters,
    fold_args, # list
    preprocess_fn = NULL,
    preprocess_once = FALSE,
    cutoff = 0.5,
    positive = 2,
    metrics = list(),
    rm_nc = FALSE,
    parallel = FALSE,
    verbose = FALSE,
){


  model_fn <- function(train_data, formula, hyperparameters){
    folded_train_data <- purrr::exec(groupdata2::fold, !!!fold_args, data=train_data)

    inner_cv <- cross_validate_fn(
      data=train_data,
      formulas = formula,
      type = type,
      model_fn = model_fn,
      predict_fn = predict_fn,
      preprocess_fn = preprocess_fn,
      preprocess_once = preprocess_once,
      hyperparameters = hyperparameters,
      fold_cols = ".folds", # TODO Make specifiable? What if it is already in the data?
      cutoff = cutoff,
      positive = positive,
      metrics = metrics,
      rm_nc = rm_nc,
      parallel = parallel,
      verbose = verbose)
  }

  # TODO:
  # Extract best hyperparameters
  # Refit model with those hparams on all data
  # Return best model and the scores per hyperparameters
}
