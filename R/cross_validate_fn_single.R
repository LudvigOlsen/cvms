cross_validate_fn_single <- function(data, model_fn, fold_eval_fn, eval_aggregation_fn,
                                     eval_model_specifics=list(), folds_col =".folds",
                                     ..., model_verbose = FALSE){

  # eval_fn: "regression", "binomial", "multiclass", "multilabel", "custom"/function
  #   custom: returns predictions and true labels/values in tibble
  # Actually, it might be better that the user passes the premade functions or a custom function


  # Check arguments


  # get number of folds - aka. number of levels in folds column
  n_folds <- nlevels(data[[folds_col]])

  # Loop through the folds
  # .. Create a test_set and a training_set
  # .. Train the model on the training_set
  # .. Test the model on the test_set

  fold_lists_list <- plyr::llply(1:n_folds, function(fold){

    # Create training set for this iteration
    train_set = data[data[[folds_col]] != fold,]
    # Create test set for this iteration
    test_set = data[data[[folds_col]] == fold,]

    model_fn(train_set = train_set,
             test_set = test_set,
             fold = fold,
             model_verbose = model_verbose,
             ...)

  })

  # Extract model dataframe from fold_lists_list
  predictions_and_observations_list = fold_lists_list %c% 'predictions_and_observations'
  predictions_and_observations = dplyr::bind_rows(predictions_and_observations_list)

  # Extract models
  models = fold_lists_list %c% 'model'

  # Evaluate folds
  fold_evaluations <- plyr::llply(1:n_folds, function(fold_){
    fold_eval_fn(predictions_and_observations %>% dplyr::filter(fold == fold_),
                 models[[fold_]], fold_)
  })

  # Aggregate fold evaluations
  model_evaluation <- eval_aggregation_fn(fold_evaluations, n_folds, eval_model_specifics)

  return(model_evaluation)

}
