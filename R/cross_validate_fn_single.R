cross_validate_fn_single <- function(data, model_fn, fold_eval_fn, eval_aggregation_fn,
                                     model_specifics=list(), model_specifics_update_fn=NULL,
                                     folds_col =".folds"){

  # eval_fn: "regression", "binomial", "multiclass", "multilabel", "custom"/function
  #   custom: returns predictions and true labels/values in tibble
  # Actually, it might be better that the user passes the premade functions or a custom function


  # Check arguments
  # Check model_specifics arguments
  if (!is.null(model_specifics_update_fn)){
    model_specifics <- model_specifics_update_fn(model_specifics)
  }

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
             model_specifics=model_specifics)

  })

  # Extract model dataframe from fold_lists_list
  predictions_and_targets_list = fold_lists_list %c% 'predictions_and_targets'
  predictions_and_targets = dplyr::bind_rows(predictions_and_targets_list)

  # Extract models
  models = fold_lists_list %c% 'model'

  # Evaluate folds
  fold_evaluations <- plyr::llply(1:n_folds, function(fold_){
    fold_eval_fn(predictions_and_targets %>% dplyr::filter(fold == fold_),
                 models[[fold_]], fold_, model_specifics=model_specifics)
  })

  # Aggregate fold evaluations
  model_evaluation <- eval_aggregation_fn(fold_evaluations, n_folds,
                                          model_specifics=model_specifics)

  return(model_evaluation)

}
