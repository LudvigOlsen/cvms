cross_validate_fn_single <- function(data, model_fn, evaluation_type="linear_regression",
                                     model_specifics=list(), model_specifics_update_fn=NULL,
                                     folds_col =".folds"){


  # TODO: the below comment is not correct
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
  # .. Create a test_data and a training_set
  # .. Train the model on the training_set
  # .. Test the model on the test_data

  fold_lists_list <- plyr::llply(1:n_folds, function(fold){

    # Create training set for this iteration
    train_data = data[data[[folds_col]] != fold,]
    # Create test set for this iteration
    test_data = data[data[[folds_col]] == fold,]

    model_fn(train_data = train_data,
             test_data = test_data,
             fold = fold,
             model_specifics=model_specifics)

  })

  # Extract model dataframe from fold_lists_list
  predictions_and_targets_list = fold_lists_list %c% 'predictions_and_targets'
  predictions_and_targets = dplyr::bind_rows(predictions_and_targets_list)

  # TODO Check that the right columns exist !!!

  # Extract models
  models = fold_lists_list %c% 'model'
  n_conv_warns <- length(models)-n_folds

  model_evaluation <- evaluate(predictions_and_targets,
                               type=evaluation_type,
                               predictions_col = "prediction",
                               targets_col = "target",
                               folds_col = "fold",
                               models=models,
                               model_specifics=model_specifics) %>%
    mutate(Folds = n_folds,
           `Convergence Warnings` = n_conv_warns)

  return(model_evaluation)

}
