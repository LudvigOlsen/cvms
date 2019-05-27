# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

cross_validate_fn_single <- function(data, model_fn, evaluation_type="linear_regression",
                                     model_specifics=list(), model_specifics_update_fn=NULL,
                                     fold_cols =".folds"){

  # TODO: the below comment is not correct
  # eval_fn: "regression", "binomial", "multiclass", "multilabel", "custom"/function
  #   custom: returns predictions and true labels/values in tibble
  # Actually, it might be better that the user passes the premade functions or a custom function


  # Check arguments
  # Check model_specifics arguments
  if (!is.null(model_specifics_update_fn)){
    model_specifics <- model_specifics_update_fn(model_specifics)
  }

  if (length(fold_cols) > 1){
    # Create a "map" of folds per fold column
    folds_map_and_n_folds <- create_folds_map(data, fold_cols)
    folds_map <- folds_map_and_n_folds[["folds_map"]]
    n_folds <- folds_map_and_n_folds[["n_folds"]]

  } else {
    # Get number of folds - aka. number of levels in folds column
    n_folds <- nlevels(data[[fold_cols]])
  }


  # Loop through the folds
  # .. Create a test_data and a training_set
  # .. Train the model on the training_set
  # .. Test the model on the test_data

  fold_lists_list <- plyr::llply(1:n_folds, function(fold){

    if(length(fold_cols)>1){
      current_fold_info <- folds_map %>%
        dplyr::filter(abs_fold == fold)

      rel_fold <- current_fold_info[["rel_fold"]]
      abs_fold <- current_fold_info[["abs_fold"]]
      current_fold_col_idx <- current_fold_info[["fold_col_idx"]]
      current_fold_col_name <- as.character(current_fold_info[["fold_col_name"]])

    } else {
      rel_fold <- fold
      abs_fold <- fold
      current_fold_col_idx <- 1
      current_fold_col_name <- fold_cols
    }

    # Create training set for this iteration
    train_data = data[data[[current_fold_col_name]] != rel_fold,]
    # Create test set for this iteration
    test_data = data[data[[current_fold_col_name]] == rel_fold,]

    # Remove folds column(s) from subsets, so we can use "y ~ ." method
    # when defining the model formula.
    train_data <- train_data %>%
      dplyr::ungroup() %>%
      dplyr::select(-dplyr::one_of(fold_cols))

    test_data <- test_data %>%
      dplyr::ungroup() %>%
      dplyr::select(-dplyr::one_of(fold_cols))

    model_fn(train_data = train_data,
             test_data = test_data,
             fold_info = list(rel_fold=rel_fold,
                              abs_fold=abs_fold,
                              fold_column=current_fold_col_name),
             model_specifics=model_specifics)

  })

  # Extract model dataframe from fold_lists_list
  predictions_and_targets_list = fold_lists_list %c% 'predictions_and_targets'
  predictions_and_targets = dplyr::bind_rows(predictions_and_targets_list)

  # TODO Check that the right columns exist !!!

  # Extract models
  models = fold_lists_list %c% 'model'
  n_conv_warns <- count_named_nulls_in_list(models)

  # Extract singular fit message
  singular_fit_messages = fold_lists_list %c% 'yielded_singular_fit_message'
  n_singular_fit_messages <- sum(unlist(singular_fit_messages))

  model_evaluation <- evaluate(data = predictions_and_targets,
                               type = evaluation_type,
                               predictions_col = "prediction",
                               targets_col = "target",
                               fold_info_cols = list(rel_fold="rel_fold",
                                                     abs_fold="abs_fold",
                                                     fold_column="fold_column"),
                               models=models,
                               model_specifics=model_specifics) %>%
    mutate(Folds = n_folds,
           `Fold Columns` = length(fold_cols),
           `Convergence Warnings` = n_conv_warns,
           `Singular Fit Messages` = n_singular_fit_messages)

  return(model_evaluation)

}
