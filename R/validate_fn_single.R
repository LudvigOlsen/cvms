#' @importFrom dplyr %>%
validate_fn_single = function(train_data,
                             model_fn,
                             fold_eval_fn,
                             eval_aggregation_fn,
                             model_specifics=list(),
                             model_specifics_update_fn=NULL,
                             test_data = NULL,
                             partitions_col = '.partitions',
                             err_nc = FALSE) {

  # Set errors if input variables aren't what we expect / can handle
  # WORK ON THIS SECTION!
  stopifnot(
    is.data.frame(train_data),
    is.data.frame(test_data) || is.null(test_data))

  if (is.null(test_data)) {
    stopifnot(is.factor(train_data[[partitions_col]]))
  }

  # Check arguments
  # Check model_specifics arguments
  if (!is.null(model_specifics_update_fn)){
    model_specifics <- model_specifics_update_fn(model_specifics)
  }

  # If train and test data is not already split,
  # get train and test set
  if (is.null(test_data)) {
    # Create test set
    test_data = train_data[train_data[[partitions_col]] == 2,]
    # Create training set
    train_data = train_data[train_data[[partitions_col]] == 1,]
  }

  # Train and test the model

  fitting_output <- model_fn(train_data = train_data,
                             test_data = test_data,
                             fold = 1, # we'll remove this later
                             model_specifics=model_specifics)

  predictions_and_targets <- fitting_output[["predictions_and_targets"]]

  # Extract models
  model = fitting_output[["model"]]

  model_evaluation <- validate_evaluation(fold_eval_fn, eval_aggregation_fn,
                                          predictions_and_targets, model,
                                          model_specifics)

  if (isTRUE(err_nc) && model_evaluation[["Convergence Warnings"]] != 0) {
    stop("Model did not converge.")
  }

  return(list("Results" = model_evaluation, "Model" = model))

}
