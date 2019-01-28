evaluate <- function(data, type="linear_regression",
                     predictions_col = "prediction",
                     targets_col = "target",
                     folds_col = "fold",
                     models=NULL,
                     model_specifics=list()){

  stopifnot(type %in% c("linear_regression", "binomial")) #, "multiclass", "multilabel"))

  # data is a table with predictions, targets and folds
  # predictions can be values, logits, or classes, depending on evaluation type

  if (type == "linear_regression") {
    results <- linear_regression_eval(data,
                                      models,
                                      predictions_col = predictions_col,
                                      targets_col = targets_col,
                                      folds_col = folds_col,
                                      model_specifics = model_specifics)

    results[["coefficients"]] <- get_nested_model_coefficients(models)

  } else if (type == "binomial"){
    results <- binomial_classification_eval(data[[predictions_col]],
                                           data[[targets_col]],
                                           models=models,
                                           cutoff=model_specifics[["cutoff"]],
                                           positive=model_specifics[["positive"]])
  }


  return(results)
}
