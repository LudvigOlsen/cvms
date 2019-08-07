# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

evaluate <- function(data, type="linear_regression",
                     predictions_col = "prediction",
                     targets_col = "target",
                     fold_info_cols = list(rel_fold="rel_fold",
                                           abs_fold="abs_fold",
                                           fold_column="fold_column"),
                     models=NULL,
                     model_specifics=list()){

  stopifnot(type %in% c("linear_regression", "binomial", "multinomial")) #, "multiclass", "multilabel"))

  # data is a table with predictions, targets and folds
  # predictions can be values, logits, or classes, depending on evaluation type

  if (type == "linear_regression") {
    results <- linear_regression_eval(data,
                                      models = models,
                                      predictions_col = predictions_col,
                                      targets_col = targets_col,
                                      fold_info_cols = list(rel_fold="rel_fold",
                                                            abs_fold="abs_fold",
                                                            fold_column="fold_column"),
                                      model_specifics = model_specifics)

  } else if (type == "binomial"){

    results <- binomial_classification_eval(data,
                                            predictions_col = predictions_col,
                                            targets_col = targets_col,
                                            fold_info_cols = list(
                                              rel_fold = "rel_fold",
                                              abs_fold = "abs_fold",
                                              fold_column = "fold_column"),
                                            models = models,
                                            cutoff = model_specifics[["cutoff"]],
                                            positive = model_specifics[["positive"]])
  } else if (type == "multinomial"){

    results <- multinomial_classification_eval(data,
                                               predictions_col = predictions_col,
                                               targets_col = targets_col,
                                               fold_info_cols = list(
                                                 rel_fold = "rel_fold",
                                                 abs_fold = "abs_fold",
                                                 fold_column = "fold_column"),
                                               models = models)
  }


  return(results)
}
