# Evaluates single model object
# and extracts information like coefficients
internal_evaluate_predictions <- function(data,
                                          predictions_col,
                                          targets_col,
                                          model_was_null_col,
                                          type,
                                          fold_info_cols = list(rel_fold = "rel_fold",
                                                                abs_fold = "abs_fold",
                                                                fold_column = "fold_column"),
                                          model_specifics,
                                          metrics,
                                          id_col = NULL,
                                          id_method = NULL,
                                          include_fold_columns = TRUE,
                                          include_predictions = TRUE,
                                          na.rm = dplyr::case_when(
                                            type == "gaussian" ~ TRUE,
                                            type == "binomial" ~ FALSE,
                                            type == "multinomial" ~ FALSE
                                          )){

  if (type == "gaussian") {
    eval_pred_fn <- evaluate_predictions_gaussian
  } else if (type == "binomial") {
    eval_pred_fn <- evaluate_predictions_binomial
  } else if (type == "multinomial") {
    eval_pred_fn <- evaluate_predictions_multinomial
  }

  eval_pred_fn(
    data = data,
    predictions_col = predictions_col,
    targets_col = targets_col,
    model_was_null_col = model_was_null_col,
    id_col = id_col,
    id_method = id_method,
    fold_info_cols = fold_info_cols,
    model_specifics = model_specifics,
    metrics = metrics,
    include_fold_columns = include_fold_columns,
    include_predictions = include_predictions,
    na.rm = na.rm)

}
