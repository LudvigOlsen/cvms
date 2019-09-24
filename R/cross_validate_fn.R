
# @param predict_fn Function for predicting the test folds. Note that
#  predict_fn and predict_type are mutually exclusive. (so should throw error if both are passed?)
# @param predict_type The type argument for stats::predict. When the defaults fail,
#  provide it such that the output is as follows:
#  binomial: single vector with probabilies (0-1)
#  gaussian: single vector with the predicted value
#  multinomial: data frame with one column per class with probabilities, with column names
#  being the name of the class as written in the target column (response variable in formula)
#
# Note that the model object metrics (r2, AIC, etc.) can only be computed for certain models
# Turn them off by metrics_list = list("default_model_metrics" = FALSE)
# model_fn = function(data,formula){lm(formula=formula, data = data)}
cross_validate_fn <- function(data, model_fn, formulas,
                              fold_cols = '.folds',
                              type = 'gaussian',
                              cutoff = 0.5,
                              positive = 2,
                              predict_type = NULL,
                              predict_fn = NULL,
                              rm_nc = FALSE,
                              parallel = FALSE, model_verbose = FALSE){

  return(custom_cross_validate_list(data = data,
                                    formulas = formulas,
                                    model_fn = model_fn,
                                    fold_cols = fold_cols,
                                    family = type,
                                    cutoff = cutoff,
                                    positive = positive,
                                    predict_type = predict_type,
                                    predict_fn = predict_fn,
                                    rm_nc = rm_nc,
                                    model_verbose = model_verbose,
                                    parallel_ = parallel,
                                    parallelize = "models"))
}
