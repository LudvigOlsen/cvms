
# Note that the model object metrics (r2, AIC, etc.) can only be computed for certain models
# Turn them off by metrics_list = list("default_model_metrics" = FALSE)
# model_fn = function(data,formula){lm(formula=formula, data = data)}
cross_validate_fn <- function(data, model_fn, formulas,
                              fold_cols = '.folds', type = 'gaussian',
                              cutoff = 0.5, positive = 2, rm_nc = FALSE,
                              parallel = FALSE, model_verbose = FALSE){

  return(custom_cross_validate_list(data = data,
                                    formulas = formulas,
                                    model_fn = model_fn,
                                    fold_cols = fold_cols,
                                    family = type,
                                    cutoff = cutoff,
                                    positive = positive,
                                    rm_nc = rm_nc,
                                    model_verbose = model_verbose,
                                    parallel_ = parallel,
                                    parallelize = "models"))


}
