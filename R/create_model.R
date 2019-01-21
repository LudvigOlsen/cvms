create_model_ = function(model, model_type, training_set, family, link, control, REML, fold, model_verbose){

  run_basic_model(fit_model_, model, model_type, training_set,
                  family, link, control, REML, model_verbose,
                  warn_info=list(model_formula=model, fold=fold,
                                 model_verbose=model_verbose))

}

