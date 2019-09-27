custom_fit_model <- function(model_specifics, train_set){

  # Extract arguments from model_specifics
  model_formula <- model_specifics[["model_formula"]]
  model_fn <- model_specifics[["model_fn"]]

  # Some model types don't accept string formulas
  # so we convert it to an actual formula object
  if (is.character(model_formula))
    model_formula <- stats::as.formula(model_formula)

  model_fn(formula = model_formula,
           train_data = train_set)

}
