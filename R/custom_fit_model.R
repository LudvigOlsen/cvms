custom_fit_model <- function(model_specifics, train_set){

  # Extract arguments from model_specifics
  model_formula <- model_specifics[["model_formula"]]
  model_fn <- model_specifics[["model_fn"]]

  model_fn(formula = model_formula,
           data = train_set)

}
