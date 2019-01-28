basics_update_model_specifics <- function(model_specifics){

  # Check required arguments
  check_argument_in_model_specifics("model_formula", model_specifics)
  check_argument_in_model_specifics("family", model_specifics)

  # Set default arguments
  model_specifics[["link"]] = default_link(model_specifics[["link"]], family = model_specifics[["family"]])
  model_specifics[["control"]] = default_control(model_specifics[["control"]], family = model_specifics[["family"]],
                                                 link=model_specifics[["link"]])

  # err=TRUE means that it throws an error and warns to pass as named arguments.
  # We don't use this right now, as arguments that wasn't passed (and should take default value) are NULL.
  model_specifics <- replace_argument_in_model_specifics_if_null("REML", model_specifics, FALSE, err=FALSE)
  model_specifics <- replace_argument_in_model_specifics_if_null("positive", model_specifics, 1, err=FALSE)
  model_specifics <- replace_argument_in_model_specifics_if_null("cutoff", model_specifics, 0.5, err=FALSE)
  model_specifics <- replace_argument_in_model_specifics_if_null("model_verbose", model_specifics, FALSE, err=FALSE)

  if (isTRUE(model_specifics[["model_verbose"]])){
    print(paste0(
      "Updated model_specifics to { ",
      "model_formula = ", model_specifics[["model_formula"]],
      ", family = ", model_specifics[["family"]],
      ", link = ", model_specifics[["link"]],
      ", control = (", paste0(model_specifics[["control"]],collapse=", "), ")",
      ", REML = ", model_specifics[["REML"]],
      ", positive = ", model_specifics[["positive"]],
      ", cutoff = ", model_specifics[["cutoff"]],
      ", model_verbose = ", model_specifics[["model_verbose"]],
      " }. Note: If incorrect, remember to name arguments in model_specific."
    ))
  }

  model_specifics

}
