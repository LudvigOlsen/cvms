custom_update_model_specifics <- function(model_specifics){

  # Check required arguments
  check_argument_in_model_specifics("model_formula", model_specifics)
  check_argument_in_model_specifics("family", model_specifics)
  check_argument_in_model_specifics("model_fn", model_specifics)
  check_argument_in_model_specifics("caller", model_specifics)
  check_argument_in_model_specifics("predict_type", model_specifics)
  check_argument_in_model_specifics("predict_fn", model_specifics)


  # These args should be NULL in custom
  stop_if_argument_not_null("REML", model_specifics)
  stop_if_argument_not_null("link", model_specifics)
  stop_if_argument_not_null("control", model_specifics)

  family_ <- model_specifics[["family"]]

  # err=TRUE means that it throws an error and warns to pass as named arguments.
  # We don't use this right now, as arguments that wasn't passed (and should take default value) are NULL.
  model_specifics <- replace_argument_in_model_specifics_if_null("positive", model_specifics, 1, err=FALSE)
  model_specifics <- replace_argument_in_model_specifics_if_null("cutoff", model_specifics, 0.5, err=FALSE)
  model_specifics <- replace_argument_in_model_specifics_if_null("model_verbose", model_specifics, FALSE, err=FALSE)

  if (isTRUE(model_specifics[["model_verbose"]])){
    message(paste0(
      "Updated model_specifics to { ",
      "model_formula = ", model_specifics[["model_formula"]],
      ", family = ", model_specifics[["family"]],
      ", positive = ", model_specifics[["positive"]],
      ", cutoff = ", model_specifics[["cutoff"]],
      ", predict_type = ", model_specifics[["predict_type"]],
      ", predict_fn = ", capture_fn(model_specifics[["predict_fn"]]),",",
      ", model_verbose = ", model_specifics[["model_verbose"]],
      ", model_fn = ", capture_fn(model_specifics[["model_fn"]]),",",
      " }. Note: If incorrect, remember to name arguments in model_specific."
    ))
  }

  model_specifics

}

capture_fn <- function(fn){
  if (!is.null(fn)){
    return(paste0(capture.output(fn), collapse = " \n "))
  }
  return("NULL")
}
