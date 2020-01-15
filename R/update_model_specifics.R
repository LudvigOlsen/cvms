update_model_specifics <- function(model_specifics) {

  # Check required arguments
  check_argument_in_model_specifics("model_formula", model_specifics)
  check_argument_in_model_specifics("family", model_specifics)
  check_argument_in_model_specifics("caller", model_specifics)

  if (model_specifics[["caller"]] %ni% c("baseline()", "evaluate()")) {
    check_argument_in_model_specifics("model_fn", model_specifics)
    check_argument_in_model_specifics("predict_fn", model_specifics)
    check_argument_in_model_specifics("preprocess_fn", model_specifics)
    check_argument_in_model_specifics("preprocess_once", model_specifics)
    check_argument_in_model_specifics("hparams", model_specifics)
  }

  # These args should not be NULL
  if (model_specifics[["caller"]] %in% c(
    "cross_validate_fn()", "validate_fn()",
    "cross_validate()", "validate()"
  )) {
    stop_if_argument_is_null("model_fn", model_specifics)
    stop_if_argument_is_null("predict_fn", model_specifics)
    check_argument_in_model_specifics("observation_id_col", model_specifics)
  }
  stop_if_argument_is_null("caller", model_specifics)

  # These args should be NULL unless caller is cross_validate()
  if (model_specifics[["caller"]] %ni% c("cross_validate()", "validate()", "baseline()")) {
    stop_if_argument_not_null("REML", model_specifics)
    stop_if_argument_not_null("link", model_specifics)
    stop_if_argument_not_null("control", model_specifics)
  }

  # These args should be functions
  if (!is.null(model_specifics[["predict_fn"]])) {
    stop_if_argument_is_not_function("predict_fn", model_specifics)
  }
  if (!is.null(model_specifics[["model_fn"]])) {
    stop_if_argument_is_not_function("model_fn", model_specifics)
  }
  if (!is.null(model_specifics[["preprocess_fn"]])) {
    stop_if_argument_is_not_function("preprocess_fn", model_specifics)
  }

  family_ <- model_specifics[["family"]]

  # err=TRUE means that it throws an error and warns to pass as named arguments.
  # We don't use this right now, as arguments that wasn't passed (and should take default value) are NULL.
  model_specifics <- replace_argument_in_model_specifics_if_null("positive", model_specifics, 2, err = FALSE)
  model_specifics <- replace_argument_in_model_specifics_if_null("cutoff", model_specifics, 0.5, err = FALSE)
  model_specifics <- replace_argument_in_model_specifics_if_null("model_verbose", model_specifics, FALSE, err = FALSE)

  # if (isTRUE(model_specifics[["model_verbose"]])){
  #   message(paste0(
  #     "Updated model_specifics to { ",
  #     "model_formula = ", model_specifics[["model_formula"]],
  #     ", family = ", model_specifics[["family"]],
  #     ", positive = ", model_specifics[["positive"]],
  #     ", cutoff = ", model_specifics[["cutoff"]],
  #     ", model_verbose = ", model_specifics[["model_verbose"]],
  #     ", model_fn = ", capture_fn(model_specifics[["model_fn"]]),",",
  #     ", predict_fn = ", capture_fn(model_specifics[["predict_fn"]]),",",
  #     ", preprocess_fn = ", capture_fn(model_specifics[["preprocess_fn"]]),",",
  #     ", preprocess_once = ", model_specifics[["preprocess_once"]],
  #     ", hparams = ", paste_hparams(extract_hparams(model_specifics[["hparams"]])),",",
  #     " }. Note: If incorrect, remember to name arguments in model_specific."
  #   ))
  # }

  model_specifics
}

capture_fn <- function(fn) {
  if (!is.null(fn)) {
    return(paste0(utils::capture.output(fn), collapse = " \n "))
  }
  return("NULL")
}
