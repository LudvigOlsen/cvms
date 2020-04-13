extract_special_fn_specific_hparams <- function(hyperparameters) {
  if (!is.data.frame(hyperparameters) && is.list(hyperparameters)) {

    # When using validate() or cross_validate()
    # we need to extract a few hparams
    # Hyperparameters for REML, link, control, is_*validate
    is_special_fn <- extract_from_hparams_for_validate(
      hyperparameters = hyperparameters,
      param = "is_special_fn"
    )

    if (is.null(is_special_fn)) {
      is_special_fn <- FALSE
    }
  } else {
    is_special_fn <- FALSE
  }

  if (isTRUE(is_special_fn)) {

    # Note that we do not fill in defaults here, as that would mean,
    # they could be different from their settings in the hyperparameters
    # which could lead to mistakes. They are instead filled in
    # within call_cross_validate().

    REML <-
      extract_from_hparams_for_validate(
        hyperparameters = hyperparameters,
        param = "REML"
      )
    link <-
      extract_from_hparams_for_validate(
        hyperparameters = hyperparameters,
        param = "link"
      )
    control <-
      extract_from_hparams_for_validate(
        hyperparameters = hyperparameters,
        param = "control"
      )
  } else {
    REML <- NULL
    link <- NULL
    control <- NULL
  }

  list(
    "is_special_fn" = is_special_fn,
    "REML" = REML,
    "link" = link,
    "control" = control
  )
}
