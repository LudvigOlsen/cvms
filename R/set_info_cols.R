set_info_cols <- function(family, info_cols_list = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_choice(
    x = family,
    choices = c("gaussian", "binomial", "multinomial"),
    add = assert_collection
  )
  checkmate::assert_list(
    x = info_cols_list,
    types = c("logical"),
    names = "named",
    any.missing = FALSE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (family == "gaussian") {
    default_cols <- list(
      "Predictions" = TRUE,
      "Results" = TRUE,
      "Coefficients" = TRUE,
      "Preprocess" = TRUE,
      "Folds" = TRUE,
      "Fold Columns" = TRUE,
      "Convergence Warnings" = TRUE,
      "Singular Fit Messages" = FALSE,
      "Other Warnings" = TRUE,
      "Warnings and Messages" = TRUE,
      "Family" = TRUE,
      "HParams" = TRUE,
      "Model" = FALSE,
      "Dependent" = TRUE,
      "Fixed" = TRUE,
      "Random" = TRUE
    )
  } else if (family == "binomial") {
    default_cols <- list(
      "Predictions" = TRUE,
      "ROC" = TRUE,
      "Confusion Matrix" = TRUE,
      "Results" = TRUE,
      "Coefficients" = TRUE,
      "Preprocess" = TRUE,
      "Folds" = TRUE,
      "Fold Columns" = TRUE,
      "Convergence Warnings" = TRUE,
      "Singular Fit Messages" = FALSE,
      "Other Warnings" = TRUE,
      "Warnings and Messages" = TRUE,
      "Family" = TRUE,
      "HParams" = TRUE,
      "Model" = FALSE,
      "Dependent" = TRUE,
      "Fixed" = TRUE,
      "Random" = TRUE
    )
  } else if (family == "multinomial") {
    default_cols <- list(
      "Predictions" = TRUE,
      "ROC" = TRUE,
      "Confusion Matrix" = TRUE,
      "Results" = TRUE,
      "Class Level Results" = TRUE,
      "Coefficients" = TRUE,
      "Preprocess" = TRUE,
      "Folds" = TRUE,
      "Fold Columns" = TRUE,
      "Convergence Warnings" = TRUE,
      "Other Warnings" = TRUE,
      "Warnings and Messages" = TRUE,
      "Family" = TRUE,
      "HParams" = TRUE,
      "Model" = FALSE,
      "Dependent" = TRUE,
      "Fixed" = TRUE,
      "Random" = TRUE
    )
  }

  info_cols <- default_cols

  if (!is.null(info_cols_list)) {
    if (!is.list(info_cols_list) && info_cols_list == "all") {

      # Set all info_cols to TRUE
      for (info_col in seq_along(info_cols)) {
        info_cols[[info_col]] <- TRUE
      }
    } else if (length(info_cols_list) > 0) {

      # Check for unknown column names
      unknown_colnames <- setdiff(names(info_cols_list), names(info_cols))
      if (length(unknown_colnames) > 0) {
        stop(paste0(
          "'info_cols_list' contained unknown column names: ",
          paste0(unknown_colnames, collapse = ", "),
          "."
        ))
      }

      # Check for unknown values (Those not TRUE/FALSE)
      if (any(unlist(lapply(info_cols_list, function(x) {
        !(is.logical(x) && !is.na(x))
      })))) {
        stop("The values in 'info_cols_list' must be either TRUE or FALSE.")
      }

      # Update info_cols as specified by user
      for (info_col in seq_along(info_cols_list)) {
        if (is.null(info_cols_list[[info_col]])) {
          stop("info_cols in 'info_cols_list' should be logical (TRUE/FALSE) not NULL.")
        }
        info_cols[[names(info_cols_list)[[info_col]]]] <- info_cols_list[[info_col]]
      }
    }
  }

  # Extract the info_cols names
  # We need to provide these,
  # as the whole conversion below adds dots instead of spaces
  # info_cols_names <- names(info_cols)

  # Extract and return names of the metrics set to TRUE
  names(
    which(
      sapply(info_cols, function(y) isTRUE(y))
    )
  )
}
