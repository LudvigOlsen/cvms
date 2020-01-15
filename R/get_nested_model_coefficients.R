# Try to retrieve model coefficients (For a single model)
# fold_info contains the fold and fold column for each model
get_nested_model_coefficients <- function(model,
                                          # The datasets need to be available in the
                                          # environment when using broom::tidy
                                          train_data = NULL,
                                          test_data = NULL,
                                          fold_info = NULL,
                                          include_fold_columns = TRUE) {
  if (is.null(model)) {
    NA_coeffs <- tibble::tibble(
      "Fold Column" = NA,
      "Fold" = NA,
      "term" = NA,
      "estimate" = NA,
      "std.error" = NA,
      "statistic" = NA
    )

    if (!isTRUE(include_fold_columns)) {
      NA_coeffs <- NA_coeffs %>%
        base_deselect(cols = c("Fold Column", "Fold"))
    }

    nested_NA_coeffs <- NA_coeffs %>%
      nest_models() %>%
      dplyr::pull(.data$Coefficients)

    return(nested_NA_coeffs)
  }

  coefs_tidy <- tryCatch(
    {
      broom::tidy(model, effects = c("fixed")) # Custom tidy methods?
    },
    error = function(e) {
      # If broom::tidy wasn't implemented for the model type
      # let's grab the coefficients manually if possible

      if (grepl("Error: No tidy method for objects of class",
        as.character(e),
        ignore.case = TRUE
      )) {
        # Try to extract coefficients
        coefs <- tryCatch(
          {
            stats::coef(model)
          },
          error = function(e) {
            return(NA)
          }
        )

        # If successful, manually create tidy tibble
        if ((length(coefs) == 1 && !is.na(coefs)) ||
          length(coefs) > 1) {
          terms <- names(coefs)
          return(tibble::tibble(term = terms, estimate = coefs))

          # Else, return default NA coefficient tibble
        } else {
          return(
            get_nested_model_coefficients(model = NULL, include_fold_columns = include_fold_columns)
          )
        }
      } else {
        warning(e)
        # Return default NA coefficient tibble
        return(
          get_nested_model_coefficients(model = NULL, include_fold_columns = include_fold_columns)
        )
      }
    },
    warning = function(w) {
      warning(w)
      return(
        get_nested_model_coefficients(model = NULL, include_fold_columns = include_fold_columns)
      )
    }
  )

  # Add fold info
  if (isTRUE(include_fold_columns)) {
    coefs_tidy <- coefs_tidy %>%
      tibble::add_column(
        `Fold Column` = as.character(fold_info[["fold_column"]]),
        Fold = fold_info[["rel_fold"]],
        .before = "term"
      )
  }

  # Nest and return coefficients
  coefs_tidy %>%
    nest_models() %>%
    dplyr::pull(.data$Coefficients)
}
