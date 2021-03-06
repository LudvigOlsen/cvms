# Try to retrieve model coefficients (For a single model)
# fold_info contains the fold and fold column for each model
get_nested_model_coefficients <- function(model,
                                          # The datasets need to be available in the
                                          # environment when using parameters::model_parameters
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
      # Custom tidy methods?
      parameters::model_parameters(model) %>%
        parameters::standardize_names(style = "broom") %>%
        tibble::as_tibble()
    },
    error = function(e) {
      # If parameters::model_parameters wasn't implemented for the model type
      # let's grab the coefficients manually if possible

      if (grepl("Error: Sorry, `model_parameters()` does currently not work for objects of class",
                as.character(e),
                ignore.case = TRUE)) {
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
            get_nested_model_coefficients(model = NULL, include_fold_columns = include_fold_columns)[[1]]
          )
        }
      } else {
        warning(e)
        # Return default NA coefficient tibble
        return(
          get_nested_model_coefficients(model = NULL, include_fold_columns = include_fold_columns)[[1]]
        )
      }
    },
    warning = function(w) {
      warning(w)
      return(
        get_nested_model_coefficients(model = NULL, include_fold_columns = include_fold_columns)[[1]]
      )
    }
  )

  # Remove names
  coefs_tidy[["estimate"]] <- unname(coefs_tidy[["estimate"]])

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
