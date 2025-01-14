# Try to retrieve model coefficients (For a single model)
# fold_info contains the fold and fold column for each model
get_nested_model_coefficients <- function(model,
                                          # The datasets need to be available in the
                                          # environment when using parameters::model_parameters
                                          train_data = NULL,
                                          test_data = NULL,
                                          fold_info = NULL,
                                          include_fold_columns = TRUE,
                                          caller = "") {
  # model <- NULL
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

  coefs_tidy <- tryCatch({

    # Extract the model parameters quietly
    model_params_process <- run_quietly(
      fn = function(model) {
        parameters::model_parameters(model) %>%
          parameters::standardize_names(style = "broom") %>%
          tibble::as_tibble()
      },
      model = model,
      msg_context_fn = function(m){m}
    )

    # Extract the parameters and side effects
    model_params <- model_params_process[["result"]]
    warnings <- model_params_process[["warnings"]]
    messages <- model_params_process[["messages"]]

    # Function for creating message with context
    message_creator <- function(m){
      create_message(
        m = m,
        caller = caller,
        formula = NULL,
        fold_col = fold_info[["fold_column"]],
        fold = fold_info[["rel_fold"]]
      )
    }

    # Message the caught messages to the user
    throw_messages(messages = messages, msg_context_fn = message_creator)
    # Throw the caught warnings
    throw_warnings(warnings = warnings, msg_context_fn = message_creator)

    # If we have warnings, we return an NA tibble
    # TODO Is this behavior we want? Depends on potential warnings thrown by model_parameters()
    # It is consistent with previous implementation
    if (length(warnings) > 0) {
      get_nested_model_coefficients(model = NULL, include_fold_columns = include_fold_columns)[[1]]
    } else {
      model_params
    }
  },
    error = function(e) {
      # If parameters::model_parameters wasn't implemented for the model type
      # let's grab the coefficients manually if possible

      if (grepl("not supported",
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
        if (is.data.frame(coefs) || is.matrix(coefs) && !is.null(colnames(coefs))) {
          if (!is.null(rownames(coefs))){
            coefs <- tibble::as_tibble(coefs, rownames = "RowName")
          } else {
            coefs <- tibble::as_tibble(coefs)
          }
          return(coefs)
        }
        else if ((length(coefs) == 1 && !is.na(coefs)) ||
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
