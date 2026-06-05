# Try to retrieve model coefficients (For a single model)
# fold_info contains the fold and fold column for each model
get_nested_model_coefficients <- function(
  model,
  # The datasets need to be available in the
  # environment when using parameters::model_parameters
  train_data = NULL,
  test_data = NULL,
  fold_info = NULL,
  include_fold_columns = TRUE,
  caller = ""
) {
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

  # Some parameters/insight methods reconstruct the model data from the stored
  # model call. The fitted call may contain names like train_data and formula
  # from cvms' model-fitting function; those names no longer exist when
  # coefficients are extracted. Use a temporary copy that can resolve them,
  # without changing the model returned to users.
  model_for_parameters <- prepare_model_for_parameters(
    model = model,
    train_data = train_data
  )

  coefs_tidy <- tryCatch(
    {
      # Extract the model parameters quietly
      model_params_process <- run_quietly(
        fn = function(model) {
          parameters::model_parameters(model) %>%
            parameters::standardize_names(style = "broom") %>%
            tibble::as_tibble()
        },
        model = model_for_parameters,
        msg_context_fn = function(m) {
          m
        }
      )

      # Extract the parameters and side effects
      model_params <- model_params_process[["result"]]
      warnings <- model_params_process[["warnings"]]
      messages <- model_params_process[["messages"]]

      # Function for creating message with context
      message_creator <- function(m) {
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
        ignore.case = TRUE
      )) {
        # Try to extract coefficients
        coefs <- tryCatch(
          {
            stats::coef(model)
          },
          error = function(e) {
            NA
          }
        )

        # If successful, manually create tidy tibble
        if (is.data.frame(coefs) || is.matrix(coefs) && !is.null(colnames(coefs))) {
          if (!is.null(rownames(coefs))) {
            coefs <- tibble::as_tibble(coefs, rownames = "RowName")
          } else {
            coefs <- tibble::as_tibble(coefs)
          }
          return(coefs)
        } else if (
          (length(coefs) == 1 && !is.na(coefs)) ||
            length(coefs) > 1
        ) {
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


prepare_model_for_parameters <- function(model, train_data = NULL) {
  # This helper is only useful when training data can be exposed as named model
  # variables. If we do not have a data frame, there are no columns to bind into
  # the model's recovery environment, so return the model unchanged.
  if (is.null(train_data) || !is.data.frame(train_data)) {
    return(model)
  }

  model_copy <- model
  # stats::getCall() works for S3 and S4 model objects. If a model class does
  # not implement it, fall back to the storage-specific component helper below.
  model_call <- tryCatch(
    stats::getCall(model_copy),
    error = function(e) get_model_component(model_copy, "call")
  )
  has_standard_call <- is.call(model_call) || is.pairlist(model_call)

  # Formula-based model objects often store calls such as
  # nnet::multinom(formula = formula, data = train_data). The function frame
  # that held formula and train_data is gone after cvms finishes model fitting,
  # so expose equivalent objects in a temporary environment.
  formula <- tryCatch(
    stats::formula(model_copy),
    error = function(e) NULL
  )

  parent_env <- if (inherits(formula, "formula")) {
    environment(formula)
  } else {
    parent.frame()
  }
  if (is.null(parent_env)) {
    parent_env <- emptyenv()
  }

  data_env <- new.env(parent = parent_env)

  # Make formula terms available by column name for insight::get_data() and
  # similar recovery paths. If conversion fails for a nonstandard data frame
  # subclass, the named train_data bindings below are still attempted.
  tryCatch(
    list2env(as.list(train_data), envir = data_env),
    error = function(e) NULL
  )

  # Cover cvms' own argument names and the actual symbol used in model$call$data
  # when that call follows the usual formula-model convention.
  data_names <- c(".cvms_train_data", "train_data", "train_set")
  if (isTRUE(has_standard_call)) {
    data_arg <- model_call[["data"]]
    if (!is.null(data_arg) && is.name(data_arg)) {
      data_names <- unique(c(data_names, as.character(data_arg)))
    }
  }
  for (data_name in data_names) {
    assign(data_name, train_data, envir = data_env)
  }

  if (inherits(formula, "formula")) {
    environment(formula) <- data_env
    assign(".cvms_formula", formula, envir = data_env)

    # Cover cvms' formula argument name and the actual formula symbol used in
    # the stored call. This lets calls like formula = formula be reconstructed.
    formula_names <- "formula"
    if (isTRUE(has_standard_call)) {
      formula_arg <- model_call[["formula"]]
      if (!is.null(formula_arg) && is.name(formula_arg)) {
        formula_names <- unique(c(formula_names, as.character(formula_arg)))
      }
    }
    for (formula_name in formula_names) {
      assign(formula_name, formula, envir = data_env)
    }
  }

  # If the call stores an inline formula instead of a symbol, point that formula
  # at the temporary environment as well. Nonstandard calls are left untouched.
  if (isTRUE(has_standard_call) && inherits(model_call[["formula"]], "formula")) {
    environment(model_call[["formula"]]) <- data_env
    model_copy <- set_model_component(model_copy, "call", model_call)
  }

  # Terms/formula slots are another place downstream packages inspect when
  # trying to recover response and predictor variables.
  model_terms <- get_model_component(model_copy, "terms")
  if (inherits(model_terms, "formula")) {
    environment(model_terms) <- data_env
    model_copy <- set_model_component(model_copy, "terms", model_terms)
  }

  model_formula <- get_model_component(model_copy, "formula")
  if (inherits(model_formula, "formula")) {
    environment(model_formula) <- data_env
    model_copy <- set_model_component(model_copy, "formula", model_formula)
  }

  model_copy
}


get_model_component <- function(model, component) {
  # S3 models like lm and multinom are usually list-like, while lme4 models are
  # S4 objects with slots. Read through the representation the model actually
  # uses, and return NULL when that component is not available.
  tryCatch(
    {
      if (isS4(model) && component %in% methods::slotNames(model)) {
        methods::slot(model, component)
      } else {
        model[[component]]
      }
    },
    error = function(e) NULL
  )
}


set_model_component <- function(model, component, value) {
  # The temporary model copy should be updated in the same place the original
  # class stores that component. If the class blocks assignment, keep the copy as
  # it is and let the other recovery paths handle coefficient extraction.
  tryCatch(
    {
      if (isS4(model) && component %in% methods::slotNames(model)) {
        methods::slot(model, component) <- value
      } else {
        model[[component]] <- value
      }
      model
    },
    error = function(e) model
  )
}
