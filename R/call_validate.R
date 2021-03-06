call_validate <- function(train_data,
                          test_data,
                          formulas,
                          partitions_col,
                          family,
                          control,
                          REML,
                          cutoff,
                          positive,
                          metrics,
                          preprocessing,
                          err_nc,
                          rm_nc,
                          parallel,
                          verbose) {

  # Set default arguments
  link <- default_link(NULL, family = family)
  control <- default_control(control, family = family, link = link)
  if (checkmate::test_string(x = metrics, pattern = "^all$")) {
    metrics <- list("all" = TRUE)
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_choice(
    x = preprocessing,
    choices = c(
      "standardize",
      "scale",
      "center",
      "range"
    ),
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_choice(
    x = family,
    choices = c(
      "gaussian",
      "binomial",
      "multinomial"
    ),
    add = assert_collection
  )
  checkmate::assert_list(
    x = metrics,
    types = "logical",
    any.missing = FALSE,
    names = "named",
    add = assert_collection
  )
  checkmate::assert_flag(x = rm_nc, add = assert_collection)
  checkmate::assert_flag(x = REML, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Add AIC, AICc, BIC, r2m, r2c (unless disabled by user)
  metrics <- basics_update_metrics(metrics = metrics, family = family)

  results <- validate_list(
    train_data = train_data,
    test_data = test_data,
    partitions_col = partitions_col,
    formulas = formulas,
    model_fn = basics_model_fn,
    predict_fn = basics_predict_fn,
    preprocess_fn = basics_pick_preprocess_fn(
      preprocessing = preprocessing
    ),
    hyperparameters = basics_hparams(
      REML = REML, control = control,
      model_verbose = verbose,
      family = family
    ),
    family = family,
    cutoff = cutoff,
    positive = positive,
    metrics = metrics,
    info_cols = list(
      "Singular Fit Messages" = TRUE,
      "HParams" = FALSE,
      "Model" = TRUE
    ),
    err_nc = err_nc,
    rm_nc = FALSE, # Done below instead
    return_models = TRUE,
    verbose = verbose,
    parallel_ = parallel,
    caller = "validate()"
  )

  # If asked to remove non-converged models from results
  if (isTRUE(rm_nc)) {
    results <- results[results[["Convergence Warnings"]] == 0, ]
  }

  results
}


#' @importFrom plyr ldply
#' @importFrom dplyr %>%
#' @importFrom tidyr separate
call_cross_validate <- function(data,
                                formulas,
                                fold_cols,
                                family,
                                control,
                                REML,
                                cutoff,
                                positive,
                                metrics,
                                preprocessing,
                                rm_nc,
                                parallel,
                                verbose) {

  # Set default arguments
  link <- default_link(NULL, family = family)
  control <- default_control(control, family = family, link = link)
  if (checkmate::test_string(x = metrics, pattern = "^all$")) {
    metrics <- list("all" = TRUE)
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_choice(
    x = preprocessing,
    choices = c(
      "standardize",
      "scale",
      "center",
      "range"
    ),
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_choice(
    x = family,
    choices = c(
      "gaussian",
      "binomial",
      "multinomial"
    ),
    add = assert_collection
  )
  checkmate::assert_list(
    x = metrics,
    types = "logical",
    any.missing = FALSE,
    names = "named",
    add = assert_collection
  )
  checkmate::assert_flag(x = REML, add = assert_collection)
  checkmate::assert_flag(x = rm_nc, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Add AIC, AICc, BIC, r2m, r2c (unless disabled by user)
  metrics <- basics_update_metrics(metrics = metrics, family = family)

  results <- cross_validate_list(
    data = data,
    formulas = formulas,
    model_fn = basics_model_fn,
    predict_fn = basics_predict_fn,
    preprocess_fn = basics_pick_preprocess_fn(
      preprocessing = preprocessing
    ),
    hyperparameters = basics_hparams(
      REML = REML, control = control,
      model_verbose = verbose,
      family = family
    ),
    family = family,
    fold_cols = fold_cols,
    cutoff = cutoff,
    positive = positive,
    metrics = metrics,
    info_cols = list(
      "Singular Fit Messages" = TRUE,
      "HParams" = FALSE
    ),
    rm_nc = FALSE, # Done below instead
    verbose = verbose,
    parallel_ = parallel,
    caller = "cross_validate()"
  )

  # If asked to remove non-converged models from results
  if (isTRUE(rm_nc)) {
    results <- results[results[["Convergence Warnings"]] == 0, ]
  }

  results
}


### Basics, shared between validate and cross_validate

basics_update_metrics <- function(metrics, family) {

  # For cross_validate we expect AIC to be comparable
  # Although with REML = TRUE we might not be able to compare
  # models with and without random effects   TODO (add to documentation)

  if (family == "gaussian") {
    metric_names <- names(metrics)
    if (is.list(metrics) &&
      length(setdiff(c("AIC", "AICc", "BIC"), metric_names)) > 1) {
      if ("AIC" %ni% metric_names) {
        metrics[["AIC"]] <- TRUE
      }
      if ("AICc" %ni% metric_names) {
        metrics[["AICc"]] <- TRUE
      }
      if ("BIC" %ni% metric_names) {
        metrics[["BIC"]] <- TRUE
      }
    }
  }

  metrics
}

basics_hparams <- function(REML, control, model_verbose, family) {
  list(
    "REML" = REML,
    "control" = list(control),
    "model_verbose" = model_verbose,
    "family" = family,
    "is_special_fn" = TRUE
  )
}

basics_model_fn <- function(train_data, formula, hyperparameters) {

  # Extract hyperparameters
  REML <- hyperparameters[["REML"]]
  control <- hyperparameters[["control"]][[1]]
  model_verbose <- hyperparameters[["model_verbose"]]
  family_ <- hyperparameters[["family"]]

  # Check for random effects
  contains_random_effects <- rand_effects(formula)

  # Identify model function
  if (family_ == "gaussian") {
    model_type <- "lm"
  }
  if (family_ == "binomial") {
    model_type <- "glm"
  }
  if (isTRUE(contains_random_effects)) model_type <- paste0(model_type, "er")

  # Fitting the correct model

  # Checks the model_type and fits the model on the train_data
  if (model_type == "lm") {
    message_if_model_verbose(model_verbose,
      msg = "Used lm() to fit the model.'"
    )

    return(lm(formula = formula, data = train_data))
  } else if (model_type == "lmer") {
    message_if_model_verbose(model_verbose,
      msg = "Used lme4::lmer() to fit the model.'"
    )

    return(lme4::lmer(
      formula = formula, data = train_data,
      REML = REML, control = control
    ))
  } else if (model_type == "glm") {
    message_if_model_verbose(model_verbose,
      msg = "Used glm() to fit the model.'"
    )

    return(glm(
      formula = formula, data = train_data,
      family = family_
    ))
  } else if (model_type == "glmer") {
    message_if_model_verbose(
      model_verbose,
      msg = "Used lme4::glmer() to fit the model.'"
    )

    # Fit the model using glmer()
    # Return this model to model_temp

    return(lme4::glmer(
      formula = formula,
      data = train_data,
      family = family_,
      control = control
    ))
  }
}

basics_predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {

  # Extract family from hyperparameters
  family_ <- hyperparameters[["family"]]
  contains_random_effects <- rand_effects(formula)
  link <- hyperparameters[["link"]]

  # Note: The same predict_fn can be used for (g)lm and (g)lmer
  # so this conditional selection is technically unnecessary
  if (family_ == "gaussian") {
    if (!isTRUE(contains_random_effects)) {
      p_fn <- predict_functions("lm")
    } else {
      p_fn <- predict_functions("lmer")
    }
  } else if (family_ == "binomial") {
    if (!isTRUE(contains_random_effects)) {
      p_fn <- predict_functions("glm_binomial")
    } else {
      p_fn <- predict_functions("glmer_binomial")
    }
  }

  p_fn(
    test_data = test_data, model = model,
    formula = formula, hyperparameters = hyperparameters
  )
}

basics_pick_preprocess_fn <- function(preprocessing) {
  if (is.null(preprocessing)) {
    return(NULL)
  }

  if (preprocessing == "standardize") {
    preprocess_fn <- preprocess_functions("standardize")
  } else if (preprocessing == "scale") {
    preprocess_fn <- preprocess_functions("scale")
  } else if (preprocessing == "center") {
    preprocess_fn <- preprocess_functions("center")
  } else if (preprocessing == "range") {
    preprocess_fn <- preprocess_functions("range")
  } else {
    stop("'preprocessing' was not found.")
  }

  preprocess_fn
}

message_if_model_verbose <- function(model_verbose, msg, caller = "cross_validate()") {
  if (isTRUE(model_verbose)) {
    message(paste0(caller, ": ", msg))
  }
}
