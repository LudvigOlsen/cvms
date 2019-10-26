call_validate <- function(train_data,
                          test_data,
                          formulas,
                          partitions_col,
                          family,
                          link,
                          control,
                          REML,
                          cutoff,
                          positive,
                          metrics,
                          err_nc,
                          rm_nc,
                          parallel,
                          model_verbose){


  # Set default arguments
  link <- default_link(link, family = family)
  control <- default_control(control, family = family, link = link)
  if(!is_logical_scalar_not_na(REML))
    stop("cross_validate(): 'REML' must be either TRUE or FALSE.")
  if(!is_logical_scalar_not_na(model_verbose))
    stop("cross_validate(): 'model_verbose' must be either TRUE or FALSE.")
  if (!is.character(family))
    stop("cross_validate(): 'family' must have type character.")

  results <- validate_list(
    train_data = train_data,
    test_data = test_data,
    partitions_col = partitions_col,
    formulas = formulas,
    model_fn = basics_model_fn,
    predict_fn = basics_predict_fn,
    hyperparameters = basics_hparams(
      REML = REML, link = link, control = control,
      model_verbose = model_verbose, family = family),
    family = family,
    cutoff = cutoff,
    positive = positive,
    metrics = metrics,
    info_cols = list("Singular Fit Messages" = TRUE,
                     "HParams" = FALSE,
                     "Model" = TRUE),
    err_nc = err_nc,
    rm_nc = FALSE, # Done below instead
    return_models = TRUE,
    verbose = model_verbose,
    parallel_ = parallel,
    caller = "validate()"
  ) %>%
    tibble::add_column("Link" = link,
                       .after = "Family")

  # If asked to remove non-converged models from results
  if (isTRUE(rm_nc)){

    results <- results %>%
      dplyr::filter(.data$`Convergence Warnings` == 0)

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
                                link,
                                control,
                                REML,
                                cutoff,
                                positive,
                                metrics,
                                rm_nc,
                                parallel,
                                model_verbose){

  # Set default arguments
  link <- default_link(link, family = family)
  control <- default_control(control, family = family, link = link)
  if(!is_logical_scalar_not_na(REML))
    stop("cross_validate(): 'REML' must be either TRUE or FALSE.")
  if(!is_logical_scalar_not_na(model_verbose))
    stop("cross_validate(): 'model_verbose' must be either TRUE or FALSE.")
  if (!is.character(family))
    stop("cross_validate(): 'family' must have type character.")

  results <- cross_validate_list(
    data = data,
    formulas = formulas,
    model_fn = basics_model_fn,
    predict_fn = basics_predict_fn,
    hyperparameters = basics_hparams(
      REML = REML, link = link, control = control,
      model_verbose = model_verbose, family = family),
    family = family,
    fold_cols = fold_cols,
    cutoff = cutoff,
    positive = positive,
    metrics = metrics,
    info_cols = list("Singular Fit Messages" = TRUE,
                     "HParams" = FALSE),
    rm_nc = FALSE, # Done below instead
    verbose = model_verbose,
    parallel_ = parallel,
    caller = "cross_validate()"
    ) %>%
    tibble::add_column("Link" = link,
                       .after = "Family")

  # If asked to remove non-converged models from results
  if (isTRUE(rm_nc)){

    results <- results %>%
      dplyr::filter(.data$`Convergence Warnings` == 0)

  }

  results

}


### Basics, shared between validate and cross_validate

basics_hparams <- function(REML, link, control, model_verbose, family){
  list(
  "REML" = REML,
  "link" = link,
  "control" = list(control),
  "model_verbose" = model_verbose,
  "family" = family,
  "is_special_fn" = TRUE)
}

basics_model_fn <- function(train_data, formula, hyperparameters){

  # Extract hyperparameters
  REML <- hyperparameters[["REML"]]
  link <- hyperparameters[["link"]]
  control <- hyperparameters[["control"]][[1]]
  model_verbose <- hyperparameters[["model_verbose"]]
  family_ <- hyperparameters[["family"]]

  # Check for random effects
  contains_random_effects <- rand_effects(as.character(formula))

  # Identify model function
  if (family_ == "gaussian")
    model_type <- "lm"
  if (family_ == "binomial")
    model_type <- "glm"
  if (isTRUE(contains_random_effects)) model_type <- paste0(model_type, "er")

  # Fitting the correct model

  # Checks the model_type and fits the model on the train_data
  if (model_type == 'lm'){

    # Fit the model using lm() or glm() depending on link function
    # Return this model to model_temp
    if (is.null(link) || link == 'identity'){

      message_if_model_verbose(model_verbose,
                               msg = "Used lm() to fit the model.'")

      return(lm(formula = formula, data = train_data))

    } else {

      message_if_model_verbose(model_verbose,
                               msg = "Used glm() to fit the model.'")

      return(glm(formula = formula, data = train_data,
                 family = gaussian(link = link)))
    }

  } else if (model_type == 'lmer'){

    # Fit the model using lmer() or glmer() depending on link function
    if (is.null(link) || link == 'identity'){

      message_if_model_verbose(model_verbose,
                               msg = "Used lme4::lmer() to fit the model.'")

      return(lme4::lmer(formula = formula, data = train_data,
                        REML = REML, control = control))

    } else {

      message_if_model_verbose(model_verbose,
                               msg = "Used lme4::glmer() to fit the model.'")

      return(lme4::glmer(formula = formula, data = train_data,
                         family = gaussian(link = link),
                         control = control))
    }

  } else if (model_type == 'glm'){

    # Fit the model using glm()

    if (!is.null(link)){

      message_if_model_verbose(model_verbose,
                               msg = "Used glm() to fit the model.'")

      return(glm(formula = formula, data = train_data,
                 family = binomial(link = link)))

    } else {

      message_if_model_verbose(model_verbose,
                               msg = "Used glm() to fit the model.'")

      return(glm(formula = formula, data = train_data,
                 family = family_))

    }

  } else if (model_type == 'glmer'){

    message_if_model_verbose(
      model_verbose,
      msg = "Used lme4::glmer() to fit the model.'")

    # Fit the model using glmer()
    # Return this model to model_temp

    return(lme4::glmer(
      formula = formula,
      data = train_data,
      family = binomial(link = link),
      control = control
    ))

  }
}

basics_predict_fn <- function(test_data, model, formula, hyperparameters){

  # Extract family from hyperparameters
  family_ <- hyperparameters[["family"]]

  # Note: The same predict_fn can be used for (g)lm and (g)lmer
  if (family_ == "gaussian"){
    p_fn <- example_predict_functions("lm")
  } else if (family_ == "binomial"){
    p_fn <- example_predict_functions("glm_binomial")
  }

  p_fn(test_data = test_data, model = model,
       formula = formula, hyperparameters = hyperparameters)
}

message_if_model_verbose <- function(model_verbose, msg, caller = "cross_validate()"){
  if (model_verbose == TRUE){
    message(paste0(caller, ": ", msg))
  }
}

