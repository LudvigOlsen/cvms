#' @importFrom plyr ldply
#' @importFrom dplyr mutate %>%
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

  hparams <- list(
    "REML" = REML,
    "link" = link,
    "control" = list(control),
    "model_verbose" = model_verbose,
    "family" = family,
    "is_cross_validate" = TRUE
  )

  cv_model_fn <- function(train_data, formula, hyperparameters){

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

        return(lm(formula, train_data))

      } else {

        message_if_model_verbose(model_verbose,
                                 msg = "Used glm() to fit the model.'")

        return(glm(formula, train_data,
                   family = gaussian(link = link)))
      }

    } else if (model_type == 'lmer'){

      # Fit the model using lmer() or glmer() depending on link function
      if (is.null(link) || link == 'identity'){

        message_if_model_verbose(model_verbose,
                                 msg = "Used lme4::lmer() to fit the model.'")

        return(lme4::lmer(formula, train_data, REML = REML, control = control))

      } else {

        message_if_model_verbose(model_verbose,
                                 msg = "Used lme4::glmer() to fit the model.'")

        return(lme4::glmer(formula, train_data,
                           family = gaussian(link = link),
                           control = control))
      }

    } else if (model_type == 'glm'){

      # Fit the model using glm()

      if (!is.null(link)){

        message_if_model_verbose(model_verbose,
                                 msg = "Used glm() to fit the model.'")

        return(glm(formula, train_data, family = binomial(link = link)))

      } else {

        message_if_model_verbose(model_verbose,
                                 msg = "Used glm() to fit the model.'")

        return(glm(formula, train_data, family = family_))

      }

    } else if (model_type == 'glmer'){

      message_if_model_verbose(
        model_verbose,
        msg = "Used lme4::glmer() to fit the model.'")

      # Fit the model using glmer()
      # Return this model to model_temp

      return(lme4::glmer(
        formula,
        train_data,
        family = binomial(link = link),
        control = control
      ))

    }
  }

  cv_predict_fn <- function(test_data, model, formula, hyperparameters){

    # Extract family from hyperparameters
    family_ <- hyperparameters[["family"]]

    # Note: The same predict_fn can be used for (g)lm and (g)lmer
    if (family_ == "gaussian"){
      p_fn <- example_predict_functions("lm")
    } else if (family == "binomial"){
      p_fn <- example_predict_functions("glm_binomial")
    }

    p_fn(test_data = test_data, model = model,
         formula = formula, hyperparameters = hyperparameters)
  }

  results <- cross_validate_fn(
    data = data,
    formulas = formulas,
    model_fn = cv_model_fn,
    predict_fn = cv_predict_fn,
    hyperparameters = hparams,
    type = family,
    fold_cols = fold_cols,
    cutoff = cutoff,
    positive = positive,
    metrics = metrics,
    rm_nc = FALSE, # Done below instead
    parallel = parallel
    ) %>%
    tibble::add_column("Link" = link,
                       .after = "Family")

  # Remove the hyperparameters
  # As they contain internal information (noise)
  results[["HParams"]] <- NULL

  # If asked to remove non-converged models from output
  if (isTRUE(rm_nc)){

    output <- output %>%
      dplyr::filter(.data$`Convergence Warnings` == 0)

  }

  results
}


message_if_model_verbose <- function(model_verbose, msg, caller = "cross_validate()"){
  if (model_verbose == TRUE){
    message(paste0(caller, ": ", msg))
  }
}

