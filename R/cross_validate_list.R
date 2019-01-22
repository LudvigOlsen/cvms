
#' @importFrom plyr ldply
#' @importFrom dplyr mutate %>%
#' @importFrom tidyr separate
cross_validate_list = function(data, model_list, folds_col = '.folds', family='gaussian',
                               link = NULL, control = NULL, REML=FALSE,
                               cutoff=0.5, positive=1, rmnc = FALSE, model_verbose=FALSE){

  # If link is NULL we pass it
  # the default link function for the family
  # link <- default_link(link, family) # Is done at a later step

  # Set errors if input variables aren't what we expect / can handle
  # WORK ON THIS SECTION!
  stopifnot(is.data.frame(data),
            is.factor(data[[folds_col]]),
            positive %in% c(1,2),
            family %in% c("gaussian", "binomial")
  )

  # Get evaluation functions
  if (family == "gaussian"){
    fold_evaluation_fn <- fold_evaluation_lm_lmer
    eval_aggregation_fn <- eval_aggregation_lm_lmer
  } else if (family == "binomial"){
    fold_evaluation_fn <- fold_evaluation_binomial_glm_glmer
    eval_aggregation_fn <- eval_aggregation_binomial_glm_glmer
  } else {stop("Only two families allowed currently!")}

  # cross_validate() all the models using ldply()
  model_cvs_df = ldply(model_list,.fun = function(model_formula){

    # cross_validate_single(data = data, model = x, folds_col = folds_col,
    #                       family = family, link = link, control=control,
    #                       REML = REML, cutoff = cutoff,
    #                       positive = positive,
    #                       model_verbose = model_verbose)

    cross_validate_fn_single(data = data, model_fn = model_fn_basics,
                             fold_eval_fn = fold_evaluation_fn,
                             eval_aggregation_fn = eval_aggregation_fn,
                             model_specifics = list(
                               model_formula=model_formula,
                               family=family,
                               REML=REML,
                               link=link,
                               cutoff = cutoff,
                               positive = positive,
                               model_verbose = model_verbose),
                             model_specifics_update_fn = update_and_check_model_specifics_basics,
                             folds_col = folds_col)

    }) %>% tibble::as_tibble()


  # Now we want to take the model from the model_list and split it up into
  # fixed effects and random effects
  # Some users might want to mix models with an without random effects,
  # and so we first try to seperate into fixed and random,
  # and if no random effects are found for any of the models,
  # we remove the column "random".
  # Models without random effects will get NA in the random column.

  mixed_effects <- extract_model_effects(model_list)

  # we put the two dataframes together
  output <- dplyr::bind_cols(model_cvs_df, mixed_effects)

  # If asked to remove non-converged models from output
  if (isTRUE(rmnc)){

    output <- output %>%
      dplyr::filter(`Convergence Warnings` == 0)

  }


  #and return it
  return(output)

}
