
#' @importFrom plyr ldply
#' @importFrom dplyr mutate %>%
#' @importFrom tidyr separate
cross_validate_list = function(data, model_list, folds_col = '.folds', family='gaussian',
                               link = NULL, control = NULL, REML=FALSE,
                               cutoff=0.5, positive=1, rmnc = FALSE, model_verbose=FALSE){

  # If link is NULL we pass it
  # the default link function for the family
  link <- default_link(link, family)

  # cross_validate() all the models using ldply()
  model_cvs_df = ldply(model_list,.fun = function(x){

    cross_validate_single(data = data, model = x, folds_col = folds_col,
                          family = family, link = link, control=control,
                          REML = REML, cutoff = cutoff,
                          positive = positive,
                          model_verbose = model_verbose)

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
