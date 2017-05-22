
#' @importFrom plyr ldply
#' @importFrom dplyr mutate %>%
#' @importFrom tidyr separate
cross_validate_list = function(data, model_list, folds_col = '.folds', family='gaussian', REML=FALSE,
                               cutoff=0.5, positive=1, rmnc = FALSE, model_verbose=FALSE){


  # cross_validate() all the models using ldply()
  model_cvs_df = ldply(model_list,.fun = function(x){

    cross_validate_single(data, x, folds_col = folds_col,
                          family=family, REML=REML,
                          cutoff=cutoff, positive=positive,
                          model_verbose=model_verbose)

    }) %>% tibble::as_tibble()


  # Now we want to take the model from the model_list and split it up into
  # fixed effects and random effects
  # Some users might want to mix models with an without random effects,
  # and so we first try to seperate into fixed and random,
  # and if no random effects are found for any of the models,
  # we remove the column "random".
  # Models without random effects will get NA in the random column.


  # First we create a dataframe with the list of models
  mixed_effects = tibble::tibble("model" = as.character(model_list))

  suppressWarnings((
    # Then we use tidyr() to create a pipeline
    mixed_effects = mixed_effects %>%

      # First remove all whitespaces
      mutate(model = gsub("\\s", "", .$model)) %>%

      # Seperate model into dependent variable and predictors
      tidyr::separate(model, into = c("Dependent", "Predictors"), sep = "~") %>%

      # Then we separate the model by "("  - because: diagnosis + (1|subject)
      # The first part is placed in a column called "fixed"
      # the other part in a column called "random"
      # We use "extra = 'merge'" to only split into the 2 given parts
      # as it will elsewise split the string whenever "\\(" occurs
      tidyr::separate(Predictors, into = c("Fixed", "Random"), sep = "\\(", extra = "merge") %>%


      # Then we clean up those strings a bit
      # From fixed we remove the last "+"
      # From random we remove all "(" and ")"
      mutate(
        Fixed = gsub("[+]$", "", Fixed),
        Random = gsub('\\(|\\)','', Random)

      )))


  # If all models are without random effects,
  # drop column random
  if (all(is.na(mixed_effects$Random))){

    mixed_effects$Random = NULL

  }

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
