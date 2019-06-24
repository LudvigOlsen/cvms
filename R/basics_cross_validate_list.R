# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @importFrom plyr ldply
#' @importFrom dplyr mutate %>%
#' @importFrom tidyr separate
basics_cross_validate_list = function(data, model_list, fold_cols = '.folds', family='gaussian',
                               link = NULL, control = NULL, REML=FALSE,
                               cutoff=0.5, positive=2, rm_nc = FALSE,
                               model_verbose=FALSE, parallel_ = FALSE,
                               parallelize = "models"){

  # If link is NULL we pass it
  # the default link function for the family
  # link <- default_link(link, family) # Is done at a later step

  # Set errors if input variables aren't what we expect / can handle
  # WORK ON THIS SECTION!
  stopifnot(is.data.frame(data),
            is.character(positive) || positive %in% c(1,2),
            family %in% c("gaussian", "binomial")
  )

  # Check that the folds column(s) is/are factor(s)
  if (length(fold_cols) == 1){
    stopifnot(is.factor(data[[fold_cols]]))
  } else {
    fcols <- data %>% dplyr::select(dplyr::one_of(fold_cols)) %>%
      sapply(is.factor)
    if (FALSE %in% fcols) {stop("At least one of the fold columns is not a factor.")}
  }

  # Get evaluation functions
  if (family == "gaussian"){
    evaluation_type = "linear_regression"
  } else if (family == "binomial"){
    evaluation_type = "binomial"
  } else {stop("Only two families allowed currently!")}

  # Create model_specifics object
  # Update to get default values when an argument was not specified
  model_specifics <- list(
    model_formula="",
    family=family,
    REML=REML,
    link=link,
    cutoff = cutoff,
    positive = positive,
    model_verbose = model_verbose) %>%
    basics_update_model_specifics()

  # cross_validate() all the models using ldply()
  model_cvs_df = ldply(model_list, .parallel = all(parallel_, parallelize == "models"), .fun = function(model_formula){
    model_specifics[["model_formula"]] <- model_formula
    cross_validate_fn_single(data = data, model_fn = basics_model_fn,
                             evaluation_type = evaluation_type,
                             model_specifics = model_specifics,
                             model_specifics_update_fn = NULL, # did this above
                             fold_cols = fold_cols,
                             parallel_ = all(parallel_, parallelize == "folds"))
    }) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Family = model_specifics[["family"]],
                  Link = model_specifics[["link"]])

  # Now we want to take the model from the model_list and split it up into
  # fixed effects and random effects
  # Some users might want to mix models with an without random effects,
  # and so we first try to seperate into fixed and random,
  # and if no random effects are found for any of the models,
  # we remove the column "random".
  # Models without random effects will get NA in the random column.

  mixed_effects <- extract_model_effects(model_list)

  # we put the two data frames together
  output <- dplyr::bind_cols(model_cvs_df, mixed_effects)

  # If asked to remove non-converged models from output
  if (isTRUE(rm_nc)){

    output <- output %>%
      dplyr::filter(.data$`Convergence Warnings` == 0)

  }

  # and return it
  return(output)

}
