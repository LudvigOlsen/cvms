
#' @importFrom dplyr mutate %>%
#' @importFrom tidyr separate
basics_validate_list = function(train_data, model_list, family='gaussian',
                                link = NULL, control = NULL, REML=FALSE,
                                cutoff=0.5, positive=1, err_nc = FALSE,
                                rm_nc = FALSE, test_data = NULL,
                                partitions_col = '.partitions',
                                model_verbose=FALSE){

  # If train and test data is not already split,
  # get train and test set
  if (is.null(test_data)) {
    # Create test set
    test_data = train_data[train_data[[partitions_col]] == 2,]
    # Create training set
    train_data = train_data[train_data[[partitions_col]] == 1,]
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

  # validate() all the models using ldply()
   validation_output = plyr::llply(model_list,.fun = function(model_formula){
    model_specifics[["model_formula"]] <- model_formula
    validate_fn_single(train_data=train_data,
                                   model_fn = basics_model_fn,
                                   evaluation_type = evaluation_type,
                                   model_specifics = model_specifics,
                                   model_specifics_update_fn = NULL,
                                   test_data = test_data,
                                   partitions_col = partitions_col,
                                   err_nc = err_nc)
  })

  results_list <- validation_output %c% "Results"
  results <- results_list %>%
    dplyr::bind_rows() %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Family = model_specifics[["family"]],
                  Link = model_specifics[["link"]])

  models <- validation_output %c% "Model"


  # Now we want to take the model from the model_list and split it up into
  # fixed effects and random effects
  # Some users might want to mix models with an without random effects,
  # and so we first try to seperate into fixed and random,
  # and if no random effects are found for any of the models,
  # we remove the column "random".
  # Models without random effects will get NA in the random column.

  mixed_effects <- extract_model_effects(model_list)

  # we put the two dataframes together
  output <- dplyr::bind_cols(results, mixed_effects)

  # If asked to remove non-converged models from output
  if (isTRUE(rm_nc)){

    output <- output %>%
      dplyr::filter(`Convergence Warnings` == 0)

  }

  return(list("Results" = output, "Models" = models))

}
