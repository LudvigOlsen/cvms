custom_cross_validate_list = function(data,
                                      formulas,
                                      model_fn,
                                      fold_cols = '.folds',
                                      family = 'gaussian',
                                      cutoff = 0.5,
                                      positive = 2,
                                      predict_type = NULL,
                                      predict_fn = NULL,
                                      metrics = list(),
                                      rm_nc = FALSE,
                                      model_verbose = FALSE,
                                      parallel_ = FALSE,
                                      parallelize = "models") {

  # Set errors if input variables aren't what we expect / can handle
  # WORK ON THIS SECTION!
  stopifnot(is.data.frame(data),
            is.character(positive) || positive %in% c(1,2)
  )

  if (!is.null(predict_type) && !is.null(predict_fn)){
    stop("cross_validate_fn(): Both 'predict_type' and 'predict_fn' were specified. Please specify only one of them.")
  }

  # metrics

  if (!(is.list(metrics) || metrics == "all")){
    stop("'metrics' must be either a list or the string 'all'.")
  }

  if (is.list(metrics) && length(metrics) > 0){
    if (!rlang::is_named(metrics)){
      stop("when 'metrics' is a non-empty list, it must be a named list.")
    }
  }

  # Check that the fold column(s) is/are factor(s)
  if (length(fold_cols) == 1){
    stopifnot(is.factor(data[[fold_cols]]))
  } else {
    fcols <- data %>% dplyr::select(dplyr::one_of(fold_cols)) %>%
      sapply(is.factor)
    if (FALSE %in% fcols) {stop("At least one of the fold columns is not a factor.")}
  }

  # Get evaluation functions
  if (family == "gaussian"){
    evaluation_type = "gaussian"
  } else if (family == "binomial"){
    evaluation_type = "binomial"
  } else if (family == "multinomial"){
    evaluation_type = "multinomial"
  } else {stop("Only 'gaussian', 'binomial', and 'multinomial' evaluation types are currently allowed.")}

  # Create model_specifics object
  # Update to get default values when an argument was not specified
  model_specifics <- list(
    model_formula = "",
    family = family,
    REML = NULL,
    link = NULL,
    control = NULL,
    cutoff = cutoff,
    positive = positive,
    model_verbose = model_verbose,
    model_fn = model_fn,
    predict_type = predict_type,
    predict_fn = predict_fn,
    caller = "cross_validate_fn()"
  ) %>%
    custom_update_model_specifics()

  # cross_validate all the models using ldply()
  cross_validations <- plyr::llply(formulas, .parallel = all(parallel_, parallelize == "models"), .fun = function(model_formula){
    model_specifics[["model_formula"]] <- model_formula
    cross_validate_fn_single(data = data, model_fn = custom_model_fn,
                             evaluation_type = evaluation_type,
                             model_specifics = model_specifics,
                             model_specifics_update_fn = NULL, # did this above
                             fold_cols = fold_cols,
                             parallel_ = all(parallel_, parallelize == "folds"))
  })

  if (family %in% c("binomial","gaussian")){

    cross_validations_results <- cross_validations %>%
      dplyr::bind_rows() %>%
      tibble::as_tibble() %>%
      dplyr::mutate(Family = model_specifics[["family"]])

  } else if (family == "multinomial"){

    # Extract and nest class level results
    cross_validations_class_level_results <-
      plyr::ldply(cross_validations %c% "Class Level Results", function(clr) {
        legacy_nest(clr, 1:ncol(clr))
        }) %>%
      tibble::as_tibble() %>%
      dplyr::pull(.data$data)

    # Extact results and add family and class level results
    cross_validations_results <- dplyr::bind_rows(cross_validations %c% "Results") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(Family = model_specifics[["family"]]) %>%
      tibble::add_column(`Class Level Results` = cross_validations_class_level_results,
                         .before = "Predictions")
  }

  # Now we want to take the formula from the formulas and split it up into
  # fixed effects and random effects
  # Some users might want to mix models with an without random effects,
  # and so we first try to seperate into fixed and random,
  # and if no random effects are found for any of the models,
  # we remove the column "random".
  # Models without random effects will get NA in the random column.

  mixed_effects <- extract_model_effects(formulas)

  # We put the two data frames together
  output <- dplyr::bind_cols(cross_validations_results, mixed_effects)

  # If asked to remove non-converged models from output
  if (isTRUE(rm_nc)){

    output <- output %>%
      dplyr::filter(.data$`Convergence Warnings` == 0)

  }

  # and return it
  return(output)

}
