# helpers for combine_predictors()


combine_predictors_prepare_args <- function(dependent,
                                            fixed_effects,
                                            random_effects,
                                            max_fixed_effects,
                                            max_interaction_size,
                                            max_effect_frequency){

  # Specify limits for max_interaction_size
  # The other limits depends on whether interactions should be included
  args_max_max_interaction_size__ <- 3   # max_interaction_size

  # Check that max_interaction_size is correctly specified
  if (!is.null(max_interaction_size) && !is.numeric(max_interaction_size)){
    stop("max_interactions_size must be numeric scalar or NULL.")
  }
  if (max_interaction_size > args_max_max_interaction_size__ ||
      max_interaction_size < 0 || is.null(max_interaction_size)){
    stop(paste0("max_interaction_size must be numeric between 0 and ",
                args_max_max_interaction_size__, "."))
  }

  # Specify limits depending on whether we should include interactions or not
  if (max_interaction_size %in% c(0,1)){
    # Without interactions
    args_max_num_fixed_effects__ <- 256    # Length of fixed_effects
    args_max_max_fixed_effects__ <- 256    # max_fixed_effects
    args_min_max_fixed_effects__ <- 2      # max_fixed_effects
    args_max_max_effect_frequency__ <- 1
  } else {
    # With interactions
    args_max_num_fixed_effects__ <- 8      # Length of fixed_effects
    args_max_max_fixed_effects__ <- 5      # max_fixed_effects
    args_min_max_fixed_effects__ <- 2      # max_fixed_effects
    args_max_max_effect_frequency__ <- args_max_max_fixed_effects__
  }

  ## Check inputs

  # Check dependent variable is correctly specified
  if (is.null(dependent) || !is.character(dependent)){
    stop("Please specify the name of the dependent variable.")
  }

  if (!is.null(random_effects) && !is.character(random_effects)){
    stop("random_effects must be either a string or NULL. Example: '(1|x)'.")
  }

  # Check that fixed_effects is correctly specified
  if (is.null(fixed_effects) || length(fixed_effects) < 2){
    stop("Please specify vector/list of at least 2 fixed_effects.")
  }

  # Check that we have max. 8 fixed effects
  if (length(fixed_effects) > args_max_num_fixed_effects__){
    stop(paste0("fixed_effects contained more elements than the max. limit of ",
                args_max_num_fixed_effects__, "."))
  }

  # Check that max_fixed_effects is correctly specified
  if (!is.null(max_fixed_effects) && (!is.numeric(max_fixed_effects) || length(max_fixed_effects)>1)){
    stop("max_fixed_effects must be scalar or NULL.")
  }

  # Set max_fixed_effects if it is null
  if (is.null(max_fixed_effects)){
    max_fixed_effects <- min(args_max_max_fixed_effects__, length(fixed_effects))
  }

  # Check that max_fixed_effects is in the correct range
  if (max_fixed_effects < args_min_max_fixed_effects__){
    stop(paste0("max_fixed_effects must be at least ",
                args_min_max_fixed_effects__, "."))
  } else if (max_fixed_effects > args_max_max_fixed_effects__){
    stop(paste0("max_fixed_effects was greater than the max. limit of ",
                args_max_max_fixed_effects__, "."))
  }

  # Check that max_effect_frequency is correctly specified
  if (!is.null(max_effect_frequency) && (!is.numeric(max_effect_frequency) || length(max_effect_frequency) > 1)){
    stop("max_effect_frequency must be scalar or NULL.")
  }

  # Set max_effect_frequency if it is null
  if (is.null(max_fixed_effects)){
    max_effect_frequency <- min(max_fixed_effects, args_max_max_effect_frequency__)
  }

  # Update args
  max_fixed_effects <- min(max_fixed_effects, args_max_max_fixed_effects__, length(fixed_effects))
  max_effect_frequency <- min(max_effect_frequency, max_fixed_effects)
  n_fixed_effects <- length(fixed_effects)
  max_interaction_size <- min(max_interaction_size, max_fixed_effects)
  should_contain_interactions <- max_interaction_size %in% c(2:3)

  # Interchangeable fixed effects
  if (is.list(fixed_effects)){
    # Create map and update fixed_effects vector
    interchangeable_effects <- create_interchangeable_effects_combinations(fixed_effects)
    fixed_effects <- interchangeable_effects[["fixed_effects"]]
    interchangeable_effects_combinations <- interchangeable_effects[["interchangeable_effects_combinations"]]
  } else {
    if (!is.character(fixed_effects)){
      stop("fixed_effects must be of type character.")
    }
    interchangeable_effects_combinations <- NULL
  }

  # Check fixed effects for illegal symbols
  if (!is.null(interchangeable_effects_combinations)){
    check_fixed_effects(unlist(interchangeable_effects_combinations, recursive = TRUE))
  } else {
    check_fixed_effects(unlist(fixed_effects, recursive = TRUE))
  }

  # Sort fixed effects
  fixed_effects <- sort(fixed_effects, decreasing = FALSE)

  return(list(
    "dependent" = dependent,
    "fixed_effects" = fixed_effects,
    "random_effects" = random_effects,
    "max_fixed_effects" = max_fixed_effects,
    "max_interaction_size" = max_interaction_size,
    "max_effect_frequency" = max_effect_frequency,
    "n_fixed_effects" = n_fixed_effects,
    "should_contain_interactions" = should_contain_interactions,
    "interchangeable_effects_combinations" = interchangeable_effects_combinations
  ))

}

# Checks that a fixed effect does not contain a white space, "*" or "+"! or "\n"
check_fixed_effects <- function(fixed_effects){
  pasted <- paste0(fixed_effects, collapse="")
  # Check for white space, "*", or "+"
  if(stringr::str_detect(pasted, "[\\s|\\*|\\+]")){
    stop("At least one of the fixed effect contained a whitespace, '+' or '*'.")
  }
}



fetch_formulas <- function(n_fixed_effects,
                           max_interaction_size_,
                           max_fixed_effects_,
                           max_effect_frequency_){

  # Filter the precomputed formulas table
  # Order by size of formula
  # Return formulas as tibble

  # Predefine variable to avoid NSE notes in R CMD check
  min_num_fixed_effects <- max_interaction_size <- num_effects <- NULL
  max_effect_frequency <- nchars <- formula_ <- NULL

  # Filter precomputed formulas
  formulas_table <- data.table(cvms::precomputed.formulas)[
    min_num_fixed_effects <= n_fixed_effects &
      max_interaction_size <= max_interaction_size_ &
      num_effects <= max_fixed_effects_ &
      max_effect_frequency <= max_effect_frequency_
  ][, nchars := nchar(formula_)]
  setorder(formulas_table, nchars)
  formulas_table <- formulas_table[, formula_]

  tibble::enframe(formulas_table, name = NULL, value="formula_")

}

