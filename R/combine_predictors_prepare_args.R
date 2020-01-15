# helpers for combine_predictors()


#   __________________ #< f90d404d71c20ca708231d17fc2aa919 ># __________________
#   Prepare arguments                                                       ####


combine_predictors_prepare_args <- function(dependent,
                                            fixed_effects,
                                            random_effects,
                                            max_fixed_effects,
                                            max_interaction_size,
                                            max_effect_frequency){

  # Specify limits for max_interaction_size
  # The other limits depends on whether interactions should be included
  args_max_max_interaction_size__ <- 3   # max_interaction_size

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

  # Set max_fixed_effects if it is null
  if (is.null(max_fixed_effects)){
    max_fixed_effects <- min(args_max_max_fixed_effects__, length(fixed_effects))
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


#   __________________ #< 7f9a244895e07c7868b5f3dbb9c7d5db ># __________________
#   Check fixed effects                                                     ####


# Checks that a fixed effect does not contain a white space, "*" or "+"! or "\n"
check_fixed_effects <- function(fixed_effects){
  pasted <- paste0(fixed_effects, collapse="")
  # Check for white space, "*", or "+"
  if(stringr::str_detect(pasted, "[\\s|\\*|\\+]")){
    stop("At least one of the fixed effect contained a whitespace, '+' or '*'.")
  }
}


#   __________________ #< 6dd73511abb4b3cd09b51033479a70eb ># __________________
#   Fetch formulas                                                          ####


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

  tibble::enframe(formulas_table, name = NULL, value = "formula_")

}

