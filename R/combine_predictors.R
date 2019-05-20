#' @title Combine predictors to create model formulas
#' @description Create model formulas with every combination
#'  of your fixed effects, along with the dependent variable and random effects.
#'  Be aware of exponential time increase with number of fixed effects.
#' @return
#'  List of model formulas.
#'
#'  E.g.:
#'
#'  \code{c("y ~ x1 + (1|z)", "y ~ x2 + (1|z)",
#'  "y ~ x1 + x2 + (1|z)", "y ~ x1 * x2 + (1|z)")}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @param dependent Name of dependent variable. (Character)
#' @param fixed_effects List of fixed effects. (Character)
#' @param random_effects List of random effects. (Character)
#' @param max_fixed_effects Maximum number of fixed effects in a model formula. (Integer)
#'
#'   Due to the exponential time increase with number of fixed effects,
#'   it can help to restrict the number of fixed effects.
#' @param max_interaction_size Maximum number of interaction terms in a row. (Integer)
#'
#'  The model formula can contain more interaction terms (I.e. "\code{*}") but will need a "\code{+}" in-between.
#'
#'  Use this to limit the \code{n}-way interactions allowed (where \code{n == max_interaction_size + 1}).
#' @examples
#' # Attach libraries
#' library(cvms)
#'
#' # Create effect names
#' dependent <- "y"
#' fixed_effects <- c("a","b","c","d")
#' random_effects <- "(1|e)"
#'
#' # Create model formulas
#' combine_predictors(dependent, fixed_effects,
#'                    random_effects)
#' @importFrom purrr pmap_dbl pmap_df
#' @importFrom rlang .data
#' @importFrom utils combn
#' @importFrom combinat permn
combine_predictors <- function(dependent, fixed_effects,
                               random_effects=NULL,
                               max_fixed_effects=NULL,
                               max_interaction_size=NULL){

  # Check inputs
  if (is.null(dependent)){
    stop("Please specify dependent variable.")
  }

  if (is.null(fixed_effects) || length(fixed_effects) == 0){
    stop("Please specify vector/list of fixed_effects.")
  }

  if (!is.null(max_interaction_size) && !is.numeric(max_interaction_size)){
    stop("max_interactions must be scalar or NULL.")
  }

  if (!is.null(max_fixed_effects) && !is.numeric(max_fixed_effects)){
    stop("max_fixed_effects must be scalar or NULL.")
  }

  if (!is.null(random_effects) && !is.character(random_effects)){
    stop("random_effects must be either a string or NULL. Example: '(1|x)'.")
  }

  # Find number of fixed effects
  if (!is.null(max_fixed_effects)){
    n_fixed_effects <- max_fixed_effects
  } else {
    n_fixed_effects <- length(fixed_effects)
  }

  should_contain_interactions <- is.null(max_interaction_size) || max_interaction_size != 0

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

  ## Operator combinations (with and without interactions)

  operator_combinations <- t(combn(rep(c(" + "," * "), n_fixed_effects, each=TRUE), n_fixed_effects-1)) %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    dplyr::distinct() %>%
    dplyr::mutate(n_interactions = purrr::pmap_dbl(., get_max_nway_interaction))

  operator_combinations_no_interactions <- operator_combinations %>%
    dplyr::filter(.data$n_interactions == 0) %>%
    dplyr::select(-.data$n_interactions)

  operator_combinations_interactions <- operator_combinations %>%
    dplyr::filter(.data$n_interactions > 0)

  if (!is.null(max_interaction_size)){
    operator_combinations_interactions <- operator_combinations_interactions %>%
      dplyr::filter(.data$n_interactions <= max_interaction_size) %>%
      dplyr::select(-.data$n_interactions)
  }


  ## Effect combinations, with and without interactions

  # Without interactions
  effect_combinations <- plyr::ldply(1:n_fixed_effects, function(i){
    as.data.frame(t(combn(fixed_effects, i)), stringsAsFactors=FALSE)
  })

  effect_combinations[is.na(effect_combinations)] <- "__NA__"

  # With interactions
  if (should_contain_interactions){
    col_combinations <- permn(colnames(effect_combinations)[-1])
    effect_combinations_interactions <- plyr::ldply(1:length(col_combinations), function(i){
      col_combination <- c("V1", col_combinations[[i]])
      effect_combinations %>%
        dplyr::select(dplyr::one_of(col_combination)) %>%
        dplyr::rename_all(list(~paste0("V",c(1:n_fixed_effects))))
    }) %>% dplyr::distinct() %>%
      dplyr::mutate(NA_left_of_value = purrr::pmap_dbl(., contains_NA_left_of_value)) %>%
      dplyr::filter(!.data$NA_left_of_value) %>%
      dplyr::select(-.data$NA_left_of_value)

    # Create the interactions map
    # I.e.
    #   for each interaction : [one permutation -> all permutations]
    interactions_map <- generate_interactions_map(effect_combinations_interactions)

    colnames(effect_combinations_interactions) <- paste0("eff_", 1:n_fixed_effects)
    colnames(operator_combinations_interactions) <- paste0("op_", 1:(n_fixed_effects-1))
  }

  colnames(effect_combinations) <- paste0("eff_", 1:n_fixed_effects)
  colnames(operator_combinations_no_interactions) <- paste0("op_", 1:(n_fixed_effects-1))
  efx_cols <- colnames(effect_combinations)
  ops_cols <- colnames(operator_combinations_no_interactions)

  # Combine column names c(eff_1, op_1, eff_2, op_2, eff_3), etc.
  combined_col_names <- plyr::llply(1:n_fixed_effects, function(i){
    if (i < n_fixed_effects){
      c(efx_cols[[i]], ops_cols[[i]])
    } else {
      efx_cols[[i]]
    }
  }) %>% unlist()

  # Create the formula data frame with no interactions
  formulas_df_no_interactions <- tidyr::crossing(effect_combinations, operator_combinations_no_interactions) %>%
    dplyr::select(dplyr::one_of(combined_col_names)) %>%
    dplyr::distinct()

  if (should_contain_interactions){
    # Create the formula data frame with interactions
    # Add the rows with no interactions
    formulas_df <- tidyr::crossing(effect_combinations_interactions, operator_combinations_interactions) %>%
      dplyr::select(dplyr::one_of(combined_col_names)) %>%
      dplyr::distinct()

    # Deduplicate rows with the same interactions (including permutations).
    formulas_df <- remove_duplicates_and_rows_without_interactions(formulas_df, interactions_map) %>%
      dplyr::bind_rows(formulas_df_no_interactions)
  } else {
    formulas_df <- formulas_df_no_interactions
  }

  # Add formula versions with the interchangeable effects
  if (!is.null(interchangeable_effects_combinations)){
    formulas_df <- tidyr::crossing(formulas_df,
                                   interchangeable_effects_combinations) %>%
      tidyr::gather(key="to_replace", value="replacement",
                    (ncol(formulas_df)+1):(ncol(formulas_df)+ncol(interchangeable_effects_combinations))) %>%
      dplyr::mutate_at(.vars = 1:ncol(formulas_df),
                       .funs = list(~stringr::str_replace_all(., .data$to_replace,
                                                              as.character(.data$replacement)))) %>%
      dplyr::select(-dplyr::one_of("to_replace", "replacement")) %>%
      dplyr::bind_rows(formulas_df) %>%
      dplyr::distinct()
  }

  # Convert formulas data frame to actual formula strings.
  formulas <- formulas_df %>%
    dplyr::mutate(Formula = purrr::pmap_chr(., paste0)) %>%
    dplyr::mutate(NAs = stringr::str_count(.data$Formula, "__NA__"),
                  Formula = stringr::str_sub(.data$Formula, 1, ifelse(.data$NAs > 0, -9*.data$NAs, -1)),
                  Formula = trimws(.data$Formula, "b")) %>%
    dplyr::arrange(.data$Formula) %>%
    dplyr::pull(.data$Formula) %>%
    unique()

  if (is.null(random_effects)){
    return( paste0(dependent, " ~ ", formulas) )
  } else {
    return( paste0(dependent, " ~ ", formulas, " + ", random_effects) )
  }

}


#' Deduplicates rows with the same interactions (including permutations).
#' Removes rows without interactions, as we got these covered already.
remove_duplicates_and_rows_without_interactions <- function(formulas_df, interactions_map){

  # Create "flag"/mask data frame with 1 if a term or interaction is in the formula
  # and NA if not
  # Replace interactions with their value in interactions_map
  formulas_term_flags <- formulas_df %>%
    purrr::pmap_dfr(., find_interactions, interactions_map=interactions_map)

  # Get column names (predictors and interactions)
  # Interactions are called "pred1.|_|.pred2" to avoid conflicts with user names
  all_flag_cols <- colnames(formulas_term_flags)
  flag_cols_with_interactions <- all_flag_cols[stringr::str_detect(all_flag_cols, ".|_|.")]

  # Add term flag columns to formulas data frame
  # Remove duplicate rows (by the flag columns)
  # If two rows has the same interaction, but in a different permutation
  # this will catch it and remove it!
  formulas_df <- formulas_df %>%
    dplyr::bind_cols(formulas_term_flags) %>%
    dplyr::distinct(!!! rlang::syms(all_flag_cols), .keep_all = TRUE)

  # Remove columns with no interactions
  formulas_df <- formulas_df[rowSums(is.na(formulas_df[,flag_cols_with_interactions])) != length(flag_cols_with_interactions),] %>%
    dplyr::select(-dplyr::one_of(all_flag_cols))

  formulas_df
}

#' Finds the interactions in a formula row.
#' Replaces interactions by value in interactions_map.
#' Returns data frame row with effects and interactions as columns
#' and "1" as value.
find_interactions <- function(..., interactions_map){
  formula_parts <- unname(c(...))

  # Concatenate formula parts and replace * with .|_|.
  pasted <- paste0(formula_parts, collapse = "") %>%
    stringr::str_replace_all(" \\* ", ".|_|.")

  # Split by " + "
  parts <- stringr::str_split(pasted, " \\+ ") %>% unlist()

  # Replace interactions by their values in the
  # interaction map
  parts <- plyr::llply(parts, function(part){
    if (stringr::str_detect(part, stringr::fixed(".|_|."))){
      NAs <- sum(stringr::str_count(part, "__NA__"))*11
      if (NAs >= nchar(part)) return(NULL)
      part <- stringr::str_sub(part, 1, (nchar(part) - NAs))
      return(interactions_map[[part]])
    } else {
      if (part == "__NA__") { return(NULL) }
      return(part)
    }
  }) %>% unlist()

  # Return data frame row with formula parts as columns
  # and the value 1 in all columns
  vals_to_cols(parts, fill_with=1)

}

#' Generate interactions map with all permutations of
#' an interaction being mapped to one of the permutations
generate_interactions_map <- function(possible_interactions){

  # Create data frame where the effects are columns
  # with the value 1 if present in original row and NA otherwise
  possible_interactions_effects <- possible_interactions %>%
    purrr::pmap_dfr(., vals_to_cols)

  # Combine effects and the "included effects" flag data frame
  # Arrange by the new columns
  possible_interactions <- possible_interactions %>%
    dplyr::bind_cols(possible_interactions_effects) %>%
    dplyr::arrange(!!! rlang::syms(colnames(possible_interactions_effects)))

  # Extract the sorted flag columns
  possible_interactions_effects <-  possible_interactions %>%
    dplyr::select(dplyr::one_of(colnames(possible_interactions_effects))) %>%
    dplyr::select(-dplyr::one_of("__NA__"))

  # Extract the keys for the map
  # I.e. One permutation of each possible interaction
  possible_interactions_effects_keys <-  possible_interactions_effects %>%
    dplyr::mutate(key = purrr::pmap_chr(., paste_as_interactions)) %>%
    dplyr::pull(.data$key)

  # Extract all possible interactions for the map
  # This includes all possible permutations
  possible_interactions <- possible_interactions %>%
    dplyr::select(-dplyr::one_of(c("__NA__", colnames(possible_interactions_effects)))) %>%
    dplyr::mutate(interaction = purrr::pmap_chr(., paste_as_interactions)) %>%
    dplyr::pull(.data$interaction)

  # Create map (named list)
  setNames(as.list(possible_interactions_effects_keys), possible_interactions)

}

#' Create data frame row with values as columns.
#' Fill with the values or a specified value.
vals_to_cols <- function(..., fill_with=NULL){
  vals <- unname(c(...))

  if (is.null(fill_with)){
    fill_with <- vals
  }

  setNames(data.frame(matrix(data=fill_with, ncol = length(vals), nrow = 1), stringsAsFactors = FALSE),
           c(vals))
}

#' Paste effects as interactions.
#' Collapse with ".|_|." instead of " * ".
paste_as_interactions <- function(...){
  effects_ <- unname(c(...))
  effects_ <- effects_[effects_ != "__NA__"]
  effects_ <- effects_[!is.na(effects_)]
  paste0(effects_, collapse = ".|_|.")
}

#' Finds the largest interaction.
get_max_nway_interaction <- function(...){
  ops <- unname(c(...))

  if (" * " %ni% ops) return(0)

  rle_ <- rle(ops)
  max(rle_$lengths[rle_$values == " * "])
}

#' Detect if a row has a "__NA__" value left
#' of a non-"__NA__" value.
contains_NA_left_of_value <- function(...){
  r <- unname(c(...))

  rle_ <- rle(r)

  total_NAs <- sum(stringr::str_count(rle_$values, "__NA__"))

  if (total_NAs == 0) {
    return(FALSE)
  } else if (total_NAs > 1) {
    return(TRUE)
  }

  if (rle_$values[length(rle_$values)] != "__NA__"){
    return(TRUE)
  }
  return(FALSE)
}

#' Creates map of interchangeable effects and their combinations.
#' Removes all but the first interchangeable effects from fixed_effects.
#' With this, we can create our formulas with one combination of effects
#' and add versions with the combinations of interchangeable effects afterwards.
create_interchangeable_effects_combinations <- function(fixed_effects){

  # Check if any element is a list with more than one element
  contains_interchangeable_effects <- plyr::llply(fixed_effects, function(x) {
    is.list(x) && length(x) > 1
  }) %>%
    unlist()

  if (any(contains_interchangeable_effects)){
    map_of_interchangeable_effects <- plyr::llply(fixed_effects[contains_interchangeable_effects], function(x) {
      key <- x[[1]]
      values <- setNames(list(x[-1]), key)
      values
    }) %>%
      unlist(recursive=FALSE)

    fixed_effects <- c(unlist(fixed_effects[!contains_interchangeable_effects]),
                       names(map_of_interchangeable_effects))

    interchangeable_effects_combinations <- setNames(expand.grid(map_of_interchangeable_effects),
                                                     names(map_of_interchangeable_effects))

  } else {
    fixed_effects <- unlist(fixed_effects)
    interchangeable_effects_combinations <- NULL
  }

  return(list("fixed_effects" = fixed_effects,
              "interchangeable_effects_combinations" = interchangeable_effects_combinations))

}
