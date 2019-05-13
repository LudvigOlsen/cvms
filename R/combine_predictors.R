
#' Create model formulas with all combinations of the fixed effects
#' Random effects will be added as supplied
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

  if (!is.character(fixed_effects)){
    stop("fixed_effects must be of type character.")
  }

  # Find number of fixed effects
  if (!is.null(max_fixed_effects)){
    n_fixed_effects <- max_fixed_effects
  } else {
    n_fixed_effects <- length(fixed_effects)
  }

  effect_combinations <- plyr::ldply(1:n_fixed_effects, function(i){
    as.data.frame(t(combn(fixed_effects, i)), stringsAsFactors=FALSE)
  })

  effect_combinations[is.na(effect_combinations)] <- "__NA__"

  operator_combinations <- t(combn(rep(c("+","*"), n_fixed_effects, each=TRUE), n_fixed_effects-1)) %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    dplyr::distinct()

  if (!is.null(max_interaction_size)){
    operator_combinations <-  operator_combinations %>%
      dplyr::mutate(n_interactions = purrr::pmap_dbl(., get_max_nway_interaction)) %>%
      dplyr::filter(.data$n_interactions <= max_interaction_size) %>%
      dplyr::select(-.data$n_interactions)
  }

  formulas <- purrr::pmap_df(effect_combinations, create_formulas,
                             prepared_operators=prepare_operators(operator_combinations)) %>%
    dplyr::mutate(NAs = stringr::str_count(.data$Formula, "__NA__"),
                  Formula = stringr::str_sub(.data$Formula, 1, ifelse(NAs > 0, -9*NAs, -1)),
                  Formula = trimws(.data$Formula, "b")) %>%
                  dplyr::pull(.data$Formula) %>%
                  unique()

  if (is.null(random_effects)){
    return( paste0(dependent, " ~ ", formulas) )
  } else {
    return( paste0(dependent, " ~ ", formulas, " + ", random_effects) )
  }

}

prepare_operators <- function(operators){
  operators$extra <- " "
  as.data.frame(t(operators), stringsAsFactors=FALSE)
}

create_formulas <- function(..., prepared_operators){

  effects <- unname(c(...))

  if (nrow(prepared_operators) - length(effects) != 0){
    stop("Number of columns in prepared_operators should be length(effects).")
  }

  formulas <- prepared_operators %>%
    dplyr::mutate("effects_" = effects) %>%
    dplyr::select(.data$effects_, dplyr::everything()) %>%
    tidyr::gather(key="combi",value="op", -dplyr::one_of("effects_")) %>%
    dplyr::mutate(pasted = paste0(.data$effects_," ",op," ")) %>%
    groupdata2::group(n=length(effects), method = "greedy") %>%
    dplyr::summarise(formula_ = trimws(paste0(.data$pasted, collapse = ""), "r")) %>%
    dplyr::pull(.data$formula_) %>%
    tibble::enframe(name=NULL, value="Formula")

  formulas
}


# Finds the largest interaction
# Example:
#   get_max_nway_interaction(c("*","+","+","+","+","*","*","+","*","*","*")))
#   returns: 3
# ... are all the operators as arguments
# Because that works with pmap
get_max_nway_interaction <- function(...){
  ops <- unname(c(...))

  if ("*" %ni% ops) return(0)

  rle_ <- rle(ops)
  max(rle_$lengths[rle_$values == "*"])
}


