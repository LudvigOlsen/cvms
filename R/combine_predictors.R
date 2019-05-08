
#' Create model formulas with all combinations of the fixed effects
#' Random effects will be added as supplied
combine_predictors <- function(dependent, fixed_effects, random_effects=NULL, max_interactions=NULL){

  # Check inputs
  if (is.null(dependent)){
    stop("Please specify dependent variable.")
  }

  if (is.null(fixed_effects) || length(fixed_effects) == 0){
    stop("Please specify vector/list of fixed_effects.")
  }

  if (!is.null(max_interactions) && !is.numeric(max_interactions)){
    stop("max_interactions must be scalar or NULL.")
  }

  if (!is.null(random_effects) && !is.character(random_effects)){
    stop("random_effects must be either a string or NULL. Example: '(1|x)'")
  }

  n_fixed_effects <- length(fixed_effects)

  effect_combinations <- plyr::ldply(1:n_fixed_effects, function(i){
    as.data.frame(t(combn(fixed_effects, i)), stringsAsFactors=FALSE)
  })

  effect_combinations[is.na(effect_combinations)] <- "__NA__"

  operator_combinations <- t(combn(rep(c("+","*"), n_fixed_effects, each=TRUE), n_fixed_effects-1)) %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    dplyr::distinct()

  if (!is.null(max_interactions)){
    operator_combinations <-  operator_combinations %>%
      dplyr::mutate(n_interactions = rowSums(.=="*")) %>%
      dplyr::filter(.data$n_interactions <= max_interactions) %>%
      dplyr::select(-.data$n_interactions)
  }

  fixed_effects <- effect_combinations %>%
    map_all_rows(create_formulas, operator_combinations) %>%
    dplyr::pull(.data$Result) %>%
    unlist() %>%
    tibble::enframe(name=NULL) %>%
    dplyr::mutate(NAs = stringr::str_count(.data$value, "__NA__"),
                  value = stringr::str_sub(.data$value, 1, ifelse(NAs > 0, -9*NAs, -1)),
                  value = trimws(.data$value, "b")) %>%
    dplyr::pull(.data$value) %>%
    unique()

  if (is.null(random_effects)){
    return( paste0(dependent, " ~ ", fixed_effects) )
  } else {
    return( paste0(dependent, " ~ ", fixed_effects, " + ", random_effects) )
  }


}

create_formulas <- function(effects, operators){

  if (is.data.frame(effects) && ncol(effects) > 1){
    effects <- effects %>% t() %>% .[,1]
    names(effects) <- NULL  # use unname() ?
  }

  if (ncol(operators) - length(effects) == -1){
    operators$extra <- " "
  } else { stop("Number of columns in operators should be length(effects)-1.")}

  formulas <- as.data.frame(t(operators), stringsAsFactors=FALSE) %>%
    # dplyr::as_tibble() %>%
    dplyr::mutate("effects_" = effects, "effect_index"=1:dplyr::n()) %>%
    dplyr::select(.data$effects_, dplyr::everything()) %>%
    tidyr::gather(key="combi",value="op", -dplyr::one_of("effects_", "effect_index")) %>%
    dplyr::mutate(pasted = paste0(.data$effects_," ",op," ")) %>%
    groupdata2::group(n=length(effects), method = "greedy") %>%
    dplyr::summarise(formula_ = trimws(paste0(.data$pasted, collapse = ""), "r")) %>%
    dplyr::pull(.data$formula_)

  formulas
}

# https://stackoverflow.com/questions/46850278/dplyr-mutate-how-do-i-pass-one-row-as-a-function-argument
map_all_rows <- function(data, func, ...){
  data %>%
    dplyr::rowwise() %>%
    dplyr::do( X = as.data.frame(.) ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate( Result = purrr::map(X, func, ...) )
}




