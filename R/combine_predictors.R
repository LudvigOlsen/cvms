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

  operator_combinations <- t(combn(rep(c(" + "," * "), n_fixed_effects, each=TRUE), n_fixed_effects-1)) %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    dplyr::distinct()

  if (!is.null(max_interaction_size)){
    operator_combinations <-  operator_combinations %>%
      dplyr::mutate(n_interactions = purrr::pmap_dbl(., get_max_nway_interaction)) %>%
      dplyr::filter(.data$n_interactions <= max_interaction_size) %>%
      dplyr::select(-.data$n_interactions)
  }

  colnames(effect_combinations) <- paste0("eff_", 1:n_fixed_effects)
  colnames(operator_combinations) <- paste0("op_", 1:(n_fixed_effects-1))
  efx_cols <- colnames(effect_combinations)
  ops_cols <- colnames(operator_combinations)

  combined_col_names <- plyr::llply(1:n_fixed_effects, function(i){
    if (i < n_fixed_effects){
      c(efx_cols[[i]], ops_cols[[i]])
    } else {
      efx_cols[[i]]
    }
  }) %>% unlist()

  formulas <- tidyr::crossing(effect_combinations, operator_combinations) %>%
    dplyr::select(dplyr::one_of(combined_col_names)) %>%
    dplyr::distinct() %>%
    dplyr::mutate(Formula = purrr::pmap_chr(., paste0)) %>%
    dplyr::mutate(NAs = stringr::str_count(.data$Formula, "__NA__"),
                  Formula = stringr::str_sub(.data$Formula, 1, ifelse(.data$NAs > 0, -9*.data$NAs, -1)),
                  Formula = trimws(.data$Formula, "b")) %>%
                  dplyr::pull(.data$Formula) %>%
                  unique()

  if (is.null(random_effects)){
    return( paste0(dependent, " ~ ", formulas) )
  } else {
    return( paste0(dependent, " ~ ", formulas, " + ", random_effects) )
  }

}


# Finds the largest interaction
# Example:
#   get_max_nway_interaction(c("*","+","+","+","+","*","*","+","*","*","*")))
#   returns: 3
# ... are all the operators as arguments
# Because that works with pmap
get_max_nway_interaction <- function(...){
  ops <- unname(c(...))

  if (" * " %ni% ops) return(0)

  rle_ <- rle(ops)
  max(rle_$lengths[rle_$values == " * "])
}


