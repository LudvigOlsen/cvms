#' @title Generate model formulas by combining predictors
#' @description Create model formulas with every combination
#'  of your fixed effects, along with the dependent variable and random effects.
#'  \code{259,358} formulas have been precomputed with two- and three-way interactions
#'  for up to \code{8} fixed effects, with up to \code{5} included effects per formula.
#'  Uses the \code{+} and \code{*} operators, so lower order interactions are
#'  automatically included.
#' @return
#'  List of model formulas.
#'
#'  E.g.:
#'
#'  \code{c("y ~ x1 + (1|z)", "y ~ x2 + (1|z)",
#'  "y ~ x1 + x2 + (1|z)", "y ~ x1 * x2 + (1|z)")}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @aliases generate_formulas
#' @param dependent Name of dependent variable. (Character)
#' @param fixed_effects List of fixed effects. (Character)
#'
#'  Max. limit of \code{8} effects \strong{when interactions are included}!
#'
#'  A fixed effect name cannot contain: white spaces, \code{"*"} or \code{"+"}.
#'
#'  Effects in sublists will be interchanged. This can be useful, when
#'  we have multiple versions of a predictor (e.g. \code{x1} and \code{log(x1)}) that we
#'  do not wish to have in the same formula.
#'
#'  Example of interchangeable effects:
#'
#'  \code{list( list( "x1", "log_x1" ), "x2", "x3" )}
#' @param random_effects The random effects structure. (Character)
#'
#'  Is appended to the model formulas.
#' @param max_fixed_effects Maximum number of fixed effects in a model formula. (Integer)
#'
#'  Max. limit of \code{5} \strong{when interactions are included}!
#' @param max_interaction_size Maximum number of effects in an interaction. (Integer)
#'
#'  Max. limit of \code{3}.
#'
#'  Use this to limit the \code{n}-way interactions allowed.
#'  \code{0} or \code{1} excludes interactions all together.
#'
#'  A model formula can contain multiple interactions.
#' @param max_effect_frequency Maximum number of times an effect is included in a formula string.
#' @examples
#' # Attach libraries
#' library(cvms)
#'
#' # Create effect names
#' dependent <- "y"
#' fixed_effects <- c("a","b","c")
#' random_effects <- "(1|e)"
#'
#' \dontrun{
#' # Create model formulas
#' combine_predictors(dependent, fixed_effects,
#'                    random_effects)
#'
#' }
#' # Create effect names with interchangeable effects in sublists
#' fixed_effects <- list("a",list("b","log_b"),"c")
#'
#' \dontrun{
#' # Create model formulas
#' combine_predictors(dependent, fixed_effects,
#'                    random_effects)
#' }
#' @importFrom purrr pmap_dbl pmap_df
#' @importFrom rlang .data
#' @importFrom utils combn head
#' @importFrom stats setNames formula terms.formula
#' @import data.table
combine_predictors <- function(dependent,
                               fixed_effects,
                               random_effects = NULL,
                               max_fixed_effects = 5,
                               max_interaction_size = 3,
                               max_effect_frequency = NULL){

  args_ <- combine_predictors_prepare_args(dependent = dependent,
                                           fixed_effects = fixed_effects,
                                           random_effects = random_effects,
                                           max_fixed_effects = max_fixed_effects,
                                           max_interaction_size = max_interaction_size,
                                           max_effect_frequency = max_effect_frequency)

  dependent <- args_[["dependent"]]
  fixed_effects <- args_[["fixed_effects"]]
  random_effects <- args_[["random_effects"]]
  max_fixed_effects <- args_[["max_fixed_effects"]]
  max_interaction_size <- args_[["max_interaction_size"]]
  max_effect_frequency <- args_[["max_effect_frequency"]]
  n_fixed_effects <- args_[["n_fixed_effects"]]
  should_contain_interactions <- args_[["should_contain_interactions"]]
  interchangeable_effects_combinations <- args_[["interchangeable_effects_combinations"]]

  # Generate / fetch formulas

  if (!should_contain_interactions){

    ## Effect combinations without interactions

    # Without interactions
    effect_combinations <- plyr::ldply(1:max_fixed_effects, function(i){
      as.data.frame(t(combn(fixed_effects, i)), stringsAsFactors=FALSE)
    })

    # effect_combinations[is.na(effect_combinations)] <- "__NA__"

    # Create formulas without interactions
    formulas <- effect_combinations %>%
      dplyr::mutate(formula_ = purrr::pmap_chr(., paste_columns, collapse=" + ")) %>%
      dplyr::select(.data$formula_)

  } else {

    # Create the map between each fixed effect and its letter ("A"="efx1", etc.)
    pattern_replacement <- setNames(paste0(" ",fixed_effects),
                                    paste0("(\\s|^)",LETTERS[1:n_fixed_effects], "(?!\\S)"))

    # Fetch the precomputed formulas and replace names of the effects
    formulas <- fetch_formulas(n_fixed_effects = n_fixed_effects,
                               max_interaction_size_ = max_interaction_size,
                               max_fixed_effects_ = max_fixed_effects,
                               max_effect_frequency_ = max_effect_frequency) %>%
      dplyr::mutate(formula_ = stringr::str_replace_all(.data$formula_,
                                                        pattern_replacement),
                    formula_ = trimws(.data$formula_))

  }

  # Add formula versions with the interchangeable effects
  if (!is.null(interchangeable_effects_combinations)){

    formulas <- tidyr::crossing(formulas,
                                interchangeable_effects_combinations)

    # Replace with the interchangeable effects
    plyr::l_ply(2:ncol(formulas), function(column){
      formulas <<- formulas %>%
        dplyr::mutate_at(
          .vars = 1,
          .funs = list(~stringr::str_replace_all(
            ., colnames(formulas)[[column]],
            as.character(!! as.name(colnames(formulas)[[column]])))))
    })

    formulas <- formulas %>%
      dplyr::select(.data$formula_) %>%
      dplyr::distinct()
  }

  formulas <- formulas %>%
    dplyr::mutate(n_efxs = stringr::str_count(.data$formula_, "\\+|\\*")) %>%
    dplyr::arrange(.data$n_efxs, .data$formula_) %>%
    dplyr::pull(.data$formula_)

  if (is.null(random_effects)){
    return( paste0(dependent, " ~ ", formulas) )
  } else {
    return( paste0(dependent, " ~ ", formulas, " + ", random_effects) )
  }

}

counts_as_row <- function(...){
  dplyr::bind_rows(table(...))
}

sort_rowwise <- function(data, vars, decreasing=FALSE, na.last = TRUE){
  data[vars] <- t(apply(data[vars], 1,
                        FUN=function(x) sort(x, decreasing = decreasing, na.last=na.last)))

  data
}

# Create data frame row with values as columns.
# Fill with the values or a specified value.
# @param ... Values.
#
#  Intended to be used with
#  purrr::pmap, where ... is a row of values.
# @param fill_with Value to fill cells with.
#  If \code{NULL}, the values in \code{...} are used.
vals_to_cols <- function(..., fill_with=NULL){
  vals <- unname(c(...))

  if (is.null(fill_with)){
    fill_with <- vals
  }

  setNames(data.frame(
    matrix(data=fill_with, ncol = length(vals), nrow = 1),
    stringsAsFactors = FALSE),
    c(vals))
}

# Paste effects as interactions.
# Collapse with ".|_|." instead of " * ".
# @param ... Values.
#
#  Intended to be used with
#  purrr::pmap, where ... is a row of values.
paste_columns <- function(..., collapse=" * "){
  effects_ <- unname(c(...))
  effects_ <- effects_[effects_ != "__NA__"]
  effects_ <- effects_[!is.na(effects_)]
  effects_ <- sort(effects_, decreasing = FALSE)
  paste0(effects_, collapse = collapse)
}


# Detect if a row has a "__NA__" value left
# of a non-"__NA__" value.
# @param ... Values.
#
#  Intended to be used with
#  purrr::pmap, where ... is a row of values.
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

# Creates map of interchangeable effects and their combinations.
# Removes all but the first interchangeable effects from fixed_effects.
# With this, we can create our formulas with one combination of effects
# and add versions with the combinations of interchangeable effects afterwards.
# @param fixed_effects Vector of fixed effects. (Character)
create_interchangeable_effects_combinations <- function(fixed_effects){

  # Check if any element is a list with more than one element
  contains_interchangeable_effects <- plyr::llply(fixed_effects, function(x) {
    is.list(x) && length(x) > 1
  }) %>%
    unlist()

  if (any(contains_interchangeable_effects)){
    map_of_interchangeable_effects <- plyr::llply(fixed_effects[contains_interchangeable_effects], function(x) {
      key <- x[[1]]
      values <- setNames(list(x), key)
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

# Get effects and all possible interactions
get_terms_matrix <- function(fixed_effects){
  interacting_effects <- paste0(fixed_effects, collapse = "*")
  interaction_formula <- formula(paste0("1 ~ ", interacting_effects))
  terms_matrix <- t(attr(terms.formula(interaction_formula), "factors"))
  terms <- rownames(terms_matrix)
  terms_matrix <- dplyr::as_tibble(terms_matrix)
  terms_matrix[["1"]] <- NULL
  terms_matrix$terms <- stringr::str_replace_all(terms, ":", " \\* ")
  terms_matrix %>%
    dplyr::select(.data$terms, dplyr::everything()) %>%
    dplyr::mutate(num_terms = rowSums(.[2:(length(fixed_effects)+1)]))
}

# Note DO NOT DELETE #############################
# Also used by data-raw/combine_predictors_table()
get_min_n_fixed_effects <- function(..., fixed_effects){
  n_fixed_effects <- length(fixed_effects)
  r <- c(...)[rev(fixed_effects)]
  n_fixed_effects + 1 - match(1,r, nomatch=NA)
}
