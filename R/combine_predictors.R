#' @title Generate model formulas by combining predictors
#' @description Create model formulas with every combination
#'  of your fixed effects, along with the dependent variable and random effects.
#'  Be aware of exponential time increase with number of fixed effects.
#'  Allows for limiting the size of interactions and the number of fixed effects in a formula.
#'  Currently use the \code{+} and \code{*} operators,
#'  and has the constraint that a fixed effect is only included once in a formula.
#' @return
#'  List of model formulas.
#'
#'  E.g.:
#'
#'  \code{c("y ~ x1 + (1|z)", "y ~ x2 + (1|z)",
#'  "y ~ x1 + x2 + (1|z)", "y ~ x1 * x2 + (1|z)")}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @aliases generate_formulas
#' @param dependent Name of dependent variable. (Character)
#' @param fixed_effects List of fixed effects. (Character)
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
#'   Due to the exponential time increase with number of fixed effects,
#'   it can help to restrict the number of fixed effects in a formula.
#' @param max_interaction_size Maximum number of effects in an interaction. (Integer)
#'
#'  Use this to limit the \code{n}-way interactions allowed.
#'
#'  A model formula can contain multiple interactions.
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
#'
#' # Create effect names with interchangeable effects in sublists
#' fixed_effects <- list("a",list("b","log_b"),"c","d")
#'
#' # Create model formulas
#' combine_predictors(dependent, fixed_effects,
#'                    random_effects)
#' @importFrom purrr pmap_dbl pmap_df
#' @importFrom rlang .data
#' @importFrom utils combn head
#' @importFrom combinat permn
#' @importFrom stats setNames
combine_predictors <- function(dependent, fixed_effects,
                               random_effects = NULL,
                               max_fixed_effects = NULL,
                               max_interaction_size = NULL){

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
  if (!is.null(max_fixed_effects) && max_fixed_effects<2){
    stop("max_fixed_effects must be at least 2")
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
  n_total_fixed_effects <- length(fixed_effects) # For indexing columns

  # If we only want to use 3 fixed effects, we shouldn't have a 4-way interaction
  if (!is.null(max_interaction_size) && !is.null(max_fixed_effects)){
    max_interaction_size <- min(max_interaction_size, max_fixed_effects)
  }

  if (n_fixed_effects < 2) {stop("The number of fixed effects should be at least 2.")}

  should_contain_interactions <- is.null(max_interaction_size) || max_interaction_size %ni% c(0,1)

  # # Change it to be a count of asterisks (*)
  # if (should_contain_interactions && !is.null(max_interaction_size)){
  #   max_num_asterisks <- max_interaction_size - 1
  # } else {
  #   max_num_asterisks <- NULL
  # }

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

  # Sort fixed effects
  fixed_effects <- sort(fixed_effects, decreasing=FALSE)

  ## Effect combinations, with and without interactions

  # Without interactions
  effect_combinations <- plyr::ldply(1:n_fixed_effects, function(i){
    as.data.frame(t(combn(fixed_effects, i)), stringsAsFactors=FALSE)
  })

  effect_combinations[is.na(effect_combinations)] <- "__NA__"

  # With interactions
  if (should_contain_interactions){

    # # Create all permutations of the effect columns (except first column)
    # col_combinations <- permn(colnames(effect_combinations)[-1])
    #
    # # Create rows for each permutation and keep the useful ones
    # effect_combinations_interactions <- plyr::ldply(1:length(col_combinations), function(i){
    #
    #   col_combination <- c("V1", col_combinations[[i]])
    #   effect_combinations <- effect_combinations[col_combination] #%>%
    #     #dplyr::select(dplyr::one_of(col_combination))
    #   colnames(effect_combinations) <- paste0("V",c(1:n_fixed_effects))
    #   effect_combinations
    #
    # }) %>% dplyr::distinct() %>%
    #   dplyr::mutate(NA_left_of_value = purrr::pmap_dbl(., contains_NA_left_of_value)) %>%
    #   dplyr::filter(!.data$NA_left_of_value) %>%
    #   dplyr::select(-.data$NA_left_of_value)

    # Generate the interactions, with flags for included effects
    # interactions_df <- generate_interactions(effect_combinations_interactions) %>%
    #   dplyr::mutate(num_asterisks = rowSums(.[2:(n_total_fixed_effects+1)])-1) #%>% print()

    interactions_df <- get_terms_matrix(fixed_effects)

    # Remove interaction larger than max_num_asterisks
    if (!is.null(max_interaction_size)){
    interactions_df <- interactions_df %>%
      dplyr::filter(.data$num_terms <= max_interaction_size)
    }

    print(interactions_df)

  }

  # Create formulas
  # Different approaches if with/without interactions
  if (should_contain_interactions){

    # Combine interactions
    print(n_fixed_effects)
    interaction_combinations <- plyr::ldply(1:n_fixed_effects, function(i){
      if (i == 1) {
        data.frame("X1" = interactions_df[["terms"]], stringsAsFactors=FALSE)
      } else {
        interactions_ <- interactions_df %>%
          dplyr::filter(.data$num_terms <= (n_fixed_effects-i+2)) %>%
          dplyr::pull(.data$terms)
        print(interactions_)
        data.frame(t(combn(interactions_, i)), stringsAsFactors=FALSE)
      }

    })
    # print(interaction_combinations)
    # stop()

    print(nrow(interaction_combinations))

    # # Filter combinations
    # interaction_combinations <- interaction_combinations %>% #nrow() %>% print() %>% stop()
    #   # Filter such that effect 1 is not the first in column 1 and 2
    #   dplyr::filter(
    #     is.na(.data$X2) |
    #       !(stringr::str_detect(.data$X1, stringr::fixed(fixed_effects[[1]])) &
    #           stringr::str_detect(.data$X2, stringr::fixed(fixed_effects[[1]])))
    #     ) %>%
    #   # Filter such that effect 2 is not the first in column 1 and 2
    #   # This assumes at least two fixed effects.
    #   dplyr::filter(
    #     is.na(.data$X2) |
    #       !(stringr::str_detect(.data$X1, stringr::fixed(fixed_effects[[2]])) &
    #           stringr::str_detect(.data$X2, stringr::fixed(fixed_effects[[2]])))
    #   ) %>%
    #   dplyr::mutate(combination = 1:dplyr::n())

    print(nrow(interaction_combinations))

    # Find the combinations to keep
    combinations_to_keep <- interaction_combinations %>%

      # # Add count of terms (+ / *) and filter out rows with too many
      # tidyr::unite("pasted", 1:n_fixed_effects, sep=" + ", remove = FALSE) %>%
      # dplyr::mutate(pasted = stringr::str_replace_all(.data$pasted, "\\+ NA", ""),
      #               terms_ = stringr::str_count(.data$pasted, "\\+|\\*")) %>%
      # dplyr::filter(.data$terms_ <= n_fixed_effects) %>%

      # Find and remove duplicate effects
      tidyr::gather(key="column", value="terms", 1:n_fixed_effects) %>%
      dplyr::inner_join(interactions_df, by="terms") %>%
      dplyr::group_by(.data$combination) %>%
      dplyr::summarise_at(.vars = dplyr::vars(fixed_effects),
                          .funs = list(~sum(.))) %>%
      dplyr::filter_at(dplyr::vars(-.data$combination), dplyr::all_vars(. < 2)) %>%
      dplyr::mutate(total_num_effects = rowSums(.[2:(n_total_fixed_effects+1)])) %>%
      dplyr::filter(.data$total_num_effects <= n_fixed_effects) %>%
      dplyr::pull(.data$combination)

    formulas <- interaction_combinations %>%
      dplyr::filter(.data$combination %in% combinations_to_keep) %>%
      sort_rowwise(1:(n_fixed_effects)) %>%
      dplyr::select(-.data$combination) %>%
      dplyr::mutate(form = purrr::pmap_chr(., paste_columns, collapse=" + ")) %>%
      dplyr::select(.data$form)

  } else {

    # Create formulas without interactions
    formulas <- effect_combinations %>%
      dplyr::mutate(form = purrr::pmap_chr(., paste_columns, collapse=" + ")) %>%
      dplyr::select(.data$form)
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
          .funs = list(~stringr::str_replace_all(.,
                                                 colnames(formulas)[[column]],
                                                 as.character(!! as.name(colnames(formulas)[[column]])))))
    })

    formulas <- formulas %>%
      dplyr::select(.data$form) %>%
      dplyr::distinct()
  }

  formulas <- formulas %>%
    dplyr::mutate(n_efxs = stringr::str_count(.data$form, "\\+|\\*")) %>%
    dplyr::arrange(.data$n_efxs, .data$form) %>%
    dplyr::pull(.data$form)

  if (is.null(random_effects)){
    return( paste0(dependent, " ~ ", formulas) )
  } else {
    return( paste0(dependent, " ~ ", formulas, " + ", random_effects) )
  }

}

# Generate interactions (One permutation only)
# Add columns describing if an effect is in the interaction
# @param possible_interactions Data Frame.
generate_interactions <- function(possible_interactions){   # TODO Rename function

  # Convert __NA__ into NA
  possible_interactions[possible_interactions == "__NA__"] <- NA
  # Sort values rowwise, so 1,4,3,4 -> 1,3,4,4
  # Remove duplicate rows
  possible_interactions <- possible_interactions %>%
    sort_rowwise(1:ncol(possible_interactions), na.last = TRUE) %>%
    dplyr::distinct()

  # Create data frame with a flag/mask for whether the effect is in the interaction
  effect_counts <- dplyr::bind_rows(apply(possible_interactions, MARGIN=1, counts_as_row))
  effect_counts[is.na(effect_counts)] <- 0

  # Add the effect flag columns to the interactions
  possible_interactions %>%
    dplyr::mutate(interaction = purrr::pmap_chr(., paste_columns)) %>%
    dplyr::select(.data$interaction) %>%
    dplyr::bind_cols(effect_counts)
}

counts_as_row <- function(...){
  dplyr::bind_rows(table(...))
}

sort_rowwise <- function(data, vars, decreasing=FALSE, na.last = TRUE){
  data[vars] <- t(apply(data[vars], 1,
                        FUN=function(x) sort(x, decreasing=decreasing, na.last=na.last)))

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
  terms_matrix$terms <- stringr::str_replace_all(terms, ":", "\\*")
  terms_matrix %>%
    dplyr::select(.data$terms, dplyr::everything()) %>%
    dplyr::mutate(num_terms = rowSums(.[2:(length(fixed_effects)+1)]))
}


