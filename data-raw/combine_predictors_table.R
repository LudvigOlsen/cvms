## code to prepare `combine_predictors_table` dataset goes here

# file_ = "data/combine_predictors_table_5_effects.rda"
# Note: The largest interaction is not included (if n = 3, then only up to 2-way interactions included)
building_combine_predictors_table <- function(n_fixed_effects = 5){

  if (n_fixed_effects > 26){
    # LETTERS is only 26 characters
    # That number will take forever to run anyway,
    # but more effects should of course be supported in some way
    stop("Currently only up to 26 fixed effects supported.")
  }

  fixed_effects <- LETTERS[1:n_fixed_effects]
  n_fixed_effects <- length(fixed_effects)
  terms_matrix <- get_terms_matrix(fixed_effects)
  # print(terms_matrix)

  terms <- terms_matrix$terms
  crossed <- tidyr::crossing(terms, terms) %>%
    dplyr::mutate(comparison = 1:dplyr::n()) %>%
    dplyr::filter(terms != terms1) %>%
    dplyr::rename(left = terms,
                  right = terms1) %>%
    tidyr::gather(key="side", value="terms", 1:2) %>%
    dplyr::full_join(terms_matrix, by="terms") %>%
    dplyr::arrange(comparison, desc(num_terms))

  #print(crossed)

  comparisons <- crossed %>%
    dplyr::group_by(comparison) %>%
    dplyr::mutate(ind = 1:dplyr::n(),
                  mult = ifelse(ind == 2, -1, 1)) %>%
    dplyr::mutate_at(.vars = fixed_effects, .funs = list(~{. * mult})) %>%
    dplyr::group_by(comparison)

  comparisons_with_same_num_terms <- comparisons %>%
    dplyr::summarise(diff_num_terms = diff(num_terms)) %>% # print() %>%
    dplyr::filter(diff_num_terms == 0) %>% # print() %>%
    dplyr::pull(.data$comparison)

  # print(comparisons_with_same_num_terms)

  comparisons_that_add_effect <- comparisons %>%
    dplyr::filter(comparison %ni% comparisons_with_same_num_terms) %>%
    dplyr::summarise_at(.vars = fixed_effects, .funs = list(~sum(.))) %>%
    dplyr::filter_at(.vars = fixed_effects, dplyr::any_vars(. == -1)) %>% # print() %>%
    dplyr::pull(.data$comparison)

  # print(comparisons_that_add_effect)

  comparisons_to_use <- c(comparisons_with_same_num_terms, comparisons_that_add_effect)

  # print(comparisons_to_use)

  allowed_crossings <- crossed %>%
    dplyr::filter(comparison %in% comparisons_to_use) %>%
    dplyr::arrange(comparison, terms) %>%
    dplyr::group_by(comparison)

  included_effects <- allowed_crossings %>%
    dplyr::summarise_at(.vars = fixed_effects, .funs = list(~{ifelse(sum(.) > 0, 1, 0)}))

  max_num_interaction_terms <- allowed_crossings %>%
    dplyr::summarise(max_interaction_size = max(num_terms))
  # required_n_fixed_effects = max(which(. == 1)))

  allowed_crossings <- allowed_crossings %>%
    dplyr::mutate(side = c("left","right")) %>%
    dplyr::select(c(comparison, terms, side)) %>%
    tidyr::spread(key = side, value = terms) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(included_effects, by = "comparison") %>%
    dplyr::inner_join(max_num_interaction_terms, by = "comparison") %>%
    dplyr::select(-comparison) %>%
    dplyr::mutate(num_terms = rowSums(.[3:(n_fixed_effects+2)])) %>%
    dplyr::arrange(left, right) %>%
    dplyr::distinct()

  allowed_crossings[["min_n_fixed_effects"]] <- apply(
    allowed_crossings, 1,
    function(x) {n_fixed_effects + 1 - match(1,x[rev(fixed_effects)], nomatch=NA )})

  return(allowed_crossings)
}

# A larger number of fixed effects can take a long time.
combine_predictors_table_5_effects <- building_combine_predictors_table(5)
combine_predictors_table_7_effects <- building_combine_predictors_table(7)
combine_predictors_table_10_effects <- building_combine_predictors_table(10)

usethis::use_data(combine_predictors_table_5_effects,
                  combine_predictors_table_7_effects,
                  combine_predictors_table_10_effects,
                  internal=TRUE, overwrite = TRUE)
