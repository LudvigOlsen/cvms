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
  terms_matrix <- get_terms_matrix(fixed_effects) %>%
    dplyr::mutate(has_NA = FALSE)

  # Extract terms and combine them two by two
  terms <- terms_matrix$terms
  combs <- t(combn(terms, 2))

  # Create row for terms matrix with NA term and 0's everywhere
  zero_cols <- dplyr::bind_rows(setNames(rep(0, n_fixed_effects), fixed_effects))
  NA_row <- tibble::tibble("terms" = NA, "num_terms"= 0, "has_NA" = TRUE) %>%
    dplyr::bind_cols(zero_cols)

  # Add a NA term to the terms matrix
  # for when we join with the combinations
  terms_matrix <- terms_matrix %>%
    dplyr::bind_rows(NA_row)

  # ...
  crossed <- tibble::tibble("left" = combs[,1],
                            "right" = combs[,2]) %>%
    dplyr::bind_rows(tibble::tibble("left" = terms,
                                    "right" = NA)) %>%
    dplyr::mutate(comparison = 1:dplyr::n()) %>%
    tidyr::gather(key="side", value="terms", 1:2) %>%
    dplyr::left_join(terms_matrix, by="terms") %>%
    dplyr::arrange(comparison, desc(num_terms))

  comparisons_with_NA <- crossed %>%
    dplyr::filter(.data$has_NA) %>%
    dplyr::pull(.data$comparison)

  # print(comparisons_with_NA)

  comparisons <- crossed %>%
    dplyr::filter(.data$comparison %ni% comparisons_with_NA) %>%
    dplyr::group_by(comparison) %>%
    dplyr::mutate(ind = 1:dplyr::n(),
                  mult = ifelse(ind == 2, -1, 1)) %>%
    dplyr::mutate_at(.vars = fixed_effects, .funs = list(~{. * mult})) %>%
    dplyr::group_by(comparison)

  comparisons_with_same_num_terms <- comparisons %>%
    dplyr::summarise(diff_num_terms = diff(num_terms)) %>%
    dplyr::filter(diff_num_terms == 0) %>%
    dplyr::pull(.data$comparison)

  comparisons_that_add_effect <- comparisons %>%
    dplyr::filter(comparison %ni% comparisons_with_same_num_terms) %>%
    dplyr::summarise_at(.vars = fixed_effects, .funs = list(~sum(.))) %>%
    dplyr::filter_at(.vars = fixed_effects, dplyr::any_vars(. == -1)) %>% # print() %>%
    dplyr::pull(.data$comparison)

  # print(comparisons_that_add_effect)

  comparisons_to_use <- c(comparisons_with_same_num_terms,
                          comparisons_that_add_effect,
                          comparisons_with_NA)

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
    dplyr::distinct() %>%
    dplyr::mutate(min_n_fixed_effects = furrr::future_pmap_dbl(.,
                                                               get_min_n_fixed_effects,
                                                               fixed_effects=fixed_effects,
                                                               n_fixed_effects=n_fixed_effects))

  return(allowed_crossings)
}

get_min_n_fixed_effects <- function(..., fixed_effects, n_fixed_effects){
  r <- c(...)[rev(fixed_effects)]
  n_fixed_effects + 1 - match(1,r, nomatch=NA)
}

plan(multiprocess)

# A larger number of fixed effects can take a long time.
# We also save the tables to /data to begin with for backup
combine_predictors_table_5_effects <- building_combine_predictors_table(5)
usethis::use_data(combine_predictors_table_5_effects,
                  internal=FALSE, overwrite = TRUE)

combine_predictors_table_7_effects <- building_combine_predictors_table(7)
usethis::use_data(combine_predictors_table_7_effects,
                  internal=FALSE, overwrite = TRUE)

combine_predictors_table_10_effects <- building_combine_predictors_table(10)
usethis::use_data(combine_predictors_table_10_effects,
                  internal=FALSE, overwrite = TRUE)

# TODO list:
# 1. We might be able to build with more effects if we set an upper limit to max_interaction_size

usethis::use_data(combine_predictors_table_5_effects,
                  combine_predictors_table_7_effects,
                  combine_predictors_table_10_effects,
                  internal=TRUE, overwrite = TRUE)
