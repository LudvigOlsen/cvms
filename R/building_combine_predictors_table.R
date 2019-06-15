

building_combine_predictors_table <- function(){

  fixed_effects <- LETTERS[1:25] # c("A","B","C","D") #,"E","F","G")
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

  #allowed_crossings$position <-


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

  combine_predictors_table <- allowed_crossings
  save(combine_predictors_table, file="combine_predictors_table.RData")

  # print(allowed_crossings)
  View(allowed_crossings)


  # for (i in rev(1:15)){
  #   current_row <- terms_matrix[i,]
  #   lower_rows <- terms_matrix[c(1:(i-1)),]
  #
  #
  #   print("current:")
  #   print(current_row)
  #   print("lower")
  #   print(lower_rows)
  #   stop()
  # }

}

# set_diff_for_pmap <- function(..., effect_names){
#   r <- unname(c(...))
#
#   rle_ <- rle(r)
#
#   total_NAs <- sum(stringr::str_count(rle_$values, "__NA__"))
#
#   if (total_NAs == 0) {
#     return(FALSE)
#   } else if (total_NAs > 1) {
#     return(TRUE)
#   }
#
#   if (rle_$values[length(rle_$values)] != "__NA__"){
#     return(TRUE)
#   }
#   return(FALSE)
# }
