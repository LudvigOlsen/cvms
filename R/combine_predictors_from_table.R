# extra_terms are only added when add_NAs is TRUE
# This is useful if we want to add the full interaction that is missing from the table
combine_predictors_from_table <- function(n_fixed_effects,
                                          max_interaction_size = NULL,
                                          max_fixed_effects = NULL,
                                          add_NAs = TRUE,
                                          extra_terms = NULL){

  predictors_table <- choose_combine_predictors_table(n_fixed_effects,
                                                      max_interaction_size_ = max_interaction_size,
                                                      max_fixed_effects_ = max_fixed_effects)

  doubled_predictors_table <- predictors_table %>%
    dplyr::bind_rows(
      dplyr::rename(predictors_table,
                    left = .data$right,
                    right = .data$left)) %>%
    dplyr::rename(key_term = .data$left,
                  compatible_term = .data$right) %>%
    dplyr::select(c(.data$key_term, .data$compatible_term))

  if (isTRUE(add_NAs)){
    # Add a c(NA,NA) row
    # Add NA as compatible term for all unique key terms
    unique_terms <- c(NA, unique(doubled_predictors_table[["key_term"]]))

    # Add extra terms if supplied
    if (!is.null(extra_terms) && is.character(extra_terms)){
      unique_terms <- unique(c(unique_terms, extra_terms))
    }

    doubled_predictors_table <- doubled_predictors_table %>%
      dplyr::bind_rows(tibble::tibble("key_term" = unique_terms,
                                      "compatible_term" = NA))

  }


  compatibility_dict <- collections::Dict$new()

  plyr::a_ply(doubled_predictors_table, .margins = 1, function(ro){
    if (compatibility_dict$has(ro[["key_term"]])){
      compatibility_dict$set(ro[["key_term"]],
                             c(
                               compatibility_dict$get(ro[["key_term"]]),
                               ro[["compatible_term"]]
                             )
      )
    } else {
      compatibility_dict$set(ro[["key_term"]], ro[["compatible_term"]])
    }

  })

  list("doubled_predictors_table" = doubled_predictors_table,
       "compatibility_dict" = compatibility_dict)

}

choose_combine_predictors_table <- function(n_fixed_effects,
                                            max_interaction_size_ = NULL,
                                            max_fixed_effects_ = NULL){

  # TODO Use switch instead
  if (n_fixed_effects <= 5){
    predictors_table <- combine_predictors_table_5_effects
  } else if (n_fixed_effects <= 7){
    predictors_table <- combine_predictors_table_7_effects
  } else if (n_fixed_effects <= 10){
    predictors_table <- combine_predictors_table_10_effects
  } else if (n_fixed_effects <= 15){
    stop("table not yet computed")
    predictors_table <- combine_predictors_table_15_effects
  }

  predictors_table <- predictors_table %>%
    dplyr::filter(.data$min_n_fixed_effects <= n_fixed_effects)

  if (!is.null(max_interaction_size_)){
    predictors_table <- predictors_table %>%
      dplyr::filter(.data$max_interaction_size <= max_interaction_size_)
  }

  if (!is.null(max_fixed_effects_)){
    predictors_table <- predictors_table %>%
      dplyr::filter(.data$num_terms <= max_fixed_effects_)
  }

  predictors_table

}

############ TODO This is too slow. We have to change it to use data.table functionality in some way
check_compatible <- function(..., dict_, var_indices_=NULL){

  r <- unname(c(...))
  r <- r[!is.na(r)]

  if (length(r) == 1){
    return(TRUE)
  }

  if (!is.null(var_indices_)){
    r <- r[var_indices_]
  }

  all(unlist(plyr::llply(seq_along(r), function(i){
    if (!dict_$has(r[[i]])) return(FALSE)
    r_compatibles <- dict_$get(r[[i]])
    non_compatibles <- setdiff(r[-i], r_compatibles)
    identical(non_compatibles, character(0))
  })))

}
#
# building_repeated_dt_anti_join <- function(){
#
#   dtf <- data.frame("X1"=rep(LETTERS, each=10),
#                     "X2"=rev(rep(LETTERS, 10)),
#                     "X3"=rep(c(LETTERS[16:26], LETTERS[1:15]), 10))
#
#   dft_as_dt <- data.table(dtf)
#   #View(dft_as_dt)
#
#   dt2 <- data.table("left"=rep(LETTERS, each=2),
#                     "right"=rev(rep(LETTERS, 2)))
#
#   #View(dt2)
#
#   updated_dt1 <- repeated_dt_join(dft_as_dt, dt2,
#                              all_vars = c("X1","X2","X3"),
#                              by2 = c("left","right"))
#
#   View(updated_dt1)
#
# }

# Repeated data.table anti-join
# by2 can only have length 2 atm.
repeated_dt_join <- function(dt1, dt2, all_vars, by2, flip = TRUE){

  if (length(by2) != 2) stop("by2 != 2, not supported")

  column_comparisons <- data.frame(t(combn(all_vars, length(by2))), stringsAsFactors = FALSE)
  colnames(column_comparisons) <- c("key","compatible")

  if (isTRUE(flip)){
    column_comparisons <- column_comparisons %>%
      dplyr::bind_rows(
        dplyr::rename(column_comparisons,
                      key = compatible,
                      compatible = key)
      )
  }

  column_comparisons <- column_comparisons %>%
    dplyr::mutate(on1 = paste0(.data$key, "==", by2[[1]]),
                  on2 = paste0(.data$compatible, "==", by2[[2]]))

  plyr::l_ply(1:nrow(column_comparisons), function(i){
    if (nrow(dt1) == 0) return(dt1) # Would perhaps be better to break the loop somehow?
    on1 <- column_comparisons[["on1"]][[i]]
    on2 <- column_comparisons[["on2"]][[i]]
    on = c(on1, on2)
    # print(on)

    dt1 <<- dt1[dt2, on = on, nomatch=NULL]

  })

  dt1


}


