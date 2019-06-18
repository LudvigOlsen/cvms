## code to prepare `precomputed_formulas_table` dataset goes here

combine_predictors_build_formulas <- function(fixed_effects,
                                              max_fixed_effects = 6,
                                              max_interaction_size = 3){

  # Check inputs
  if (is.null(fixed_effects) || length(fixed_effects) == 0){
    stop("Please specify vector/list of fixed_effects.")
  }

  if (!is.numeric(max_interaction_size)){
    stop("max_interactions must be numeric scalar in the range [0,3].")
  }

  if (max_interaction_size > 3 || max_interaction_size < 0 || is.null(max_interaction_size)){
    stop("max_interaction_size must be numeric scalar between 0 and 3.")
  }

  if (!is.null(max_fixed_effects) && !is.numeric(max_fixed_effects)){
    stop("max_fixed_effects must be scalar or NULL.")
  }

  if (!is.null(max_fixed_effects) && max_fixed_effects<2){
    stop("max_fixed_effects must be at least 2")
  }

  # Find number of fixed effects
  if (!is.null(max_fixed_effects)){
    n_fixed_effects <- max_fixed_effects
  } else {
    n_fixed_effects <- length(fixed_effects)
  }
  n_total_fixed_effects <- length(fixed_effects) # For indexing columns

  # If we only want to use 3 fixed effects, we shouldn't have a 4-way interaction
  if (!is.null(max_fixed_effects)){
    max_interaction_size <- min(max_interaction_size, max_fixed_effects)
  }

  if (n_fixed_effects < 2) {stop("The number of fixed effects should be at least 2.")}

  should_contain_interactions <- max_interaction_size %in% c(2:3)

  # Sort fixed effects
  fixed_effects <- sort(fixed_effects, decreasing = FALSE)

  ## Effect combinations with interactions
  # With interactions
  if (should_contain_interactions){

    interactions_df <- get_terms_matrix(fixed_effects)

    interactions_df <- interactions_df %>%
      dplyr::filter(.data$num_terms <= max_interaction_size)

    # Create formulas
    # Different approaches if with/without interactions

    # Get compatibility dict
    compatible_predictors_table <- combine_predictors_from_table(
      n_total_fixed_effects,
      max_fixed_effects = n_fixed_effects,
      max_interaction_size = max_interaction_size)

    # Combine interactions
    interaction_combinations <- plyr::ldply(1:n_fixed_effects, function(i){
      if (i == 1) {
        data.frame("X1" = interactions_df[["terms"]], stringsAsFactors=FALSE)
      } else {
        interactions_ <- interactions_df %>%
          dplyr::filter(.data$num_terms <= (n_fixed_effects-i+2)) %>%
          dplyr::pull(.data$terms)
        data.frame(t(combn(interactions_, i)), stringsAsFactors=FALSE)
      }
    })

    # Filter out incompatible terms
    interaction_combinations <- repeated_dt_join(
      data.table(interaction_combinations),
      data.table(compatible_predictors_table),
      all_vars = colnames(interaction_combinations),
      by2 = c("key_term", "compatible_term"),
      flip = FALSE)

    # Need to go before the creation of the combination column
    int_cols <- colnames(interaction_combinations)

    interaction_combinations[["combination"]] <- 1:nrow(interaction_combinations)

    # Gather terms and join with interactions_df
    interaction_combinations_long <- melt(
      interaction_combinations, id.vars = c("combination"),
      measure.vars = int_cols, na.rm=FALSE)[
        data.table(interactions_df), on = "value==terms"]

    # Find max effect flags
    interaction_combinations_max <- interaction_combinations_long[
      , lapply(.SD, max, na.rm=TRUE), by=combination, .SDcols=c(fixed_effects, "num_terms")]

    # Find how many times the most frequent effect appears
    # First, sum the fixed effects flags
    # then, get the max value of those sums row-wise
    # then, deselect fixed effects columns
    interaction_combinations_max_effect_frequency <- interaction_combinations_long[
      , lapply(.SD, sum, na.rm=TRUE), by=combination, .SDcols=c(fixed_effects)][
        , max_effect_frequency := do.call(pmax, .SD), .SDcols = fixed_effects
        ][, (fixed_effects):=NULL]

    # Join with the stats just created
    interaction_combinations <- interaction_combinations[interaction_combinations_max, on="combination"][
      interaction_combinations_max_effect_frequency, on="combination"]
    interaction_combinations[["combination"]] <- NULL

    # TODO Can these operations be performed faster with data.table?
    interaction_combinations <- dplyr::as_tibble(interaction_combinations) %>%
      dplyr::rename(max_interaction_size = .data$num_terms)

    interaction_combinations <- interaction_combinations %>%
      dplyr::mutate(num_effects = rowSums(.[(n_fixed_effects + 1):(
        n_fixed_effects + n_total_fixed_effects)]))

    interaction_combinations <-  interaction_combinations %>%
      dplyr::mutate(min_n_fixed_effects = furrr::future_pmap_dbl(
        ., get_min_n_fixed_effects, fixed_effects=fixed_effects))

    interaction_combinations <-  interaction_combinations %>%
      dplyr::select(-fixed_effects) %>%
      dplyr::mutate(formula_ = furrr::future_pmap_chr(.[1:n_fixed_effects],
                                                      paste_columns,
                                                      collapse=" + ")) %>%
      dplyr::select(-c(1:n_fixed_effects)) %>%
      dplyr::select(formula_, dplyr::everything())

    if (!is.null(max_fixed_effects)){
      interaction_combinations <- interaction_combinations %>%
        dplyr::filter(.data$num_effects <= max_fixed_effects)
    }

    return(interaction_combinations)


  } else {
    stop("Generation without interactions is not supported yet!")
  }

}

###

combine_predictors_from_table <- function(n_fixed_effects,
                                          max_interaction_size = NULL,
                                          max_fixed_effects = NULL,
                                          add_all_NA_row = TRUE){

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

  if(isTRUE(add_all_NA_row)){
    doubled_predictors_table <- doubled_predictors_table %>%
      dplyr::bind_rows(tibble::tibble("key_term" = NA, "compatible_term" = NA))
  }

  doubled_predictors_table
}

###

choose_combine_predictors_table <- function(n_fixed_effects,
                                            max_interaction_size_ = NULL,
                                            max_fixed_effects_ = NULL){

  predictors_table <- cvms::compatible.formula.terms

  predictors_table <- predictors_table %>%
    dplyr::filter(.data$min_num_fixed_effects <= n_fixed_effects)

  if (!is.null(max_interaction_size_)){
    predictors_table <- predictors_table %>%
      dplyr::filter(.data$max_interaction_size <= max_interaction_size_)
  }

  if (!is.null(max_fixed_effects_)){
    predictors_table <- predictors_table %>%
      dplyr::filter(.data$num_effects <= max_fixed_effects_)
  }

  predictors_table

}

###

# Repeated data.table anti-join
# by2 can only have length 2 atm.
repeated_dt_join <- function(dt1, dt2, all_vars, by2, flip = TRUE){

  if (length(by2) != 2) stop("by2 != 2 is not supported")

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
    dt1 <<- dt1[dt2, on = on, nomatch=NULL]
  })

  dt1
}

##################### Run computation #####################



precomputed_formulas_table <- combine_predictors_build_formulas(LETTERS[1:8],
                                                                max_fixed_effects = 5,
                                                                max_interaction_size = 3)

# Make sure only to overwrite when we actually want to!
usethis::use_data(precomputed_formulas_table, overwrite = FALSE)
usethis::use_data(precomputed.formulas, internal = TRUE, overwrite = FALSE)
