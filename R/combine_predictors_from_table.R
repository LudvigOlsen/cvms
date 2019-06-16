combine_predictors_from_table <- function(n_fixed_effects,
                                          max_interaction_size = NULL,
                                          max_fixed_effects = NULL){

  predictors_table <- choose_combine_predictors_table(n_fixed_effects,
                                                      max_interaction_size_ = max_interaction_size,
                                                      max_fixed_effects_ = max_fixed_effects)
  predictors_table
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
