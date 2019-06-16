combine_predictors_from_table <- function(n_fixed_effects){

  predictors_table <- load_combine_predictors_table(n_fixed_effects)
  predictors_table
}

load_combine_predictors_table <- function(n_fixed_effects){

  # TODO Use switch instead
  if (n_fixed_effects <= 5){
    return(combine_predictors_table_5_effects)
  } else if (n_fixed_effects <= 10){
    return(combine_predictors_table_10_effects)
  } else if (n_fixed_effects <= 15){
    return(combine_predictors_table_15_effects)
  } else if (n_fixed_effects <= 20){
    return(combine_predictors_table_20_effects)
  }

}
