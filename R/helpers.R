# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# Get all lists in a list with a certain name
# Use: list_of_lists %c% 'list_name'
`%c%` <- function(x, n) lapply(x, `[[`, n)
# From http://stackoverflow.com/questions/5935673/accessing-same-named-list-elements-of-the-list-of-lists-in-r/5936077#5936077

# Not in
`%ni%` <- function(x, table) {
  return(!(x %in% table))

}

default_link <- function(link, family){
  if (is.null(link)){
    if(family == 'gaussian') return('identity')
    if(family == 'binomial') return('logit')
  } else return(link)
}

default_control <- function(control, family, link){
  if (is.null(control)){

     # Note that gaussian models with alternative link functions are run with glmer
    if(family == 'gaussian'){
      if (link == "identity") return(lme4::lmerControl())
      return(lme4::glmerControl())
    }
    if(family == 'binomial') return(lme4::glmerControl())

    } else return(control)
}

## For validate_single and cross_validate_single

# Extract y_column from model
extract_y <- function(model){
  splits <- unlist(strsplit(model, '\\s*~'))
  if (length(splits)<2) return(NULL)
  return(splits[1])
}

# Check if there are random effects
# returns a list, e.g. (False, False, True)
rand_effects <- function(model){
  grepl('\\(\\d', model, perl=TRUE)
}

count_convergence_warnings <- function(convergences){ # "Yes" or "No"
  # Count the convergence warnings
  conv_warns = as.integer(table(convergences)['No'])
  if (is.na(conv_warns)){
    conv_warns = 0
  }
  return(conv_warns)
}

count_named_nulls_in_list <- function(l){
  plyr::llply(l, function(e){
    ifelse(is.null(e), 1, 0)
  }) %>% unlist() %>% sum()
}

### Model specifics

check_model_specifics <- function(passed_model_specifics, required_named_arguments){
  # Check that model_specifics contains all named arguments
  diff <- setdiff(names(passed_model_specifics), required_named_arguments)
  if (length(diff) > 0) {
    stop(paste0("model_specifics must (only) contain all named arguments. Be sure to name arguments. Wrongly passed argument(s): ",
                diff))
  }
}

check_argument_in_model_specifics <- function(var_name, model_specifics){
  if (var_name %ni% names(model_specifics))
    stop(paste0(var_name," is a required named argument in model_specifics. Be sure to name arguments."))
}

replace_argument_in_model_specifics_if_null <- function(var_name, model_specifics, new_value, err=TRUE){
  if (is.null(model_specifics[[var_name]])){

    if (isTRUE(err)){
      stop(paste0(var_name," was NULL in model_specifics. Remember to name arguments, i.e. ",
                  var_name, " = ??? when passing model_specifics."))
    }

    model_specifics[[var_name]] = new_value

  }
  model_specifics
}

### Results

nest_results <- function(results){

  # Make results into a tibble
  iter_results <- tibble::as_tibble(results)
  rownames(iter_results) <- NULL
  iter_results <- iter_results %>%
    tidyr::nest() %>%
    dplyr::rename(results = data)

  iter_results
}

nest_models <- function(models){
  # Make tidied models into a tibble
  iter_models <- tibble::as_tibble(models)
  if ("p.value" %ni% colnames(iter_models)){
    iter_models[["p.value"]] <- NA
  }
  iter_models <- iter_models %>%
    tidyr::nest() %>%
    dplyr::rename(Coefficients = data)

  iter_models
}

levels_as_characters <- function(col){

  levs <- levels(factor(col))

  cat_levels <- plyr::llply(1:length(levs), function(i){
    as.character(levs[i])
  }) %>% unlist()

  cat_levels
}

assign_if_not_null_named_lists <- function(var, var_name, list_name){
  if (is.null(var)){stop(paste0(var_name, " is NULL. The arguments in the ",list_name," list must be named."))}
  var
}

# Removes pattern from all column names
remove_from_colnames <- function(data, pattern){
  colnames(data) <- colnames(data) %>%
    tibble::enframe(name=NULL) %>%
    dplyr::mutate(colname = stringr::str_remove_all(.data$value, pattern)) %>%
    dplyr::pull(.data$colname)

  return(data)
}


# Returns list with folds_map and n_folds
create_folds_map <- function(data, fold_cols){

  # Create a map of number of folds per fold column
  # The range tells what fold column a specific fold belongs to.
  folds_map <- plyr::llply(1:length(fold_cols), function(fold_column){
    nlevels(data[[ fold_cols[[fold_column]] ]])
  }) %>%
    unlist() %>%
    tibble::enframe(name="fold_col", value="num_folds")

  # Create ranges
  first_start <- folds_map$num_folds[[1]]
  folds_map <- folds_map %>%
    dplyr::mutate(end_ = cumsum(.data$num_folds),
                  start_ = .data$end_ - (first_start-1))

  # Calculate number of folds
  n_folds <- sum(folds_map$num_folds)

  # Expand ranges to long format
  folds_map_expanded <- plyr::ldply(1:length(fold_cols), function(fold_column){
    data.frame("fold_col_idx"=fold_column,
               "fold_col_name"=fold_cols[[fold_column]],
               abs_fold=c(folds_map[["start_"]][[fold_column]]:folds_map[["end_"]][[fold_column]]),
               rel_fold=c(1:folds_map[["num_folds"]][[fold_column]]))
  }) %>% dplyr::as_tibble()

  return(
    list(
    "folds_map" = folds_map_expanded,
    "n_folds" = n_folds)
    )

}

# Creates data frame with existing combinations of fold column, abs_fold and rel_fold
# For adding the info to other data frames via joins
create_fold_and_fold_column_map <- function(data, fold_info_cols){
  data %>%
    dplyr::select(dplyr::one_of(fold_info_cols[["fold_column"]],
                                fold_info_cols[["abs_fold"]],
                                fold_info_cols[["rel_fold"]]
    )) %>%
    dplyr::distinct()
}

# Extracts the major and minor version numbers.
check_R_version <- function(){
  major <- as.integer(R.Version()$major)
  minor <- as.numeric(strsplit(R.Version()$minor, ".", fixed = TRUE)[[1]][[1]])
  list("major"=major, "minor"=minor)
}

# Skips testthat test, if the R version is below 3.6.0
# WHY? Due to the change in the random sampling generator
# tests fail on R versions below 3.6.0.
# It is possible to fix this by using the old generator for
# unit tests, but that would take a long time to convert,
# and most likely the code works the same on v3.5
skip_test_if_old_R_version <- function(min_R_version = "3.6"){
  if(check_R_version()[["minor"]] < strsplit(min_R_version, ".", fixed = TRUE)[[1]][[2]]){
    testthat::skip(message = paste0("Skipping test as R version is < ", min_R_version, "."))
  }
}

# Wrapper for setting seed with the sample generator for R versions <3.6
# Used for unittests
# Partly contributed by R. Mark Sharp
set_seed_for_R_compatibility <- function(seed = 1) {
  version <- check_R_version()
  if ((version[["major"]] == 3 && version[["minor"]] >= 6) || version[["major"]] > 3) {
    args <- list(seed, sample.kind = "Rounding")
  } else {
    args <- list(seed)
  }
  suppressWarnings(do.call(set.seed, args))
}
