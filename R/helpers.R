
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
# returns TRUE or FALSE
rand_effects <- function(model){
  any(grepl('\\(\\d', model, perl=TRUE))
}

count_convergence_warnings <- function(convergences){ # "Yes" or "No"
  # Count the convergence warnings
  if (length(setdiff(convergences, c("Yes","No"))) > 0){
    stop(paste0(
      "'convergences' can only contain 'Yes' and 'No'. Found: ",
      paste0(setdiff(convergences, c("Yes", "No")), collapse = ", "), "."
    ))
  }
  conv_warns <- as.integer(table(convergences)['No'])
  if (is.na(conv_warns)){
    conv_warns <- 0
  }
  return(conv_warns)
}

count_nulls_in_list <- function(l){
  plyr::llply(l, function(e){
    ifelse(is.null(e), 1, 0)
  }) %>% unlist() %>% sum()
}

contains_na <- function(v){
  sum(is.na(v)) > 0
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

    model_specifics[[var_name]] <- new_value

  }

  model_specifics
}

stop_if_argument_not_null <- function(var_name, model_specifics){
  if (!is.null(model_specifics[[var_name]])){
    stop(paste0("'", var_name, "' was not NULL."))
  }
}

stop_if_argument_is_null <- function(var_name, model_specifics){
  if (is.null(model_specifics[[var_name]])){
    stop(paste0("'", var_name, "' was NULL."))
  }
}

stop_if_argument_is_not_function <- function(var_name, model_specifics){
  if (!is.function(model_specifics[[var_name]])){
    stop(paste0("'", var_name, "' was not a function."))
  }
}

### Results

nest_results <- function(results){

  # Make results into a tibble
  iter_results <- tibble::as_tibble(results)
  rownames(iter_results) <- NULL
  iter_results <- iter_results %>%
    legacy_nest() %>%
    dplyr::rename(results = data)

  iter_results
}

nest_models <- function(model_coefs){
  # Make tidied models into a tibble
  iter_models <- tibble::as_tibble(model_coefs)
  if ("p.value" %ni% colnames(iter_models)){
    iter_models[["p.value"]] <- NA
  }
  iter_models <- iter_models %>%
    legacy_nest() %>%
    dplyr::rename(Coefficients = data)

  iter_models
}

levels_as_characters <- function(col){

  levs <- levels(factor(col))

  cat_levels <- plyr::llply(seq_len(length(levs)), function(i){
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
  folds_map <- plyr::llply(seq_len(length(fold_cols)), function(fold_column){
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
  folds_map_expanded <-
    plyr::ldply(seq_len(length(fold_cols)), function(fold_column) {
      data.frame(
        "fold_col_idx" = fold_column,
        "fold_col_name" = fold_cols[[fold_column]],
        abs_fold = c(folds_map[["start_"]][[fold_column]]:folds_map[["end_"]][[fold_column]]),
        rel_fold = c(1:folds_map[["num_folds"]][[fold_column]])
      )
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
    dplyr::rename(fold_column = fold_info_cols[["fold_column"]],
                  abs_fold = fold_info_cols[["abs_fold"]],
                  rel_fold = fold_info_cols[["rel_fold"]]) %>%
    dplyr::distinct()
}

# Extracts the major and minor version numbers.
check_R_version <- function(){
  major <- as.integer(R.Version()$major)
  minor <- as.numeric(strsplit(R.Version()$minor, ".", fixed = TRUE)[[1]][[1]])
  list("major" = major, "minor" = minor)
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

# Numeric Argument Checks

is_wholenumber_ <- function(n) {

  # If n is a whole number
  # .. return TRUE
  # else
  # .. return FALSE

  return( floor(n) == n )
}

arg_is_wholenumber_ <- function(n){

  # Checks if n is a whole number of either
  # type integer or numeric
  # Returns TRUE if yes, else FALSE

  # If n is an integer, return TRUE
  # else check if it is a numeric
  # .. if yes, check if it is a whole number
  # .... if yes, return TRUE
  # .... if no, return FALSE
  # .. if not a numeric
  # .... return FALSE

  if ( is.integer(n) ){

    return(TRUE)

  } else if ( is.numeric(n) ){

    if ( is_wholenumber_(n) ){

      return(TRUE)

    } else {

      return(FALSE)
    }

  } else {

    return(FALSE)
  }
}

arg_is_number_ <- function(n){

  # Checks if n is either an integer or a numeric
  # Returns TRUE if yes, FALSE if no

  if ( is.integer(n) || is.numeric(n) ){

    return(TRUE)

  } else {

    return(FALSE)

  }

}

is_logical_scalar_not_na <- function(arg){
  rlang::is_scalar_logical(arg) && !is.na(arg)
}

is_between_ <- function(x, a, b) {

  # Checks if x is between a and b

  x > a & x < b
}

logsumexp <- function(x) {
  y <- max(x)
  y + log(sum(exp(x - y)))
}

softmax_row <- function(...) {
  x <- unname(c(...))
  x <- exp(x - logsumexp(x))
  # Convert to row in tibble
  # TODO There must be a better way
  x <- dplyr::as_tibble(t(matrix(x)),
                        .name_repair = ~ paste0("V", seq_len(length(x))))
  x
}

softmax_vector <- function(...){
  x <- unname(c(...))
  exp(x - logsumexp(x))
}

# TODO Add tests of this !!!
softmax <- function(data, cols=NULL){

  # Convert to tibble
  data <- dplyr::as_tibble(data)

  # TODO is this necessary?
  if (!is.null(cols)){

    if (is.numeric(cols)){
      data_to_process <- data %>% dplyr::select(cols)
      data_to_leave <- data %>% dplyr::select(-cols)
      cols <- colnames(data_to_process)

      } else if (is.character(cols)){
      data_to_process <- data %>% dplyr::select(dplyr::one_of(cols))
      data_to_leave <- data %>% dplyr::select(-dplyr::one_of(cols))
      }

  } else {
    data_to_process <- data
    data_to_leave <- NULL
    cols <- colnames(data)
  }

  # Test that the probability columns are numeric
  if (any(!sapply(data_to_process, is.numeric))) {
    stop("softmax only works on numeric columns.")
  }

  processed_data <- purrr::pmap_dfr(data_to_process,
           softmax_row)
  colnames(processed_data) <- cols
  dplyr::bind_cols(processed_data, data_to_leave)

}

# Add underscore until var name is unique
create_tmp_var <- function(data, tmp_var = ".tmp_index_"){
  while (tmp_var %in% colnames(data)){
    tmp_var <- paste0(tmp_var, "_")
  }
  tmp_var
}

# Tidyr legacy

# https://tidyr.tidyverse.org/dev/articles/in-packages.html
tidyr_new_interface <- function() {
  utils::packageVersion("tidyr") > "0.8.99"
}

# As the upcoming tidyr v1.0.0 has breaking changes
# to nest (and unnest!), we make sure it's compatible for now
# TODO replace nest_legacy with the new nest syntax within the
# code, once people have moved to that.
legacy_nest <- function(...){
  if (tidyr_new_interface()){
    tidyr::nest_legacy(...)
  } else {
    tidyr::nest(...)
  }
}

legacy_unnest <- function(...){
  if (tidyr_new_interface()){
    tidyr::unnest_legacy(...)
  } else {
    tidyr::unnest(...)
  }
}

# Keras check
# testthat utilty for skipping tests when Keras isn't available
# skip_if_no_keras <- function(version = NULL) {
#   if (!keras::is_keras_available(version))
#     testthat::skip("Required keras version not available for testing")
# }

# Used for checking warnings in testthat
# Why?:
# I had a case where test() used '' but console outputted ‘’
# So I just strip for punctuation in such cases (Should be used sparingly)
strip_for_punctuation <- function(strings){
  gsub("[[:punct:][:blank:]]+", " ", strings)
}

# Wraps tibble::add_column
reposition_column <- function(data, col, .before = NULL, .after = NULL){
  col_values <- data[[col]]
  data[[col]] <- NULL
  data %>%
    tibble::add_column(!!(col) := col_values, .before = .before, .after = .after)
}


arg_not_used <- function(arg, arg_name, family, current_fn, message_fn=message){
  if (!is.null(arg)){
    message_fn(paste0("'",arg_name,"' was not used for ", family, " version of ", current_fn, "()."))
  }
}

# rep_list <- function(l, n, recursive_unlist = FALSE){
#   #unlist(
#     rep(l, n) # , recursive = recursive_unlist)
# }


## *_cross_validate_list args

check_fold_col_factor <- function(data, fold_cols){
  # Check that the fold column(s) is/are factor(s)
  if (length(fold_cols) == 1){
    if (fold_cols %ni% names(data)){
      stop(paste0("'",fold_cols,"' not found in 'data'."))
    }
    stopifnot(is.factor(data[[fold_cols]]))
  } else {
    fcols <- data %>% dplyr::select(dplyr::one_of(fold_cols)) %>%
      sapply(is.factor)
    if (FALSE %in% fcols) {stop("At least one of the fold columns is not a factor.")}
  }
}

# Check metrics argument
check_metrics_list <- function(metrics){

  if (!(is.list(metrics) || metrics == "all")){
    stop("'metrics' must be either a list or the string 'all'.")
  }

  if (is.list(metrics) && length(metrics) > 0){
    if (!rlang::is_named(metrics)){
      stop("when 'metrics' is a non-empty list, it must be a named list.")
    }
  }
}

# Creates initial warnings and messages tibble
# cols: Message, Type, Function
create_warnings_and_messages_tibble <- function(warnings, messages, fn){
  tibble::tibble("Message" = warnings,
                 "Type" = "warning") %>%
    dplyr::bind_rows(tibble::tibble("Message" = messages,
                                    "Type" = "message")) %>%
    dplyr::mutate(Function = fn)
}


# Never used, but removes R CMD check NOTEs
rcmd_import_handler <- function(){
  lifecycle::deprecate_soft()
}

# Wraps dplyr::as_tibble()
# If x is NULL, returns NULL
to_tibble <- function(x, x_name, caller = ""){
  if (!is.null(x)){
    x <- tryCatch({
      dplyr::as_tibble(x)
    }, error = function(e){
      stop(paste0(caller, ": Could not convert '", x_name, "' to a tibble."))
    })
  }
  x
}
