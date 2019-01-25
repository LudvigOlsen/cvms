
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
  unlist(strsplit(model, '\\s*~'))[1]
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
  iter_models <- iter_models %>%
    mutate(p.value = ifelse(exists('p.value', where = iter_models), p.value, NA)) %>%
    tidyr::nest() %>%
    dplyr::rename(coefficients = data)

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
