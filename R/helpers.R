
# Get all lists in a list with a certain name
# Use: list_of_lists %c% 'list_name'
`%c%` <- function(x, n)lapply(x, `[[`, n)
# From http://stackoverflow.com/questions/5935673/accessing-same-named-list-elements-of-the-list-of-lists-in-r/5936077#5936077

default_link <- function(link, family){
  if (is.null(link)){
    if(family == 'gaussian') return('identity')
    if(family == 'binomial') return('logit')
  } else return(link)

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
