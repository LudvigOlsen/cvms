
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
