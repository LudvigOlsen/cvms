

#' @title Reconstruct model formulas from results tibbles
#' @description In the results tibble from cross_validate and validate,
#'  the model formulas have been split into the columns Dependent, Fixed and Random.
#'  Quickly reconstruct the model formulas from these columns.
#' @return
#'  List of model formulas.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param results Data frame with results from \code{\link{cross_validate}()} or \code{\link{validate}()}. (tbl)
#'
#'  Must contain at least the columns "\code{Dependent}" and "\code{Fixed}". For random effects
#'  the "\code{Random}" column should be included.
#' @param topn Number of top rows to return. Simply applies head() to the results tibble.
reconstruct_formulas <- function(results, topn=NULL){

  cols_ <- colnames(results)
  if ("Dependent" %ni% cols_) stop("results must contain the column 'Dependent'.")
  if ("Fixed" %ni% cols_) stop("results must contain the column 'Fixed'.")

  if (!is.null(topn)){
    if (!is.integer(topn) && !is.numeric(topn)) stop("topn must be numeric, integer or NULL.")

    # Subset by topn
    results <- head(x=results, n=topn)
  }

  if ("Random" %ni% cols_){
    return( paste0(results[["Dependent"]], " ~ ", space_operators(results[["Fixed"]])) )
  } else {
    return( paste0(results[["Dependent"]], " ~ ", space_operators(results[["Fixed"]]),
                   " + ", space_operators(results[["Random"]]) ))
  }


}

space_operators <- function(strs){
  strs <- stringr::str_replace_all(strs, "\\+", " + ")
  stringr::str_replace_all(strs, "\\*", " * ")
}
