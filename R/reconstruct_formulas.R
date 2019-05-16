

#' @title Reconstruct model formulas from results tibbles
#' @description In the results tibble from cross_validate and validate,
#'  the model formulas have been split into the columns Dependent, Fixed and Random.
#'  Quickly reconstruct the model formulas from these column.
#'
#'  NOTE: Currently only works properly without random effects!
#' @return
#'  List of model formulas.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @param results Data frame with results from cross_validate or validate(). (tbl)
#'
#'  Must contain at least the columns "Dependent" and "Fixed". For random effects
#'  the "Random" column should be included.
#' @param topn Number of top rows to return. Simply applies head() to the results tibble.
#'
#'  Set to NULL to return all rows.
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
    # TODO Fix formatting (add paranthesis etc., what if multiple random effect specifications?)
    return( paste0(results[["Dependent"]], " ~ ", space_operators(results[["Fixed"]]), " + ", results[["Random"]]) )
  }


}

space_operators <- function(strs){
  strs <- stringr::str_replace_all(strs, "\\+", " + ")
  stringr::str_replace_all(strs, "\\*", " * ")
}
