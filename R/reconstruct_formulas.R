

#' @title Reconstruct model formulas from results tibbles
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  In the results tibble from cross_validate and validate,
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
reconstruct_formulas <- function(results, topn = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(
    x = results,
    min.rows = 1,
    min.cols = 2,
    col.names = "named",
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(colnames(results),
    must.include = c("Dependent", "Fixed"),
    what = "colnames",
    add = assert_collection
  )
  checkmate::assert_count(
    x = topn,
    null.ok = TRUE,
    positive = TRUE,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (!is.null(topn)) {
    # Subset by topn
    results <- head(x = results, n = topn)
  }

  if ("Random" %ni% colnames(results)) {
    return(paste0(results[["Dependent"]], " ~ ", space_operators(results[["Fixed"]])))
  } else {
    return(paste0(
      results[["Dependent"]], " ~ ", space_operators(results[["Fixed"]]),
      " + ", space_operators(results[["Random"]])
    ))
  }
}

space_operators <- function(strs) {
  strs <- stringr::str_replace_all(strs, "\\+", " + ")
  stringr::str_replace_all(strs, "\\*", " * ")
}
