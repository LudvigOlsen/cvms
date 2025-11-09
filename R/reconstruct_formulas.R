#' @title Reconstruct model formulas from results tibbles
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  In the (cross-)validation results from functions like
#'  \code{\link[cvms:cross_validate]{cross_validate()}},
#'  the model formulas have been split into the columns
#'  \code{Dependent}, \code{Fixed} and \code{Random}.
#'  Quickly reconstruct the model formulas from these columns.
#' @return
#'  \code{list} of model formulas.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param results \code{data.frame} with results from
#' \code{\link[cvms:cross_validate]{cross_validate()}}
#'  or \code{\link[cvms:validate]{validate()}}. (tbl)
#'
#'  Must contain at least the columns \code{"Dependent"} and \code{"Fixed"}. For random effects,
#'  the \code{"Random"} column should be included.
#' @param topn Number of top rows to return. Simply applies \code{head()} to the results \code{tibble}.
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
