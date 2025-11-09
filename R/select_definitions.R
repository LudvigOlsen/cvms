#' @title Select model definition columns
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Select the columns that define the models, such as the formula terms
#'  and hyperparameters.
#'
#'  If an expected column is not in the \code{`results`} \code{tibble}, it is simply ignored.
#' @return
#'  The model definition columns from the results \code{tibble}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param results Results \code{tibble}. E.g. from
#'  \code{\link[cvms:cross_validate]{cross_validate()}} or \code{\link[cvms:evaluate]{evaluate()}}.
#' @param unnest_hparams Whether to unnest the \code{HParams} column. (Logical)
#' @param additional_includes Names of additional columns to select. (Character)
select_definitions <- function(results,
                               unnest_hparams = TRUE,
                               additional_includes = NULL) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(
    x = results,
    min.rows = 1, min.cols = 1,
    col.names = "named",
    add = assert_collection
  )
  checkmate::assert_flag(x = unnest_hparams, add = assert_collection)
  checkmate::assert_character(
    x = additional_includes, null.ok = TRUE,
    add = assert_collection
  )
  if (!is.null(additional_includes)) {
    checkmate::reportAssertions(assert_collection)
    checkmate::assert_names(
      x = colnames(results),
      must.include = additional_includes,
      what = "colnames"
    )
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  definition_cols <- c("Dependent", "Fixed", "Random", "HParams")
  definition_cols <- intersect(definition_cols, colnames(results))
  definition_cols <- add_additional_colnames(
    definition_cols, additional_includes
  )

  definitions <- base_select(results, cols = definition_cols)

  if (isTRUE(unnest_hparams) && "HParams" %in% definition_cols) {
    definitions <- tidyr::unnest(definitions, cols = "HParams")
  }

  definitions
}
