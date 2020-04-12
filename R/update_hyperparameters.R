

#   __________________ #< 60cfc78f594e5611a6eaaf34a2b212ae ># __________________
#   Update hyperparameters                                                  ####


#' @title Check and update hyperparameters
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  \enumerate{
#'    \item Checks if the required hyperparameters are present and
#'          throws an error when it is not the case.
#'    \item Inserts the missing hyperparameters with the supplied
#'          default values.
#'  }
#'
#'  For managing hyperparameters in custom model functions for
#'  \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}} or
#'  \code{\link[cvms:validate_fn]{validate_fn()}}.
#' @return A named list with the updated hyperparameters.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family example functions
#' @param ... Default values for missing hyperparameters.
#'
#'  E.g.:
#'
#'  \code{kernel = "linear", cost = 10}
#' @param hyperparameters List of hyperparameters as supplied to
#'  \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}}.
#'  Can also be a single-row data frame.
#' @param .required Names of required hyperparameters. If any of these
#'  are not present in the hyperparameters, an error is thrown.
#' @examples
#' \donttest{
#' # Attach packages
#' library(cvms)
#'
#' # Create a list of hyperparameters
#' hparams <- list(
#'   "kernel" = "radial",
#'   "scale" = TRUE
#' )
#'
#' # Update hyperparameters with defaults
#' # Only 'cost' is changed as it's missing
#' update_hyperparameters(
#'   cost = 10,
#'   kernel = "linear",
#'   "scale" = FALSE,
#'   hyperparameters = hparams
#' )
#'
#' # 'cost' is required
#' # throws error
#' xpectr::capture_side_effects(
#'   update_hyperparameters(
#'     kernel = "linear",
#'     "scale" = FALSE,
#'     hyperparameters = hparams,
#'     .required = "cost"
#'   )
#' )
#'
#' }
update_hyperparameters <- function(..., hyperparameters, .required = NULL){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert(
    checkmate::check_list(hyperparameters, names = "unique"),
    checkmate::check_data_frame(hyperparameters, col.names = "unique", nrows = 1)
  )
  checkmate::assert_character(
    .required,
    any.missing = FALSE,
    null.ok = TRUE,
    unique = TRUE,
    names = "unnamed",
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Check the required hyperparameters are present
  if (!is.null(.required) &&
      length(setdiff(.required, names(hyperparameters)))>0) {
    stop(paste0(
      "these hyperparameters are required but were not present: ",
      paste0(setdiff(.required, names(hyperparameters)), collapse = ", ")
    ))
  }

  # Update each key -> val pair
  key_val_pairs <- list(...)
  if (length(key_val_pairs) > 0){
    keys <- non_empty_names(key_val_pairs)
    if (length(keys) != length(key_val_pairs)){
      stop("all arguments in '...' must be named.")
    }
    hp_keys <- non_empty_names(hyperparameters)
    all_keys <- union(keys, hp_keys)
    hyperparameters <- plyr::llply(all_keys, function(k){
      if (k %ni% hp_keys){
        hp_val <- key_val_pairs[[k]]
      } else {
        hp_val <- hyperparameters[[k]]
      }
      hp_val
    }) %>% setNames(nm = all_keys)
  }

  hyperparameters

}
