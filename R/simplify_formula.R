
#' @title Simplify formula with inline functions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Extracts all variables from a formula object and creates
#'  a new formula with all predictor variables added together without the
#'  inline functions.
#'
#'  E.g.:
#'
#'  \code{y ~ x*z + log(a) + (1|b)}
#'
#'  becomes
#'
#'  \code{y ~ x + z + a + b}.
#'
#'  This is useful when passing a formula to \code{\link[recipes:recipe]{recipes::recipe()}}
#'  for preprocessing a dataset, as used in the
#'  \code{\link[cvms:example_preprocess_functions]{example_preprocess_functions()}}.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param formula Formula object.
#'
#'  If a string is passed, it will be converted with \code{\link[stats:as.formula]{as.formula()}}.
#'
#'  When a side \emph{only} contains a \code{NULL}, it is kept. Otherwise \code{NULL}s are removed.
#'
#'  An intercept (\code{1}) will only be kept if there are no variables on that side of the formula.
#' @param data Data frame. Used to extract variables when the formula contains a "\code{.}".
#' @param string_out Whether to return as a string. (Logical)
#' @examples
#' # Attach cvms
#' library(cvms)
#'
#' # Create formula
#' f1 <- "y ~ x*z + log(a) + (1|b)"
#'
#' # Simplify formula (as string)
#' simplify_formula(f1)
#'
#' # Simplify formula (as formula)
#' simplify_formula(as.formula(f1))
#' @importFrom stats as.formula
simplify_formula <- function(formula, data = NULL, string_out = FALSE) {

  # Convert to formula object
  if (is.character(formula)) {
    formula <- tryCatch(as.formula(formula), error = function(e){
      stop(paste0("Could not convert 'formula' to a formula object. Got error:\n  ", e))
    })
    envir <- globalenv()
  } else if (rlang::is_formula(formula)) {
    # If formula has a special environment,
    # we wish to keep that
    # Errors will be messaged later, so we just ignore them for now
    envir <- tryCatch(attributes(terms(formula, data = data))$.Environment,
                      error = function(e){return(NULL)})
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(
    x = data, null.ok = TRUE,
    min.cols = 1, add = assert_collection
  )
  checkmate::assert_formula(x = formula, add = assert_collection)
  checkmate::assert_flag(x = string_out, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (as.character(formula)[[1]] != "~"){
    assert_collection$push("'formula' did not contain '~'. This use case is not currently supported.")
  }
  checkmate::reportAssertions(assert_collection)
  tryCatch(terms(formula, data = data), error = function(e){
    assert_collection$push(paste0(
      "Could not extract variables from 'formula': ", e
    ))
  })
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # TODO Add option to remove random effects before simplification

  # Extract variables
  # Get the lhs and rhs
  # Note that we need to do it like this instead of with as.character
  # for cases with no lhs or rhs
  formula_parts <- unlist(strsplit(deparse(formula), "~"))
  y <- get_vars_from_side(formula_parts[[1]])
  x <- get_vars_from_side(formula_parts[[2]])
  if (length(x) == 0){
    stop("the rhs of 'formula' did not contain any variables or allowed values.")
  }
  if ("." %in% x) {
    if (is.null(data))
      stop("when 'formula' contains a '.', 'data' must be a data frame, not NULL.")
    if (length(x) > 1){
      warning(paste0(
        "simplify_formula(): when a formula contains '.', ",
        "any other right-hand side terms will be ignored."
      ))
    }
    x <- colnames(data)
    x <- x[x %ni% y]
  }

  # Create simplified formula
  form <- paste0(paste0(y, collapse = " + "), " ~ ", paste0(x, collapse = " + "))
  if (!isTRUE(string_out))
    form <- as.formula(form, env = envir)
  form
}

# Get all vars from one side of a formula
get_vars_from_side <- function(string){
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = string, add = assert_collection)
  if (grepl("~", string))
    assert_collection$push("'string' cannot contain '~'. Use on one side at a time only.")
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (trimws(string) %in% c("", "NULL", "1")){
    return(trimws(string))
  }
  all.vars(as.formula(paste0("~", string)))
}
