
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
#' @param data Data frame. Used to extract variables when the formula contains a "\code{.}".
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
simplify_formula <- function(formula, data = NULL){

  # TODO Add option to remove random effects before simplification

  if (is.character(formula))
    formula <- as.formula(formula)

  # Extract variables
  vars <- all.vars(formula)
  y <- vars[1]
  x <- vars[-1]
  if ("." %in% x){
    if (is.null(data)){
      stop("when 'formula' contains a '.', 'data' must be a data frame, not NULL.")
    }
    if (length(x) > 1)
      warning(paste0("simplify_formula(): when a formula contains '.', ",
                     "any other right-hand side terms will be ignored."))
    x <- colnames(data)
    x <- x[x != y]
  }

  # Create simplified formula
  form <- paste0(y, " ~ ", paste0(x, collapse = " + "))
  form <- as.formula(form)
  form
}
