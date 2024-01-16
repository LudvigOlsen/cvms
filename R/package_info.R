#' cvms: A package for cross-validating regression and classification models
#'
#' Perform (repeated) cross-validation on a list of model formulas. Validate the best model on a validation set.
#' Perform baseline evaluations on your test set. Generate model formulas by combining your fixed effects.
#' Evaluate predictions from an external model.
#'
#' Returns results in a \code{tibble} for easy comparison, reporting and further analysis.
#'
#' The main functions are:
#' \code{\link[cvms:cross_validate]{cross_validate()}},
#' \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}},
#' \code{\link[cvms:validate]{validate()}},
#' \code{\link[cvms:validate_fn]{validate_fn()}},
#' \code{\link[cvms:baseline]{baseline()}},
#' and \code{\link[cvms:evaluate]{evaluate()}}.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @docType _PACKAGE
#' @name cvms
NULL


# R CMD check NOTE handling
if (getRversion() >= "2.15.1") utils::globalVariables(c("."))
