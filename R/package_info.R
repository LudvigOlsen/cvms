#' cvms: A package for cross-validating regression and classification models
#'
#' Perform (repeated) cross-validation on a list of model formulas. Validate the best model on a validation set.
#' Perform baseline evaluations on your test set. Generate model formulas by combining your fixed effects.
#' Evaluate predictions from an external model.
#'
#' Returns results in a tibble for easy comparison, reporting and further analysis.
#'
#' The cvms package provides 6 main functions:
#' \code{cross_validate}, \code{cross_validate_fn}, \code{validate}, \code{validate_fn},
#' \code{baseline}, and \code{evaluate}.
#'
#' And a couple of helper functions:
#' \code{combine_predictors}, \code{select_metrics}, \code{reconstruct_formulas},
#' \code{most_challenging}, \code{cv_plot}.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @docType package
#' @name cvms
NULL


# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
