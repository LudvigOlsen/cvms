#' cvms: A package for cross-validating gaussian and binomial regression models
#'
#' Perform (repeated) cross-validation on a list of model formulas. Validate the best model on a validation set.
#' Perform baseline evaluations on your test set. Generate model formulas by combining your fixed effects.
#' Evaluate predictions from an external model.
#'
#' Returns results in a tibble for easy comparison, reporting and further analysis.
#'
#' The cvms package provides 4 main functions:
#' \code{cross_validate}, \code{validate}, \code{baseline}, and \code{evaluate}.
#'
#' And a couple of helper functions:
#' \code{combine_predictors}, \code{select_metrics}, \code{reconstruct_formulas}, \code{cv_plot}.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @docType package
#' @name cvms
NULL
