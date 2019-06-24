#' Participant scores
#'
#' Made-up experiment data with 10 participants and two diagnoses.
#' Test scores for 3 sessions per participant, where participants improve their scores each session.
#'
#' @format A data frame with 30 rows and 5 variables:
#' \describe{
#'   \item{participant}{participant identifier, 10 levels}
#'   \item{age}{age of the participant, in years}
#'   \item{diagnosis}{diagnosis of the participant, either 1 or 0}
#'   \item{score}{test score of the participant, on a 0-100 scale}
#'   \item{session}{testing session identifier, 1 to 3}
#' }
#'
#' @name participant.scores
#' @docType data
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @keywords data
#' @importFrom utils data
NULL


#' Precomputed formulas
#'
#' Fixed effect combinations for model formulas with/without two- and three-way interactions.
#' Up to eight fixed effects in total with up to five fixed effects per formula.
#'
#' Effects are represented by the first eight capital letters.
#'
#' Used by \code{\link[cvms:combine_predictors]{combine_predictors}}.
#'
#' @format A data frame with 259,358 rows and 5 variables:
#' \describe{
#'   \item{formula_}{combination of fixed effects, separated by "\code{+}" and "\code{*}"}
#'   \item{max_interaction_size}{maximum interaction size in the formula, up to \code{3}}
#'   \item{max_effect_frequency}{maximum count of an effect in the formula, e.g. the \code{3} A's in \code{"A * B + A * C + A * D"}}
#'   \item{num_effects}{number of unique effects included in the formula}
#'   \item{min_num_fixed_effects}{minimum number of fixed effects required to use the formula,
#'   i.e. the index in the alphabet of the last of the alphabetically ordered effects (letters) in the formula,
#'   so \code{4} for the formula: \code{"A + B + D"}  }
#' }
#'
#'
#' @name precomputed.formulas
#' @docType data
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @keywords data
#' @importFrom utils data
NULL

#' Compatible formula terms
#'
#' 162,660 pairs of compatible terms for building model formulas with up to 15 fixed effects.
#'
#' A term is either a fixed effect or an interaction between fixed effects (up to three-way), where
#' the effects are separated by the "\code{*}" operator.
#'
#' Two terms are compatible if they are not redundant,
#' meaning that both add a fixed effect to the formula. E.g. as the interaction
#' \code{"x1 * x2 * x3"} expands to \code{"x1 + x2 + x3 + x1 * x2 + x1 * x3 + x2 * x3 + x1 * x2 * x3"},
#' the higher order interaction makes these "sub terms" redundant. Note: All terms are compatible with \code{NA}.
#'
#' Effects are represented by the first fifteen capital letters.
#'
#' Used to generate the model formulas for \code{\link[cvms:combine_predictors]{combine_predictors}}.
#'
#' @format A data frame with 162,660 rows and 5 variables:
#' \describe{
#'   \item{left}{term, fixed effect or interaction, with fixed effects separated by "\code{*}"}
#'   \item{right}{term, fixed effect or interaction, with fixed effects separated by "\code{*}"}
#'   \item{max_interaction_size}{maximum interaction size in the two terms, up to \code{3}}
#'   \item{num_effects}{number of unique fixed effects in the two terms, up to \code{5}}
#'   \item{min_num_fixed_effects}{minimum number of fixed effects required to use a formula with the two terms,
#'   i.e. the index in the alphabet of the last of the alphabetically ordered effects (letters) in the two terms,
#'   so \code{4} if \code{left == "A"} and \code{right == "D"} }
#' }
#'
#' @name compatible.formula.terms
#' @docType data
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @keywords data
#' @importFrom utils data
NULL
