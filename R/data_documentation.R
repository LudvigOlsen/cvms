

#   __________________ #< 46cee5975e2599650854199c167237ea ># __________________
#   Participant scores                                                      ####


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


#   __________________ #< b6c73b247744b99379849097e8fb31d9 ># __________________
#   Wine varieties                                                          ####


#' Wine varieties
#'
#' A list of wine varieties in an approximately Zipfian distribution, ordered by descending frequencies.
#'
#' Based on the wine-reviews (v4) kaggle dataset by Zack Thoutt:
#' https://www.kaggle.com/zynicide/wine-reviews
#'
#' @format A data frame with 368 rows and 1 variable:
#' \describe{
#'   \item{Variety}{Wine variety, 10 levels}
#' }
#'
#' @name wines
#' @docType data
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @keywords data
NULL


#   __________________ #< 07b8cd91e79d9ef88b399ee7eb9c13b6 ># __________________
#   Musicians                                                               ####


#' Musician groups
#'
#' Made-up data on 60 musicians in 4 groups for multiclass classification.
#'
#' @format A data frame with 60 rows and 9 variables:
#' \describe{
#'   \item{ID}{Musician identifier, 60 levels}
#'   \item{Age}{Age of the musician. Between 17 and 66 years.}
#'   \item{Class}{The class of the musician. One of \code{"A"}, \code{"B"}, \code{"C"}, and \code{"D"}.}
#'   \item{Height}{Height of the musician. Between \code{146} and \code{196} centimeters.}
#'   \item{Drums}{Whether the musician plays drums. \code{0} = No, \code{1} = Yes.}
#'   \item{Bass}{Whether the musician plays bass. \code{0} = No, \code{1} = Yes.}
#'   \item{Guitar}{Whether the musician plays guitar. \code{0} = No, \code{1} = Yes.}
#'   \item{Keys}{Whether the musician plays keys. \code{0} = No, \code{1} = Yes.}
#'   \item{Vocals}{Whether the musician sings. \code{0} = No, \code{1} = Yes.}
#' }
#'
#' @name musicians
#' @docType data
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @keywords data
#' @seealso predicted.musicians
NULL


#   __________________ #< 57249e222977febb757f8e3f7e83d239 ># __________________
#   Predicted musician groups                                               ####

#' Predicted musician groups
#'
#' Predictions by 3 classifiers of the 4 classes in the
#' \code{\link[cvms:musicians]{musicians}} dataset.
#' Obtained with 5-fold stratified cross-validation (3 repetitions).
#' The three classifiers were fit using \code{nnet::multinom},
#' \code{randomForest::randomForest}, and \code{e1071::svm}.
#'
#' Used formula: "Class ~ Height + Age + Drums + Bass + Guitar + Keys + Vocals"
#'
#' @format A data frame with 540 rows and 10 variables:
#' \describe{
#'   \item{Classifier}{The applied classifier.
#'   One of \code{"nnet_multinom"}, \code{"randomForest"}, and \code{"e1071_svm"}.}
#'   \item{Fold Column}{The fold column name. Each is a unique 5-fold split.
#'   One of \code{".folds_1"}, \code{".folds_2"}, and \code{".folds_3"}.}
#'   \item{Fold}{The fold. \code{1} to \code{5}.}
#'   \item{ID}{Musician identifier, 60 levels}
#'   \item{Target}{The actual class of the musician.
#'   One of \code{"A"}, \code{"B"}, \code{"C"}, and \code{"D"}.}
#'   \item{A}{The probability of class \code{"A"}.}
#'   \item{B}{The probability of class \code{"B"}.}
#'   \item{C}{The probability of class \code{"C"}.}
#'   \item{D}{The probability of class \code{"D"}.}
#'   \item{Predicted Class}{The predicted class. The argmax of the four probability columns.}
#' }
#'
#' @name predicted.musicians
#' @docType data
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @keywords data
#' @seealso musicians
#' @examples
#' # Attach packages
#' library(cvms)
#' library(dplyr)
#'
#' \donttest{
#' # Evaluate each fold column
#' predicted.musicians %>%
#'   dplyr::group_by(Classifier, `Fold Column`) %>%
#'   evaluate(target_col = "Target",
#'            prediction_cols = c("A", "B", "C", "D"),
#'            type = "multinomial")
#'
#' # Overall ID evaluation
#' # I.e. if we average all 9 sets of predictions,
#' # how well did we predict the targets?
#' overall_id_eval <- predicted.musicians %>%
#'   evaluate(target_col = "Target",
#'            prediction_cols = c("A", "B", "C", "D"),
#'            type = "multinomial",
#'            id_col = "ID")
#' overall_id_eval
#' # Plot the confusion matrix
#' plot_confusion_matrix(overall_id_eval$`Confusion Matrix`[[1]])
#' }
NULL

#   __________________ #< fd40ca54a2ecc06121fe9aa0e3b6d351 ># __________________
#   Precomputed formulas                                                    ####


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
#' @name precomputed.formulas
#' @docType data
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @keywords data
NULL


#   __________________ #< a1c9080fd9157d0da24b06e43f86dc1f ># __________________
#   Compatible formula terms                                                ####


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
NULL
