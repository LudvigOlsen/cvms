#   __________________ #< ccfcdb8c94c692518bdaf6f1509380e1 ># __________________
#   Cross-validate lm lmer glm glmer                                        ####


#' @title Cross-validate regression models for model selection
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}
#'
#'  Cross-validate one or multiple linear or logistic regression
#'  models at once. Perform repeated cross-validation.
#'  Returns results in a \code{tibble} for easy comparison,
#'  reporting and further analysis.
#'
#'  See \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}} for use
#'  with custom model functions.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @author Benjamin Hugh Zachariae
#' @export
#' @family validation functions
#' @param data \code{data.frame}.
#'
#'  Must include one or more grouping factors for identifying folds
#'   - as made with \code{\link[groupdata2:fold]{groupdata2::fold()}}.
#' @param formulas Model formulas as strings. (Character)
#'
#'  E.g. \code{c("y~x", "y~z")}.
#'
#'  Can contain random effects.
#'
#'  E.g. \code{c("y~x+(1|r)", "y~z+(1|r)")}.
#' @param fold_cols Name(s) of grouping factor(s) for identifying folds. (Character)
#'
#'  Include names of multiple grouping factors for repeated cross-validation.
#' @param family Name of the family. (Character)
#'
#'  Currently supports \strong{\code{"gaussian"}} for linear regression
#'  with \code{\link[stats:lm]{lm()}} / \code{\link[lme4:lmer]{lme4::lmer()}}
#'  and \strong{\code{"binomial"}} for binary classification
#'  with \code{\link[stats:glm]{glm()}} / \code{\link[lme4:glmer]{lme4::glmer()}}.
#'
#'  See \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}} for use with other model functions.
#' @param control Construct control structures for mixed model fitting
#'  (with \code{\link[lme4:lmer]{lme4::lmer()}} or \code{\link[lme4:glmer]{lme4::glmer()}}).
#'  See \code{\link[lme4:lmerControl]{lme4::lmerControl}} and
#'  \code{\link[lme4:glmerControl]{lme4::glmerControl}}.
#'
#'  N.B. Ignored if fitting \code{\link[stats:lm]{lm()}} or \code{\link[stats:glm]{glm()}} models.
#' @param REML Restricted Maximum Likelihood. (Logical)
#' @param cutoff Threshold for predicted classes. (Numeric)
#'
#'  N.B. \strong{Binomial models only}
#' @param positive Level from dependent variable to predict.
#'  Either as character (\emph{preferable}) or level index (\code{1} or \code{2} - alphabetically).
#'
#'  E.g. if we have the levels \code{"cat"} and \code{"dog"} and we want \code{"dog"} to be the positive class,
#'  we can either provide \code{"dog"} or \code{2}, as alphabetically, \code{"dog"} comes after \code{"cat"}.
#'
#'  \strong{Note:} For \emph{reproducibility}, it's preferable to \strong{specify the name directly}, as
#'  different \code{\link[base:locales]{locales}} may sort the levels differently.
#'
#'  Used when calculating confusion matrix metrics and creating \code{ROC} curves.
#'
#'  The \code{Process} column in the output can be used to verify this setting.
#'
#'  N.B. Only affects evaluation metrics, not the model training or returned predictions.
#'
#'  N.B. \strong{Binomial models only}.
#' @param metrics \code{list} for enabling/disabling metrics.
#'
#'  E.g. \code{list("RMSE" = FALSE)} would remove \code{RMSE} from the results,
#'  and \code{list("Accuracy" = TRUE)} would add the regular \code{Accuracy} metric
#'  to the classification results.
#'  Default values (\code{TRUE}/\code{FALSE}) will be used for the remaining available metrics.
#'
#'  You can enable/disable all metrics at once by including
#'  \code{"all" = TRUE/FALSE} in the \code{list}. This is done prior to enabling/disabling
#'  individual metrics, why \code{list("all" = FALSE, "RMSE" = TRUE)}
#'  would return only the \code{RMSE} metric.
#'
#'  The \code{list} can be created with
#'  \code{\link[cvms:gaussian_metrics]{gaussian_metrics()}} or
#'  \code{\link[cvms:binomial_metrics]{binomial_metrics()}}.
#'
#'  Also accepts the string \code{"all"}.
#' @param preprocessing Name of preprocessing to apply.
#'
#'  Available preprocessings are:
#'
#'  \tabular{rrr}{
#'   \strong{Name} \tab \strong{Description} \cr
#'   "standardize" \tab Centers and scales the numeric predictors.\cr
#'   "range" \tab Normalizes the numeric predictors to the \code{0}-\code{1} range.
#'   Values outside the min/max range in the test fold are truncated to \code{0}/\code{1}.\cr
#'   "scale" \tab Scales the numeric predictors to have a standard deviation of one.\cr
#'   "center" \tab Centers the numeric predictors to have a mean of zero.\cr
#'  }
#'
#'  The preprocessing parameters (\code{mean}, \code{SD}, etc.) are extracted from the training folds and
#'  applied to both the training folds and the test fold.
#'  They are returned in the \strong{Preprocess} column for inspection.
#'
#'  N.B. The preprocessings should not affect the results
#'  to a noticeable degree, although \code{"range"} might due to the truncation.
#' @param rm_nc Remove non-converged models from output. (Logical)
#' @param verbose Whether to message process information
#'  like the number of model instances to fit and which model function was applied. (Logical)
#' @param parallel Whether to cross-validate the \code{list} of models in parallel. (Logical)
#'
#'  Remember to register a parallel backend first.
#'  E.g. with \code{doParallel::registerDoParallel}.
#' @details
#'
#'  Packages used:
#'
#'  \subsection{Models}{
#'
#'  Gaussian: \code{\link[stats:lm]{stats::lm}}, \code{\link[lme4:lmer]{lme4::lmer}}
#'
#'  Binomial: \code{\link[stats:glm]{stats::glm}}, \code{\link[lme4:glmer]{lme4::glmer}}
#'  }
#'  \subsection{Results}{
#'  \subsection{Shared}{
#'
#'  \code{AIC} : \code{\link[stats:AIC]{stats::AIC}}
#'
#'  \code{AICc} : \code{\link[MuMIn:AICc]{MuMIn::AICc}}
#'
#'  \code{BIC} : \code{\link[stats:BIC]{stats::BIC}}
#'
#'  }
#'  \subsection{Gaussian}{
#'
#'  \code{r2m} : \code{\link[MuMIn:r.squaredGLMM]{MuMIn::r.squaredGLMM}}
#'
#'  \code{r2c} : \code{\link[MuMIn:r.squaredGLMM]{MuMIn::r.squaredGLMM}}
#'
#'  }
#'  \subsection{Binomial}{
#'
#'  \code{ROC and AUC}: \code{\link[pROC:roc]{pROC::roc}}
#'
#'  }
#'  }
#' @return
#'  \code{tibble} with results for each model.
#'
#'  \subsection{Shared across families}{
#'  A nested \code{tibble} with \strong{coefficients} of the models from all iterations.
#'
#'  Number of \emph{total} \strong{folds}.
#'
#'  Number of \strong{fold columns}.
#'
#'  Count of \strong{convergence warnings}. Consider discarding models that did not converge on all
#'  iterations. Note: you might still see results, but these should be taken with a grain of salt!
#'
#'  Count of \strong{other warnings}. These are warnings without keywords such as "convergence".
#'
#'  Count of \strong{Singular Fit messages}.
#'  See \code{\link[lme4:isSingular]{lme4::isSingular}} for more information.
#'
#'  Nested \code{tibble} with the \strong{warnings and messages} caught for each model.
#'
#'  A nested \strong{Process} information object with information
#'  about the evaluation.
#'
#'  Name of \strong{dependent} variable.
#'
#'  Names of \strong{fixed} effects.
#'
#'  Names of \strong{random} effects, if any.
#'
#'  Nested \code{tibble} with \strong{preprocess}ing parameters, if any.
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Gaussian Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  Average \strong{\code{RMSE}}, \strong{\code{MAE}}, \strong{\code{NRMSE(IQR)}},
#'  \strong{\code{RRSE}}, \strong{\code{RAE}}, \strong{\code{RMSLE}},
#'  \strong{\code{AIC}}, \strong{\code{AICc}},
#'  and \strong{\code{BIC}} of all the iterations*,
#'  \emph{\strong{omitting potential NAs} from non-converged iterations}.
#'  Note that the Information Criterion metrics (\code{AIC}, \code{AICc}, and \code{BIC}) are also averages.
#'
#'  See the additional metrics (disabled by default) at \code{\link[cvms:gaussian_metrics]{?gaussian_metrics}}.
#'
#'  A nested \code{tibble} with the \strong{predictions} and targets.
#'
#'  A nested \code{tibble} with the non-averaged \strong{results} from all iterations.
#'
#'  * In \emph{repeated cross-validation},
#'  the metrics are first averaged for each fold column (repetition) and then averaged again.
#'
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Binomial Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  Based on the \strong{collected} predictions from the test folds*,
#'  a confusion matrix and a \code{ROC} curve are created to get the following:
#'
#'  \code{ROC}:
#'
#'  \strong{\code{AUC}}, \strong{\code{Lower CI}}, and \strong{\code{Upper CI}}
#'
#'  \code{Confusion Matrix}:
#'
#'  \strong{\code{Balanced Accuracy}},
#'  \strong{\code{F1}},
#'  \strong{\code{Sensitivity}},
#'  \strong{\code{Specificity}},
#'  \strong{\code{Positive Predictive Value}},
#'  \strong{\code{Negative Predictive Value}},
#'  \strong{\code{Kappa}},
#'  \strong{\code{Detection Rate}},
#'  \strong{\code{Detection Prevalence}},
#'  \strong{\code{Prevalence}}, and
#'  \strong{\code{MCC}} (Matthews correlation coefficient).
#'
#'  See the additional metrics (disabled by default) at
#'  \code{\link[cvms:binomial_metrics]{?binomial_metrics}}.
#'
#'  Also includes:
#'
#'  A nested \code{tibble} with \strong{predictions}, predicted classes (depends on \code{cutoff}), and the targets.
#'  Note, that the predictions are \emph{not necessarily} of the \emph{specified} \code{positive} class, but of
#'  the \emph{model's} positive class (second level of dependent variable, alphabetically).
#'
#'  The \code{\link[pROC:roc]{pROC::roc}} \strong{\code{ROC}} curve object(s).
#'
#'  A nested \code{tibble} with the \strong{confusion matrix}/matrices.
#'  The \code{Pos_} columns tells you whether a row is a
#'  True Positive (\code{TP}), True Negative (\code{TN}),
#'  False Positive (\code{FP}), or False Negative (\code{FN}),
#'  depending on which level is the "positive" class. I.e. the level you wish to predict.
#'
#'  A nested \code{tibble} with the \strong{results} from all fold columns.
#'
#'  The name of the \strong{Positive Class}.
#'
#'  * In \emph{repeated cross-validation}, an evaluation is made per fold column (repetition) and averaged.
#'
#'  }
#' @examples
#' \donttest{
#' # Attach packages
#' library(cvms)
#' library(groupdata2) # fold()
#' library(dplyr) # %>% arrange()
#'
#' # Data is part of cvms
#' data <- participant.scores
#'
#' # Set seed for reproducibility
#' set.seed(7)
#'
#' # Fold data
#' data <- fold(
#'   data,
#'   k = 4,
#'   cat_col = "diagnosis",
#'   id_col = "participant"
#' ) %>%
#'   arrange(.folds)
#'
#' #
#' # Cross-validate a single model
#' #
#'
#' # Gaussian
#' cross_validate(
#'   data,
#'   formulas = "score~diagnosis",
#'   family = "gaussian",
#'   REML = FALSE
#' )
#'
#' # Binomial
#' cross_validate(
#'   data,
#'   formulas = "diagnosis~score",
#'   family = "binomial"
#' )
#'
#' #
#' # Cross-validate multiple models
#' #
#'
#' formulas <- c(
#'   "score~diagnosis+(1|session)",
#'   "score~age+(1|session)"
#' )
#'
#' cross_validate(
#'   data,
#'   formulas = formulas,
#'   family = "gaussian",
#'   REML = FALSE
#' )
#'
#' #
#' # Use parallelization
#' #
#'
#' # Attach doParallel and register four cores
#' # Uncomment:
#' # library(doParallel)
#' # registerDoParallel(4)
#'
#' # Cross-validate a list of model formulas in parallel
#' # Make sure to uncomment the parallel argument
#' cross_validate(
#'   data,
#'   formulas = formulas,
#'   family = "gaussian"
#'   # , parallel = TRUE  # Uncomment
#' )
#' }
#' @importFrom stats binomial gaussian glm lm
#' @importFrom rlang .data
#' @importFrom lifecycle deprecated deprecate_warn deprecate_stop
cross_validate <- function(
  data,
  formulas,
  family,
  fold_cols = ".folds",
  control = NULL,
  REML = FALSE,
  cutoff = 0.5,
  positive = 2,
  metrics = list(),
  preprocessing = NULL,
  rm_nc = FALSE,
  parallel = FALSE,
  verbose = FALSE
) {
  # This function does not accept 'multinomial'
  checkmate::assert_choice(
    x = family,
    choices = c(
      "gaussian",
      "binomial"
    )
  )

  call_cross_validate(
    data = data,
    formulas = formulas,
    fold_cols = fold_cols,
    family = family,
    preprocessing = preprocessing,
    control = control,
    REML = REML,
    cutoff = cutoff,
    positive = positive,
    metrics = metrics,
    rm_nc = rm_nc,
    parallel = parallel,
    verbose = verbose
  )
}
