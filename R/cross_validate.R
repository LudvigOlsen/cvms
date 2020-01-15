

#   __________________ #< ccfcdb8c94c692518bdaf6f1509380e1 ># __________________
#   Cross-validate lm lmer glm glmer                                        ####


#' @title Cross-validate regression models for model selection
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}
#'
#'  Cross-validate one or multiple linear or logistic regression
#'  models at once. Perform repeated cross-validation.
#'  Returns results in a tibble for easy comparison,
#'  reporting and further analysis.
#'
#'  See \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}} for use
#'  with custom model functions.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @author Benjamin Hugh Zachariae
#' @export
#' @family validation functions
#' @param data Data frame.
#'
#'  Must include grouping factor for identifying folds
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
#' @param family Name of family. (Character)
#'
#'  Currently supports \code{"gaussian"} and \code{"binomial"}.
#' @param control Construct control structures for mixed model fitting
#'  (i.e. \code{\link[lme4]{lmer}} and \code{\link[lme4]{glmer}}).
#'  See \code{\link[lme4:lmerControl]{lme4::lmerControl}} and
#'  \code{\link[lme4:glmerControl]{lme4::glmerControl}}.
#'
#'  N.B. Ignored if fitting \code{\link[stats]{lm}} or \code{\link[stats]{glm}} models.
#' @param REML Restricted Maximum Likelihood. (Logical)
#' @param cutoff Threshold for predicted classes. (Numeric)
#'
#'  N.B. \strong{Binomial models only}
#' @param positive Level from dependent variable to predict.
#'  Either as character or level index (\code{1} or \code{2} - alphabetically).
#'
#'  E.g. if we have the levels \code{"cat"} and \code{"dog"} and we want \code{"dog"} to be the positive class,
#'  we can either provide \code{"dog"} or \code{2}, as alphabetically, \code{"dog"} comes after \code{"cat"}.
#'
#'  Used when calculating confusion matrix metrics and creating ROC curves.
#'
#'  N.B. Only affects evaluation metrics, not the model training or returned predictions.
#'
#'  N.B. \strong{Binomial models only}.
#' @param metrics List for enabling/disabling metrics.
#'
#'   E.g. \code{list("RMSE" = FALSE)} would remove RMSE from the results,
#'   and \code{list("Accuracy" = TRUE)} would add the regular accuracy metric
#'   to the classification results.
#'   Default values (TRUE/FALSE) will be used for the remaining available metrics.
#'
#'   You can enable/disable all metrics at once by including
#'   \code{"all" = TRUE/FALSE} in the list. This is done prior to enabling/disabling
#'   individual metrics, why f.i. \code{list("all" = FALSE, "RMSE" = TRUE)} would return only the RMSE metric.
#'
#'   Also accepts the string \code{"all"}.
#' @param preprocessing Name of preprocessing to apply.
#'
#'  Available preprocessings are:
#'
#'  \tabular{rrr}{
#'   \strong{Name} \tab \strong{Description} \cr
#'   "standardize" \tab Centers and scales the numeric predictors.\cr
#'   "range" \tab Normalizes the numeric predictors to the 0-1 range.
#'   Values outside the min/max range in the test fold are truncated to 0/1.\cr
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
#' @param parallel Whether to cross-validate the list of models in parallel. (Logical)
#'
#'  Remember to register a parallel backend first.
#'  E.g. with \code{doParallel::registerDoParallel}.
#' @param link,models,model_verbose Deprecated.
#' @details
#'
#'  Packages used:
#'
#'  \subsection{Models}{
#'
#'  Gaussian: \link[stats:lm]{stats::lm}, \code{\link[lme4:lmer]{lme4::lmer}}
#'
#'  Binomial: \code{\link[stats:glm]{stats::glm}}, \code{\link[lme4:glmer]{lme4::glmer}}
#'  }
#'  \subsection{Results}{
#'  \subsection{Shared}{
#'
#'  AIC : \code{\link[stats:AIC]{stats::AIC}}
#'
#'  AICc : \code{\link[MuMIn:AICc]{MuMIn::AICc}}
#'
#'  BIC : \code{\link[stats:BIC]{stats::BIC}}
#'
#'  }
#'  \subsection{Gaussian}{
#'
#'  r2m : \code{\link[MuMIn:r.squaredGLMM]{MuMIn::r.squaredGLMM}}
#'
#'  r2c : \code{\link[MuMIn:r.squaredGLMM]{MuMIn::r.squaredGLMM}}
#'
#'  }
#'  \subsection{Binomial}{
#'
#'  ROC: \code{\link[pROC:roc]{pROC::roc}}
#'
#'  }
#'  }
#' @return
#'  Tbl (tibble) with results for each model.
#'
#'  \subsection{Shared across families}{
#'  A nested tibble with \strong{coefficients} of the models from all iterations.
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
#'  Count of \strong{Singular Fit messages}. See \code{\link[lme4:isSingular]{lme4::isSingular}} for more information.
#'
#'  Nested tibble with the \strong{warnings and messages} caught for each model.
#'
#'  Specified \strong{family}.
#'
#'  Name of \strong{dependent} variable.
#'
#'  Names of \strong{fixed} effects.
#'
#'  Names of \strong{random} effects, if any.
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Gaussian Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  Average \strong{RMSE}, \strong{MAE}, \strong{NRMSE}, \strong{RMSEIQR},
#'  \strong{r2m}, \strong{r2c}, \strong{AIC}, \strong{AICc},
#'  and \strong{BIC} of all the iterations*,
#'  \emph{\strong{omitting potential NAs} from non-converged iterations}.
#'  Note that the Information Criteria metrics (AIC, AICc, and BIC) are also averages.
#'
#'  A nested tibble with the \strong{predictions} and targets.
#'
#'  A nested tibble with the non-averaged \strong{results} from all iterations.
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
#'  a confusion matrix and a ROC curve are created to get the following:
#'
#'  ROC:
#'
#'  \strong{AUC}, \strong{Lower CI}, and \strong{Upper CI}
#'
#'  Confusion Matrix:
#'
#'  \strong{Balanced Accuracy}, \strong{F1},
#'  \strong{Sensitivity}, \strong{Specificity},
#'  \strong{Positive Prediction Value},
#'  \strong{Negative Prediction Value},
#'  \strong{Kappa},
#'  \strong{Detection Rate},
#'  \strong{Detection Prevalence},
#'  \strong{Prevalence}, and
#'  \strong{MCC} (Matthews correlation coefficient).
#'
#'  Other available metrics (disabled by default, see \code{metrics}):
#'  \strong{Accuracy}, \strong{AIC}, \strong{AICc}, \strong{BIC}.
#'
#'  Also includes:
#'
#'  A nested tibble with \strong{predictions}, predicted classes (depends on \code{cutoff}), and the targets.
#'  Note, that the \strong{predictions are not necessarily of the specified \code{positive} class}, but of
#'  the model's positive class (second level of dependent variable, alphabetically).
#'
#'  A nested tibble with the sensativities and specificities from the \strong{ROC} curve(s).
#'
#'  A nested tibble with the \strong{confusion matrix}/matrices.
#'  The \code{Pos_} columns tells you whether a row is a
#'  True Positive (TP), True Negative (TN), False Positive (FP), or False Negative (FN),
#'  depending on which level is the "positive" class. I.e. the level you wish to predict.
#'
#'  A nested tibble with the \strong{results} from all fold columns.
#'
#'  * In \emph{repeated cross-validation}, an evaluation is made per fold column (repetition) and averaged.
#'
#'  }
#' @examples
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
#' data <- fold(data, k = 4,
#'              cat_col = 'diagnosis',
#'              id_col = 'participant') %>%
#'         arrange(.folds)
#'
#' # Cross-validate a single model
#'
#' \donttest{
#' # Gaussian
#' cross_validate(data,
#'                formulas = "score~diagnosis",
#'                family = 'gaussian',
#'                REML = FALSE)
#'
#' # Binomial
#' cross_validate(data,
#'                formulas = "diagnosis~score",
#'                family='binomial')
#'
#' # Cross-validate multiple models
#'
#' formulas <- c("score~diagnosis+(1|session)",
#'               "score~age+(1|session)")
#'
#' cross_validate(data,
#'                formulas = formulas,
#'                family = 'gaussian',
#'                REML = FALSE)
#' }
#' # Use parallelization
#'
#' \donttest{
#' # Attach doParallel and register four cores
#' # Uncomment:
#' # library(doParallel)
#' # registerDoParallel(4)
#'
#' # Create list of 20 model formulas
#' formulas <- rep(c("score~diagnosis+(1|session)",
#'                   "score~age+(1|session)"), 10)
#'
#' # Cross-validate a list of 20 model formulas in parallel
#' system.time({cross_validate(data,
#'                             formulas = formulas,
#'                             family = 'gaussian',
#'                             parallel = TRUE)})
#'
#' # Cross-validate a list of 20 model formulas sequentially
#' system.time({cross_validate(data,
#'                             formulas = formulas,
#'                             family = 'gaussian',
#'                             parallel = FALSE)})
#' }
#'
#' @importFrom stats binomial gaussian glm lm
#' @importFrom rlang .data
#' @importFrom lifecycle deprecated deprecate_warn deprecate_stop
#' @importFrom rtilities2 message_if stop_if create_tmp_name
cross_validate <- function(data, formulas, fold_cols = '.folds', family = 'gaussian',
                           control = NULL, REML = FALSE,
                           cutoff = 0.5, positive = 2,
                           metrics = list(),
                           preprocessing = NULL,
                           rm_nc = FALSE,
                           parallel = FALSE, verbose = FALSE,
                           link = deprecated(),
                           models = deprecated(),
                           model_verbose = deprecated()){

  if (!rlang::is_missing(link))
    deprecate_stop("1.0.0", "cvms::cross_validate(link = )")

  if (!rlang::is_missing(models)){
    deprecate_warn("1.0.0", "cvms::cross_validate(models = )",
                   "cvms::cross_validate(formulas = )")
    formulas <- models
  }

  if (!rlang::is_missing(model_verbose)){
    deprecate_warn("1.0.0", "cvms::cross_validate(model_verbose = )",
                   "cvms::cross_validate(verbose = )")
    verbose <- model_verbose
  }

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

