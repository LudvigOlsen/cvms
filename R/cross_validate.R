# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @title Cross-validate regression models for model selection
#' @description Cross-validate one or multiple gaussian or binomial
#'  models at once. Perform repeated cross-validation.
#'  Returns results in a tibble for easy comparison,
#'  reporting and further analysis.
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
#'  \subsection{Gaussian}{
#'
#'  r2m : \code{\link[MuMIn:r.squaredGLMM]{MuMIn::r.squaredGLMM}}
#'
#'  r2c : \code{\link[MuMIn:r.squaredGLMM]{MuMIn::r.squaredGLMM}}
#'
#'  AIC : \code{\link[stats:AIC]{stats::AIC}}
#'
#'  AICc : \code{\link[AICcmodavg:AICc]{AICcmodavg::AICc}}
#'
#'  BIC : \code{\link[stats:BIC]{stats::BIC}}
#'
#'  }
#'  \subsection{Binomial}{
#'
#'  Confusion matrix: \code{\link[caret:confusionMatrix]{caret::confusionMatrix}}
#'
#'  ROC: \code{\link[pROC:roc]{pROC::roc}}
#'
#'  MCC: \code{\link[mltools:mcc]{mltools::mcc}}
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
#'  Count of \strong{Singular Fit messages}. See \code{?\link[lme4:isSingular]{lme4::isSingular}} for more information.
#'
#'  Specified \strong{family}.
#'
#'  Specified \strong{link} function.
#'
#'  Name of \strong{dependent} variable.
#'
#'  Names of \strong{fixed} effects.
#'
#'  Names of \strong{random} effects, if any.
#'  }
#'
#'  \subsection{Gaussian Results}{
#'  Average \strong{RMSE}, \strong{MAE}, \strong{r2m}, \strong{r2c}, \strong{AIC}, \strong{AICc},
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
#'  \subsection{Binomial Results}{
#'  Based on the collected predictions from the test folds*,
#'  a confusion matrix and a ROC curve are created to get the following:
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
#'  ROC:
#'
#'  \strong{AUC}, \strong{Lower CI}, and \strong{Upper CI}
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
#'  A nested tibble with the \strong{results} from all fold columns, if using repeated cross-validation.
#'
#'  * In \emph{repeated cross-validation}, an evaluation is made per fold column (repetition) and averaged.
#'
#'  }
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @author Benjamin Hugh Zachariae
#' @export
#' @param data Data frame.
#'
#'  Must include grouping factor for identifying folds
#'   - as made with \code{\link[groupdata2:fold]{groupdata2::fold()}}.
#' @param models Model formulas as strings. (Character)
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
#'  Currently supports "gaussian" and "binomial".
#' @param link Link function. (Character)
#'
#'  E.g. \code{link = "log"} with \code{family = "gaussian"} will
#'  use \code{family = gaussian(link = "log")}.
#'
#'  See \code{\link[stats:family]{stats::family}} for available link functions.
#'
#'  \subsection{Default link functions}{
#'
#'  Gaussian: \code{'identity'}.
#'
#'  Binomial: \code{'logit'}.}
#' @param control Construct control structures for mixed model fitting
#'  (i.e. \code{\link[lme4]{lmer}} and \code{\link[lme4]{glmer}}).
#'  See \code{\link[lme4:lmerControl]{lme4::lmerControl}} and
#'  \code{\link[lme4:glmerControl]{lme4::glmerControl}}.
#'
#'  N.B. Ignored if fitting \code{\link[stats]{lm}} or \code{\link[stats]{glm}} models.
#' @param REML Restricted Maximum Likelihood. (Logical)
#' @param cutoff Threshold for predicted classes. (Numeric)
#'
#'  N.B. Binomial models only.
#' @param positive Level from dependent variable to predict.
#'  Either as character or level index (\code{1} or \code{2} - alphabetically).
#'  Used when creating confusion matrices and ROC curves.
#'
#'  N.B. Only affects evaluation metrics, not the model training or returned predictions.
#'
#'  N.B. Binomial models only.
#' @param rm_nc Remove non-converged models from output. (Logical)
#' @param model_verbose Print name of used model function on each iteration. (Logical)
#' @param parallel Whether to cross-validate the list of models in parallel. (Logical)
#'
#'  Remember to register a parallel backend first.
#'  E.g. with \code{doParallel::registerDoParallel}.
#' @examples
#' # Attach libraries
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
#'           cat_col = 'diagnosis',
#'           id_col = 'participant') %>%
#'         arrange(.folds)
#'
#' # Cross-validate a single model
#'
#' \dontrun{
#' # Gaussian
#' cross_validate(data,
#'                models = "score~diagnosis",
#'                family='gaussian',
#'                REML = FALSE)
#'
#' # Binomial
#' cross_validate(data,
#'                models = "diagnosis~score",
#'                family='binomial')
#'
#' # Cross-validate multiple models
#'
#' models <- c("score~diagnosis+(1|session)",
#'             "score~age+(1|session)")
#'
#' cross_validate(data,
#'                models = models,
#'                family='gaussian',
#'                REML = FALSE)
#'
#' # Use non-default link functions
#'
#' cross_validate(data,
#'                models = "score~diagnosis",
#'                family = 'gaussian',
#'                link = 'log',
#'                REML = FALSE)
#' }
#' # Use parallelization
#'
#' \dontrun{
#' # Attach doParallel and register four cores
#' library(doParallel)
#' registerDoParallel(4)
#'
#' # Create list of 20 model formulas
#' models <- rep(c("score~diagnosis+(1|session)",
#'                 "score~age+(1|session)"), 10)
#'
#' # Cross-validate a list of 20 model formulas in parallel
#' system.time({cross_validate(data,
#'                             models = models,
#'                             family = 'gaussian',
#'                             parallel = TRUE)})
#'
#' # Cross-validate a list of 20 model formulas sequentially
#' system.time({cross_validate(data,
#'                             models = models,
#'                             family = 'gaussian',
#'                             parallel = FALSE)})
#' }
#'
#' @importFrom stats binomial gaussian glm lm
#' @importFrom rlang .data
cross_validate <- function(data, models, fold_cols = '.folds', family = 'gaussian',
                           link = NULL, control = NULL, REML = FALSE,
                           cutoff = 0.5, positive = 2, rm_nc = FALSE,
                           parallel = FALSE, model_verbose = FALSE){


  return(basics_cross_validate_list(data = data,
                                   model_list = models,
                                   fold_cols = fold_cols,
                                   family = family,
                                   link = link,
                                   control=control,
                                   REML = REML,
                                   cutoff = cutoff,
                                   positive = positive,
                                   rm_nc = rm_nc,
                                   model_verbose = model_verbose,
                                   parallel_ = parallel,
                                   parallelize = "models"))



}

