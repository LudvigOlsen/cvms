# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @title Validate regression models on a test set
#' @description Train gaussian or binomial models on a
#'  full training set and validate it by predicting the test/validation set.
#'  Returns results in a tibble for easy reporting, along with the trained models.
#' @inheritParams cross_validate
#' @param train_data Data Frame.
#' @param test_data Data Frame. If specifying \code{partitions_col}, this can be \code{NULL}.
#' @param partitions_col Name of grouping factor for identifying partitions. (Character)
#'
#'  Rows with the value \code{1} in \code{partitions_col} are used as training set and
#'  rows with the value \code{2} are used as test set.
#'
#'  N.B. Only used if \code{test_data} is \code{NULL}.
#' @param err_nc Raise error if model does not converge. (Logical)
#' @param parallel Whether to validate the list of models in parallel. (Logical)
#'
#'  Remember to register a parallel backend first.
#'  E.g. with \code{doParallel::registerDoParallel}.
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
#'  \strong{Gaussian}:
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
#'  \strong{Binomial}:
#'
#'  Confusion matrix: \code{\link[caret:confusionMatrix]{caret::confusionMatrix}}
#'
#'  ROC: \code{\link[pROC:roc]{pROC::roc}}
#'
#'  MCC: \code{\link[mltools:mcc]{mltools::mcc}}
#'  }
#' @return List containing tbl (tibble) with results and the trained model object.
#'  The tibble contains:
#'
#'  \subsection{Gaussian Results}{
#'  \strong{RMSE}, \strong{MAE}, \strong{r2m}, \strong{r2c}, \strong{AIC}, \strong{AICc},
#'  and \strong{BIC}.
#'
#'  Count of \strong{convergence warnings}. Consider discarding the model if it did not converge.
#'
#'  Specified \strong{family}.
#'
#'  A nested tibble with model \strong{coefficients}.
#'
#'  A nested tibble with the \strong{predictions} and targets.
#'
#'  Name of \strong{dependent} variable.
#'
#'  Names of \strong{fixed} effects.
#'
#'  Names of \strong{random} effects if any.
#'
#'  }
#'
#'  \subsection{Binomial Results}{
#'  Based on predictions of the test set,
#'  a confusion matrix and ROC curve are used to get the following:
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
#'  A nested tibble with model \strong{coefficients}.
#'
#'  Count of \strong{convergence warnings}. Consider discarding the model if it did not converge.
#'
#'  Count of \strong{Singular Fit messages}. See \code{?\link[lme4:isSingular]{lme4::isSingular}} for more information.
#'
#'  Specified \strong{family}.
#'
#'  A tibble with \strong{predictions}, predicted classes (depends on \code{cutoff}), and the targets.
#'
#'  A tibble with the sensativities and specificities from the \strong{ROC} curve.
#'
#'  Name of \strong{dependent} variable.
#'
#'  Names of \strong{fixed} effects.
#'
#'  Names of \strong{random} effects if any.
#'
#'  }
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @examples
#' # Attach packages
#' library(cvms)
#' library(groupdata2) # partition()
#' library(dplyr) # %>% arrange()
#'
#' # Data is part of cvms
#' data <- participant.scores
#'
#' # Set seed for reproducibility
#' set.seed(7)
#'
#' # Partition data
#' # Keep as single data frame
#' # We could also have fed validate() separate train and test sets.
#' data_partitioned <- partition(data,
#'                               p = 0.7,
#'                               cat_col = 'diagnosis',
#'                               id_col = 'participant',
#'                               list_out=FALSE) %>%
#'     arrange(.partitions)
#'
#' # Validate a model
#'
#' # Gaussian
#' validate(data_partitioned,
#'          models = "score~diagnosis",
#'          partitions_col = '.partitions',
#'          family='gaussian',
#'          REML = FALSE)
#'
#' # Binomial
#' validate(data_partitioned,
#'          models = "diagnosis~score",
#'          partitions_col = '.partitions',
#'          family='binomial')
#'
#' # Use non-default link functions
#'
#' validate(data_partitioned,
#'          models = "score~diagnosis",
#'          partitions_col = '.partitions',
#'          family = 'gaussian',
#'          link = 'log',
#'          REML = FALSE)
#'
#' ## Feed separate train and test sets
#'
#' # Partition data to list of data frames
#' # The first data frame will be train (70% of the data)
#' # The second will be test (30% of the data)
#' data_partitioned <- partition(data, p = 0.7,
#'                               cat_col = 'diagnosis',
#'                               id_col = 'participant',
#'                               list_out=TRUE)
#' train_data <- data_partitioned[[1]]
#' test_data <- data_partitioned[[2]]
#'
#' # Validate a model
#'
#' # Gaussian
#' validate(train_data,
#'          test_data = test_data,
#'          models = "score~diagnosis",
#'          family='gaussian',
#'          REML = FALSE)
#'
validate <- function(train_data,
                     models,
                     test_data = NULL,
                     partitions_col = '.partitions',
                     family = 'gaussian',
                     link = NULL,
                     control = NULL,
                     REML = FALSE,
                     cutoff = 0.5,
                     positive = 2,
                     err_nc = FALSE,
                     rm_nc = FALSE,
                     parallel = FALSE,
                     model_verbose = FALSE) {
  basics_validate_list(
    train_data = train_data,
    model_list = models,
    test_data = test_data,
    partitions_col = partitions_col,
    family = family,
    link = link,
    control = control,
    REML = REML,
    cutoff = cutoff,
    positive = positive,
    err_nc = err_nc,
    rm_nc = rm_nc,
    parallel_ = parallel,
    model_verbose = model_verbose
  )

}
