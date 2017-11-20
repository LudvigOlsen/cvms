#' @title Validate regression model on test set
#' @description Train a gaussian or binomial model on full training set and validate it by predicting the test/validation set.
#'  Returns results in a tibble for easy reporting.
#'
#'  \strong{validate() is under development! Large changes may occur.}
#' @inheritParams cross_validate
#' @param train_data Dataframe.
#' @param model Model formula as string. (Character)
#'
#'  E.g. \code{"y~x"}.
#'
#'  Can contain random effects.
#'
#'  E.g. \code{"y~x+(1|r)"}.
#' @param test_data Dataframe.
#' @param partitions_col Name of grouping factor for identifying partitions. (Character)
#'  1 is training set and 2 is test set.
#'
#'  Only used if test_data is NULL.
#' @param err_nc Raise error if model does not converge. (Logical)
#' @details
#'  \subsection{Models}{
#'
#'  Gaussian: stats::lm, lme4::lmer
#'
#'  Binomial: stats::glm, lme4::glmer
#'  }
#'  \subsection{Results}{
#'  \strong{Gaussian}:
#'
#'  RMSE : hydroGOF::rmse
#'
#'  r2m : MuMIn::r.squaredGLMM
#'
#'  r2c : MuMIn::r.squaredGLMM
#'
#'  AIC : stats::AIC
#'
#'  AICc : AICcmodavg::AICc
#'
#'  BIC : stats::BIC
#'
#'  \strong{Binomial}:
#'
#'  Confusion matrix: caret::confusionMatrix
#'
#'  ROC: pROC::roc
#'  }
#' @return List containing tbl (tibble) with results and the trained model object.
#'  The tibble contains:
#'
#'  \subsection{Gaussian Results}{
#'  \strong{RMSE}, \strong{r2m}, \strong{r2c}, \strong{AIC}, \strong{AICc},
#'  and \strong{BIC}.
#'
#'  Count of \strong{convergence warnings}. Consider discarding the model if it did not converge.
#'
#'  Specified \strong{family}.
#'
#'  A tibble with \strong{coefficients} of the model.
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
#'  \strong{Kappa}, \strong{Sensitivity},
#'  \strong{Specificity}, \strong{Positive Prediction Value},
#'  \strong{Negative Prediction Value},
#'  \strong{F1}, \strong{Prevalence}, \strong{Detection Rate},
#'  \strong{Detection Prevalence}, and
#'  \strong{Balanced Accuracy}.
#'
#'  Count of \strong{convergence warnings}. Consider discarding the model if it did not converge.
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
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @author Benjamin Hugh Zachariae
#' @export
#' @examples
#' # Attach libraries
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
#' # Keep as single dataframe
#' # We could also have fed validate() separat train and test sets.
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
#'          model = "score~diagnosis",
#'          partitions_col = '.partitions',
#'          family='gaussian',
#'          REML = FALSE)
#'
#' # Binomial
#' validate(data_partitioned,
#'          model = "diagnosis~score",
#'          partitions_col = '.partitions',
#'          family='binomial')
#'
#' # Use non-default link functions
#'
#' validate(data_partitioned,
#'          model = "score~diagnosis",
#'          partitions_col = '.partitions',
#'          family = 'gaussian',
#'          link = 'log',
#'          REML = FALSE)
#'
#' ## Feed separat train and test sets
#'
#' # Partition data to list of dataframes
#' # The first dataframe will be train (70% of the data)
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
#'          model = "score~diagnosis",
#'          family='gaussian',
#'          REML = FALSE)
#'
validate <- function(train_data,
                     model,
                     test_data = NULL,
                     partitions_col = '.partitions',
                     family = 'gaussian',
                     link = NULL,
                     REML = FALSE,
                     cutoff = 0.5,
                     positive = 1,
                     err_nc = FALSE,
                     model_verbose = FALSE) {
  validate_single(
    train_data = train_data,
    model = model,
    test_data = test_data,
    partitions_col = partitions_col,
    family = family,
    link = link,
    REML = REML,
    cutoff = cutoff,
    positive = positive,
    err_nc = err_nc,
    model_verbose = model_verbose
  )


}
