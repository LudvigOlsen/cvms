

#' @title Create baseline evaluations
#' @description Create a baseline evaluation of a test set.
#'
#'  When \code{family} is \code{binomial}: evaluates \code{n} sets of random predictions
#'  against the dependent variable,
#'  along with a set of all \code{0} predictions and a set of all \code{1} predictions.
#'
#'  When \code{family} is \code{gaussian}: fits baseline models (\code{y ~ 1}) on \code{n} random
#'  subsets of \code{train_data} and evalutes each model on \code{test_data}. Also evaluates a
#'  model fitted on all rows in \code{train_data}.
#'
#'  \strong{baseline() is under development! Large changes may occur.}
#' @inheritParams cross_validate
#' @param test_data Data Frame.
#' @param train_data Data Frame. Only used when \code{family == "gaussian"}.
#' @param dependent_col Name of dependent variable in the supplied test and training sets.
#' @param n Number of random samplings to perform.
#'
#'  For \code{binomial}: The number of sets of random predictions to evaluate.
#'
#'  For \code{gaussian}: The number of random samplings of train_data to fit baseline models on.
#' @param family Name of family. (Character)
#'
#'  Currently supports \code{"gaussian"}, \code{"binomial"} and \code{"multinomial"}.
#' @param positive Level from dependent variable to predict.
#'  Either as character or level index (1 or 2 - alphabetically).
#'
#'  E.g. if we have the levels \code{"cat"} and \code{"dog"} and we want \code{"dog"} to be the positive class,
#'  we can either provide \code{"dog"} or \code{2}, as alphabetically, \code{"dog"} comes after \code{"cat"}.
#'
#'  Used when calculating confusion matrix metrics and creating ROC curves.
#'
#'  N.B. Only affects evaluation metrics, not the returned predictions.
#'
#'  \strong{N.B. Binomial only}. (Character or Integer)
#' @param cutoff Threshold for predicted classes. (Numeric)
#'
#'  N.B. \strong{Binomial only}
#' @param min_training_rows Minimum number of rows in the random subsets of \code{train_data}.
#'
#'  \strong{Gaussian only}. (Integer)
#' @param min_training_rows_left_out Minimum number of rows left out of the random subsets of \code{train_data}.
#'
#'  I.e. a subset will maximally have the size:
#'
#'  \code{max_rows_in_subset = nrow(train_data) - min_training_rows_left_out}.
#'
#'  \strong{Gaussian only}. (Integer)
#' @param parallel Whether to run the \code{n} evaluations in parallel. (Logical)
#'
#'  Remember to register a parallel backend first.
#'  E.g. with doParallel::registerDoParallel.
#' @details
#'
#'  Packages used:
#'
#'  \subsection{Models}{
#'
#'  Gaussian: \link[stats:lm]{stats::lm}
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
#'
#'  }
#' @return List containing tbl (tibble) with summarized results and tbl with random evaluations.
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Gaussian Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  The \strong{Summarized Results} tibble contains:
#'
#'  Average \strong{RMSE}, \strong{MAE}, \strong{r2m}, \strong{r2c}, \strong{AIC}, \strong{AICc}, and \strong{BIC}.
#'  }
#'
#'  The \strong{Measure} column indicates the statistical descriptor used on the evaluations.
#'  The row where \code{Measure == All_rows} is the evaluation when the baseline model
#'  is trained on all rows in \code{train_data}.
#'
#'  The \strong{Training Rows} column contains the aggregated number of rows used from \code{train_data},
#'  when fitting the baseline models.
#'
#'  ....................................................................
#'
#'  The \strong{Random Evaluations} tibble contains:
#'
#'  The \strong{non-aggregated metrics}.
#'
#'  A nested tibble with the \strong{predictions} and targets.
#'
#'  A nested tibble with the \strong{coefficients} of the baseline models.
#'
#'  Number of \strong{training rows} used when fitting the baseline model on the training set.
#'
#'  Specified \strong{family}.
#'
#'  Name of \strong{dependent} variable.
#'
#'  Name of \strong{fixed} effect (bias term only).
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Binomial Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  Based on the generated test set predictions,
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
#'  ....................................................................
#'
#'  The \strong{Summarized Results} tibble contains:
#'
#'  The \strong{Measure} column indicates the statistical descriptor used on the evaluations.
#'  The row where \code{Measure == All_0} is the evaluation when all predictions are 0.
#'  The row where \code{Measure == All_1} is the evaluation when all predictions are 1.
#'
#'  The \strong{aggregated metrics}.
#'
#'  ....................................................................
#'
#'  The \strong{Random Evaluations} tibble contains:
#'
#'  The \strong{non-aggregated metrics}.
#'
#'  A nested tibble with the \strong{predictions} and targets.
#'
#'  A nested tibble with the sensativities and specificities from the \strong{ROC} curve.
#'
#'  A nested tibble with the \strong{confusion matrix}.
#'  The \code{Pos_} columns tells you whether a row is a
#'  True Positive (TP), True Negative (TN), False Positive (FP), or False Negative (FN),
#'  depending on which level is the "positive" class. I.e. the level you wish to predict.
#'
#'  Specified \strong{family}.
#'
#'  Name of \strong{dependent} variable.
#'  }
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @examples
#' \donttest{
#' # Attach libraries
#' library(cvms)
#' library(groupdata2) # partition()
#' library(dplyr) # %>% arrange()
#'
#' # Data is part of cvms
#' data <- participant.scores
#'
#' # Set seed for reproducibility
#' set.seed(1)
#'
#' # Partition data
#' partitions <- partition(data, p = 0.7, list_out=TRUE)
#' train_set <- partitions[[1]]
#' test_set <- partitions[[2]]
#'
#' # Create baseline evaluations
#' # Note: usually n=100 is a good setting
#'
#' # Gaussian
#' baseline(test_data = test_set, train_data = train_set,
#'          dependent_col = "score", n = 2, family = "gaussian")
#'
#' # Binomial
#' baseline(test_data = test_set, dependent_col="diagnosis",
#'          n = 2, family="binomial")
#'
#' # Parallelize evaluations
#'
#' # Attach doParallel and register four cores
#' # Uncomment:
#' # library(doParallel)
#' # registerDoParallel(4)
#'
#' # Binomial
#' baseline(test_data = test_set, dependent_col = "diagnosis",
#'          n = 4, family = "binomial", parallel = TRUE)
#'
#' # Gaussian
#' baseline(test_data = test_set, train_data = train_set,
#'          dependent_col = "score", n = 4, family = "gaussian",
#'          parallel = TRUE)
#' }
#' @importFrom stats runif
baseline <- function(test_data,
                     dependent_col,
                     train_data = NULL,
                     n = 100,
                     # how many times to randomly sample probabilities (bootstrapping?)
                     family = 'binomial',
                     # Binomial
                     positive = 2,
                     cutoff = 0.5,
                     # Gaussian
                     min_training_rows = 5,
                     min_training_rows_left_out = 3,
                     # Parallelization
                     parallel = FALSE) {

  if (family == "binomial"){

    if (!is.null(train_data)){
      message("train_data was not used for binomial baseline.")
    }

    return(
      create_binomial_baseline_evaluations(
        test_data = test_data,
        dependent_col = dependent_col,
        reps = n,
        positive = positive,
        cutoff = cutoff,
        parallel_ = parallel
      )
    )

  } else if (family == "multinomial"){

    if (!is.null(train_data)){
      message("train_data was not used for multinomial baseline.")
    }

    return(
      create_multinomial_baseline_evaluations(test_data = test_data,
                                              dependent_col = dependent_col,
                                              reps = n,
                                              parallel_ = parallel
      )
    )

  } else if (family == "gaussian"){

    if (is.null(train_data)){
      stop("train_data must be passed for Gaussian baseline.")
    }

    return(
      create_gaussian_baseline_evaluations(train_data = train_data,
                                           test_data = test_data,
                                           dependent_col = dependent_col,
                                           n_samplings = n,
                                           min_training_rows = min_training_rows,
                                           min_training_rows_left_out = min_training_rows_left_out,
                                           parallel_ = parallel
                                           )
    )
  }

}
