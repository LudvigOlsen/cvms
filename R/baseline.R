

#' @title Create baseline evaluations
#' @description Create a baseline evaluation of a test set.
#'
#'  When \code{family == binomial}: evaluates \code{n} sets of random samples,
#'  along with a set of all \code{0} predictions and a set of all \code{1} predictions.
#'
#'  When \code{family == gaussian}: fits baseline models (\code{y ~ 1}) on \code{n} random
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
#'  For binomial: The number of sets of random predictions to evaluate.
#'
#'  For gaussian: The number of random samplings of train_data to fit baseline models on.
#' @param positive Level from dependent variable to predict. Either as character or level index (1 or 2 - alphabetically). Used when creating confusion matrices and ROC curves.
#'
#'  N.B. Only affects evaluation metrics, not the returned predictions.
#'
#'  Binomial only. (Character or Integer)
#' @param min_training_rows Minimum number of rows in the random subsets of \code{train_data}.
#'
#'  Gaussian only. (Integer)
#' @param min_training_rows_left_out Minimum number of rows left out of the random subsets of \code{train_data}.
#'
#'  I.e. a subset will maximally have the size:
#'
#'  \code{max_rows_in_subset = nrow(train_data) - min_training_rows_left_out}.
#'
#'  Gaussian only. (Integer)
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
#'  RMSE : \code{\link[hydroGOF:rmse]{hydroGOF::rmse}}
#'
#'  MAE : \code{\link[hydroGOF:mae]{hydroGOF::mae}}
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
#'  \subsection{Gaussian Results}{
#'
#'  The \strong{summarized results} tibble contains:
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
#'  The \strong{random evaluations} tibble contains:
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
#'  \subsection{Binomial Results}{
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
#'  The \strong{summarized results} tibble contains:
#'
#'  The \strong{Measure} column indicates the statistical descriptor used on the evaluations.
#'  The row where \code{Measure == All_0} is the evaluation when all predictions are 0.
#'  The row where \code{Measure == All_1} is the evaluation when all predictions are 1.
#'
#'  The \strong{aggregated metrics}.
#'
#'  The \strong{random evaluations} tibble contains:
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
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
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
#' set.seed(1)
#'
#' # Partition data
#' partitions <- partition(data, p = 0.7, list_out=TRUE)
#' train_set <- partitions[[1]]
#' test_set <- partitions[[2]]
#'
#' # Create baseline evaluations
#'
#' # Gaussian
#' baseline(test_data = test_set, train_data = train_set,
#'          dependent_col="score", n=10, family="gaussian")
#'
#' # Binomial
#' baseline(test_data = test_set, dependent_col="diagnosis",
#'          n=10, family="binomial")
#' @importFrom stats runif
baseline <- function(test_data,
                     dependent_col,
                     train_data = NULL,
                     n=100, # how many times to randomly sample probabilities (bootstrapping?)
                     family = 'binomial',

                     # Binomial
                     positive = 2,
                     cutoff = 0.5,

                     # Gaussian
                     min_training_rows = 5,
                     min_training_rows_left_out = 3){

  if (family == "binomial"){

    if (!is.null(train_data)){
      message("train_data was not used for binomial baseline.")
    }

    return(
      create_binomial_baseline_evaluations(
        test_data = test_data,
        dependent_col = dependent_col,
        reps=n,
        positive = positive,
        cutoff = cutoff)
    )

  } else if (family == "gaussian"){

    if (is.null(train_data)){
      stop("train_data must be passed for gaussian baseline.")
    }

    return(
      create_gaussian_baseline_evaluations(train_data = train_data,
                                           test_data = test_data,
                                           dependent_col = dependent_col,
                                           n_samplings = n,
                                           min_training_rows = 5,
                                           min_training_rows_left_out = 3)
    )
  }

}
