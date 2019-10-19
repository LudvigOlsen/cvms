

#' @title Create baseline evaluations
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Create a baseline evaluation of a test set.
#'
#'  When \code{family} is \code{gaussian}: fits baseline models (\code{y ~ 1}) on \code{n} random
#'  subsets of \code{train_data} and evalutes each model on \code{test_data}. Also evaluates a
#'  model fitted on all rows in \code{train_data}.
#'
#'  When \code{family} is \code{binomial}: evaluates \code{n} sets of random predictions
#'  against the dependent variable, along with a set of all \code{0} predictions and
#'  a set of all \code{1} predictions.
#'
#'  When \code{family} is \code{multinomial}: creates one-vs-all (binomial)
#'  baseline evaluations for \code{n} sets of random predictions against the dependent variable,
#'  along with sets of "all class x,y,z,..." predictions.
#'
#' @inheritParams cross_validate
#' @param test_data Data Frame.
#' @param train_data Data Frame. Only used when \code{family == "gaussian"}.
#' @param dependent_col Name of dependent variable in the supplied test and training sets.
#' @param n Number of random samplings to perform.
#'
#'  For \code{gaussian}: The number of random samplings of train_data to fit baseline models on.
#'
#'  For \code{binomial} and \code{multinomial}: The number of sets of random predictions to evaluate.
#'
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
#' @param random_effects Random effects structure for Gaussian baseline model. (Character)
#'
#'  E.g. with \code{"(1|ID)"}, the model becomes \code{"y ~ 1 + (1|ID)"}.
#'
#'  N.B. \strong{Gaussian only}
#' @param random_generator_fn Function for generating random numbers when \code{type} is \code{"multinomial"}.
#'  The softmax function is applied to the generated numbers to transform them to probabilities.
#'
#'  The first argument must be the number of random numbers to generate,
#'  as no other arguments are supplied.
#'
#'  To test the effect of using different functions,
#'  see \code{\link[cvms:multiclass_probability_tibble]{multiclass_probability_tibble}}.
#'
#'  N.B. \strong{Multinomial only}
#' @param min_training_rows Minimum number of rows in the random subsets of \code{train_data}.
#'
#'  \strong{Gaussian only}. (Integer)
#' @param min_training_rows_left_out Minimum number of rows left out of the random subsets of \code{train_data}.
#'
#'  I.e. a subset will maximally have the size:
#'
#'  \code{max_rows_in_subset = nrow(train_data) - min_training_rows_left_out}.
#'
#'  N.B. \strong{Gaussian only}. (Integer)
#' @param REML Whether to use Restricted Maximum Likelihood. (Logical)
#'
#'  N.B. \strong{Gaussian only}. (Integer)
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
#'  Gaussian: \code{\link[stats:lm]{stats::lm}}
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
#'  AICc : \code{\link[MuMIn:AICc]{MuMIn::AICc}}
#'
#'  BIC : \code{\link[stats:BIC]{stats::BIC}}
#'
#'  \strong{Binomial} and \strong{Multinomial}:
#'
#'  Confusion matrix and related metrics: \code{\link[caret:confusionMatrix]{caret::confusionMatrix}}
#'
#'  ROC and related metrics: \code{\link[pROC:roc]{pROC::roc}}
#'
#'  MCC: \code{\link[mltools:mcc]{mltools::mcc}}
#'
#'  }
#' @return List containing:
#'
#'  \enumerate{
#'   \item a tibble with summarized results (called \code{summarized_metrics})
#'   \item a tibble with random evaluations (\code{random_evaluations})
#'   \item a tibble with the summarized class level results
#'         (\code{summarized_class_level_results})
#'         \strong{(Multinomial only)}
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Gaussian Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  The \strong{Summarized Results} tibble contains:
#'
#'  Average \strong{RMSE}, \strong{MAE}, \strong{r2m}, \strong{r2c},
#'  \strong{AIC}, \strong{AICc}, and \strong{BIC}.
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
#'  \strong{Random} effects structure (if specified).
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
#'  ----------------------------------------------------------------
#'
#'  \subsection{Multinomial Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  Based on the generated test set predictions,
#'  one-vs-all (binomial) evaluations are performed and aggregated
#'  to get the same metrics as in the \code{binomial} results, with the
#'  addition of \strong{Overall Accuracy} in the summarized results.
#'
#'  ....................................................................
#'
#'  The \strong{Summarized Results} tibble contains:
#'
#'  Summary of the random evaluations.
#'
#'  \strong{How}: First, the one-vs-all binomial evaluations are aggregated by repetition
#'  (ignoring \code{NA}s), and then, these aggregations are summarized. Besides the
#'  metrics from the binomial evaluations (see \emph{Binomial Results} above), it
#'  also includes the \strong{Overall Accuracy} metric.
#'
#'  The \strong{Measure} column indicates the statistical descriptor used on the evaluations.
#'  The \strong{Mean}, \strong{Median}, \strong{SD}, and \strong{IQR} describe the
#'  repetition evaluations (similar to the \emph{Random Evaluations} tibble, but ignoring \code{NA}s when aggregating,
#'  as the \code{NA}s and \code{INF}s are counted instead), while the \strong{Max}, \strong{Min}, \strong{NAs}, and
#'  \strong{INFs} are extracted from the \emph{Summarized Class Level Results} tibble, to get
#'  the overall values. The \code{NA}s and \code{INF}s are only counted in the one-vs-all evaluations.
#'
#'  The rows where \code{Measure == All_<<class name>>} are the evaluations when all
#'  the observations are predicted to be in that class.
#'
#'  ....................................................................
#'
#'  The \strong{Summarized Class Level Results} tibble contains:
#'
#'  The (nested) summarized results for each class, with the same metrics and descriptors as
#'  the \emph{Summarized Results} tibble. Use \code{\link[tidyr:unnest]{tidyr::unnest}}
#'  on the tibble to inspect the results.
#'
#'  \strong{How}: The one-vs-all evaluations are summarized by class.
#'
#'  The rows where \code{Measure == All_0} are the evaluations when none of the observations
#'  are predicted to be in that class, while the rows where \code{Measure == All_1} are the
#'  evaluations when all of the observations are predicted to be in that class.
#'
#'  ....................................................................
#'
#'  The \strong{Random Evaluation} tibble contains:
#'
#'  The repetition results with the same metrics as the \emph{Summarized Results} tibble.
#'
#'  \strong{How}: The one-vs-all evaluations are aggregated by repetition.
#'  \code{NA}'s are not ignored, meaning that any \code{NA} from a one-vs-all evaluation
#'  will lead to an \code{NA} result for that repetition.
#'
#'  Also includes:
#'
#'  A nested tibble with the one-vs-all binomial evaluations (\strong{Class Level Results}),
#'  including nested \strong{ROC} curves and \strong{Confusion Matrices}, and the
#'  \strong{Support} column, which is a count of how many observations from the
#'  class is in the test set.
#'
#'  A nested tibble with the \strong{predictions} and targets.
#'
#'  A nested tibble with the multiclass \strong{confusion matrix}.
#'
#'  Specified \strong{family}.
#'
#'  Name of \strong{dependent} variable.
#'
#'  }
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @examples
#' \donttest{
#' # Attach packages
#' library(cvms)
#' library(groupdata2) # partition()
#' library(dplyr) # %>% arrange()
#' library(tibble)
#'
#' # Data is part of cvms
#' data <- participant.scores
#'
#' # Set seed for reproducibility
#' set.seed(1)
#'
#' # Partition data
#' partitions <- partition(data, p = 0.7, list_out = TRUE)
#' train_set <- partitions[[1]]
#' test_set <- partitions[[2]]
#'
#' # Create baseline evaluations
#' # Note: usually n=100 is a good setting
#'
#' # Gaussian
#' baseline(test_data = test_set, train_data = train_set,
#'          dependent_col = "score", random_effects = "(1|session)",
#'          n = 2, family = "gaussian")
#'
#' # Binomial
#' baseline(test_data = test_set, dependent_col = "diagnosis",
#'          n = 2, family = "binomial")
#'
#' # Multinomial
#'
#' # Create some data with multiple classes
#' multiclass_data <- tibble(
#'     "target" = rep(paste0("class_", 1:5), each = 10)) %>%
#'     dplyr::sample_n(35)
#'
#' baseline(test_data = multiclass_data,
#'          dependent_col = "target",
#'          n = 4, family = "multinomial")
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
#'          dependent_col = "score", random_effects = "(1|session)",
#'          n = 4, family = "gaussian", parallel = TRUE)
#'
#' # Multinomial
#' (mb <- baseline(test_data = multiclass_data,
#'                dependent_col = "target",
#'                n = 4, family = "multinomial",
#'                parallel = TRUE))
#'
#' # Inspect the summarized class level results
#' # for class_2
#' mb$summarized_class_level_results %>%
#'  dplyr::filter(Class == "class_2") %>%
#'  tidyr::unnest(Results)
#'
#' # Multinomial with custom random generator function
#' # that creates very "certain" predictions
#' # (once softmax is applied)
#'
#' rcertain <- function(n){
#'     (runif(n, min = 1, max = 100)^1.4)/100
#' }
#'
#' baseline(test_data = multiclass_data,
#'          dependent_col = "target",
#'          n = 4, family = "multinomial",
#'          parallel = TRUE,
#'          random_generator_fn = rcertain)
#'
#' }
#' @importFrom stats runif rnorm terms
baseline <- function(test_data,
                     dependent_col,
                     train_data = NULL,
                     # how many times to randomly sample probabilities (bootstrapping?)
                     n = 100,
                     family = 'binomial',
                     # Binomial
                     positive = 2,
                     cutoff = 0.5,
                     # Multinomial
                     random_generator_fn = runif,
                     # Gaussian
                     random_effects = NULL,
                     min_training_rows = 5,
                     min_training_rows_left_out = 3,
                     REML = FALSE,
                     # Parallelization
                     parallel = FALSE) {

  # Start by converting the train and test data to tibbles
  # Will be NULL if they were NULL before
  test_data <- to_tibble(test_data, "test_data", caller = "baseline()")
  train_data <- to_tibble(train_data, "train_data", caller = "baseline()")

  if (family == "binomial"){

    arg_not_used(arg = train_data, arg_name = "train_data",
                 family = "binomial", current_fn = "baseline")
    arg_not_used(arg = random_effects, arg_name = "random_effects",
                 family = "binomial", current_fn = "baseline")
    if (!isTRUE(all.equal(random_generator_fn, runif))){
      message(paste0("'random_generator_fn' was not default function. ",
                     "Note that the 'random_generator_fn' is not used in ",
                     "the binomial version of baseline()."))
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

    arg_not_used(arg = train_data, arg_name = "train_data",
                 family = "multinomial", current_fn = "baseline")
    arg_not_used(arg = random_effects, arg_name = "random_effects",
                 family = "multinomial", current_fn = "baseline")

    return(
      create_multinomial_baseline_evaluations(test_data = test_data,
                                              dependent_col = dependent_col,
                                              reps = n,
                                              parallel_ = parallel,
                                              random_generator_fn = random_generator_fn
      )
    )

  } else if (family == "gaussian"){

    if (is.null(train_data)){
      stop("train_data must be passed for Gaussian baseline.")
    }

    if (!isTRUE(all.equal(random_generator_fn, runif))){
      message(paste0("'random_generator_fn' was not default function. ",
                     "Note that the 'random_generator_fn' is not used in ",
                     "the Gaussian version of baseline()."))
    }

    return(
      create_gaussian_baseline_evaluations(train_data = train_data,
                                           test_data = test_data,
                                           dependent_col = dependent_col,
                                           random_effects = random_effects,
                                           n_samplings = n,
                                           min_training_rows = min_training_rows,
                                           min_training_rows_left_out = min_training_rows_left_out,
                                           REML = REML,
                                           parallel_ = parallel
                                           )
    )
  }

}
