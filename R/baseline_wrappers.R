

#   __________________ #< f399ddb1c0983a2c6c10141a8d3dc0a4 ># __________________
#   Baseline wrappers                                                       ####


##  .................. #< a48abd2c11e944e8d6d2fd8272e3b773 ># ..................
##  Baseline Gaussian                                                       ####


#' @title Create baseline evaluations for regression models
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Create a baseline evaluation of a test set.
#'
#'  In modelling, a \emph{baseline} is a result that
#'  is meaningful to compare the results from our models to. In regression, we
#'  want our model to be better than a model without any predictors. If our
#'  model does not perform better than such a simple model, it's unlikely to
#'  be useful.
#'
#'  \code{baseline_gaussian()} fits the intercept-only model (\code{y ~ 1}) on \code{`n`} random
#'  subsets of \code{`train_data`} and evaluates each model on \code{`test_data`}. Additionally, it evaluates a
#'  model fitted on all rows in \code{`train_data`}.
#' @param test_data \code{data.frame}.
#' @param train_data \code{data.frame}.
#' @param dependent_col Name of dependent variable in the supplied test and training sets.
#' @param n The number of random samplings of \code{`train_data`} to fit baseline models on. (Default is \code{100})
#' @param metrics \code{list} for enabling/disabling metrics.
#'
#'   E.g. \code{list("RMSE" = FALSE)} would remove \code{RMSE} from the results,
#'   and \code{list("TAE" = TRUE)} would add the \code{Total Absolute Error} metric
#'   to the results.
#'   Default values (\code{TRUE}/\code{FALSE}) will be used for the remaining available metrics.
#'
#'   You can enable/disable all metrics at once by including
#'   \code{"all" = TRUE/FALSE} in the \code{list}. This is done prior to enabling/disabling
#'   individual metrics, why f.i. \code{list("all" = FALSE, "RMSE" = TRUE)}
#'   would return only the \code{RMSE} metric.
#'
#'   The \code{list} can be created with
#'   \code{\link[cvms:gaussian_metrics]{gaussian_metrics()}}.
#'
#'   Also accepts the string \code{"all"}.
#' @param random_effects Random effects structure for the baseline model. (Character)
#'
#'  E.g. with \code{"(1|ID)"}, the model becomes \code{"y ~ 1 + (1|ID)"}.
#' @param min_training_rows Minimum number of rows in the random subsets of \code{`train_data`}.
#' @param min_training_rows_left_out Minimum number of rows left out of the random subsets of \code{`train_data`}.
#'
#'  I.e. a subset will maximally have the size:
#'
#'  \code{max_rows_in_subset = nrow(`train_data`) - `min_training_rows_left_out`}.
#' @param REML Whether to use Restricted Maximum Likelihood. (Logical)
#' @param parallel Whether to run the \code{`n`} evaluations in parallel. (Logical)
#'
#'  Remember to register a parallel backend first.
#'  E.g. with \code{doParallel::registerDoParallel}.
#' @details
#'
#'  Packages used:
#'
#'  \subsection{Models}{
#'
#'  \code{\link[stats:lm]{stats::lm}}, \code{\link[lme4:lmer]{lme4::lmer}}
#'  }
#'  \subsection{Results}{
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
#'  }
#' @return \code{list} containing:
#'
#'  \enumerate{
#'   \item a \code{tibble} with summarized results (called \code{summarized_metrics})
#'   \item a \code{tibble} with random evaluations (\code{random_evaluations})
#'  }
#'
#'  ....................................................................
#'
#'  The \strong{Summarized Results} \code{tibble} contains:
#'
#'  Average \strong{\code{RMSE}}, \strong{\code{MAE}}, \strong{\code{NRMSE(IQR)}},
#'  \strong{\code{RRSE}}, \strong{\code{RAE}}, \strong{\code{RMSLE}}.
#'
#'  See the additional metrics (disabled by default) at \code{\link[cvms:gaussian_metrics]{?gaussian_metrics}}.
#'
#'  The \strong{Measure} column indicates the statistical descriptor used on the evaluations.
#'  The row where \code{Measure == All_rows} is the evaluation when the baseline model
#'  is trained on all rows in \code{`train_data`}.
#'
#'  The \strong{Training Rows} column contains the aggregated number of rows used from \code{`train_data`},
#'  when fitting the baseline models.
#'
#'  ....................................................................
#'
#'  The \strong{Random Evaluations} \code{tibble} contains:
#'
#'  The \strong{non-aggregated metrics}.
#'
#'  A nested \code{tibble} with the \strong{predictions} and targets.
#'
#'  A nested \code{tibble} with the \strong{coefficients} of the baseline models.
#'
#'  Number of \strong{training rows} used when fitting the baseline model on the training set.
#'
#'  A nested \strong{Process} information object with information
#'  about the evaluation.
#'
#'  Name of \strong{dependent} variable.
#'
#'  Name of \strong{fixed} effect (bias term only).
#'
#'  \strong{Random} effects structure (if specified).
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @family baseline functions
#' @export
#' @examples
#' \donttest{
#' # Attach packages
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
#' partitions <- partition(data, p = 0.7, list_out = TRUE)
#' train_set <- partitions[[1]]
#' test_set <- partitions[[2]]
#'
#' # Create baseline evaluations
#' # Note: usually n=100 is a good setting
#'
#' baseline_gaussian(
#'   test_data = test_set,
#'   train_data = train_set,
#'   dependent_col = "score",
#'   random_effects = "(1|session)",
#'   n = 2
#' )
#'
#' # Parallelize evaluations
#'
#' # Attach doParallel and register four cores
#' # Uncomment:
#' # library(doParallel)
#' # registerDoParallel(4)
#'
#' # Make sure to uncomment the parallel argument
#' baseline_gaussian(
#'   test_data = test_set,
#'   train_data = train_set,
#'   dependent_col = "score",
#'   random_effects = "(1|session)",
#'   n = 4
#'   #, parallel = TRUE  # Uncomment
#' )
#' }
baseline_gaussian <- function(test_data,
                              train_data,
                              dependent_col,
                              n = 100,
                              metrics = list(),
                              random_effects = NULL,
                              min_training_rows = 5,
                              min_training_rows_left_out = 3,
                              REML = FALSE,
                              parallel = FALSE) {
  baseline(
    test_data = test_data,
    dependent_col = dependent_col,
    family = "gaussian",
    train_data = train_data,
    n = n,
    metrics = metrics,
    random_effects = random_effects,
    min_training_rows = min_training_rows,
    min_training_rows_left_out = min_training_rows_left_out,
    REML = REML,
    parallel = parallel
  )
}


##  .................. #< d292ff4df2c7c31e7b34c48dd9e0523a ># ..................
##  Baseline binomial                                                       ####


#' @title Create baseline evaluations for binary classification
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Create a baseline evaluation of a test set.
#'
#'  In modelling, a \emph{baseline} is a result that
#'  is meaningful to compare the results from our models to. For instance, in
#'  classification, we usually want our results to be better than \emph{random guessing}.
#'  E.g. if we have three classes, we can expect an accuracy of \code{33.33\%}, as for every
#'  observation we have \code{1/3} chance of guessing the correct class. So our model should achieve
#'  a higher accuracy than \code{33.33\%} before it is more useful to us than guessing.
#'
#'  While this expected value is often fairly straightforward to find analytically, it
#'  only represents what we can expect on average. In reality, it's possible to get far better
#'  results than that by guessing.
#'  \strong{\code{baseline_binomial()}}
#'  finds the range of likely values by evaluating multiple sets
#'  of random predictions and summarizing them with a set of useful descriptors. Additionally,
#'  it evaluates a set of all \code{0} predictions and
#'  a set of all \code{1} predictions.
#'
#' @param test_data \code{data.frame}.
#' @param dependent_col Name of dependent variable in the supplied test and training sets.
#' @param n The number of sets of random predictions to evaluate. (Default is \code{100})
#' @param metrics \code{list} for enabling/disabling metrics.
#'
#'   E.g. \code{list("F1" = FALSE)} would remove \code{F1} from the results,
#'   and \code{list("Accuracy" = TRUE)} would add the regular \code{Accuracy} metric
#'   to the results.
#'   Default values (\code{TRUE}/\code{FALSE}) will be used for the remaining available metrics.
#'
#'   You can enable/disable all metrics at once by including
#'   \code{"all" = TRUE/FALSE} in the \code{list}. This is done prior to enabling/disabling
#'   individual metrics, why f.i. \code{list("all" = FALSE, "Accuracy" = TRUE)}
#'   would return only the \code{Accuracy} metric.
#'
#'   The \code{list} can be created with
#'   \code{\link[cvms:binomial_metrics]{binomial_metrics()}}.
#'
#'   Also accepts the string \code{"all"}.
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
#'  N.B. Only affects evaluation metrics, not the returned predictions.
#' @param cutoff Threshold for predicted classes. (Numeric)
#' @param parallel Whether to run the \code{`n`} evaluations in parallel. (Logical)
#'
#'  Remember to register a parallel backend first.
#'  E.g. with \code{doParallel::registerDoParallel}.
#' @details
#'
#'  Packages used:
#'
#'  \code{ROC} and \code{AUC}: \code{\link[pROC:roc]{pROC::roc}}
#' @return \code{list} containing:
#'
#'  \enumerate{
#'   \item a \code{tibble} with summarized results (called \code{summarized_metrics})
#'   \item a \code{tibble} with random evaluations (\code{random_evaluations})
#'  }
#'
#'  ....................................................................
#'
#'  Based on the generated test set predictions,
#'  a confusion matrix and \code{ROC} curve are used to get the following:
#'
#'  \code{ROC}:
#'
#'  \strong{\code{AUC}}, \strong{\code{Lower CI}}, and \strong{\code{Upper CI}}
#'
#'  Note, that the \code{ROC} curve is only computed when \code{AUC} is enabled.
#'
#'  \code{Confusion Matrix}:
#'
#'  \strong{\code{Balanced Accuracy}},
#'  \strong{\code{Accuracy}},
#'  \strong{\code{F1}},
#'  \strong{\code{Sensitivity}}, \strong{\code{Specificity}},
#'  \strong{\code{Positive Predictive Value}},
#'  \strong{\code{Negative Predictive Value}},
#'  \strong{\code{Kappa}},
#'  \strong{\code{Detection Rate}},
#'  \strong{\code{Detection Prevalence}},
#'  \strong{\code{Prevalence}}, and
#'  \strong{\code{MCC}} (Matthews correlation coefficient).
#'
#'  ....................................................................
#'
#'  The \strong{Summarized Results} \code{tibble} contains:
#'
#'  The \strong{Measure} column indicates the statistical descriptor used on the evaluations.
#'  The row where \code{Measure == All_0} is the evaluation when all predictions are \code{0}.
#'  The row where \code{Measure == All_1} is the evaluation when all predictions are \code{1}.
#'
#'  The \strong{aggregated metrics}.
#'
#'  ....................................................................
#'
#'  The \strong{Random Evaluations} \code{tibble} contains:
#'
#'  The \strong{non-aggregated metrics}.
#'
#'  A nested \code{tibble} with the \strong{predictions} and targets.
#'
#'  A \code{list} of \strong{ROC} curve objects (if computed).
#'
#'  A nested \code{tibble} with the \strong{confusion matrix}.
#'  The \code{Pos_} columns tells you whether a row is a
#'  True Positive (\code{TP}), True Negative (\code{TN}), False Positive (\code{FP}),
#'  or False Negative (\code{FN}), depending on which level is the "positive" class.
#'  I.e. the level you wish to predict.
#'
#'  A nested \strong{Process} information object with information
#'  about the evaluation.
#'
#'  Name of \strong{dependent} variable.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family baseline functions
#' @examples
#' \donttest{
#' # Attach packages
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
#' partitions <- partition(data, p = 0.7, list_out = TRUE)
#' train_set <- partitions[[1]]
#' test_set <- partitions[[2]]
#'
#' # Create baseline evaluations
#' # Note: usually n=100 is a good setting
#'
#' baseline_binomial(
#'   test_data = test_set,
#'   dependent_col = "diagnosis",
#'   n = 2
#' )
#'
#' # Parallelize evaluations
#'
#' # Attach doParallel and register four cores
#' # Uncomment:
#' # library(doParallel)
#' # registerDoParallel(4)
#'
#' # Make sure to uncomment the parallel argument
#' baseline_binomial(
#'   test_data = test_set,
#'   dependent_col = "diagnosis",
#'   n = 4
#'   #, parallel = TRUE  # Uncomment
#' )
#' }
baseline_binomial <- function(test_data,
                              dependent_col,
                              n = 100,
                              metrics = list(),
                              positive = 2,
                              cutoff = 0.5,
                              parallel = FALSE) {
  baseline(
    test_data = test_data,
    dependent_col = dependent_col,
    family = "binomial",
    n = n,
    metrics = metrics,
    positive = positive,
    cutoff = cutoff,
    parallel = parallel
  )
}


##  .................. #< bb1cf76b66161d5938fb586f177b8b19 ># ..................
##  Baseline multinomial                                                    ####


#' @title Create baseline evaluations
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Create a baseline evaluation of a test set.
#'
#'  In modelling, a \emph{baseline} is a result that
#'  is meaningful to compare the results from our models to. For instance, in
#'  classification, we usually want our results to be better than \emph{random guessing}.
#'  E.g. if we have three classes, we can expect an accuracy of \code{33.33\%}, as for every
#'  observation we have \code{1/3} chance of guessing the correct class. So our model should achieve
#'  a higher accuracy than \code{33.33\%} before it is more useful to us than guessing.
#'
#'  While this expected value is often fairly straightforward to find analytically, it
#'  only represents what we can expect on average. In reality, it's possible to get far better
#'  results than that by guessing.
#'  \strong{\code{baseline_multinomial()}}
#'  finds the range of likely values by evaluating multiple sets
#'  of random predictions and summarizing them with a set of useful descriptors.
#'
#'  Technically, it creates \emph{one-vs-all} (binomial) baseline evaluations
#'  for the \code{`n`} sets of random predictions and summarizes them. Additionally,
#'  sets of "all class x,y,z,..." predictions are evaluated.
#' @param test_data \code{data.frame}.
#' @param dependent_col Name of dependent variable in the supplied test and training sets.
#' @param n The number of sets of random predictions to evaluate. (Default is \code{100})
#' @param metrics \code{list} for enabling/disabling metrics.
#'
#'   E.g. \code{list("F1" = FALSE)} would remove \code{F1} from the results,
#'   and \code{list("Accuracy" = TRUE)} would add the regular \code{Accuracy} metric
#'   to the results.
#'   Default values (\code{TRUE}/\code{FALSE}) will be used for the remaining available metrics.
#'
#'   You can enable/disable all metrics at once by including
#'   \code{"all" = TRUE/FALSE} in the \code{list}. This is done prior to enabling/disabling
#'   individual metrics, why f.i. \code{list("all" = FALSE, "Accuracy" = TRUE)}
#'   would return only the \code{Accuracy} metric.
#'
#'   The \code{list} can be created with
#'   \code{\link[cvms:multinomial_metrics]{multinomial_metrics()}}.
#'
#'   Also accepts the string \code{"all"}.
#' @param random_generator_fn Function for generating random numbers.
#'  The \code{softmax} function is applied to the generated numbers to transform them to probabilities.
#'
#'  The first argument must be the number of random numbers to generate,
#'  as no other arguments are supplied.
#'
#'  To test the effect of using different functions,
#'  see \code{\link[cvms:multiclass_probability_tibble]{multiclass_probability_tibble()}}.
#' @param parallel Whether to run the \code{`n`} evaluations in parallel. (Logical)
#'
#'  Remember to register a parallel backend first.
#'  E.g. with \code{doParallel::registerDoParallel}.
#' @details
#'
#'  Packages used:
#'
#'  Multiclass \code{ROC} curve and \code{AUC}:
#'  \code{\link[pROC:multiclass.roc]{pROC::multiclass.roc}}
#' @return \code{list} containing:
#'
#'  \enumerate{
#'   \item a \code{tibble} with summarized results (called \code{summarized_metrics})
#'   \item a \code{tibble} with random evaluations (\code{random_evaluations})
#'   \item a \code{tibble} with the summarized class level results
#'         (\code{summarized_class_level_results})
#'  }
#'
#'  ....................................................................
#'
#'  \subsection{Macro metrics}{
#'
#'  Based on the generated predictions,
#'  \emph{one-vs-all} (binomial) evaluations are performed and aggregated
#'  to get the following \strong{macro} metrics:
#'
#'  \strong{\code{Balanced Accuracy}},
#'  \strong{\code{F1}},
#'  \strong{\code{Sensitivity}},
#'  \strong{\code{Specificity}},
#'  \strong{\code{Positive Predictive Value}},
#'  \strong{\code{Negative Predictive Value}},
#'  \strong{\code{Kappa}},
#'  \strong{\code{Detection Rate}},
#'  \strong{\code{Detection Prevalence}}, and
#'  \strong{\code{Prevalence}}.
#'
#'  In general, the metrics mentioned in
#'  \code{\link[cvms:binomial_metrics]{binomial_metrics()}}
#'  can be enabled as macro metrics
#'  (excluding \code{MCC}, \code{AUC}, \code{Lower CI},
#'  \code{Upper CI}, and the \code{AIC/AICc/BIC} metrics).
#'  These metrics also has a weighted average
#'  version.
#'
#'  \strong{N.B.} we also refer to the \emph{one-vs-all evaluations} as the \emph{class level results}.
#'  }
#'
#'  \subsection{Multiclass metrics}{
#'
#'  In addition, the \strong{\code{Overall Accuracy}} and \emph{multiclass}
#'  \strong{\code{MCC}} metrics are computed. \emph{Multiclass} \code{AUC} can be enabled but
#'  is slow to calculate with many classes.
#'  }
#'  ....................................................................
#'
#'  The \strong{Summarized Results} \code{tibble} contains:
#'
#'  Summary of the random evaluations.
#'
#'  \strong{How}: The one-vs-all binomial evaluations are aggregated by repetition and summarized. Besides the
#'  metrics from the binomial evaluations, it
#'  also includes \strong{\code{Overall Accuracy}} and \emph{multiclass} \strong{\code{MCC}}.
#'
#'  The \strong{Measure} column indicates the statistical descriptor used on the evaluations.
#'  The \strong{Mean}, \strong{Median}, \strong{SD}, \strong{IQR}, \strong{Max}, \strong{Min},
#'  \strong{NAs}, and \strong{INFs} measures describe the \emph{Random Evaluations} \code{tibble},
#'  while the \strong{CL_Max}, \strong{CL_Min}, \strong{CL_NAs}, and
#'  \strong{CL_INFs} describe the \strong{C}lass \strong{L}evel results.
#'
#'  The rows where \code{Measure == All_<<class name>>} are the evaluations when all
#'  the observations are predicted to be in that class.
#'
#'  ....................................................................
#'
#'  The \strong{Summarized Class Level Results} \code{tibble} contains:
#'
#'  The (nested) summarized results for each class, with the same metrics and descriptors as
#'  the \emph{Summarized Results} \code{tibble}. Use \code{\link[tidyr:unnest]{tidyr::unnest}}
#'  on the \code{tibble} to inspect the results.
#'
#'  \strong{How}: The one-vs-all evaluations are summarized by class.
#'
#'  The rows where \code{Measure == All_0} are the evaluations when none of the observations
#'  are predicted to be in that class, while the rows where \code{Measure == All_1} are the
#'  evaluations when all of the observations are predicted to be in that class.
#'
#'  ....................................................................
#'
#'  The \strong{Random Evaluations} \code{tibble} contains:
#'
#'  The repetition results with the same metrics as the \emph{Summarized Results} \code{tibble}.
#'
#'  \strong{How}: The one-vs-all evaluations are aggregated by repetition.
#'  If a metric contains one or more \code{NAs} in the one-vs-all evaluations, it
#'  will lead to an \code{NA} result for that repetition.
#'
#'  Also includes:
#'
#'  A nested \code{tibble} with the one-vs-all binomial evaluations (\strong{Class Level Results}),
#'  including nested \strong{Confusion Matrices} and the
#'  \strong{Support} column, which is a count of how many observations from the
#'  class is in the test set.
#'
#'  A nested \code{tibble} with the \strong{predictions} and targets.
#'
#'  A \code{list} of \strong{ROC} curve objects.
#'
#'  A nested \code{tibble} with the multiclass \strong{confusion matrix}.
#'
#'  A nested \strong{Process} information object with information
#'  about the evaluation.
#'
#'  Name of \strong{dependent} variable.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family baseline functions
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
#' # Create some data with multiple classes
#' multiclass_data <- tibble(
#'   "target" = rep(paste0("class_", 1:5), each = 10)
#' ) %>%
#'   dplyr::sample_n(35)
#'
#' baseline_multinomial(
#'   test_data = multiclass_data,
#'   dependent_col = "target",
#'   n = 4
#' )
#'
#' # Parallelize evaluations
#'
#' # Attach doParallel and register four cores
#' # Uncomment:
#' # library(doParallel)
#' # registerDoParallel(4)
#'
#' # Make sure to uncomment the parallel argument
#' (mb <- baseline_multinomial(
#'   test_data = multiclass_data,
#'   dependent_col = "target",
#'   n = 6
#'   #, parallel = TRUE  # Uncomment
#' ))
#'
#' # Inspect the summarized class level results
#' # for class_2
#' mb$summarized_class_level_results %>%
#'   dplyr::filter(Class == "class_2") %>%
#'   tidyr::unnest(Results)
#'
#' # Multinomial with custom random generator function
#' # that creates very "certain" predictions
#' # (once softmax is applied)
#'
#' rcertain <- function(n) {
#'   (runif(n, min = 1, max = 100)^1.4) / 100
#' }
#'
#' # Make sure to uncomment the parallel argument
#' baseline_multinomial(
#'   test_data = multiclass_data,
#'   dependent_col = "target",
#'   n = 6,
#'   random_generator_fn = rcertain
#'   #, parallel = TRUE  # Uncomment
#' )
#' }
baseline_multinomial <- function(test_data,
                                 dependent_col,
                                 n = 100,
                                 metrics = list(),
                                 random_generator_fn = runif,
                                 parallel = FALSE) {

  baseline(
    test_data = test_data,
    dependent_col = dependent_col,
    family = "multinomial",
    n = n,
    metrics = metrics,
    random_generator_fn = random_generator_fn,
    parallel = parallel
  )
}


