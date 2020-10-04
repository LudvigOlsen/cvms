

#   __________________ #< c52208f58dea1e7a0b9da8e2ec9c59ac ># __________________
#   Baseline                                                                ####


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
#'  \strong{\code{baseline()}} (\code{binomial}, \code{multinomial})
#'  finds the range of likely values by evaluating multiple sets
#'  of random predictions and summarizing them with a set of useful descriptors.
#'  If random guessing frequently obtains an accuracy of \code{40\%}, perhaps our model
#'  should have better performance than this, before we declare it better than guessing.
#'
#'  \subsection{\strong{How}}{
#'
#'  When \code{`family`} is \code{binomial}: evaluates \code{`n`} sets of random predictions
#'  against the dependent variable, along with a set of all \code{0} predictions and
#'  a set of all \code{1} predictions. See also \code{\link[cvms:baseline_binomial]{baseline_binomial()}}.
#'
#'  When \code{`family`} is \code{multinomial}: creates \emph{one-vs-all} (binomial)
#'  baseline evaluations for \code{`n`} sets of random predictions against the dependent variable,
#'  along with sets of "all class x,y,z,..." predictions.
#'  See also \code{\link[cvms:baseline_multinomial]{baseline_multinomial()}}.
#'
#'  When \code{`family`} is \code{gaussian}: fits baseline models (\code{y ~ 1}) on \code{`n`} random
#'  subsets of \code{`train_data`} and evaluates each model on \code{`test_data`}. Also evaluates a
#'  model fitted on all rows in \code{`train_data`}.
#'  See also \code{\link[cvms:baseline_gaussian]{baseline_gaussian()}}.
#'  }
#'
#'  \subsection{\strong{Wrapper functions}}{
#'
#'  Consider using one of the wrappers, as they are simpler to use and understand:
#'  \strong{\code{\link[cvms:baseline_gaussian]{baseline_gaussian()}}},
#'  \strong{\code{\link[cvms:baseline_multinomial]{baseline_multinomial()}}}, and
#'  \strong{\code{\link[cvms:baseline_binomial]{baseline_binomial()}}}.
#'  }
#'
#' @inheritParams evaluate
#' @param test_data \code{data.frame}.
#' @param train_data \code{data.frame}. Only used when \code{`family`} is \code{"gaussian"}.
#' @param dependent_col Name of dependent variable in the supplied test and training sets.
#' @param n Number of random samplings to perform. (Default is \code{100})
#'
#'  For \code{gaussian}: The number of random samplings of \code{`train_data`} to fit baseline models on.
#'
#'  For \code{binomial} and \code{multinomial}: The number of sets of random predictions to evaluate.
#'
#' @param family Name of family. (Character)
#'
#'  Currently supports \code{"gaussian"}, \code{"binomial"} and \code{"multinomial"}.
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
#'
#'  N.B. \strong{Binomial only}. (Character or Integer)
#' @param cutoff Threshold for predicted classes. (Numeric)
#'
#'  N.B. \strong{Binomial only}
#' @param random_effects Random effects structure for the Gaussian baseline model. (Character)
#'
#'  E.g. with \code{"(1|ID)"}, the model becomes \code{"y ~ 1 + (1|ID)"}.
#'
#'  N.B. \strong{Gaussian only}
#' @param random_generator_fn Function for generating random numbers when \code{type} is \code{"multinomial"}.
#'  The \code{softmax} function is applied to the generated numbers to transform them to probabilities.
#'
#'  The first argument must be the number of random numbers to generate,
#'  as no other arguments are supplied.
#'
#'  To test the effect of using different functions,
#'  see \code{\link[cvms:multiclass_probability_tibble]{multiclass_probability_tibble()}}.
#'
#'  N.B. \strong{Multinomial only}
#' @param min_training_rows Minimum number of rows in the random subsets of \code{`train_data`}.
#'
#'  \strong{Gaussian only}. (Integer)
#' @param min_training_rows_left_out Minimum number of rows left out of the random subsets of \code{`train_data`}.
#'
#'  I.e. a subset will maximally have the size:
#'
#'  \code{max_rows_in_subset = nrow(`train_data`) - `min_training_rows_left_out`}.
#'
#'  N.B. \strong{Gaussian only}. (Integer)
#' @param REML Whether to use Restricted Maximum Likelihood. (Logical)
#'
#'  N.B. \strong{Gaussian only}. (Integer)
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
#'  Gaussian: \code{\link[stats:lm]{stats::lm}}, \code{\link[lme4:lmer]{lme4::lmer}}
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
#'  ROC and related metrics:
#'
#'  Binomial: \code{\link[pROC:roc]{pROC::roc}}
#'
#'  Multinomial: \code{\link[pROC:multiclass.roc]{pROC::multiclass.roc}}
#'
#'  }
#' @return \code{list} containing:
#'
#'  \enumerate{
#'   \item a \code{tibble} with summarized results (called \code{summarized_metrics})
#'   \item a \code{tibble} with random evaluations (\code{random_evaluations})
#'   \item a \code{tibble} with the summarized class level results
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
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Binomial Results}{
#'
#'  ----------------------------------------------------------------
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
#'  to get the same metrics as in the \code{binomial} results
#'  (excluding \code{MCC}, \code{AUC}, \code{Lower CI} and \code{Upper CI}),
#'  with the addition of \strong{Overall Accuracy} and \emph{multiclass}
#'  \strong{MCC} in the summarized results.
#'  It is possible to enable multiclass \strong{AUC} as well, which has been
#'  disabled by default as it is slow to calculate when there's a large set of classes.
#'
#'  Note: we also refer to the \emph{one-vs-all evaluations} as the \emph{class level results}.
#'
#'  ....................................................................
#'
#'  The \strong{Summarized Results} \code{tibble} contains:
#'
#'  Summary of the random evaluations.
#'
#'  \strong{How}: First, the one-vs-all binomial evaluations are aggregated by repetition,
#'  then, these aggregations are summarized. Besides the
#'  metrics from the binomial evaluations (see \emph{Binomial Results} above), it
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
#'  }
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
#' # Gaussian
#' baseline(
#'   test_data = test_set, train_data = train_set,
#'   dependent_col = "score", random_effects = "(1|session)",
#'   n = 2, family = "gaussian"
#' )
#'
#' # Binomial
#' baseline(
#'   test_data = test_set, dependent_col = "diagnosis",
#'   n = 2, family = "binomial"
#' )
#'
#' # Multinomial
#'
#' # Create some data with multiple classes
#' multiclass_data <- tibble(
#'   "target" = rep(paste0("class_", 1:5), each = 10)
#' ) %>%
#'   dplyr::sample_n(35)
#'
#' baseline(
#'   test_data = multiclass_data,
#'   dependent_col = "target",
#'   n = 4, family = "multinomial"
#' )
#'
#' # Parallelize evaluations
#'
#' # Attach doParallel and register four cores
#' # Uncomment:
#' # library(doParallel)
#' # registerDoParallel(4)
#'
#' # Binomial
#' baseline(
#'   test_data = test_set, dependent_col = "diagnosis",
#'   n = 4, family = "binomial"
#'   #, parallel = TRUE   # Uncomment
#' )
#'
#' # Gaussian
#' baseline(
#'   test_data = test_set, train_data = train_set,
#'   dependent_col = "score", random_effects = "(1|session)",
#'   n = 4, family = "gaussian"
#'   #, parallel = TRUE   # Uncomment
#' )
#'
#' # Multinomial
#' (mb <- baseline(
#'   test_data = multiclass_data,
#'   dependent_col = "target",
#'   n = 6, family = "multinomial"
#'   #, parallel = TRUE   # Uncomment
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
#' baseline(
#'   test_data = multiclass_data,
#'   dependent_col = "target",
#'   n = 6, family = "multinomial",
#'   random_generator_fn = rcertain
#'   #, parallel = TRUE  # Uncomment
#' )
#' }
#' @importFrom stats runif rnorm terms IQR median predict sd reformulate
baseline <- function(test_data,
                     dependent_col,
                     family,
                     train_data = NULL,
                     # how many times to randomly sample probabilities (bootstrapping?)
                     n = 100,
                     metrics = list(),
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

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()

  # Data frames
  checkmate::assert_data_frame(test_data,
    col.names = "named",
    add = assert_collection
  )
  checkmate::assert_data_frame(train_data,
    col.names = "named",
    null.ok = family != "gaussian",
    add = assert_collection
  )
  checkmate::assert_names(colnames(test_data),
    must.include = dependent_col,
    add = assert_collection
  )
  if (!is.null(train_data)) {
    checkmate::assert_names(colnames(train_data),
      must.include = dependent_col,
      add = assert_collection
    )
  }

  # Character
  checkmate::assert_string(x = dependent_col, add = assert_collection)
  checkmate::assert_choice(
    x = family,
    choices = c("gaussian", "binomial", "multinomial"),
    add = assert_collection
  )
  checkmate::assert_string(
    x = random_effects, null.ok = TRUE,
    add = assert_collection
  )

  # Numeric
  checkmate::assert_count(
    x = n,
    positive = TRUE,
    add = assert_collection
  )

  # positive
  checkmate::assert(
    checkmate::check_choice(
      x = positive,
      choices = c(1, 2)
    ),
    checkmate::check_string(x = positive)
  )

  checkmate::assert_number(
    x = cutoff,
    lower = 0,
    upper = 1,
    add = assert_collection
  )
  checkmate::assert_count(
    x = min_training_rows,
    positive = TRUE,
    add = assert_collection
  )
  checkmate::assert_count(
    x = min_training_rows_left_out,
    positive = TRUE,
    add = assert_collection
  )

  # Flags
  checkmate::assert_flag(x = REML, add = assert_collection)
  checkmate::assert_flag(x = parallel, add = assert_collection)

  # Functional
  checkmate::assert_function(
    x = random_generator_fn,
    add = assert_collection
  )

  # Not added to collection, but probably doesn't matter much
  checkmate::assert(
    checkmate::check_string(x = metrics, fixed = "all"),
    checkmate::check_list(
      x = metrics, any.missing = FALSE,
      types = c("logical", "character")
    )
  )

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####


  # Start by converting the train and test data to tibbles
  # Will be NULL if they were NULL before
  test_data <- to_tibble(test_data, "test_data", caller = "baseline()")
  train_data <- to_tibble(train_data, "train_data", caller = "baseline()")

  if (family == "binomial") {
    arg_not_used(
      arg = train_data, arg_name = "train_data",
      family = "binomial", current_fn = "baseline"
    )
    arg_not_used(
      arg = random_effects, arg_name = "random_effects",
      family = "binomial", current_fn = "baseline"
    )
    if (!isTRUE(all.equal(random_generator_fn, runif))) {
      message(paste0(
        "'random_generator_fn' was not default function. ",
        "Note that the 'random_generator_fn' is not used in ",
        "the binomial version of baseline()."
      ))
    }
    unaccepted_metrics <- intersect(names(metrics), c("AIC", "AICc", "BIC"))
    if (length(unaccepted_metrics) > 0) {
      stop(paste0(
        "binomial baseline() does not accept the following metric",
        ifelse(length(unaccepted_metrics) > 1, "s", ""),
        ": ", paste(unaccepted_metrics, collapse = ", "), "."
      ))
    }

    # Enable Accuracy if not otherwise specified
    if (checkmate::test_string(x = metrics, pattern = "^all$")) {
      metrics <- list("all" = TRUE)
    }
    metrics <- add_metric_if_not_specified(metrics, "Accuracy", value=TRUE, check_all=TRUE)

    return(
      create_binomial_baseline_evaluations(
        test_data = test_data,
        dependent_col = dependent_col,
        reps = n,
        positive = positive,
        cutoff = cutoff,
        metrics = metrics,
        parallel_ = parallel
      )
    )
  } else if (family == "multinomial") {
    arg_not_used(
      arg = train_data, arg_name = "train_data",
      family = "multinomial", current_fn = "baseline"
    )
    arg_not_used(
      arg = random_effects, arg_name = "random_effects",
      family = "multinomial", current_fn = "baseline"
    )
    unaccepted_metrics <- intersect(names(metrics), c("AIC", "AICc", "BIC"))
    if (length(unaccepted_metrics) > 0) {
      stop(paste0(
        "multinomial baseline() does not accept the following metric",
        ifelse(length(unaccepted_metrics) > 1, "s", ""),
        ": ", paste(unaccepted_metrics, collapse = ", "), "."
      ))
    }

    return(
      create_multinomial_baseline_evaluations(
        test_data = test_data,
        dependent_col = dependent_col,
        reps = n,
        metrics = metrics,
        parallel_ = parallel,
        random_generator_fn = random_generator_fn
      )
    )
  } else if (family == "gaussian") {
    if (is.null(train_data)) {
      stop("'train_data' must be passed for Gaussian baseline.")
    }

    if (!isTRUE(all.equal(random_generator_fn, runif))) {
      message(paste0(
        "'random_generator_fn' was not default function. ",
        "Note that the 'random_generator_fn' is not used in ",
        "the Gaussian version of baseline()."
      ))
    }

    return(
      create_gaussian_baseline_evaluations(
        train_data = train_data,
        test_data = test_data,
        dependent_col = dependent_col,
        random_effects = random_effects,
        n_samplings = n,
        min_training_rows = min_training_rows,
        min_training_rows_left_out = min_training_rows_left_out,
        REML = REML,
        metrics = metrics,
        parallel_ = parallel
      )
    )
  }
}
