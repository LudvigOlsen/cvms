

#   __________________ #< 3755b5a0653d6b77bd3e8d85c1780003 ># __________________
#   Validate lm lmer glm glmer                                              ####


#' @title Validate regression models on a test set
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("stable")}
#'
#'  Train linear or logistic regression models on a full training set and validate it by
#'  predicting the test/validation set.
#'  Returns results in a tibble for easy reporting, along with the trained models.
#' @inheritParams cross_validate
#' @param train_data Data Frame.
#'
#'  Can contain a grouping factor for identifying partitions - as made with
#'  \code{\link[groupdata2:partition]{groupdata2::partition()}}.
#'  See \code{partitions_col}.
#' @param test_data Data Frame. If specifying \code{partitions_col}, this can be \code{NULL}.
#' @param partitions_col Name of grouping factor for identifying partitions. (Character)
#'
#'  Rows with the value \code{1} in \code{partitions_col} are used as training set and
#'  rows with the value \code{2} are used as test set.
#'
#'  N.B. \strong{Only used if \code{test_data} is \code{NULL}}.
#' @param err_nc Raise error if model does not converge. (Logical)
#' @param parallel Whether to validate the list of models in parallel. (Logical)
#'
#'  Remember to register a parallel backend first.
#'  E.g. with \code{doParallel::registerDoParallel}.
#' @param link,models,model_verbose Deprecated.
#' @inherit cross_validate details
#' @return Tbl (tibble) with the results and model objects.
#'
#'  \subsection{Shared across families}{
#'
#'  A nested tibble with \strong{coefficients} of the models from all iterations.
#'
#'  Count of \strong{convergence warnings}. Consider discarding models that did not converge on all
#'  iterations. Note: you might still see results, but these should be taken with a grain of salt!
#'
#'  Count of \strong{other warnings}. These are warnings without keywords such as "convergence".
#'
#'  Count of \strong{Singular Fit messages}. See
#'  \code{\link[lme4:isSingular]{lme4::isSingular}} for more information.
#'
#'  Nested tibble with the \strong{warnings and messages} caught for each model.
#'
#'  Specified \strong{family}.
#'
#'  Nested \strong{model} objects.
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
#'  \strong{RMSE}, \strong{MAE}, \strong{NRMSE}, \strong{RMSEIQR},
#'  \strong{r2m}, \strong{r2c}, \strong{AIC}, \strong{AICc},
#'  and \strong{BIC}.
#'
#'  A nested tibble with the \strong{predictions} and targets.
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Binomial Results}{
#'
#'  ----------------------------------------------------------------
#'
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
#'  \strong{Positive Predictive Value},
#'  \strong{Negative Predictive Value},
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
#'  A tibble with \strong{predictions}, predicted classes (depends on \code{cutoff}),
#'  and the targets.
#'
#'  A tibble with the sensativities and specificities from the \strong{ROC} curve.
#'
#'  }
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family validation functions
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
#'   p = 0.7,
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   list_out = FALSE
#' ) %>%
#'   arrange(.partitions)
#'
#' # Validate a model
#'
#' # Gaussian
#' validate(data_partitioned,
#'   formulas = "score~diagnosis",
#'   partitions_col = ".partitions",
#'   family = "gaussian",
#'   REML = FALSE
#' )
#'
#' # Binomial
#' validate(data_partitioned,
#'   formulas = "diagnosis~score",
#'   partitions_col = ".partitions",
#'   family = "binomial"
#' )
#'
#' ## Feed separate train and test sets
#'
#' # Partition data to list of data frames
#' # The first data frame will be train (70% of the data)
#' # The second will be test (30% of the data)
#' data_partitioned <- partition(data,
#'   p = 0.7,
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   list_out = TRUE
#' )
#' train_data <- data_partitioned[[1]]
#' test_data <- data_partitioned[[2]]
#'
#' # Validate a model
#'
#' # Gaussian
#' validate(train_data,
#'   test_data = test_data,
#'   formulas = "score~diagnosis",
#'   family = "gaussian",
#'   REML = FALSE
#' )
validate <- function(train_data,
                     formulas,
                     family,
                     test_data = NULL,
                     partitions_col = ".partitions",
                     control = NULL,
                     REML = FALSE,
                     cutoff = 0.5,
                     positive = 2,
                     metrics = list(),
                     preprocessing = NULL,
                     err_nc = FALSE,
                     rm_nc = FALSE,
                     parallel = FALSE,
                     verbose = FALSE,
                     link = deprecated(),
                     models = deprecated(),
                     model_verbose = deprecated()) {
  if (!rlang::is_missing(link)) {
    deprecate_stop("1.0.0", "cvms::validate(link = )")
  }

  if (!rlang::is_missing(models)) {
    deprecate_warn(
      "1.0.0", "cvms::validate(models = )",
      "cvms::validate(formulas = )"
    )
    formulas <- models
  }

  if (!rlang::is_missing(model_verbose)) {
    deprecate_warn(
      "1.0.0", "cvms::validate(model_verbose = )",
      "cvms::validate(verbose = )"
    )
    verbose <- model_verbose
  }

  call_validate(
    train_data = train_data,
    test_data = test_data,
    formulas = formulas,
    partitions_col = partitions_col,
    family = family,
    control = control,
    REML = REML,
    cutoff = cutoff,
    positive = positive,
    metrics = metrics,
    preprocessing = preprocessing,
    err_nc = err_nc,
    rm_nc = rm_nc,
    parallel = parallel,
    verbose = verbose
  )
}
