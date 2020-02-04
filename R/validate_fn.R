
#' @title Validate custom model functions on a test set
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Fit your model function on a training set and validate it by
#'  predicting the test/validation set. Preprocess the train/test split.
#'  Validate different hyperparameter combinations and formulas at once.
#'  Returns results and fitted models in a tibble for easy reporting and further analysis.
#'
#'  Compared to \code{\link[cvms:validate]{validate()}},
#'  this function allows you supply a custom model function, a predict function,
#'  a preprocess function and the hyperparameter values to validate.
#'
#'  Supports regression and classification (binary and multiclass).
#'  See \code{type}.
#'
#'  Note that some metrics may not be computable for some types
#'  of model objects.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family validation functions
#' @inheritParams cross_validate_fn
#' @inheritParams validate
#' @inherit cross_validate_fn details
#' @return Tbl (tibble) with the results and model objects.
#'
#'  \subsection{Shared across families}{
#'
#'  A nested tibble with \strong{coefficients} of the models. The coefficients
#'  are extracted from the model object with \code{\link[broom:tidy]{broom::tidy()}} or
#'  \code{\link[stats:coef]{coef()}} (with some restrictions on the output).
#'  If these attempts fail, a default coefficients tibble filled with \code{NA}s is returned.
#'
#'  Nested tibble with the used \strong{preprocessing parameters},
#'  if a passed \code{preprocess_fn} returns the parameters in a tibble.
#'
#'  Count of \strong{convergence warnings}, using a limited set of keywords (e.g. "convergence"). If a
#'  convergence warning does not contain one of these keywords, it will be counted with \strong{other warnings}.
#'  Consider discarding models that did not converge on all iterations.
#'  Note: you might still see results, but these should be taken with a grain of salt!
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
#'
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Gaussian Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  \strong{RMSE}, \strong{MAE}, \strong{r2m}, \strong{NRMSE}, \strong{RMSEIQR},
#'  \strong{r2c}, \strong{AIC}, \strong{AICc},
#'  and \strong{BIC}. Some metrics may return \code{NA} if they can't be
#'  extracted from the fitted model objects.
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
#'  A nested tibble with the \strong{predictions}, predicted classes (depends on \code{cutoff}), and targets.
#'  Note, that the \strong{predictions are not necessarily of the specified \code{positive} class}, but of
#'  the model's positive class (second level of dependent variable, alphabetically).
#'
#'   A list of \strong{ROC} curve objects.
#'
#'  A nested tibble with the \strong{confusion matrix}.
#'  The \code{Pos_} columns tells you whether a row is a
#'  True Positive (TP), True Negative (TN), False Positive (FP), or False Negative (FN),
#'  depending on which level is the "positive" class. I.e. the level you wish to predict.
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Multinomial Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  For each class, a \emph{one-vs-all} binomial evaluation is performed. This creates
#'  a \strong{class level results} tibble containing the same metrics as the binomial results
#'  described above (excluding AUC, Lower CI and Upper CI), along with the \strong{Support} metric, which is simply a
#'  count of the class in the target column. These metrics are used to calculate the macro metrics
#'  in the output tibble. The nested class level results tibble is also included in the output tibble,
#'  and would usually be reported along with the macro and overall metrics.
#'
#'  The output tibble contains the macro and overall metrics.
#'  The metrics that share their name with the metrics in the nested
#'  class level results tibble are averages of those metrics
#'  (note: does not remove \code{NA}s before averaging).
#'  In addition to these, it also includes the \strong{Overall Accuracy}
#'  and multiclass \strong{AUC} metrics.
#'
#'  Other available metrics (disabled by default, see \code{metrics}):
#'  \strong{Accuracy}, \strong{AIC}, \strong{AICc}, \strong{BIC},
#'  \strong{Weighted Balanced Accuracy}, \strong{Weighted Accuracy},
#'  \strong{Weighted F1}, \strong{Weighted Sensitivity}, \strong{Weighted Sensitivity},
#'  \strong{Weighted Specificity}, \strong{Weighted Pos Pred Value},
#'  \strong{Weighted Neg Pred Value}, \strong{Weighted Kappa}, \strong{Weighted MCC},
#'  \strong{Weighted Detection Rate}, \strong{Weighted Detection Prevalence}, and
#'  \strong{Weighted Prevalence}.
#'
#'  Note that the "Weighted" average metrics are weighted by the \code{Support}.
#'
#'  Also includes:
#'
#'  A nested tibble with the \strong{predictions}, predicted classes, and targets.
#'
#'   A list of \strong{ROC} curve objects.
#'
#'  A nested tibble with the multiclass \strong{Confusion Matrix}.
#'
#'  \strong{Class Level Results}
#'
#'  Besides the binomial evaluation metrics and the \code{Support} metric,
#'  the nested class level results tibble also includes:
#'
#'  A nested tibble with the \strong{confusion matrix} from the one-vs-all evaluation.
#'  The \code{Pos_} columns tells you whether a row is a
#'  True Positive (TP), True Negative (TN), False Positive (FP), or False Negative (FN),
#'  depending on which level is the "positive" class. In our case, \code{1} is the current class
#'  and \code{0} represents all the other classes together.
#'
#'  }
#' @examples
#' # Attach packages
#' library(cvms)
#' library(groupdata2) # fold()
#' library(dplyr) # %>% arrange() mutate()
#'
#' # Data is part of cvms
#' data <- participant.scores
#'
#' # Set seed for reproducibility
#' set.seed(7)
#'
#' # Fold data
#' data <- fold(data,
#'   k = 4,
#'   cat_col = "diagnosis",
#'   id_col = "participant"
#' ) %>%
#'   mutate(diagnosis = as.factor(diagnosis)) %>%
#'   arrange(.folds)
validate_fn <- function(train_data,
                        formulas,
                        type,
                        model_fn,
                        predict_fn,
                        test_data = NULL,
                        preprocess_fn = NULL,
                        preprocess_once = FALSE,
                        hyperparameters = NULL,
                        partitions_col = ".partitions",
                        cutoff = 0.5,
                        positive = 2,
                        metrics = list(),
                        rm_nc = FALSE,
                        parallel = FALSE,
                        verbose = TRUE) {
  validate_list(
    train_data = train_data,
    test_data = test_data,
    formulas = formulas,
    model_fn = model_fn,
    predict_fn = predict_fn,
    preprocess_fn = preprocess_fn,
    preprocess_once = preprocess_once,
    hyperparameters = hyperparameters,
    partitions_col = partitions_col,
    family = type,
    cutoff = cutoff,
    positive = positive,
    metrics = metrics,
    info_cols = list("Model" = TRUE),
    rm_nc = rm_nc,
    parallel_ = parallel,
    verbose = verbose
  )
}
