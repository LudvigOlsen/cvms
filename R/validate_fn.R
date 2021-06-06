
#' @title Validate a custom model function on a test set
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Fit your model function on a training set and validate it by
#'  predicting a test/validation set.
#'  Validate different hyperparameter combinations and formulas at once.
#'  Preprocess the train/test split.
#'  Returns results and fitted models in a \code{tibble} for easy reporting and further analysis.
#'
#'  Compared to \code{\link[cvms:validate]{validate()}},
#'  this function allows you supply a custom model function, a predict function,
#'  a preprocess function and the hyperparameter values to validate.
#'
#'  Supports regression and classification (binary and multiclass).
#'  See \code{`type`}.
#'
#'  Note that some metrics may not be computable for some types
#'  of model objects.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family validation functions
#' @inheritParams cross_validate_fn
#' @inheritParams validate
#' @inherit cross_validate_fn details
#' @return \code{tibble} with the results and model objects.
#'
#'  \subsection{Shared across families}{
#'
#'  A nested \code{tibble} with \strong{coefficients} of the models. The coefficients
#'  are extracted from the model object with \code{\link[parameters:model_parameters]{parameters::model_parameters()}} or
#'  \code{\link[stats:coef]{coef()}} (with some restrictions on the output).
#'  If these attempts fail, a default coefficients \code{tibble} filled with \code{NA}s is returned.
#'
#'  Nested \code{tibble} with the used \strong{preprocessing parameters},
#'  if a passed \code{`preprocess_fn`} returns the parameters in a \code{tibble}.
#'
#'  Count of \strong{convergence warnings}, using a limited set of keywords (e.g. "convergence"). If a
#'  convergence warning does not contain one of these keywords, it will be counted with \strong{other warnings}.
#'  Consider discarding models that did not converge on all iterations.
#'  Note: you might still see results, but these should be taken with a grain of salt!
#'
#'  Nested \code{tibble} with the \strong{warnings and messages} caught for each model.
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
#'  \strong{\code{RMSE}}, \strong{\code{MAE}}, \strong{\code{NRMSE(IQR)}},
#'  \strong{\code{RRSE}}, \strong{\code{RAE}}, and \strong{\code{RMSLE}}.
#'
#'  See the additional metrics (disabled by default) at \code{\link[cvms:gaussian_metrics]{?gaussian_metrics}}.
#'
#'  A nested \code{tibble} with the \strong{predictions} and targets.
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Binomial Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  Based on predictions of the test set,
#'  a confusion matrix and a \code{ROC} curve are created to get the following:
#'
#'  \code{ROC}:
#'
#'  \strong{\code{AUC}}, \strong{\code{Lower CI}}, and \strong{\code{Upper CI}}
#'
#'  \code{Confusion Matrix}:
#'
#'  \strong{\code{Balanced Accuracy}},
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
#'  See the additional metrics (disabled by default) at
#'  \code{\link[cvms:binomial_metrics]{?binomial_metrics}}.
#'
#'  Also includes:
#'
#'  A nested \code{tibble} with \strong{predictions}, predicted classes (depends on \code{cutoff}), and the targets.
#'  Note, that the predictions are \emph{not necessarily} of the \emph{specified} \code{positive} class, but of
#'  the \emph{model's} positive class (second level of dependent variable, alphabetically).
#'
#'  The \code{\link[pROC:roc]{pROC::roc}} \strong{\code{ROC}} curve object(s).
#'
#'  A nested \code{tibble} with the \strong{confusion matrix}/matrices.
#'  The \code{Pos_} columns tells you whether a row is a
#'  True Positive (\code{TP}), True Negative (\code{TN}),
#'  False Positive (\code{FP}), or False Negative (\code{FN}),
#'  depending on which level is the "positive" class. I.e. the level you wish to predict.
#'
#'  The name of the \strong{Positive Class}.
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Multinomial Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  For each class, a \emph{one-vs-all} binomial evaluation is performed. This creates
#'  a \strong{Class Level Results} \code{tibble} containing the same metrics as the binomial results
#'  described above (excluding \code{MCC}, \code{AUC}, \code{Lower CI} and \code{Upper CI}),
#'  along with a count of the class in the target column (\strong{\code{Support}}).
#'  These metrics are used to calculate the macro metrics. The nested class level results
#'  \code{tibble} is also included in the output \code{tibble},
#'  and could be reported along with the macro and overall metrics.
#'
#'  The output \code{tibble} contains the macro and overall metrics.
#'  The metrics that share their name with the metrics in the nested
#'  class level results \code{tibble} are averages of those metrics
#'  (note: does not remove \code{NA}s before averaging).
#'  In addition to these, it also includes the \strong{\code{Overall Accuracy}} and
#'  the multiclass \strong{\code{MCC}}.
#'
#'  Other available metrics (disabled by default, see \code{metrics}):
#'  \strong{\code{Accuracy}},
#'  \emph{multiclass} \strong{\code{AUC}},
#'  \strong{\code{Weighted Balanced Accuracy}},
#'  \strong{\code{Weighted Accuracy}},
#'  \strong{\code{Weighted F1}},
#'  \strong{\code{Weighted Sensitivity}},
#'  \strong{\code{Weighted Sensitivity}},
#'  \strong{\code{Weighted Specificity}},
#'  \strong{\code{Weighted Pos Pred Value}},
#'  \strong{\code{Weighted Neg Pred Value}},
#'  \strong{\code{Weighted Kappa}},
#'  \strong{\code{Weighted Detection Rate}},
#'  \strong{\code{Weighted Detection Prevalence}}, and
#'  \strong{\code{Weighted Prevalence}}.
#'
#'  Note that the "Weighted" average metrics are weighted by the \code{Support}.
#'
#'  Also includes:
#'
#'  A nested \code{tibble} with the \strong{predictions}, predicted classes, and targets.
#'
#'  A list of \strong{ROC} curve objects when \code{AUC} is enabled.
#'
#'  A nested \code{tibble} with the multiclass \strong{Confusion Matrix}.
#'
#'  \strong{Class Level Results}
#'
#'  Besides the binomial evaluation metrics and the \code{Support},
#'  the nested class level results \code{tibble} also contains a
#'  nested \code{tibble} with the \strong{Confusion Matrix} from the one-vs-all evaluation.
#'  The \code{Pos_} columns tells you whether a row is a
#'  True Positive (\code{TP}), True Negative (\code{TN}),
#'  False Positive (\code{FP}), or False Negative (\code{FN}),
#'  depending on which level is the "positive" class. In our case, \code{1} is the current class
#'  and \code{0} represents all the other classes together.
#'
#'  }
#' @examples
#' \donttest{
#' # Attach packages
#' library(cvms)
#' library(groupdata2) # fold()
#' library(dplyr) # %>% arrange() mutate()
#'
#' # Note: More examples of custom functions can be found at:
#' # model_fn: model_functions()
#' # predict_fn: predict_functions()
#' # preprocess_fn: preprocess_functions()
#'
#' # Data is part of cvms
#' data <- participant.scores
#'
#' # Set seed for reproducibility
#' set.seed(7)
#'
#' # Fold data
#' data <- partition(
#'   data,
#'   p = 0.8,
#'   cat_col = "diagnosis",
#'   id_col = "participant",
#'   list_out = FALSE
#' ) %>%
#'   mutate(diagnosis = as.factor(diagnosis)) %>%
#'   arrange(.partitions)
#'
#' # Formulas to validate
#'
#' formula_gaussian <- "score ~ diagnosis"
#' formula_binomial <- "diagnosis ~ score"
#'
#' #
#' # Gaussian
#' #
#'
#' # Create model function that returns a fitted model object
#' lm_model_fn <- function(train_data, formula, hyperparameters) {
#'   lm(formula = formula, data = train_data)
#' }
#'
#' # Create predict function that returns the predictions
#' lm_predict_fn <- function(test_data, model, formula,
#'                           hyperparameters, train_data) {
#'   stats::predict(
#'     object = model,
#'     newdata = test_data,
#'     type = "response",
#'     allow.new.levels = TRUE
#'   )
#' }
#'
#' # Validate the model function
#' v <- validate_fn(
#'   data,
#'   formulas = formula_gaussian,
#'   type = "gaussian",
#'   model_fn = lm_model_fn,
#'   predict_fn = lm_predict_fn,
#'   partitions_col = ".partitions"
#' )
#'
#' v
#'
#' # Extract model object
#' v$Model[[1]]
#'
#' #
#' # Binomial
#' #
#'
#' # Create model function that returns a fitted model object
#' glm_model_fn <- function(train_data, formula, hyperparameters) {
#'   glm(formula = formula, data = train_data, family = "binomial")
#' }
#'
#' # Create predict function that returns the predictions
#' glm_predict_fn <- function(test_data, model, formula,
#'                            hyperparameters, train_data) {
#'   stats::predict(
#'     object = model,
#'     newdata = test_data,
#'     type = "response",
#'     allow.new.levels = TRUE
#'   )
#' }
#'
#' # Validate the model function
#' validate_fn(
#'   data,
#'   formulas = formula_binomial,
#'   type = "binomial",
#'   model_fn = glm_model_fn,
#'   predict_fn = glm_predict_fn,
#'   partitions_col = ".partitions"
#' )
#'
#' #
#' # Support Vector Machine (svm)
#' # with known hyperparameters
#' #
#'
#' # Create model function that returns a fitted model object
#' # We use the hyperparameters arg to pass in the kernel and cost values
#' # These will usually have been found with cross_validate_fn()
#' svm_model_fn <- function(train_data, formula, hyperparameters) {
#'
#'   # Expected hyperparameters:
#'   #  - kernel
#'   #  - cost
#'   if (!"kernel" %in% names(hyperparameters))
#'     stop("'hyperparameters' must include 'kernel'")
#'   if (!"cost" %in% names(hyperparameters))
#'     stop("'hyperparameters' must include 'cost'")
#'
#'   e1071::svm(
#'     formula = formula,
#'     data = train_data,
#'     kernel = hyperparameters[["kernel"]],
#'     cost = hyperparameters[["cost"]],
#'     scale = FALSE,
#'     type = "C-classification",
#'     probability = TRUE
#'   )
#' }
#'
#' # Create predict function that returns the predictions
#' svm_predict_fn <- function(test_data, model, formula,
#'                            hyperparameters, train_data) {
#'   predictions <- stats::predict(
#'     object = model,
#'     newdata = test_data,
#'     allow.new.levels = TRUE,
#'     probability = TRUE
#'   )
#'
#'   # Extract probabilities
#'   probabilities <- dplyr::as_tibble(
#'     attr(predictions, "probabilities")
#'   )
#'
#'   # Return second column
#'   probabilities[[2]]
#' }
#'
#' # Specify hyperparameters to use
#' # We found these in the examples in ?cross_validate_fn()
#' svm_hparams <- list(
#'   "kernel" = "linear",
#'   "cost" = 10
#' )
#'
#' # Validate the model function
#' validate_fn(
#'   data,
#'   formulas = formula_binomial,
#'   type = "binomial",
#'   model_fn = svm_model_fn,
#'   predict_fn = svm_predict_fn,
#'   hyperparameters = svm_hparams,
#'   partitions_col = ".partitions"
#' )
#' }
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
