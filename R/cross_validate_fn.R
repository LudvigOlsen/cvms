
#' @title Cross-validate custom model functions for model selection
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Cross-validate your model function with one or multiple model formulas at once.
#'  Perform repeated cross-validation. Preprocess the train/test split
#'  within the cross-validation. Perform hyperparameter tuning with (sampled) grid search.
#'  Returns results in a tibble for easy comparison,
#'  reporting and further analysis.
#'
#'  Compared to \code{\link[cvms:cross_validate]{cross_validate()}},
#'  this function allows you supply a custom model function, a predict function,
#'  a preprocess function and the hyperparameter values to cross-validate.
#'
#'  Supports regression and classification (binary and multiclass).
#'  See \code{type}.
#'
#'  Note that some metrics may not be computable for some types
#'  of model objects.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family validation functions
#' @inheritParams cross_validate
#' @inheritParams evaluate
#' @param formulas Model formulas as strings. (Character)
#'
#'  Will be converted to \code{\link[stats:formula]{formula}} objects
#'  before being passed to \code{model_fn}.
#'
#'  E.g. \code{c("y~x", "y~z")}.
#'
#'  Can contain random effects.
#'
#'  E.g. \code{c("y~x+(1|r)", "y~z+(1|r)")}.
#' @param model_fn Model function that returns a fitted model object.
#'  Will usually wrap an existing model function like \code{\link[e1071:svm]{e1071::svm}}
#'  or \code{\link[nnet:multinom]{nnet::multinom}}.
#'
#'  Must have the following function arguments:
#'
#'  \code{function(train_data, formula,}
#'
#'  \verb{         }\code{hyperparameters)}
#' @param predict_fn Function for predicting the targets in the test folds/sets using the fitted model object.
#'  Will usually wrap \code{\link[stats:predict]{stats::predict()}}, but doesn't have to.
#'
#'  Must have the following function arguments:
#'
#'  \code{function(test_data, model,}
#'
#'  \verb{         }\code{formula, hyperparameters)}
#'
#'  Must return predictions in the following formats, depending on \code{type}:
#'
#'  \subsection{Binomial}{
#'  Vector or one-column matrix / data frame with probabilities (0-1)
#'  of the second class, alphabetically.
#'  E.g.:
#'
#'  \code{c(0.3, 0.5, 0.1, 0.5)}
#'  }
#'
#'  \subsection{Gaussian}{
#'  Vector or one-column matrix / data frame with the predicted value.
#'  E.g.:
#'
#'  \code{c(3.7, 0.9, 1.2, 7.3)}
#'  }
#'
#'  \subsection{Multinomial}{
#'  Data frame with one column per class containing probabilities of the class.
#'  Column names should be identical to how the class names are written in the target column.
#'  E.g.:
#'
#'  \tabular{rrr}{
#'   \strong{class_1} \tab \strong{class_2} \tab
#'   \strong{class_3} \cr
#'   0.269 \tab 0.528 \tab 0.203\cr
#'   0.368 \tab 0.322 \tab 0.310\cr
#'   0.375 \tab 0.371 \tab 0.254\cr
#'   ... \tab ... \tab ...}
#'  }
#' @param preprocess_fn Function for preprocessing the training and test sets.
#'
#'  Can, for instance, be used to standardize both the training and test sets
#'  with the scaling and centering parameters from the training set.
#'
#'  Must have the following function arguments:
#'
#'  \code{function(train_data, test_data,}
#'
#'  \verb{         }\code{formula, hyperparameters)}
#'
#'  Must return a list with the preprocessed \code{train_data} and \code{test_data}. It may also contain
#'  a tibble with the \code{parameters} used in preprocessing:
#'
#'  \code{list("train" = train_data,}
#'
#'  \verb{     }\code{"test" = test_data,}
#'
#'  \verb{     }\code{"parameters" = preprocess_parameters)}
#'
#'  Additional elements in the returned list will be ignored.
#'
#'  The optional parameters tibble will be included in the output.
#'  It could have the following format:
#'
#'  \tabular{rrr}{
#'   \strong{Measure} \tab \strong{var_1} \tab \strong{var_2} \cr
#'   Mean \tab 37.921 \tab 88.231\cr
#'   SD \tab 12.4 \tab 5.986\cr
#'   ... \tab ... \tab ...}
#'
#'  N.B. When \code{preprocess_once} is FALSE, the current formula and
#'  hyperparameters will be provided. Otherwise,
#'  these arguments will be \code{NULL}.
#' @param preprocess_once Whether to apply the preprocessing once
#'  (ignoring the formula and hyperparameters arguments in \code{preprocess_fn})
#'  or for every model separately. (Logical)
#'
#'  When preprocessing does not depend on the current formula or hyperparameters,
#'  we can do the preprocessing of each train/test split once, to save time.
#'  This \strong{may require holding a lot more data in memory} though,
#'  why it is not the default setting.
#' @param hyperparameters Either a named list with hyperparameter values to combine in a grid
#'  or a data frame with one row per hyperparameter combination.
#'
#'  \subsection{Named list for grid search}{
#'  Add \code{".n"} to sample the combinations. Can be the number of combinations to use,
#'  or a percentage between \code{0} and \code{1}.
#'
#'  E.g.
#'
#'  \code{list(".n" = 10,  # sample 10 combinations}
#'
#'  \verb{     }\code{"lrn_rate" = c(0.1, 0.01, 0.001),}
#'
#'  \verb{     }\code{"h_layers" = c(10, 100, 1000),}
#'
#'  \verb{     }\code{"drop_out" = runif(5, 0.3, 0.7))}
#'  }
#'
#'  \subsection{Data frame with specific hyperparameter combinations}{
#'  One row per combination to test.
#'
#'  E.g.
#'
#'  \tabular{rrr}{
#'   \strong{lrn_rate} \tab \strong{h_layers} \tab \strong{drop_out} \cr
#'   0.1 \tab 10 \tab 0.65\cr
#'   0.1 \tab 1000 \tab 0.65\cr
#'   0.01 \tab 1000 \tab 0.63\cr
#'   ... \tab ... \tab ...}
#'  }
#' @param metrics List for enabling/disabling metrics.
#'
#'   E.g. \code{list("RMSE" = FALSE)} would remove RMSE from the regression results,
#'   and \code{list("Accuracy" = TRUE)} would add the regular accuracy metric
#'   to the classification results.
#'   Default values (TRUE/FALSE) will be used for the remaining available metrics.
#'
#'   You can enable/disable all metrics at once by including
#'   \code{"all" = TRUE/FALSE} in the list. This is done prior to enabling/disabling
#'   individual metrics, why f.i. \code{list("all" = FALSE, "RMSE" = TRUE)} would return only the RMSE metric.
#'
#'   The list can be created with
#'   \code{\link[cvms:gaussian_metrics]{gaussian_metrics()}},
#'   \code{\link[cvms:binomial_metrics]{binomial_metrics()}}, or
#'   \code{\link[cvms:multinomial_metrics]{multinomial_metrics()}}.
#'
#'   Also accepts the string \code{"all"}.
#' @param verbose Whether to message process information
#'  like the number of model instances to fit. (Logical)
#' @details
#'
#'  Packages used:
#'
#'  \subsection{Results}{
#'  \subsection{Shared}{
#'
#'  AIC : \code{\link[stats:AIC]{stats::AIC}}
#'
#'  AICc : \code{\link[MuMIn:AICc]{MuMIn::AICc}}
#'
#'  BIC : \code{\link[stats:BIC]{stats::BIC}}
#'
#'  }
#'  \subsection{Gaussian}{
#'
#'  r2m : \code{\link[MuMIn:r.squaredGLMM]{MuMIn::r.squaredGLMM}}
#'
#'  r2c : \code{\link[MuMIn:r.squaredGLMM]{MuMIn::r.squaredGLMM}}
#'
#'  }
#'  \subsection{Binomial and Multinomial}{
#'
#'  ROC and related metrics:
#'
#'  Binomial: \code{\link[pROC:roc]{pROC::roc}}
#'
#'  Multinomial: \code{\link[pROC:multiclass.roc]{pROC::multiclass.roc}}
#'  }
#'  }
#' @return
#'  Tbl (tibble) with results for each model.
#'
#'  N.B. The \strong{Fold} column in the nested tibbles contains the test fold in that train/test split.
#'
#'  \subsection{Shared across families}{
#'  A nested tibble with \strong{coefficients} of the models from all iterations. The coefficients
#'  are extracted from the model object with \code{\link[broom:tidy]{broom::tidy()}} or
#'  \code{\link[stats:coef]{coef()}} (with some restrictions on the output).
#'  If these attempts fail, a default coefficients tibble filled with \code{NA}s is returned.
#'
#'  Nested tibble with the used \strong{preprocessing parameters},
#'  if a passed \code{preprocess_fn} returns the parameters in a tibble.
#'
#'  Number of \emph{total} \strong{folds}.
#'
#'  Number of \strong{fold columns}.
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
#'  Average \strong{RMSE}, \strong{MAE}, \strong{NRMSE}, \strong{RMSEIQR},
#'  \strong{r2m}, \strong{r2c}, \strong{AIC}, \strong{AICc},
#'  and \strong{BIC} of all the iterations*,
#'  \emph{\strong{omitting potential NAs} from non-converged iterations}. Some metrics will
#'  return \code{NA} if they can't be extracted from the fitted model objects.
#'
#'  N.B. The Information Criteria metrics (AIC, AICc, and BIC) are also averages.
#'
#'  A nested tibble with the \strong{predictions} and targets.
#'
#'  A nested tibble with the non-averaged \strong{results} from all iterations.
#'
#'  * In \emph{repeated cross-validation},
#'  the metrics are first averaged for each fold column (repetition) and then averaged again.
#'
#'  }
#'
#'  ----------------------------------------------------------------
#'
#'  \subsection{Binomial Results}{
#'
#'  ----------------------------------------------------------------
#'
#'  Based on the \strong{collected} predictions from the test folds*,
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
#'  A list of \strong{ROC} curve objects.
#'
#'  A nested tibble with the \strong{confusion matrix}/matrices.
#'  The \code{Pos_} columns tells you whether a row is a
#'  True Positive (TP), True Negative (TN), False Positive (FP), or False Negative (FN),
#'  depending on which level is the "positive" class. I.e. the level you wish to predict.
#'
#'  A nested tibble with the \strong{results} from all fold columns.
#'
#'  * In \emph{repeated cross-validation}, an evaluation is made per fold column (repetition) and averaged.
#'
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
#'  A list of \strong{ROC} curve objects.
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
#'
#' # Cross-validate multiple formulas
#'
#' formulas_gaussian <- c(
#'   "score ~ diagnosis",
#'   "score ~ age"
#' )
#' formulas_binomial <- c(
#'   "diagnosis ~ score",
#'   "diagnosis ~ age"
#' )
#' \donttest{
#' # Gaussian
#'
#' # Create model function with args 'train_data' and 'formula'
#' # that returns a fitted model object
#' lm_model_fn <- function(train_data, formula) {
#'   lm(formula = formula, data = train_data)
#' }
#'
#' # Cross-validate the model function
#' cross_validate_fn(data,
#'   model_fn = lm_model_fn,
#'   formulas = formulas_gaussian,
#'   type = "gaussian",
#'   fold_cols = ".folds"
#' )
#'
#' # Binomial
#'
#' # Create model function with args 'train_data' and 'formula'
#' # that returns a fitted model object
#' glm_model_fn <- function(train_data, formula) {
#'   glm(formula = formula, data = train_data, family = "binomial")
#' }
#'
#' # Cross-validate the model function
#' cross_validate_fn(data,
#'   model_fn = glm_model_fn,
#'   formulas = formulas_binomial,
#'   type = "binomial",
#'   fold_cols = ".folds"
#' )
#'
#' # Support Vector Machine (svm)
#'
#' # Create model function with args 'train_data' and 'formula'
#' # that returns a fitted model object
#' svm_model_fn <- function(train_data, formula) {
#'   e1071::svm(
#'     formula = formula,
#'     data = train_data,
#'     kernel = "linear",
#'     type = "C-classification"
#'   )
#' }
#'
#' # Cross-validate the model function
#' cross_validate_fn(data,
#'   model_fn = svm_model_fn,
#'   formulas = formulas_binomial,
#'   type = "binomial",
#'   fold_cols = ".folds"
#' )
#'
#' # Naive Bayes
#'
#' # Create model function with args 'train_data' and 'formula'
#' # that returns a fitted model object
#' nb_model_fn <- function(train_data, formula) {
#'   e1071::naiveBayes(
#'     formula = formula,
#'     data = train_data
#'   )
#' }
#'
#' # Create predict function with args 'test_data', 'model', and 'formula'
#' # that returns predictions in right format (here, a one-column matrix)
#' # Note the type = "raw" and that we pick the probabilities for class 1 with [,2]
#' nb_predict_fn <- function(test_data, model, formula = NULL) {
#'   stats::predict(
#'     object = model, newdata = test_data,
#'     type = "raw", allow.new.levels = TRUE
#'   )[, 2]
#' }
#'
#' # Cross-validate the model function
#' cross_validate_fn(data,
#'   model_fn = nb_model_fn,
#'   formulas = formulas_binomial,
#'   type = "binomial",
#'   predict_fn = nb_predict_fn,
#'   fold_cols = ".folds"
#' )
#' }
#' # Use parallelization
#' \donttest{
#' # Attach doParallel and register four cores
#' # Uncomment:
#' # library(doParallel)
#' # registerDoParallel(4)
#'
#' # Create list of 20 model formulas
#' formulas <- rep(c(
#'   "score~diagnosis",
#'   "score~age"
#' ), 10)
#'
#' # Cross-validate a list of 20 model formulas in parallel
#' system.time({
#'   cross_validate_fn(data,
#'     model_fn = lm_model_fn,
#'     formulas = formulas,
#'     type = "gaussian",
#'     fold_cols = ".folds",
#'     parallel = TRUE
#'   )
#' })
#'
#' # Cross-validate a list of 20 model formulas sequentially
#' system.time({
#'   cross_validate_fn(data,
#'     model_fn = lm_model_fn,
#'     formulas = formulas,
#'     type = "gaussian",
#'     fold_cols = ".folds",
#'     parallel = FALSE
#'   )
#' })
#' }
cross_validate_fn <- function(data,
                              formulas,
                              model_fn,
                              predict_fn,
                              preprocess_fn = NULL,
                              preprocess_once = FALSE,
                              hyperparameters = NULL,
                              fold_cols = ".folds",
                              type = "gaussian",
                              cutoff = 0.5,
                              positive = 2,
                              metrics = list(),
                              rm_nc = FALSE,
                              parallel = FALSE,
                              verbose = TRUE) {
  cross_validate_list(
    data = data,
    formulas = formulas,
    model_fn = model_fn,
    predict_fn = predict_fn,
    preprocess_fn = preprocess_fn,
    preprocess_once = preprocess_once,
    hyperparameters = hyperparameters,
    fold_cols = fold_cols,
    family = type,
    cutoff = cutoff,
    positive = positive,
    metrics = metrics,
    rm_nc = rm_nc,
    parallel_ = parallel,
    verbose = verbose
  )
}
