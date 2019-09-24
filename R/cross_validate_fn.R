
#
# Note that the model object metrics (r2, AIC, etc.) can only be computed for certain models
# Turn them off by metrics_list = list("default_model_metrics" = FALSE)



#' @title Cross-validate custom model function for model selection
#' @description Cross-validate your model function with
#'  one or multiple model formulas at once. Perform repeated cross-validation.
#'  Returns results in a tibble for easy comparison,
#'  reporting and further analysis.
#'
#'  Compared to \code{\link[cvms:cross_validate]{cross_validate()}},
#'  this function allows you supply a custom model function
#'  and (if needed) a custom prediction function.
#'
#'  Supports regression and classification (binary and multiclass) tasks.
#'
#'  Note that some metrics may not be computable for all types
#'  of model objects.
#' @inherit cross_validate return
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @author Benjamin Hugh Zachariae
#' @export
#' @inheritParams cross_validate
#' @inheritParams evaluate
#' @param model_fn Model function that returns a fitted model object.
#'  Will usually wrap an existing model function like \code{\link[e1071:svm]{e1071::svm}}
#'  or \code{\link[nnet:multinom]{nnet::multinom}}.
#'
#'  Must have the following function arguments:
#'
#'  \code{function(train_data, formula)}
#' @param predict_type The \code{type} argument for \code{\link[stats:predict]{predict()}}.
#'
#'  When the defaults fail, provide it such that the \code{\link[stats:predict]{predict()}}
#'  output is as follows:
#'
#'  \subsection{Binomial}
#'  Vector or one-column matrix / data frame with probabilies (0-1).
#'  E.g.:
#'
#'  \code{c(0.3, 0.5, 0.1, 0.5)}
#'
#'  \subsection{Gaussian}
#'  Vector or one-column matrix / data frame with the predicted value.
#'  E.g.:
#'
#'  \code{c(3.7, 0.9, 1.2, 7.3)}
#'
#'  \subsection{Multinomial}
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
#'
#'  N.B. \code{predict_fn} and \code{predict_type} are mutually exclusive. Specify only one of them.
#' @param predict_fn Function for predicting the targets in the test folds using the fitted model object.
#'  Will usually wrap \code{\link[stats:predict]{predict()}}, but doesn't have to.
#'  Must return predictions in the format described in \code{predict_type} above.
#'
#'  Must have the following function arguments:
#'
#'  \code{function(test_data, model, formula = NULL)}
#'
#'  N.B. \code{predict_fn} and \code{predict_type} are mutually exclusive. Specify only one of them.
#'
#' @details
#'
#'  Packages used:
#'
#'  \subsection{Results}{
#'  \subsection{Gaussian}{
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
#'  \subsection{Binomial}{
#'
#'  Confusion matrix: \code{\link[caret:confusionMatrix]{caret::confusionMatrix}}
#'
#'  ROC: \code{\link[pROC:roc]{pROC::roc}}
#'
#'  MCC: \code{\link[mltools:mcc]{mltools::mcc}}
#'  }
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
#' data <- fold(data, k = 4,
#'              cat_col = 'diagnosis',
#'              id_col = 'participant') %>%
#'     mutate(diagnosis = as.factor(diagnosis)) %>%
#'     arrange(.folds)
#'
#' # Cross-validate multiple formulas
#'
#' formulas <- c("score~diagnosis+(1|session)",
#'               "score~age+(1|session)")
#'
#' \donttest{
#' # Gaussian
#'
#' # Create model function with args 'train_data' and 'formula'
#' # that returns a fitted model object
#' lm_model_fn <- function(train_data, formula){
#'     lm(formula = formula, data = train_data)
#' }
#'
#' # Cross-validate the model function
#' cross_validate_fn(data,
#'                   model_fn = lm_model_fn,
#'                   formulas = formulas,
#'                   family = 'gaussian',
#'                   fold_cols = ".folds",
#'                   REML = FALSE)
#'
#' # Binomial
#'
#' # Create model function with args 'train_data' and 'formula'
#' # that returns a fitted model object
#' glm_model_fn <- function(train_data, formula){
#'     glm(formula = formula, data = train_data, family = "binomial")
#' }
#'
#' # Cross-validate the model function
#' cross_validate_fn(data,
#'                   model_fn = glm_model_fn,
#'                   formulas = formulas,
#'                   family = 'binomial',
#'                   fold_cols = ".folds")
#'
#' # Support Vector Machine (svm)
#'
#' # Create model function with args 'train_data' and 'formula'
#' # that returns a fitted model object
#' svm_model_fn <- function(train_data, formula){
#'     e1071::svm(formula = formula,
#'                data = train_data,
#'                kernel = "linear",
#'                type = "C-classification")
#' }
#'
#' # Cross-validate the model function
#' cross_validate_fn(data,
#'                   model_fn = svm_model_fn,
#'                   formulas = formulas,
#'                   family = 'binomial',
#'                   fold_cols = ".folds")
#'
#' # Naive Bayes
#'
#' # Create model function with args 'train_data' and 'formula'
#' # that returns a fitted model object
#' nb_model_fn <- function(train_data, formula){
#'     e1071::naiveBayes(formula = formula,
#'                       data = train_data)
#' }
#'
#' # Create predict function with args 'test_data', 'model', and 'formula'
#' # that returns predictions in right format (here, a one-column matrix)
#' # Note the type = "raw" and that we pick the probabilities for class 1 with [,2]
#' nb_predict_fn <- function(test_data, model, formula = NULL){
#'   stats::predict(object = model, newdata = test_data,
#'                  type = "raw", allow.new.levels = TRUE)[,2]
#' }
#'
#' # Cross-validate the model function
#' cross_validate_fn(data,
#'                   model_fn = svm_model_fn,
#'                   formulas = formulas,
#'                   family = 'binomial',
#'                   prediction_fn = nb_predict_fn,
#'                   fold_cols = ".folds")
#'
#' }
#' # Use parallelization
#'
#' \donttest{
#' # Attach doParallel and register four cores
#' # Uncomment:
#' # library(doParallel)
#' # registerDoParallel(4)
#'
#' # Create list of 20 model formulas
#' formulas <- rep(c("score~diagnosis",
#'                 "score~age"), 10)
#'
#' # Cross-validate a list of 20 model formulas in parallel
#' system.time({cross_validate_fn(data,
#'                                model_fn = lm_model_fn,
#'                                formulas = formulas,
#'                                family = 'gaussian',
#'                                fold_cols = ".folds",
#'                                parallel = TRUE)})
#'
#' # Cross-validate a list of 20 model formulas sequentially
#' system.time({cross_validate_fn(data,
#'                                model_fn = lm_model_fn,
#'                                formulas = formulas,
#'                                family = 'gaussian',
#'                                fold_cols = ".folds",
#'                                parallel = FALSE)})
#' }
cross_validate_fn <- function(data, model_fn, formulas,
                              fold_cols = '.folds',
                              type = 'gaussian',
                              cutoff = 0.5,
                              positive = 2,
                              predict_type = NULL,
                              predict_fn = NULL,
                              metrics = list(),
                              rm_nc = FALSE,
                              parallel = FALSE, model_verbose = FALSE){

  return(custom_cross_validate_list(data = data,
                                    formulas = formulas,
                                    model_fn = model_fn,
                                    fold_cols = fold_cols,
                                    family = type,
                                    cutoff = cutoff,
                                    positive = positive,
                                    predict_type = predict_type,
                                    predict_fn = predict_fn,
                                    metrics = metrics,
                                    rm_nc = rm_nc,
                                    model_verbose = model_verbose,
                                    parallel_ = parallel,
                                    parallelize = "models"))
}
