
#' @title Examples of predict_fn functions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Examples of predict functions that can be used in
#'  \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}}.
#'  They can either be used directly or be starting points.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family example functions
#' @param name Name of model to get predict function for,
#'  as it appears in the following table.
#'
#'  The \strong{Model Params} column lists parameters used
#'  in the respective model function.
#'
#'  \tabular{rrr}{
#'   \strong{Name} \tab \strong{Function} \tab \strong{Model Params} \cr
#'   "lm" \tab \code{\link[stats:lm]{stats::lm()}} \tab \cr
#'   "lmer" \tab \code{\link[lme4:lmer]{lme4::lmer()}} \tab \cr
#'   "glm_binomial" \tab \code{\link[stats:lm]{stats::glm()}} \tab \code{family = "binomial"}\cr
#'   "glmer_binomial" \tab \code{\link[lme4:glmer]{lme4::glmer()}} \tab \code{family = "binomial"}\cr
#'   "svm_gaussian" \tab \code{\link[e1071:svm]{e1071::svm()}} \tab \code{type = "eps-regression"}\cr
#'   "svm_binomial" \tab \code{\link[e1071:svm]{e1071::svm()}} \tab \code{type = "C-classification"}\cr
#'   "naive_bayes" \tab \code{\link[e1071:naiveBayes]{e1071::naiveBayes()}} \tab \cr
#'   "multinom" \tab \code{\link[nnet:multinom]{nnet::multinom()}} \tab \cr
#'  }
example_predict_functions <- function(name){

  if (name == "lm"){
    predict_fn <- function(test_data, model, formula){
      stats::predict(object = model,
                     newdata = test_data,
                     allow.new.levels = TRUE)
    }
  } else if (name == "lmer"){
    predict_fn <- function(test_data, model, formula){
      stats::predict(object = model,
                     newdata = test_data,
                     allow.new.levels = TRUE)
    }
  } else if (name == "glm_binomial"){
    predict_fn <- function(test_data, model, formula){
      stats::predict(object = model,
                     newdata = test_data,
                     type = "response",
                     allow.new.levels = TRUE)
    }
  } else if (name == "glmer_binomial"){
    predict_fn <- function(test_data, model, formula){
      stats::predict(object = model,
                     newdata = test_data,
                     type = "response",
                     allow.new.levels = TRUE)
    }
  } else if (name == "svm_gaussian"){
    predict_fn <- function(test_data, model, formula){
      stats::predict(object = model,
                     newdata = test_data,
                     allow.new.levels = TRUE)
    }
  } else if (name == "svm_binomial"){
    predict_fn <- function(test_data, model, formula){

      # Returns the predicted classes, not probabilities

      stats::predict(object = model,
                     newdata = test_data,
                     allow.new.levels = TRUE)
    }
  } else if (name == "naive_bayes"){
    predict_fn <- function(test_data, model, formula){
      stats::predict(object = model,
                     newdata = test_data,
                     type = "raw",
                     allow.new.levels = TRUE)[,2]
    }
  } else if (name == "multinom"){
    predict_fn <- function(test_data, model, formula){
      stats::predict(object = model,
                     newdata = test_data,
                     type = "probs",
                     allow.new.levels = TRUE)
    }
  } else {
    stop(paste0("Could not find '", name, "'."))
  }

  predict_fn
}
