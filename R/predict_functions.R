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
#' @return A function with the following form:
#'
#'  \code{function(test_data, model, formula, hyperparameters, train_data) \{}
#'
#'  \verb{    }\code{# Use model to predict test_data}
#'
#'  \verb{    }\code{# Return predictions}
#'
#'  \code{\}}
#' @param name Name of model to get predict function for,
#'  as it appears in the following table.
#'
#'  The \strong{Model HParams} column lists hyperparameters used
#'  in the respective model function.
#'
#'  \tabular{rrr}{
#'   \strong{Name} \tab \strong{Function} \tab \strong{Model HParams} \cr
#'   "lm" \tab \code{\link[stats:lm]{stats::lm()}} \tab \cr
#'   "lmer" \tab \code{\link[lme4:lmer]{lme4::lmer()}} \tab \cr
#'   "glm_binomial" \tab \code{\link[stats:lm]{stats::glm()}} \tab \code{family = "binomial"}\cr
#'   "glmer_binomial" \tab \code{\link[lme4:glmer]{lme4::glmer()}} \tab \code{family = "binomial"}\cr
#'   "svm_gaussian" \tab \code{\link[e1071:svm]{e1071::svm()}} \tab \code{type = "eps-regression"}\cr
#'   "svm_binomial" \tab \code{\link[e1071:svm]{e1071::svm()}} \tab \code{type = "C-classification"}, \code{probability = TRUE}\cr
#'   "svm_multinomial" \tab \code{\link[e1071:svm]{e1071::svm()}} \tab \code{type = "C-classification"}, \code{probability = TRUE}\cr
#'   "naive_bayes" \tab \code{\link[e1071:naiveBayes]{e1071::naiveBayes()}} \tab \cr
#'   "nnet_multinom" \tab \code{\link[nnet:multinom]{nnet::multinom()}} \tab \cr
#'   "nnet_gaussian" \tab \code{\link[nnet:multinom]{nnet::nnet()}} \tab \code{linout = TRUE} \cr
#'   "nnet_binomial" \tab \code{\link[nnet:multinom]{nnet::nnet()}} \tab \cr
#'   "randomForest_gaussian" \tab \code{\link[randomForest:randomForest]{randomForest::randomForest()}} \tab \cr
#'   "randomForest_binomial" \tab \code{\link[randomForest:randomForest]{randomForest::randomForest()}} \tab \cr
#'   "randomForest_multinomial" \tab \code{\link[randomForest:randomForest]{randomForest::randomForest()}} \tab \cr
#'  }
predict_functions <- function(name) {
  if (name %in% c(
    "lm",
    "lmer",
    "glm_binomial",
    "glmer_binomial",
    "randomForest_gaussian"
  )) {
    predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(
        object = model,
        newdata = test_data,
        type = "response",
        allow.new.levels = TRUE
      )
    }
  } else if (name %in% c("svm_gaussian")) {
    predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(
        object = model,
        newdata = test_data,
        allow.new.levels = TRUE
      )
    }
  } else if (name == "svm_binomial") {
    predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
      predictions <- stats::predict(
        object = model,
        newdata = test_data,
        allow.new.levels = TRUE,
        probability = TRUE
      )

      # Extract probabilities
      probabilities <- dplyr::as_tibble(
        attr(predictions, "probabilities")
      )

      # Return second column
      probabilities[[2]]
    }
  } else if (name == "svm_multinomial") {
    predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
      predictions <- stats::predict(
        object = model,
        newdata = test_data,
        allow.new.levels = TRUE,
        probability = TRUE
      )

      # Extract probabilities
      probabilities <- dplyr::as_tibble(
        attr(predictions, "probabilities")
      )

      # Return probabilities
      probabilities
    }
  } else if (name == "naive_bayes") {
    predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(
        object = model,
        newdata = test_data,
        type = "raw",
        allow.new.levels = TRUE
      )[, 2]
    }
  } else if (name == "nnet_multinom") {
    predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(
        object = model,
        newdata = test_data,
        type = "probs",
        allow.new.levels = TRUE
      )
    }
  } else if (name %in% c("nnet_gaussian", "nnet_binomial")) {
    predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(
        object = model,
        newdata = test_data,
        type = "raw",
        allow.new.levels = TRUE
      )
    }
  } else if (name %in% c("randomForest_binomial")) {
    predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(
        object = model,
        newdata = test_data,
        type = "prob"
      )[, 2]
    }
  } else if (name %in% c("randomForest_multinomial")) {
    predict_fn <- function(test_data, model, formula, hyperparameters, train_data) {
      stats::predict(
        object = model,
        newdata = test_data,
        type = "prob"
      )
    }
  } else {
    stop(paste0("Could not find '", name, "'."))
  }

  predict_fn
}
