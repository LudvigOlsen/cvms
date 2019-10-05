#' @title Examples of model_fn functions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Examples of model functions that can be used in
#'  \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}}.
#'  They can either be used directly or be starting points.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family example functions
#' @param name Name of model to get model function for,
#'  as it appears in the following list:
#'
#'  \tabular{rrr}{
#'   \strong{Name} \tab \strong{Function} \tab \strong{Hyperparameters} \cr
#'   "lm" \tab \code{\link[stats:lm]{stats::lm()}} \tab \cr
#'   "lmer" \tab \code{\link[lme4:lmer]{lme4::lmer()}} \tab \code{REML} \cr
#'   "glm_binomial" \tab \code{\link[stats:lm]{stats::glm()}} \tab \cr
#'   "glmer_binomial" \tab \code{\link[lme4:glmer]{lme4::glmer()}} \tab \cr
#'   "svm_gaussian" \tab \code{\link[e1071:svm]{e1071::svm()}} \tab \code{kernel}, \code{cost}\cr
#'   "svm_binomial" \tab \code{\link[e1071:svm]{e1071::svm()}} \tab \code{kernel}, \code{cost}\cr
#'   "naive_bayes" \tab \code{\link[e1071:naiveBayes]{e1071::naiveBayes()}} \tab \code{laplace} \cr
#'   }
example_model_functions <- function(name){

  if (name == "lm"){
    model_fn <- function(train_data, formula, hyperparameters){
      lm(formula = formula, data = train_data)
    }
  } else if (name == "lmer"){
    model_fn <- function(train_data, formula, hyperparameters){

      # Expected hyperparameters:
      #  - REML

      lme4::lmer(formula = formula,
                 data = train_data,
                 family = "binomial",
                 REML = hyperparameters[["REML"]])
    }
  } else if (name == "glm_binomial"){
    model_fn <- function(train_data, formula, hyperparameters){
      glm(formula = formula, data = train_data,
          family = "binomial")
    }
  } else if (name == "glmer_binomial"){
    model_fn <- function(train_data, formula, hyperparameters){
      lme4::glmer(formula = formula, data = train_data,
                  family = "binomial")
    }
  } else if (name == "svm_gaussian"){
    model_fn <- function(train_data, formula, hyperparameters){

      # Expected hyperparameters:
      #  - kernel
      #  - cost

      e1071::svm(formula = formula,
                 data = train_data,
                 kernel = hyperparameters[["kernel"]],
                 cost = hyperparameters[["cost"]],
                 scale = FALSE,
                 type = "eps-regression")
    }
  } else if (name == "svm_binomial"){
    model_fn <- function(train_data, formula, hyperparameters){

      # Expected hyperparameters:
      #  - kernel
      #  - cost

      e1071::svm(formula = formula,
                 data = train_data,
                 kernel = hyperparameters[["kernel"]],
                 cost = hyperparameters[["cost"]],
                 scale = FALSE,
                 type = "C-classification")
    }
  } else if (name == "naive_bayes"){
    model_fn <- function(train_data, formula, hyperparameters){

      # Expected hyperparameters:
      #  - laplace

      e1071::naiveBayes(
        formula = formula,
        data = train_data,
        laplace = hyperparameters[["laplace"]])
    }
  }

  model_fn

}
