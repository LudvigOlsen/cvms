#' @title Examples of model_fn functions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Examples of model functions that can be used in
#'  \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}}.
#'  They can either be used directly or be starting points.
#'
#'  The \code{\link[cvms:update_hyperparameters]{update_hyperparameters()}} function
#'  updates the list of hyperparameters with default values for missing hyperparameters.
#'  You can also specify required hyperparameters.
#' @return A function with the following form:
#'
#'  \code{function(train_data, formula, hyperparameters) \{}
#'
#'  \verb{    }\code{# Return fitted model object}
#'
#'  \code{\}}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family example functions
#' @param name Name of model to get model function for,
#'  as it appears in the following list:
#'
#'  \tabular{rrr}{
#'   \strong{Name} \tab \strong{Function} \tab \strong{Hyperparameters (default)} \cr
#'   "lm" \tab \code{\link[stats:lm]{stats::lm()}} \tab \cr
#'   "lmer" \tab \code{\link[lme4:lmer]{lme4::lmer()}} \tab \code{REML (FALSE)} \cr
#'   "glm_binomial" \tab \code{\link[stats:lm]{stats::glm()}} \tab \cr
#'   "glmer_binomial" \tab \code{\link[lme4:glmer]{lme4::glmer()}} \tab \cr
#'   "svm_gaussian" \tab \code{\link[e1071:svm]{e1071::svm()}} \tab \code{kernel ("radial")}, \code{cost (1)}\cr
#'   "svm_binomial" \tab \code{\link[e1071:svm]{e1071::svm()}} \tab \code{kernel ("radial")}, \code{cost (1)}\cr
#'   "svm_multinomial" \tab \code{\link[e1071:svm]{e1071::svm()}} \tab \code{kernel ("radial")}, \code{cost (1)}\cr
#'   "naive_bayes" \tab \code{\link[e1071:naiveBayes]{e1071::naiveBayes()}} \tab \code{laplace (0)} \cr
#'   }
model_functions <- function(name) {
  if (name == "lm") {
    model_fn <- function(train_data, formula, hyperparameters) {
      lm(formula = formula, data = train_data)
    }
  } else if (name == "lmer") {
    model_fn <- function(train_data, formula, hyperparameters) {
      # Optional hyperparameters:
      #  - REML

      # Set defaults for any missing hyperparameters
      # These are the defaults in cross_validate()
      hyperparameters <- cvms::update_hyperparameters(
        REML = FALSE,
        hyperparameters = hyperparameters
      )

      lme4::lmer(
        formula = formula,
        data = train_data,
        REML = hyperparameters[["REML"]]
      )
    }
  } else if (name == "glm_binomial") {
    model_fn <- function(train_data, formula, hyperparameters) {
      glm(
        formula = formula, data = train_data,
        family = "binomial"
      )
    }
  } else if (name == "glmer_binomial") {
    model_fn <- function(train_data, formula, hyperparameters) {
      lme4::glmer(
        formula = formula, data = train_data,
        family = "binomial"
      )
    }
  } else if (name == "svm_gaussian") {
    if (requireNamespace("e1071", quietly = TRUE)) {
      model_fn <- function(train_data, formula, hyperparameters) {
        # Optional hyperparameters:
        #  - kernel (default: "radial")
        #  - cost   (default: 1)
        #  - scale  (default: FALSE)

        # Set defaults for any missing hyperparameters
        # Except for 'scale', these are the defaults in e1071::svm
        hyperparameters <- cvms::update_hyperparameters(
          kernel = "radial",
          cost = 1,
          scale = FALSE,
          hyperparameters = hyperparameters
        )

        e1071::svm(
          formula = formula,
          data = train_data,
          kernel = hyperparameters[["kernel"]],
          cost = hyperparameters[["cost"]],
          scale = hyperparameters[["scale"]],
          type = "eps-regression"
        )
      }
    } else {
      stop("The package `e1071` was not available.")
    }
  } else if (name %in% c("svm_binomial", "svm_multinomial")) {
    if (requireNamespace("e1071", quietly = TRUE)) {
      model_fn <- function(train_data, formula, hyperparameters) {
        # Optional hyperparameters:
        #  - kernel (default: "radial")
        #  - cost   (default: 1)
        #  - scale  (default: FALSE)

        # Set defaults for any missing hyperparameters
        # Except for 'scale', these are the defaults in e1071::svm
        hyperparameters <- cvms::update_hyperparameters(
          kernel = "radial",
          cost = 1,
          scale = FALSE,
          hyperparameters = hyperparameters
        )

        e1071::svm(
          formula = formula,
          data = train_data,
          kernel = hyperparameters[["kernel"]],
          cost = hyperparameters[["cost"]],
          scale = hyperparameters[["scale"]],
          type = "C-classification",
          probability = TRUE
        )
      }
    } else {
      stop("The package `e1071` was not available.")
    }
  } else if (name == "naive_bayes") {
    if (requireNamespace("e1071", quietly = TRUE)) {
      model_fn <- function(train_data, formula, hyperparameters) {
        # Optional hyperparameters:
        #  - laplace  (default: 0)

        # Set defaults for any missing hyperparameters
        # These are the defaults in e1071::naiveBayes
        hyperparameters <- cvms::update_hyperparameters(
          laplace = 0,
          hyperparameters = hyperparameters
        )

        e1071::naiveBayes(
          formula = formula,
          data = train_data,
          laplace = hyperparameters[["laplace"]]
        )
      }
    } else {
      stop("The package `e1071` was not available.")
    }
  } else {
    stop(paste0("Could not find '", name, "'."))
  }

  model_fn
}
