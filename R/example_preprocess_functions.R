#' @title Examples of preprocess_fn functions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Examples of preprocess functions that can be used in
#'  \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}} and
#'  \code{\link[cvms:validate_fn]{validate_fn()}}.
#'  They can either be used directly or be starting points.
#'
#'  The examples use \code{\link[recipes:recipe]{recipes}},
#'  but you can also use caret::preProcess() or
#'  similar functions.
#'
#'  In these examples, the preprocessing will only affect the numeric predictors.
#'
#'  You may prefer to hardcode a formula like \code{"y ~ ."} (where
#'  \code{y} is your dependent variable) as that will allow you to set
#'  \strong{preprocess_one} to \code{TRUE} in \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}}
#'  and \code{\link[cvms:validate_fn]{validate_fn()}} and save time.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family example functions
#' @param name Name of preprocessing function
#'  as it appears in the following list:
#'
#'  \tabular{rrr}{
#'   \strong{Name} \tab \strong{Description} \cr
#'   "standardize" \tab Centers and scales the numeric predictors\cr
#'   "range" \tab Normalizes the numeric predictors to the 0-1 range\cr
#'   "scale" \tab Scales the numeric predictors to have a standard deviation of one\cr
#'   "center" \tab Centers the numeric predictors to have a mean of zero\cr
#'   "warn" \tab Identity function that throws a warning and a message\cr
#'   }
example_preprocess_functions <- function(name){

  if (name == "standardize"){

    preprocess_fn <- function(train_data, test_data, formula, hyperparameters){

      # Create simplified version of the formula
      # as recipe does not like inline functions
      # like log() or random effect structures like (1|z)
      # Example:
      # "y ~ log(x) + (1 | z)"  becomes  "y ~ x + z"
      formula <- simplify_formula(formula, train_data)

      # Create recipes object
      recipe_object <- recipes::recipe(

        # Note: If we hardcoded the formula instead of using the formula argument
        # we could preprocess the train/test splits once
        # instead of for every formula
        # Tip: Use `y ~ .` to include all predictors (where `y` is your dependent variable)
        formula = formula,
        data = train_data) %>%

        # Add preprocessing steps
        # Note: We could add specific variable to each step
        # instead of just selecting all numeric predictors
        recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes())  %>%
        recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%

        # Find parameters from the training set
        recipes::prep(training = train_data)

      # Apply preprocessing to the partitions
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)

      # Extract the preprocessing parameters
      means <- recipe_object$steps[[1]]$means
      sds <- recipe_object$steps[[2]]$sds

      # Add preprocessing parameters to a tibble
      tidy_parameters <- tibble::tibble("Measure" = c("Mean" , "SD")) %>%
        dplyr::bind_cols(dplyr::bind_rows(means, sds))

      list("train" = train_data,
           "test" = test_data,
           "parameters" = tidy_parameters)

    }

  } else if (name == "scale"){

    preprocess_fn <- function(train_data, test_data, formula, hyperparameters){

      # Create simplified version of the formula
      # as recipe does not like inline functions
      # like log() or random effect structures like (1|z)
      # Example:
      # "y ~ log(x) + (1 | z)"  becomes  "y ~ x + z"
      formula <- simplify_formula(formula, train_data)

      # Create recipes object
      recipe_object <- recipes::recipe(

        # Note: If we hardcoded the formula instead of using the formula argument
        # we could preprocess the train/test splits once
        # instead of for every formula
        # Tip: Use `y ~ .` to include all predictors (where `y` is your dependent variable)
        formula = formula,
        data = train_data) %>%

        # Add preprocessing steps
        # Note: We could add specific variable to each step
        # instead of just selecting all numeric predictors
        recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes()) %>%

        # Find parameters from the training set
        recipes::prep(training = train_data)

      # Apply preprocessing to the partitions
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)

      # Extract the preprocessing parameters
      sds <- recipe_object$steps[[1]]$sds

      # Add preprocessing parameters to a tibble
      tidy_parameters <- tibble::tibble("Measure" = c("SD")) %>%
        dplyr::bind_cols(dplyr::bind_rows(sds))

      list("train" = train_data,
           "test" = test_data,
           "parameters" = tidy_parameters)
    }

  } else if (name == "center"){

    preprocess_fn <- function(train_data, test_data, formula, hyperparameters){

      # Create simplified version of the formula
      # as recipe does not like inline functions
      # like log() or random effect structures like (1|z)
      # Example:
      # "y ~ log(x) + (1 | z)"  becomes  "y ~ x + z"
      formula <- simplify_formula(formula, train_data)

      # Create recipes object
      recipe_object <- recipes::recipe(

        # Note: If we hardcoded the formula instead of using the formula argument
        # we could preprocess the train/test splits once
        # instead of for every formula
        # Tip: Use `y ~ .` to include all predictors (where `y` is your dependent variable)
        formula = formula,
        data = train_data) %>%

        # Add preprocessing steps
        # Note: We could add specific variable to each step
        # instead of just selecting all numeric predictors
        recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) %>%

        # Find parameters from the training set
        recipes::prep(training = train_data)

      # Apply preprocessing to the partitions
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)

      # Extract the preprocessing parameters
      means <- recipe_object$steps[[1]]$means

      # Add preprocessing parameters to a tibble
      tidy_parameters <- tibble::tibble("Measure" = c("Mean")) %>%
        dplyr::bind_cols(dplyr::bind_rows(means))

      list("train" = train_data,
           "test" = test_data,
           "parameters" = tidy_parameters)
    }

  } else if (name == "range"){

    preprocess_fn <- function(train_data, test_data, formula, hyperparameters){

      # Create simplified version of the formula
      # as recipe does not like inline functions
      # like log() or random effect structures like (1|z)
      # Example:
      # "y ~ log(x) + (1 | z)"  becomes  "y ~ x + z"
      formula <- simplify_formula(formula, train_data)

      # Create recipes object
      recipe_object <- recipes::recipe(

        # Note: If we hardcoded the formula instead of using the formula argument
        # we could preprocess the train/test splits once
        # instead of for every formula
        # Tip: Use `y ~ .` to include all predictors (where `y` is your dependent variable)
        formula = formula,
        data = train_data) %>%

        # Add preprocessing steps
        # Note: We could add specific variable to each step
        # instead of just selecting all numeric variables
        recipes::step_range(recipes::all_numeric(), -recipes::all_outcomes(),
                            min = 0.0, max = 1.0)  %>%

        # Find parameters from the training set
        recipes::prep(training = train_data)

      # Apply preprocessing to the partitions
      train_data <- recipes::bake(recipe_object, train_data)
      test_data <- recipes::bake(recipe_object, test_data)

      # Extract the preprocessing parameters
      ranges <- dplyr::as_tibble(recipe_object$steps[[1]]$ranges)

      # Add preprocessing parameters to a tibble
      tidy_parameters <- tibble::tibble("Measure" = c("Min" , "Max")) %>%
        dplyr::bind_cols(ranges)

      list("train" = train_data,
           "test" = test_data,
           "parameters" = tidy_parameters)
    }

  } else if (name == "warn"){

    preprocess_fn <- function(train_data, test_data, formula, hyperparameters){

      # For testing that warnings and messages are caught

      # Throw a warning and a message
      warning("This is a preprocess_fn warning")
      message("This is a preprocess_fn message")

      list("train" = train_data,
           "test" = test_data)
    }

  } else {

    stop(paste0("Could not find '", name, "'."))

  }

  preprocess_fn

}
