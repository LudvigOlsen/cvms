#' @title Examples of preprocess_fn functions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Examples of preprocess functions that can be used in
#'  \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}}.
#'  They can either be used directly or be starting points.
#'
#'  The examples will mainly use \code{\link[recipes:recipe]{recipes}},
#'  but you can also use caret::preProcess() or
#'  similar functions.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family example functions
#' @param name Name of preprocessing function
#'  as it appears in the following list:
#'
#'  \tabular{rrr}{
#'   \strong{Name} \tab \strong{Description} \cr
#'   "standardize" \tab Centers and scales the numeric variables\cr
#'   "range" \tab Normalizes the numeric variables to 0-1 range\cr
#'   "warn" \tab Identity function that throws a warning and a message\cr
#'   }
example_preprocess_functions <- function(name){

  if (name == "standardize"){

    preprocess_fn <- function(train_data, test_data, formula, hyperparameters){

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
        recipes::step_center(recipes::all_numeric())  %>%
        recipes::step_scale(recipes::all_numeric()) %>%

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

   } else if (name == "range"){

      preprocess_fn <- function(train_data, test_data, formula, hyperparameters){

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
          recipes::step_range(recipes::all_numeric(),
                              min = 0, max = 1)  %>%

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
