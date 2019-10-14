#' @title Examples of preprocess_fn functions
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Examples of preprocess functions that can be used in
#'  \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}}.
#'  They can either be used directly or be starting points.
#'
#'  The examples will mainly use
#'  \code{\link[caret:preProcess]{caret::preProcess()}},
#'  but you can also use \code{\link[recipes:recipe]{recipes}} or
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
#'   "normalize" \tab Normalizes the numeric variables\cr
#'   "warn" \tab Identity function that throws a warning and a message\cr
#'   }
example_preprocess_functions <- function(name){

  if (name == "standardize"){
    preprocess_fn <- function(train_data, test_data, formula, hyperparameters){

      # Get centering and scaling parameters from the train_data
      preprocess_params <- caret::preProcess(train_data,
                                             method = c("scale", "center"))

      # Apply standardization to all numeric variables in
      # train_data and test_data
      train_data <- predict(preprocess_params, train_data)
      test_data <- predict(preprocess_params, test_data)

      # Extract parameters and add to tibble
      tidy_parameters <- tibble::tibble("Measure" = c("Mean", "SD")) %>%
        dplyr::bind_cols(
          dplyr::bind_rows(preprocess_params$mean,
                           preprocess_params$std)
          )

      list("train" = train_data,
           "test" = test_data,
           "parameters" = tidy_parameters)
    }
   } else if (name == "normalize"){
      preprocess_fn <- function(train_data, test_data, formula, hyperparameters){

        # Get normalization parameters from the train_data
        preprocess_params <- caret::preProcess(train_data,
                                               method = c("range"),
                                               rangeBounds = c(0,1))

        # Apply normalization to all numeric variables in
        # train_data and test_data
        train_data <- predict(preprocess_params, train_data)
        test_data <- predict(preprocess_params, test_data)

        # Extract parameters and add to tibble
        tidy_parameters <- tibble::tibble("Measure" = c("Min", "Max")) %>%
          dplyr::bind_cols(
            dplyr::as_tibble(preprocess_params$ranges)
          )

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
