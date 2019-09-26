#' @title Generate a multiclass probability tibble
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Generate a tibble with random numbers containing one column per specified class.
#'  When the softmax function is applied, the numbers become probabilities that sum to \code{1} rowwise.
#' @param num_classes The number of classes. Also the number of columns in the tibble.
#' @param num_observations The number of observations. Also the number of rows in the tibble.
#' @param apply_softmax Whether to apply the softmax function rowwise. This will transform the
#'  numbers to probabilities that sum to \code{1} rowwise.
#' @param FUN Function for generating random numbers.
#'  The first argument must be the number of random numbers to generate,
#'  as no other arguments are supplied.
#' @param class_name The prefix for the column names. The column index is appended.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @examples
#' # Attach cvms
#' library(cvms)
#'
#' # Create a tibble with 5 classes and 10 observations
#' # Apply softmax to make sure the probabilities sum to 1
#' multiclass_probability_tibble(num_classes = 5,
#'                               num_observations = 10,
#'                               apply_softmax = TRUE)
#'
#' # Using the rnorm function to generate the random numbers
#' multiclass_probability_tibble(num_classes = 5,
#'                               num_observations = 10,
#'                               apply_softmax = TRUE,
#'                               FUN = rnorm)
#'
#' # Creating a custom generator function that
#' # exponentiates the numbers to create more "certain" predictions
#' rcertain <- function(n){
#'     (runif(n, min = 1, max = 100)^1.4)/100
#' }
#' multiclass_probability_tibble(num_classes = 5,
#'                               num_observations = 10,
#'                               apply_softmax = TRUE,
#'                               FUN = rcertain)
multiclass_probability_tibble <- function(num_classes,
                                           num_observations,
                                           apply_softmax = TRUE,
                                           FUN = runif,
                                           class_name = "class_") {
  # Generate random numbers
  random_numbers <- tryCatch({
    FUN(num_classes * num_observations)
  }, error = function(e) {
    stop("Could not use 'FUN' to generate a sequence of random numbers.")
  })

  # Create the tibble
  probability_matrix <- matrix(random_numbers,
                               ncol = num_classes) %>%
    dplyr::as_tibble(.name_repair = ~ paste0(class_name, 1:num_classes))

  # Apply the softmax function if requested
  if (isTRUE(apply_softmax)){
    probability_matrix <- softmax(probability_matrix)
  }

  probability_matrix
}
