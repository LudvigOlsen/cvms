

#   __________________ #< 714f7a4cd397a84c2a8f5b22229c7ade ># __________________
#   Multiclass probability tibble                                           ####


#' @title Generate a multiclass probability tibble
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  Generate a tibble with random numbers containing one column per specified class.
#'  When the softmax function is applied, the numbers become probabilities that sum to \code{1} row-wise.
#'  Optionally, add columns with targets and predicted classes.
#' @param num_classes The number of classes. Also the number of columns in the tibble.
#' @param num_observations The number of observations. Also the number of rows in the tibble.
#' @param apply_softmax Whether to apply the softmax function row-wise. This will transform the
#'  numbers to probabilities that sum to \code{1} row-wise.
#' @param add_targets Whether to add a column with randomly selected target classes. (Logical)
#' @param add_predicted_classes Whether to add a column with the predicted classes. (Logical)
#'
#'  The class with the highest value is the predicted class.
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
#' multiclass_probability_tibble(
#'   num_classes = 5,
#'   num_observations = 10,
#'   apply_softmax = TRUE
#' )
#'
#' # Using the rnorm function to generate the random numbers
#' multiclass_probability_tibble(
#'   num_classes = 5,
#'   num_observations = 10,
#'   apply_softmax = TRUE,
#'   FUN = rnorm
#' )
#'
#' # Add targets and predicted classes
#' multiclass_probability_tibble(
#'   num_classes = 5,
#'   num_observations = 10,
#'   apply_softmax = TRUE,
#'   FUN = rnorm,
#'   add_predicted_classes = TRUE,
#'   add_targets = TRUE
#' )
#'
#' # Creating a custom generator function that
#' # exponentiates the numbers to create more "certain" predictions
#' rcertain <- function(n) {
#'   (runif(n, min = 1, max = 100)^1.4) / 100
#' }
#' multiclass_probability_tibble(
#'   num_classes = 5,
#'   num_observations = 10,
#'   apply_softmax = TRUE,
#'   FUN = rcertain
#' )
multiclass_probability_tibble <- function(num_classes,
                                          num_observations,
                                          apply_softmax = TRUE,
                                          FUN = runif,
                                          class_name = "class_",
                                          add_predicted_classes = FALSE,
                                          add_targets = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_count(x = num_classes, positive = TRUE, add = assert_collection)
  checkmate::assert_count(x = num_observations, positive = TRUE, add = assert_collection)
  checkmate::assert_flag(x = apply_softmax, add = assert_collection)
  checkmate::assert_flag(x = add_targets, add = assert_collection)
  checkmate::assert_flag(x = add_predicted_classes, add = assert_collection)
  checkmate::assert_function(x = FUN, add = assert_collection)
  checkmate::assert_string(x = class_name, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Generate random numbers
  random_numbers <- tryCatch(
    {
      FUN(num_classes * num_observations)
    },
    error = function(e) {
      stop("Could not use 'FUN' to generate a sequence of random numbers.")
    }
  )
  # Check generated numbers
  if (!is.numeric(random_numbers)){
    assert_collection$push("the output of 'FUN' was not numeric.")
  }
  if (length(random_numbers) != num_classes * num_observations){
    assert_collection$push("the output of 'FUN' did not have length 'num_classes' * 'num_observations'.")
  }
  checkmate::reportAssertions(assert_collection)

  # Create class names
  class_names <- paste0(class_name, seq_len(num_classes))

  # Create the tibble
  probability_matrix <- matrix(
    random_numbers,
    ncol = num_classes
  ) %>%
    dplyr::as_tibble(.name_repair = "minimal")
  colnames(probability_matrix) <- class_names

  # Apply the softmax function if requested
  if (isTRUE(apply_softmax)) {
    probability_matrix <- softmax(probability_matrix)
  }

  # Add predicted classes
  if (isTRUE(add_predicted_classes)){
    # Create a column with the predicted class
    predicted_class_indices <- argmax(probability_matrix)
    probability_matrix[["Predicted Class"]] <- purrr::map_chr(
      predicted_class_indices,
      .f = function(x) {
        class_names[[x]]
      }
    )
  }

  # Add targets
  if (isTRUE(add_targets)){
    targ_col <- create_tmp_name(probability_matrix, "Target")
    probability_matrix[[targ_col]] <- sample(class_names,
                                             size = num_observations,
                                             replace = TRUE)
  }

  probability_matrix
}
