
# TODO Optimize this for speed on laaaarge datasets
# TODO Convert to external function
extract_probabilities_of <- function(data, probability_cols, of_col = "Target", cat_levels = NULL, positive = 2){

  # Multinomial
  if (length(probability_cols) > 1) {
    # Extract probability of 'of_col' class
    index_map <-
      as.list(setNames(seq_along(probability_cols), probability_cols))
    of_indices <- purrr::map_int(
      data[[of_col]],
      .f = function(x) {
        index_map[[x]]
      }
    )
    prob_of <- as.data.frame(data[, probability_cols])[
      cbind(seq_along(of_indices), of_indices)]

  } else {
    # Binomial

    if (is.null(cat_levels)){
      cat_levels <- levels_as_characters(data[[of_col]], drop_unused = TRUE, sort_levels=TRUE)
    }

    if (length(cat_levels) != 2) {
      stop("When 'probability_cols' has length 1, the 'of_col' column should have 2 levels.")
    }

    if (is.numeric(positive))
      positive <- cat_levels[[positive]]

    # Probability of 'of' column
    prob_of <-
      ifelse(data[[of_col]] == positive,
             data[[probability_cols]],
             1 - data[[probability_cols]])

  }

  prob_of

}
