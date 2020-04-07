#   __________________ #< d7bdb81fcb2355ed04f88471ceaffc0d ># __________________
#   Softmax                                                                 ####

##  .................. #< 65067b110e73ea523f84706701ea20c7 ># ..................
##  Softmax function                                                        ####


# axis : "r" for row-wise, "c" for column-wise
softmax <- function(data, cols = NULL, axis = "r") {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, add = assert_collection)
  checkmate::assert_choice(x = axis, choices = c("r", "c"), add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (!is.null(cols) && !is.numeric(cols)){
    checkmate::assert_character(x = cols, null.ok = TRUE, add = assert_collection)
    checkmate::assert_names(x = colnames(data), must.include = cols,
                            type = "unique", what = "colnames",
                            add = assert_collection)
  } else {
    checkmate::assert_integerish(x = cols, lower = 1, upper = ncol(data),
                                 null.ok = TRUE, add = assert_collection)
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Convert to tibble
  data <- dplyr::as_tibble(data)

  # Original order
  col_order <- colnames(data)

  # Set cols when not provided as character
  if (is.null(cols)){
    cols <- col_order
  } else if (is.numeric(cols)) {
    cols <- col_order[cols]
  }

  # Subset the data to process and to leave alone
  data_to_process <- data[, cols]
  data_to_leave <- data[, setdiff(colnames(data), cols)]

  # Test that the probability columns are numeric
  if (!checkmate::test_data_frame(x = data_to_process, types = c("numeric"))) {
    stop("softmax() only works on numeric columns.")
  }

  if (axis == "r"){
    processed_data <- purrr::pmap_dfr(
      data_to_process,
      softmax_row
    )
  } else {
    processed_data <- data_to_process %>%
      dplyr::mutate_all(softmax_vector)
  }

  colnames(processed_data) <- cols
  dplyr::bind_cols(processed_data, data_to_leave) %>%
    base_select(cols = col_order) %>%
    dplyr::as_tibble()
}


##  .................. #< fb7b6d40446b03f933c7e0a3ae823a94 ># ..................
##  Log Sum Exp                                                             ####


logsumexp <- function(x) {
  y <- max(x)
  y + log(sum(exp(x - y)))
}


##  .................. #< d9b269d11f2602c56eb523552e196866 ># ..................
##  Softmax for row                                                         ####


softmax_row <- function(...) {
  arg_names <- non_empty_names(c(...))
  x <- as.list(softmax_vector(...))

  if (length(arg_names) == length(x)){
    names(x) <- arg_names
  } else {
    names(x) <- paste0("V", seq_len(length(x)))
  }

  as.data.frame(x)

}


##  .................. #< aea580ea3ed0a724fd524bc0ddb4efad ># ..................
##  Softmax on vector                                                       ####


softmax_vector <- function(...) {
  x <- unname(c(...))
  exp(x - logsumexp(x))
}


