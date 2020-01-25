#   __________________ #< d7bdb81fcb2355ed04f88471ceaffc0d ># __________________
#   Softmax                                                                 ####


##  .................. #< fb7b6d40446b03f933c7e0a3ae823a94 ># ..................
##  Log Sum Exp                                                             ####


logsumexp <- function(x) {
  y <- max(x)
  y + log(sum(exp(x - y)))
}


##  .................. #< d9b269d11f2602c56eb523552e196866 ># ..................
##  Softmax for row                                                         ####


softmax_row <- function(...) {
  x <- unname(c(...))
  x <- exp(x - logsumexp(x))
  # Convert to row in tibble
  # TODO There must be a better way
  x <- dplyr::as_tibble(t(matrix(x)),
                        .name_repair = ~ paste0("V", seq_len(length(x)))
  )
  x
}


##  .................. #< aea580ea3ed0a724fd524bc0ddb4efad ># ..................
##  Softmax on vector                                                       ####


softmax_vector <- function(...) {
  x <- unname(c(...))
  exp(x - logsumexp(x))
}


##  .................. #< 65067b110e73ea523f84706701ea20c7 ># ..................
##  Softmax function                                                        ####


# TODO Add tests of this !!!
softmax <- function(data, cols = NULL) {

  # Convert to tibble
  data <- dplyr::as_tibble(data)

  # TODO is this necessary?
  if (!is.null(cols)) {
    if (is.numeric(cols)) {
      data_to_process <- data[, cols]
      data_to_leave <- data[, setdiff(seq_len(ncol(data)), cols)]
      cols <- colnames(data_to_process)
    } else if (is.character(cols)) {
      data_to_process <- data[, cols]
      data_to_leave <- data[, setdiff(colnames(data), cols)]
    }
  } else {
    data_to_process <- data
    data_to_leave <- NULL
    cols <- colnames(data)
  }

  # Test that the probability columns are numeric
  # TODO use checkmate to do the check?
  if (any(!sapply(data_to_process, is.numeric))) {
    stop("softmax only works on numeric columns.")
  }

  processed_data <- purrr::pmap_dfr(
    data_to_process,
    softmax_row
  )
  colnames(processed_data) <- cols
  dplyr::bind_cols(processed_data, data_to_leave)
}

