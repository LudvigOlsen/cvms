

#   __________________ #< 0a2b50ea95d93800a253c2d2d75280f0 ># __________________
#   Create computation grid                                                 ####


# test_fold: If we only want to test on a specific fold (e.g. in validate_fn())
create_computation_grid <- function(data, hparams, formulas,
                                    fold_cols, test_fold = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, add = assert_collection)
  if (!is.null(hparams) &&
      !checkmate::test_data_frame(x = hparams, col.names = "unique") &&
      !checkmate::test_list(x = hparams, names = "unique")) {
    assert_collection$push("'hparams' must be either a data frame, a named list or 'NULL'. All names must be unique.")
  }
  checkmate::assert_character(
    x = formulas,
    min.chars = 1,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE,
    add = assert_collection
  )
  checkmate::assert_character(
    x = fold_cols,
    min.chars = 1,
    any.missing = FALSE,
    min.len = 1,
    unique = TRUE,
    add = assert_collection
  )
  checkmate::assert_count(
    x = test_fold,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Hyperparameters

  # If no hparams were passed,
  # create NA list
  if (is.null(hparams)) {
    hparams <- list(".__NA__" = NA)
  }

  # Extract n_hparam_combinations
  if (".n" %in% names(hparams)) {
    n_hparam_combinations <- hparams[[".n"]]
    hparams[[".n"]] <- NULL
  } else {
    n_hparam_combinations <- NULL
  }

  if (!is.data.frame(hparams)) {
    if (!is.list(hparams)) {
      stop("'hyperparameters' must be either a named list or a data frame.")
    }

    hparams_grid <- create_hparams_grid(hparams, n = n_hparam_combinations)
    hparam_combination <- hparams_grid[["hparam_combination"]]
  } else {

    # TODO add tests of provided df grids
    hparams_grid <- tibble::tibble("hparams" = nest_rowwise(hparams))
    hparams_grid[["hparam_combination"]] <- seq_len(nrow(hparams_grid))
    hparam_combination <- hparams_grid[["hparam_combination"]]
  }

  # Create folds map
  folds_grid <- create_folds_grid(data = data, fold_cols = fold_cols)

  if (!is.null(test_fold)) {
    folds_grid <- folds_grid[
      folds_grid[["rel_fold"]] == test_fold,
    ]
  }

  # Just the enframed formulas vector
  formulas_grid <- tibble::enframe(formulas, name = NULL, value = "Formula")

  computation_grid <- tidyr::crossing(formulas_grid, folds_grid, hparam_combination) %>%
    dplyr::left_join(hparams_grid, by = "hparam_combination") %>%
    dplyr::group_by(.data$Formula, .data$hparam_combination)

  model <- computation_grid %>% dplyr::group_indices()
  computation_grid <- computation_grid %>%
    dplyr::ungroup() %>%
    tibble::add_column(model = model, .before = "fold_col_idx")

  computation_grid
}


#   __________________ #< 0373eca5946e51fae376e885c2c68f73 ># __________________
#   Create hparams grid                                                     ####


create_hparams_grid <- function(hparams, n = NULL) {

  # Create grid of
  grid <- expand.grid(hparams, stringsAsFactors = FALSE) %>%
    dplyr::as_tibble()

  # Randomly pick  n (number or percentage)
  # hparam combinations, if specified
  if (!is.null(n)) {
    if (is_between_(n, 0, 1)) {
      sampler_fn <- dplyr::sample_frac
    } else {
      if (n > nrow(grid)) {
        stop("'n' was greater than the rows in the hparams grid.")
      }
      sampler_fn <- dplyr::sample_n
    }

    grid <- grid %>%
      sampler_fn(n)
  }

  tibble::tibble(
    "hparam_combination" = seq_len(nrow(grid)),
    "hparams" = nest_rowwise(grid)
  )
}


#   __________________ #< c32e61be9401533ec6d088fd0c445bd5 ># __________________
#   Create folds grid                                                       ####


create_folds_grid <- function(data, fold_cols) {
  # Create folds map
  folds_map_and_n_folds <- create_folds_map(data, fold_cols)
  folds_map_and_n_folds[["folds_map"]]
}
