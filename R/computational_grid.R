create_computation_grid <- function(data, hparams, formulas, fold_cols){

  # Hyperparameters

  # If no hparams were passed,
  # create NA list
  if (is.null(hparams)){
    hparams <- list(".__NA__" = NA)
  }

  # Extract n_hparam_combinations
  if (".n" %in% names(hparams)){
    n_hparam_combinations <- hparams[[".n"]]
    hparams[[".n"]] <- NULL
  } else {
    n_hparam_combinations <- NULL
  }

  hparams_grid <- create_hparams_grid(hparams, n = n_hparam_combinations)
  hparam_combination <- hparams_grid[["hparam_combination"]]


  # Create folds map
  folds_grid <- create_folds_grid(data = data, fold_cols = fold_cols)

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

create_hparams_grid <- function(hparams, n = NULL){

  # Create grid of
  grid <- expand.grid(hparams, stringsAsFactors = FALSE) %>%
    dplyr::as_tibble()

  # Randomly pick  n (number or percentage)
  # hparam combinations, if specified
  if (!is.null(n)){

    if (is_between_(n, 0, 1)){
      sampler_fn <- dplyr::sample_frac
    } else {
      if (n > nrow(grid))
        stop("'n' was greater than the rows in the hparams grid.")
      sampler_fn <- dplyr::sample_n
    }

    grid <- grid %>%
      sampler_fn(n)
  }

  tibble::tibble("hparam_combination" = seq_len(nrow(grid)),
                 "hparams" = nest_rowwise(grid))

}

create_folds_grid <- function(data, fold_cols){
  # Create folds map
  folds_map_and_n_folds <- create_folds_map(data, fold_cols)
  folds_map_and_n_folds[["folds_map"]]
}
