library(cvms)
context("create_computation_grid()")


test_that("the correct grid is created with create_computation_grid()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    num_fold_cols = 3,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  grid_1 <- create_computation_grid(
    data = dat,
    hparams = list(
      "a" = c(-1, 0, 1),
      "b" = 7,
      "c" = c(5, 2, 4, 5)
    ),
    formulas = c("y ~ x", "y ~ z"),
    fold_cols = paste0(".folds_", 1:3)
  )

  grid_1.2 <- grid_1 %>%
    dplyr::select(-.data$hparams) %>%
    dplyr::bind_cols(dplyr::bind_rows(grid_1$hparams)) %>%
    dplyr::distinct()

  expect_equal(nrow(grid_1), 288)
  expect_equal(nrow(grid_1.2), 288)
  expect_equal(nrow(dplyr::distinct(grid_1.2)), 288)
  expect_equal(as.vector(table(grid_1$Formula)), c(144, 144))
  expect_equal(as.vector(table(grid_1$fold_col_name)), c(96, 96, 96))
  expect_equal(as.vector(table(grid_1$rel_fold)), rep(72, 4))
  expect_equal(as.vector(table(grid_1$abs_fold)), rep(24, 12))
  expect_equal(as.vector(table(grid_1$model)), rep(12, 24))
  expect_equal(grid_1$Formula, rep(c("y ~ x", "y ~ z"), each = 144))
  expect_equal(grid_1$fold_col_name, factor(rep(rep(paste0(".folds_", 1:3), each = 48), 2)))
  expect_equal(grid_1.2$a, rep(c(-1, 0, 1), 96))
  expect_equal(grid_1.2$b, rep(7, 288))
  expect_equal(
    grid_1.2$c,
    c(
      5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4,
      4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2,
      2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5,
      5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5,
      5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4,
      4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2,
      2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5,
      5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5,
      5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4,
      4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2,
      2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5,
      5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5,
      5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4,
      4, 5, 5, 5, 5, 5, 5, 2, 2, 2, 4, 4, 4, 5, 5, 5
    )
  )
})

test_that("the correct sampled grid is created with create_computation_grid()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    num_fold_cols = 3,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  grid_1 <- create_computation_grid(
    data = dat,
    hparams = list(
      ".n" = 5,
      "a" = c(-1, 0, 1),
      "b" = 7,
      "c" = c(5, 2, 4, 5)
    ),
    formulas = c("y ~ x", "y ~ z"),
    fold_cols = paste0(".folds_", 1:3)
  )


  grid_1.2 <- grid_1 %>%
    dplyr::select(-.data$hparams) %>%
    dplyr::bind_cols(dplyr::bind_rows(grid_1$hparams)) %>%
    dplyr::distinct()

  expect_equal(nrow(grid_1), 120)
  expect_equal(nrow(grid_1.2), 120)
  expect_equal(nrow(dplyr::distinct(grid_1.2)), 120)
  expect_equal(as.vector(table(grid_1$Formula)), c(60, 60))
  expect_equal(as.vector(table(grid_1$fold_col_name)), c(40, 40, 40))
  expect_equal(as.vector(table(grid_1$rel_fold)), rep(30, 4))
  expect_equal(as.vector(table(grid_1$abs_fold)), rep(10, 12))
  expect_equal(as.vector(table(grid_1$model)), rep(12, 10))
  expect_equal(grid_1$Formula, rep(c("y ~ x", "y ~ z"), each = 60))
  expect_equal(grid_1$fold_col_name, factor(rep(rep(paste0(".folds_", 1:3), each = 20), 2)))
  expect_equal(grid_1.2$a, rep(c(1, -1, 0, 0, 0), 24))
  expect_equal(grid_1.2$b, rep(7, 120))
  expect_equal(grid_1.2$c, rep(c(2, 4, 2, 5, 5), 24))


  expect_error(create_computation_grid(
    data = dat,
    hparams = list(
      ".n" = 77,
      "a" = c(-1, 0, 1),
      "b" = 7,
      "c" = c(5, 2, 4, 5)
    ),
    formulas = c("y ~ x", "y ~ z"),
    fold_cols = paste0(".folds_", 1:3)
  ), "'n' was greater than the rows in the hparams grid.",
  fixed = TRUE
  )

  # Fraction

  xpectr::set_test_seed(1)
  grid_2 <- create_computation_grid(
    data = dat,
    hparams = list(
      ".n" = 0.4,
      "a" = c(-1, 0, 1),
      "b" = 7,
      "c" = c(5, 2, 4, 5)
    ),
    formulas = c("y ~ x", "y ~ z"),
    fold_cols = paste0(".folds_", 1:3)
  )

  grid_2.2 <- grid_2 %>%
    dplyr::select(-.data$hparams) %>%
    dplyr::bind_cols(dplyr::bind_rows(grid_1$hparams)) %>%
    dplyr::distinct()

  expect_equal(nrow(grid_2), 120)
  expect_equal(nrow(grid_2.2), 120)
  expect_equal(nrow(dplyr::distinct(grid_2.2)), 120)
  expect_equal(as.vector(table(grid_2$Formula)), c(60, 60))
  expect_equal(as.vector(table(grid_2$fold_col_name)), c(40, 40, 40))
  expect_equal(as.vector(table(grid_2$rel_fold)), rep(30, 4))
  expect_equal(as.vector(table(grid_2$abs_fold)), rep(10, 12))
  expect_equal(as.vector(table(grid_2$model)), rep(12, 10))
  expect_equal(grid_2$Formula, rep(c("y ~ x", "y ~ z"), each = 60))
  expect_equal(grid_2$fold_col_name, factor(rep(rep(paste0(".folds_", 1:3), each = 20), 2)))
  expect_equal(grid_2.2$a, rep(c(1, -1, 0, 0, 0), 24))
  expect_equal(grid_2.2$b, rep(7, 120))
  expect_equal(grid_2.2$c, rep(c(2, 4, 2, 5, 5), 24))
})

test_that("the correct folds map is created with create_folds_map()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
    k = 4,
    num_fold_cols = 3,
    cat_col = "diagnosis",
    id_col = "participant"
  )

  folds_map <- create_folds_map(
    data = dat,
    fold_cols = paste0(".folds_", 1:3)
  )

  n_folds <- folds_map[["n_folds"]]
  folds_map <- folds_map[["folds_map"]]

  expect_equal(folds_map$fold_col_name, factor(rep(paste0(".folds_", 1:3), each = 4)))
  expect_equal(folds_map$fold_col_idx, rep(seq_len(3), each = 4))
  expect_equal(folds_map$abs_fold, seq_len(12))
  expect_equal(folds_map$rel_fold, rep(seq_len(4), 3))
})

test_that("the grid order is as expected with create_computation_grid()", {

  # Load data and fold it
  xpectr::set_test_seed(1)
  dat <- groupdata2::fold(participant.scores,
                          k = 4,
                          num_fold_cols = 3,
                          cat_col = "diagnosis",
                          id_col = "participant"
  )

  grid_1 <- create_computation_grid(
    data = dat,
    hparams = list(
      "a" = c(-1),
      "b" = 7,
      "c" = c(5, 2)
    ),
    formulas = c("u ~ a", "y ~ z", "y ~ x", "u ~ y"),
    fold_cols = paste0(".folds_", 1:2)
  )


  ## Testing 'grid_1'                                                       ####
  ## Initially generated by xpectr
  xpectr::set_test_seed(42)
  # Testing class
  expect_equal(
    class(grid_1),
    c("tbl_df", "tbl", "data.frame"),
    fixed = TRUE)
  # Testing column values
  expect_equal(
    xpectr::smpl(grid_1[["Formula"]], n = 30),
    c("u ~ a", "u ~ a", "u ~ a", "u ~ a", "u ~ a", "u ~ y", "u ~ y",
      "u ~ y", "u ~ y", "u ~ y", "u ~ y", "u ~ y", "u ~ y", "u ~ y",
      "y ~ x", "y ~ x", "y ~ x", "y ~ x", "y ~ x", "y ~ x", "y ~ x",
      "y ~ x", "y ~ z", "y ~ z", "y ~ z", "y ~ z", "y ~ z", "y ~ z",
      "y ~ z", "y ~ z"),
    fixed = TRUE)
  expect_equal(
    xpectr::smpl(grid_1[["model"]], n = 30),
    c(2, 2, 2, 2, 1, 3, 4, 3, 4, 4, 3, 4, 4, 3, 6, 5, 5, 6, 6, 5, 5,
  6, 7, 7, 8, 7, 7, 7, 8, 7),
    tolerance = 1e-4)
  expect_equal(
    xpectr::smpl(grid_1[["fold_col_idx"]], n = 30),
    c(1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2,
  2, 1, 1, 1, 1, 1, 2, 2, 2),
    tolerance = 1e-4)
  expect_equal(
    xpectr::smpl(grid_1[["fold_col_name"]], n = 30),
    structure(c(1L, 1L, 1L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L,
      2L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 1L, 1L, 1L, 1L, 2L,
      2L, 2L), .Label = c(".folds_1", ".folds_2"), class = "factor"))
  expect_equal(
    xpectr::smpl(grid_1[["abs_fold"]], n = 30),
    c(2, 3, 4, 7, 8, 1, 1, 3, 3, 4, 5, 5, 7, 8, 1, 3, 4, 4, 5, 6, 8,
  8, 1, 2, 2, 3, 4, 6, 6, 8),
    tolerance = 1e-4)
  expect_equal(
    xpectr::smpl(grid_1[["rel_fold"]], n = 30),
    c(2, 3, 4, 3, 4, 1, 1, 3, 3, 4, 1, 1, 3, 4, 1, 3, 4, 4, 1, 2, 4,
  4, 1, 2, 2, 3, 4, 2, 2, 4),
    tolerance = 1e-4)
  expect_equal(
    xpectr::smpl(grid_1[["hparam_combination"]], n = 30),
    c(2, 2, 2, 2, 1, 1, 2, 1, 2, 2, 1, 2, 2, 1, 2, 1, 1, 2, 2, 1, 1,
  2, 1, 1, 2, 1, 1, 1, 2, 1),
    tolerance = 1e-4)
  # Testing column names
  expect_equal(
    names(grid_1),
    c("Formula", "model", "fold_col_idx", "fold_col_name", "abs_fold",
      "rel_fold", "hparam_combination", "hparams"),
    fixed = TRUE)
  # Testing column classes
  expect_equal(
    xpectr::element_classes(grid_1),
    c("character", "integer", "integer", "factor", "integer", "integer",
      "integer", ifelse(is_tibble_v2(),"list","vctrs_list_of")),
    fixed = TRUE)
  # Testing column types
  expect_equal(
    xpectr::element_types(grid_1),
    c("character", "integer", "integer", "integer", "integer", "integer",
      "integer", "list"),
    fixed = TRUE)
  # Testing dimensions
  expect_equal(
    dim(grid_1),
    c(64L, 8L))
  # Testing group keys
  expect_equal(
    colnames(dplyr::group_keys(grid_1)),
    character(0),
    fixed = TRUE)
  ## Finished testing 'grid_1'                                              ####


})

