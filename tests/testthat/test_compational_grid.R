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
