library(cvms)
context("helpers")

test_that("Helper count_named_nulls_in_list() works",{

  skip_test_if_old_R_version()

  expect_equal(count_named_nulls_in_list(list("cat"=NULL, "dog"=3, "hat"=NULL)), 2)

})
