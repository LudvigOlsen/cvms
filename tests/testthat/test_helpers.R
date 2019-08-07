library(cvms)
context("helpers")

test_that("Helper count_named_nulls_in_list() works",{

  # skip_test_if_old_R_version()

  expect_equal(count_named_nulls_in_list(list("cat"=NULL, "dog"=3, "hat"=NULL)), 2)

})

test_that("Helper count_convergence_warnings() works",{
  expect_equal(count_convergence_warnings(c("Yes","No","No","Yes","No")), 3)
  expect_error(count_convergence_warnings(c("Yes","No","No","Yes","No","Lol","Nay")),
               "'convergences' can only contain 'Yes' and 'No'. Found: Lol, Nay.", fixed=T)
})
