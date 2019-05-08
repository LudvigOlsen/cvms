library(cvms)
context("combine_predictors()")

# NOTICE:
# Numbers tested are the results I got and not "what should be"
# This will allow me to see if something changes, but it shouldn't give false confidence.


test_that("predictors are properly combined with combine_predictors()",{

  dep <- "y"
  fx <- c("a","b","c","d")
  rfx <- "(1|e)"

  formulas_1 <- combine_predictors(dep, fx, random_effects = rfx, max_interactions = NULL)
  expect_equal(length(formulas_1), 40)
  expect_equal(nchar(paste0(formulas_1, collapse=" ; ")), 909)

  formulas_2 <- combine_predictors(dep, fx, random_effects = rfx, max_interactions = 3)
  expect_equal(length(formulas_2), 40)
  expect_equal(nchar(paste0(formulas_2, collapse=" ; ")), 909)

  formulas_3 <- combine_predictors(dep, fx, random_effects = rfx, max_interactions = 2)
  expect_equal(length(formulas_3), 39)
  expect_equal(nchar(paste0(formulas_3, collapse=" ; ")), 881)

  formulas_4 <- combine_predictors(dep, fx, random_effects = rfx, max_interactions = 1)
  expect_equal(length(formulas_4), 32)
  expect_equal(nchar(paste0(formulas_4, collapse=" ; ")), 701)

  formulas_5 <- combine_predictors(dep, fx, random_effects = rfx, max_interactions = 0)
  expect_equal(length(formulas_5), 15)
  expect_equal(nchar(paste0(formulas_5, collapse=" ; ")), 305)
  expect_equal(formulas_5,
               c("y ~ a + (1|e)","y ~ b + (1|e)","y ~ c + (1|e)","y ~ d + (1|e)",
                 "y ~ a + b + (1|e)","y ~ a + c + (1|e)","y ~ a + d + (1|e)","y ~ b + c + (1|e)",
                 "y ~ b + d + (1|e)","y ~ c + d + (1|e)","y ~ a + b + c + (1|e)","y ~ a + b + d + (1|e)",
                 "y ~ a + c + d + (1|e)","y ~ b + c + d + (1|e)","y ~ a + b + c + d + (1|e)"))

  formulas_6 <- combine_predictors(dep, fx, random_effects = NULL, max_interactions = NULL)
  expect_equal(length(formulas_6), 40)
  expect_equal(nchar(paste0(formulas_6, collapse=" ; ")), 589)

  formulas_7 <- combine_predictors(dep, fx, random_effects = NULL, max_interactions = 2)
  expect_equal(length(formulas_7), 39)
  expect_equal(nchar(paste0(formulas_7, collapse=" ; ")), 569)

  expect_error(combine_predictors(dep, NULL, random_effects = NULL, max_interactions = NULL),
               "Please specify vector/list of fixed_effects.")

  expect_error(combine_predictors(NULL, fx, random_effects = NULL, max_interactions = NULL),
               "Please specify dependent variable.")

})


