library(cvms)
context("combine_predictors()")

test_that("predictors are properly combined with combine_predictors()",{

  dep <- "y"
  fx <- c("a","b","c","d")
  rfx <- "(1|e)"

  formulas_1 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = NULL)
  expect_equal(length(formulas_1), 40)
  expect_equal(nchar(paste0(formulas_1, collapse=" ; ")), 909)

  formulas_2 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = 3)
  expect_equal(length(formulas_2), 40)
  expect_equal(nchar(paste0(formulas_2, collapse=" ; ")), 909)

  formulas_3 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = 2)
  expect_equal(length(formulas_3), 39)
  expect_equal(nchar(paste0(formulas_3, collapse=" ; ")), 881)

  formulas_4 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = 1)
  expect_equal(length(formulas_4), 33)
  expect_equal(nchar(paste0(formulas_4, collapse=" ; ")), 729)

  formulas_5 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = 0)
  expect_equal(length(formulas_5), 15)
  expect_equal(nchar(paste0(formulas_5, collapse=" ; ")), 305)
  expect_equal(formulas_5,
               c("y ~ a + (1|e)","y ~ b + (1|e)","y ~ c + (1|e)","y ~ d + (1|e)",
                 "y ~ a + b + (1|e)","y ~ a + c + (1|e)","y ~ a + d + (1|e)","y ~ b + c + (1|e)",
                 "y ~ b + d + (1|e)","y ~ c + d + (1|e)","y ~ a + b + c + (1|e)","y ~ a + b + d + (1|e)",
                 "y ~ a + c + d + (1|e)","y ~ b + c + d + (1|e)","y ~ a + b + c + d + (1|e)"))

  formulas_5_2 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = 0, max_fixed_effects = 2)
  expect_equal(length(formulas_5_2), 10)
  expect_equal(nchar(paste0(formulas_5_2, collapse=" ; ")), 181)
  expect_equal(formulas_5_2,
               c("y ~ a + (1|e)","y ~ b + (1|e)","y ~ c + (1|e)","y ~ d + (1|e)",
                 "y ~ a + b + (1|e)","y ~ a + c + (1|e)","y ~ a + d + (1|e)","y ~ b + c + (1|e)",
                 "y ~ b + d + (1|e)","y ~ c + d + (1|e)"))

  formulas_6 <- combine_predictors(dep, fx, random_effects = NULL, max_interaction_size = NULL)
  expect_equal(length(formulas_6), 40)
  expect_equal(nchar(paste0(formulas_6, collapse=" ; ")), 589)

  formulas_7 <- combine_predictors(dep, fx, random_effects = NULL, max_interaction_size = 2)
  expect_equal(length(formulas_7), 39)
  expect_equal(nchar(paste0(formulas_7, collapse=" ; ")), 569)

  expect_error(combine_predictors(dep, NULL, random_effects = NULL, max_interaction_size = NULL),
               "Please specify vector/list of fixed_effects.")

  expect_error(combine_predictors(NULL, fx, random_effects = NULL, max_interaction_size = NULL),
               "Please specify dependent variable.")

  # system.time({
  #   combine_predictors("y", as.character(1:10), random_effects = NULL, max_interaction_size = NULL)
  # })

  # data.frame("t"=c(0.132,0.312,0.836,2.199,6.903,26.337,103.696),
  #            "n"=c(4,5,6,7,8,9,10)) %>%
  #   ggplot(aes(x=n,y=t)) +
  #   geom_line()


  # # length(
  # combine_predictors("y", as.character(1:8), random_effects = NULL, max_interaction_size = 3)
  #
  # data.frame("V1"=c("+","+","+"),
  #            "V2"=c("*","+","*"),
  #            "V3"=c("*","*","*"),
  #            "V4"=c("+","+","*"),
  #            "V5"=c("*","*","+"),
  #            stringsAsFactors = FALSE) %>%
  #   mutate( n_int = purr::pmap_dbl(., get_max_nway_interaction))

#
#   fx <- c("a","b","c","d")
#   combed_fx <- as.data.frame(t(combn(fx, 2)), stringsAsFactors=FALSE) %>%
#     dplyr::rename(F1 = V1, F2=V2)
#
#   ops_ <- data.frame("V1"=c("+","+","+"),
#                      "V2"=c("*","+","*"),
#                      "V3"=c("*","*","*"),
#                      "V4"=c("+","+","*"),
#                      "V5"=c("*","*","+"),
#                      stringsAsFactors = FALSE)
#
#   tidyr::crossing(combed_fx, ops_)



})




