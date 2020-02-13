library(cvms)
context("combine_predictors()")

test_that("predictors are properly combined with combine_predictors()", {
  dep <- "y"
  fx <- c("a", "b", "c", "d")
  rfx <- "(1|e)"

  formulas_1 <- combine_predictors(dep, fx, random_effects = rfx, max_effect_frequency = 1)
  expect_equal(length(formulas_1), 50)
  expect_equal(nchar(paste0(formulas_1, collapse = " ; ")), 1173)

  formulas_2 <- combine_predictors(dep, fx, random_effects = rfx, max_effect_frequency = 1, max_interaction_size = 2)
  expect_equal(length(formulas_2), 42)
  expect_equal(nchar(paste0(formulas_2, collapse = " ; ")), 965)

  formulas_3 <- combine_predictors(dep, fx, random_effects = rfx)
  expect_equal(length(formulas_3), 165)
  expect_equal(nchar(paste0(formulas_3, collapse = " ; ")), 5593)

  formulas_4 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = 2)
  expect_equal(length(formulas_4), 112)
  expect_equal(nchar(paste0(formulas_4, collapse = " ; ")), 3597)

  formulas_5_0 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = 0)
  formulas_5_1 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = 1)
  expect_equal(formulas_5_0, formulas_5_1)
  expect_equal(length(formulas_5_0), 15)
  expect_equal(nchar(paste0(formulas_5_0, collapse = " ; ")), 305)
  expect_equal(
    formulas_5_0,
    c(
      "y ~ a + (1|e)", "y ~ b + (1|e)", "y ~ c + (1|e)", "y ~ d + (1|e)",
      "y ~ a + b + (1|e)", "y ~ a + c + (1|e)", "y ~ a + d + (1|e)", "y ~ b + c + (1|e)",
      "y ~ b + d + (1|e)", "y ~ c + d + (1|e)", "y ~ a + b + c + (1|e)", "y ~ a + b + d + (1|e)",
      "y ~ a + c + d + (1|e)", "y ~ b + c + d + (1|e)", "y ~ a + b + c + d + (1|e)"
    )
  )

  formulas_5_2 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = 0, max_fixed_effects = 2)
  expect_equal(length(formulas_5_2), 10)
  expect_equal(nchar(paste0(formulas_5_2, collapse = " ; ")), 181)
  expect_equal(
    formulas_5_2,
    c(
      "y ~ a + (1|e)", "y ~ b + (1|e)", "y ~ c + (1|e)", "y ~ d + (1|e)",
      "y ~ a + b + (1|e)", "y ~ a + c + (1|e)", "y ~ a + d + (1|e)",
      "y ~ b + c + (1|e)", "y ~ b + d + (1|e)", "y ~ c + d + (1|e)"
    )
  )

  formulas_6 <- combine_predictors(dep, fx, random_effects = NULL, max_interaction_size = 3, max_effect_frequency = 1)
  expect_equal(length(formulas_6), 50)
  expect_equal(nchar(paste0(formulas_6, collapse = " ; ")), 773)

  formulas_7 <- combine_predictors(dep, fx, random_effects = NULL, max_interaction_size = 3)
  expect_equal(length(formulas_7), 165)
  expect_equal(nchar(paste0(formulas_7, collapse = " ; ")), 4273)

  expect_error(xpectr::strip_msg(combine_predictors(dep, NULL, random_effects = NULL)),
               xpectr::strip(
                 paste0(
                   "Assertion failed. One of the following must apply:\n * chec",
                   "kmate::check_list(fixed_effects): Must be of type 'list', no",
                   "t\n * 'NULL'\n * checkmate::check_character(fixed_effects): ",
                   "Must be of type\n * 'character', not 'NULL'"
                 )
               ),
               fixed = TRUE)

  expect_error(
    xpectr::strip_msg(combine_predictors(NULL, fx, random_effects = NULL)),
    xpectr::strip(
      "1 assertions failed Variable dependent Must be of type string not NULL"
    ),
    fixed = TRUE)

  formulas_8 <- combine_predictors("y", as.character(1:4),
    random_effects = NULL,
    max_interaction_size = 2, max_fixed_effects = 3,
    max_effect_frequency = 1
  )
  expect_equal(length(formulas_8), 32)

  # system.time({
  #   combine_predictors("y", as.character(1:6), random_effects = NULL, max_interaction_size = NULL)
  # })

  # system.time({
  #   combine_predictors("y", as.character(1:6), random_effects = NULL, max_interaction_size = NULL,
  #                      max_fixed_effects = NULL)
  # })
  # user  system elapsed
  # 13.921   0.182  14.200


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


test_that("interchangeable predictors are properly combined with combine_predictors()", {

  # testthat::skip(message = "Skipping check for CRAN release due to r_hub failure")
  xpectr::set_test_seed(1)
  dep <- "y"
  rfx <- "(1|e)"

  # Without interchangeable effect
  fx <- list("a", list("b"), "c", "d")

  formulas_1 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = 0)
  expect_equal(length(formulas_1), 15)
  expect_equal(nchar(paste0(formulas_1, collapse = " ; ")), 305)
  expect_equal(
    formulas_1,
    c(
      "y ~ a + (1|e)", "y ~ b + (1|e)", "y ~ c + (1|e)", "y ~ d + (1|e)",
      "y ~ a + b + (1|e)", "y ~ a + c + (1|e)", "y ~ a + d + (1|e)",
      "y ~ b + c + (1|e)", "y ~ b + d + (1|e)", "y ~ c + d + (1|e)",
      "y ~ a + b + c + (1|e)", "y ~ a + b + d + (1|e)", "y ~ a + c + d + (1|e)",
      "y ~ b + c + d + (1|e)", "y ~ a + b + c + d + (1|e)"
    )
  )

  # With interchangeable effect
  # fx <- list("a",list("b","c"),"d", list("f","g","h"), list("j","k","l"))

  fx <- list("a", list("b", "c"), "d")

  formulas_2 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = 0)
  expect_equal(formulas_2, c(
    "y ~ a + (1|e)", "y ~ b + (1|e)", "y ~ c + (1|e)", "y ~ d + (1|e)",
    "y ~ a + b + (1|e)", "y ~ a + c + (1|e)", "y ~ a + d + (1|e)",
    "y ~ b + d + (1|e)", "y ~ c + d + (1|e)", "y ~ a + b + d + (1|e)",
    "y ~ a + c + d + (1|e)"
  ))

  # With interchangeable effect and interactions

  fx <- list("a", list("b", "c"), "d")

  formulas_3 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = 2)
  expect_equal(setdiff(formulas_3, c(
    "y ~ a + (1|e)", "y ~ b + (1|e)", "y ~ c + (1|e)", "y ~ d + (1|e)",
    "y ~ a * b + (1|e)", "y ~ a * c + (1|e)", "y ~ a * d + (1|e)",
    "y ~ a + b + (1|e)", "y ~ a + c + (1|e)", "y ~ a + d + (1|e)",
    "y ~ b * d + (1|e)", "y ~ b + d + (1|e)", "y ~ c * d + (1|e)",
    "y ~ c + d + (1|e)", "y ~ a * b + d + (1|e)", "y ~ a * c + d + (1|e)",
    "y ~ a * d + b + (1|e)", "y ~ a * d + c + (1|e)", "y ~ a + b * d + (1|e)",
    "y ~ a + b + d + (1|e)", "y ~ a + c * d + (1|e)", "y ~ a + c + d + (1|e)",
    "y ~ a * b + a * d + (1|e)", "y ~ a * b + b * d + (1|e)",
    "y ~ a * c + a * d + (1|e)", "y ~ a * c + c * d + (1|e)",
    "y ~ a * d + b * d + (1|e)", "y ~ a * d + c * d + (1|e)",
    "y ~ a * b + a * d + b * d + (1|e)", "y ~ a * c + a * d + c * d + (1|e)"
  )), character())
  expect_equal(sort(formulas_3), sort(c(
    "y ~ a + (1|e)", "y ~ b + (1|e)", "y ~ c + (1|e)", "y ~ d + (1|e)",
    "y ~ a * b + (1|e)", "y ~ a * c + (1|e)", "y ~ a * d + (1|e)",
    "y ~ a + b + (1|e)", "y ~ a + c + (1|e)", "y ~ a + d + (1|e)",
    "y ~ b * d + (1|e)", "y ~ b + d + (1|e)", "y ~ c * d + (1|e)",
    "y ~ c + d + (1|e)", "y ~ a * b + d + (1|e)", "y ~ a * c + d + (1|e)",
    "y ~ a * d + b + (1|e)", "y ~ a * d + c + (1|e)", "y ~ a + b * d + (1|e)",
    "y ~ a + b + d + (1|e)", "y ~ a + c * d + (1|e)", "y ~ a + c + d + (1|e)",
    "y ~ a * b + a * d + (1|e)", "y ~ a * b + b * d + (1|e)",
    "y ~ a * c + a * d + (1|e)", "y ~ a * c + c * d + (1|e)",
    "y ~ a * d + b * d + (1|e)", "y ~ a * d + c * d + (1|e)",
    "y ~ a * b + a * d + b * d + (1|e)", "y ~ a * c + a * d + c * d + (1|e)"
  )))


  # formulas_4 <- combine_predictors(dep, fx, random_effects = rfx, max_interaction_size = NULL)

  expect_equal(
    combine_predictors(dep, c("a", "b", "c", "d"), random_effects = rfx),
    combine_predictors(dep, list("a", list("b"), "c", "d"), random_effects = rfx)
  )

  fx_1 <- list("a", "b", "c")
  fx_2 <- list("a", "e", "c")
  fx_3 <- list("a", "f", "c")
  fx_4 <- list("a", list("b", "e", "f"), "c")

  formulas_fx_1 <- combine_predictors(dep, fx_1, random_effects = rfx)
  formulas_fx_2 <- combine_predictors(dep, fx_2, random_effects = rfx)
  formulas_fx_3 <- combine_predictors(dep, fx_3, random_effects = rfx)
  formulas_fx_4 <- combine_predictors(dep, fx_4, random_effects = rfx)

  expect_equal(length(formulas_fx_4), length(unique(c(formulas_fx_1, formulas_fx_2, formulas_fx_3))))

  fx <- list(list("a", "h"), list("b", "e", "f"), list("c", "t", "k"))

  formulas_5 <- combine_predictors(dep, fx, random_effects = NULL, max_effect_frequency = 1)

  expect_equal(length(setdiff(c(
    "y ~ a * b * c", "y ~ a * b * k", "y ~ a * b * t",
    "y ~ a * e * c", "y ~ a * e * k", "y ~ a * e * t",
    "y ~ a * f * c", "y ~ a * f * k", "y ~ a * f * t",
    "y ~ h * b * c", "y ~ h * b * k", "y ~ h * b * t",
    "y ~ h * e * c", "y ~ h * e * k", "y ~ h * e * t",
    "y ~ h * f * c", "y ~ h * f * k", "y ~ h * f * t"
  ), formulas_5)), 0)
})

test_that("fixed effect replacements works with combine_predictors()", {
  model_formulas_lower <- combine_predictors(
    dependent = "Price",
    fixed_effects = tolower(c(
      "Mileage", "Cylinder",
      "Doors", "Cruise"
    )),
    max_fixed_effects = 3,
    max_interaction_size = 2,
    max_effect_frequency = 1
  )

  model_formulas_title <- combine_predictors(
    dependent = "Price",
    fixed_effects = c(
      "Mileage", "Cylinder",
      "Doors", "Cruise"
    ),
    max_fixed_effects = 3,
    max_interaction_size = 2,
    max_effect_frequency = 1
  )

  expect_equal(tolower(model_formulas_lower), tolower(model_formulas_title))

  testthat::skip(message = "Skipping check for CRAN release due to r_hub failure")
  expect_equal(model_formulas_title, c(
    "Price ~ Cruise", "Price ~ Cylinder",
    "Price ~ Doors", "Price ~ Mileage",
    "Price ~ Cruise * Cylinder", "Price ~ Cruise * Doors",
    "Price ~ Cruise * Mileage", "Price ~ Cruise + Cylinder",
    "Price ~ Cruise + Doors", "Price ~ Cruise + Mileage",
    "Price ~ Cylinder * Doors", "Price ~ Cylinder * Mileage",
    "Price ~ Cylinder + Doors", "Price ~ Cylinder + Mileage",
    "Price ~ Doors * Mileage", "Price ~ Doors + Mileage",
    "Price ~ Cruise * Cylinder + Doors", "Price ~ Cruise * Cylinder + Mileage",
    "Price ~ Cruise * Doors + Cylinder", "Price ~ Cruise * Doors + Mileage",
    "Price ~ Cruise * Mileage + Cylinder", "Price ~ Cruise * Mileage + Doors",
    "Price ~ Cruise + Cylinder * Doors", "Price ~ Cruise + Cylinder * Mileage",
    "Price ~ Cruise + Cylinder + Doors", "Price ~ Cruise + Cylinder + Mileage",
    "Price ~ Cruise + Doors * Mileage", "Price ~ Cruise + Doors + Mileage",
    "Price ~ Cylinder * Doors + Mileage", "Price ~ Cylinder * Mileage + Doors",
    "Price ~ Cylinder + Doors * Mileage", "Price ~ Cylinder + Doors + Mileage"
  ))
})

test_that("the expected errors are thrown by combine_predictors()", {
  expect_error(combine_predictors(
    dependent = "Price",
    fixed_effects = c(
      "Mileage", "Cylinder",
      "Doors", "Cruise"
    ),
    max_fixed_effects = 3,
    max_interaction_size = NA,
    max_effect_frequency = 1
  ),
  paste0(
    "1 assertions failed:\n * Variable 'max_interaction_size': M",
    "ay not be NA."
  ),
  fixed = T
  )
  expect_error(combine_predictors(
    dependent = "Price",
    fixed_effects = c(
      "Mileage", "Cylinder",
      "Doors", "Cruise"
    ),
    max_fixed_effects = 3,
    max_interaction_size = 5,
    max_effect_frequency = 1
  ),
  paste0(
    "1 assertions failed:\n * Variable 'max_interaction_size': E",
    "lement 1 is not <= 3."
  ),
  fixed = T
  )
  expect_error(combine_predictors(
    dependent = "Price",
    fixed_effects = c(
      "Mileage", "Cylinder",
      "Doors", "Cruise"
    ),
    max_fixed_effects = 3,
    max_interaction_size = -1,
    max_effect_frequency = 1
  ),
  paste0(
    "1 assertions failed:\n * Variable 'max_interaction_size': E",
    "lement 1 is not >= 0."
  ),
  fixed = T
  )
  expect_error(combine_predictors(
    dependent = "Price",
    fixed_effects = c(
      "Mileage", "Cylinder",
      "Doors", "Cruise"
    ),
    max_fixed_effects = 10
  ),
  paste0(
    "1 assertions failed:\n * Variable 'max_fixed_effects': Elem",
    "ent 1 is not <= 5."
  ),
  fixed = T
  )
  expect_error(xpectr::strip_msg(combine_predictors(
    dependent = "Price",
    fixed_effects = c(
      "Mileage", "Cylinder",
      "Doors", "Cruise"
    ),
    max_fixed_effects = -10
  )),
  xpectr::strip("1 assertions failed:\n * Variable 'max_fixed_effects': Element 1 is not >= 2."),
  fixed = T
  )
  expect_error(combine_predictors(
    dependent = "Price",
    fixed_effects = c(
      "Mileage", "Cylinder",
      "Doors", "Cruise"
    ),
    max_fixed_effects = c(2, 3)
  ),
  paste0(
    "1 assertions failed:\n * Variable 'max_fixed_effects': Must",
    " have length 1."
  ),
  fixed = T
  )
  expect_error(combine_predictors(
    dependent = "Price",
    fixed_effects = c(
      "Mileage", "Cylinder",
      "Doors", "Cruise"
    ),
    max_effect_frequency = c(2, 3)
  ),
  "1 assertions failed:\n * Variable 'max_effect_frequency': Must have length 1.",
  fixed = T
  )
  expect_error(combine_predictors(
    dependent = NULL,
    fixed_effects = c(
      "Mileage", "Cylinder",
      "Doors", "Cruise"
    )
  ),
  paste0(
    "1 assertions failed:\n * Variable 'dependent': Must be of t",
    "ype 'string', not 'NULL'."
  ),
  fixed = T
  )
  expect_error(combine_predictors(
    dependent = 3,
    fixed_effects = c(
      "Mileage", "Cylinder",
      "Doors", "Cruise"
    )
  ),
  paste0(
    "1 assertions failed:\n * Variable 'dependent': Must be of t",
    "ype 'string', not 'double'."
  ),
  fixed = T
  )

  expect_error(xpectr::strip_msg(combine_predictors(
    dependent = "Price",
    fixed_effects = NULL
  )),
  xpectr::strip(
    paste0(
      "Assertion failed. One of the following must apply:\n * chec",
      "kmate::check_list(fixed_effects): Must be of type 'list', no",
      "t 'NULL'\n * checkmate::check_character(fixed_effects): Must",
      " be of type 'character', not 'NULL'"
    )
  ),
  fixed = T)

  expect_error(xpectr::strip_msg(combine_predictors(
    dependent = "Price",
    fixed_effects = "lol"
  )),
  xpectr::strip(
    paste0(
      "Assertion failed. One of the following must apply:\n * chec",
      "kmate::check_list(fixed_effects): Must be of type 'list', no",
      "t\n * 'character'\n * checkmate::check_character(fixed_effec",
      "ts): Must have length >= 2,\n * but has length 1"
    )
  ), fixed = T)

  expect_error(xpectr::strip_msg(combine_predictors(
    dependent = "Price",
    fixed_effects = c("a", "b", "c", "d", "e", "f", "g", "h", "i")
  )),
  xpectr::strip(
    paste0("Assertion failed. One of the following must apply:\n * chec",
         "kmate::check_list(fixed_effects): Must be of type 'list', no",
         "t\n * 'character'\n * checkmate::check_character(fixed_effec",
         "ts): Must have length <= 8,\n * but has length 9")),
  fixed = T
  )

  expect_error(xpectr::strip_msg(combine_predictors(
    dependent = "Price",
    fixed_effects = c(1, 2, 3)
  )),
  xpectr::strip(
    paste0("Assertion failed. One of the following must apply:\n * chec",
         "kmate::check_list(fixed_effects): Must be of type 'list', no",
         "t\n * 'double'\n * checkmate::check_character(fixed_effects)",
         ": Must be of type\n * 'character', not 'double'")),
  fixed = T
  )
  expect_error(
    xpectr::strip_msg(combine_predictors(
    dependent = "Price",
    fixed_effects = c(
      "Mileage", "Cylinder",
      "Doors", "Cruise"
    ),
    random_effects = 28
  )),
  xpectr::strip(paste0(
    "1 assertions failed:\n * Variable 'random_effects': Must be",
    " of type 'string' (or 'NULL'), not 'double'."
  )),
  fixed = T
  )
})

test_that("get_terms_matrix() works", {
  terms_matrix <- get_terms_matrix(c("a", "b", "c"))
  expect_equal(
    terms_matrix$terms,
    c("a", "b", "c", "a * b", "a * c", "b * c", "a * b * c")
  )
  expect_equal(
    terms_matrix$a,
    c(1L, 0L, 0L, 1L, 1L, 0L, 1L)
  )
  expect_equal(
    terms_matrix$b,
    c(0L, 1L, 0L, 1L, 0L, 1L, 1L)
  )
  expect_equal(
    terms_matrix$c,
    c(0L, 0L, 1L, 0L, 1L, 1L, 1L)
  )
  expect_equal(
    terms_matrix$num_terms,
    c(1, 1, 1, 2, 2, 2, 3)
  )
})
