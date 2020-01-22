library(cvms)
context("plotting functions")

test_that("plot_confusion_matrix() returns expected plots", {

  # Note: These are just initial tests
  # There's probably a high number of errors it won't catch

  # TODO Check out https://github.com/r-lib/vdiffr
  # It may make testing easier and better

  targets <- c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  predictions <- c(1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0)

  # Create confusion matrix with default metrics
  cm <- confusion_matrix(targets, predictions)
  cm[["Confusion Matrix"]]

  expect_equal(cm[["Confusion Matrix"]][[1]]$N, c(4L, 2L, 2L, 4L))

  p1 <- plot_confusion_matrix(cm[["Confusion Matrix"]][[1]])
  expect_equal(p1$data$Target, structure(c(1L, 1L, 2L, 2L), .Label = c("0", "1"), class = "factor"))
  expect_equal(p1$data$Prediction, structure(c(1L, 2L, 1L, 2L), .Label = c("0", "1"), class = "factor"))
  expect_equal(p1$data$N, c(4L, 2L, 2L, 4L))
  expect_equal(p1$data$N_text, as.character(c(4L, 2L, 2L, 4L)))
  expect_equal(p1$data$Normalized, c(33.3333333333333, 16.6666666666667, 16.6666666666667, 33.3333333333333))
  expect_equal(p1$data$Normalized_text, c("33.3%", "16.7%", "16.7%", "33.3%"))
  expect_equal(p1$data$Class_N, c(6, 6, 6, 6))
  expect_equal(p1$data$Class_Percentage, c(66.6666666666667, 33.3333333333333, 33.3333333333333, 66.6666666666667))
  expect_equal(p1$data$Class_Percentage_text, c("66.7%", "33.3%", "33.3%", "66.7%"))
  expect_equal(p1$data$Prediction_N, c(6, 6, 6, 6))
  expect_equal(p1$data$Prediction_Percentage, c(66.6666666666667, 33.3333333333333, 33.3333333333333, 66.6666666666667))
  expect_equal(p1$data$Prediction_Percentage_text, c("66.7%", "33.3%", "33.3%", "66.7%"))


  expect_equal(length(p1$layers), 5)
  expect_equal(
    sapply(p1$layers, function(x) class(x$geom)[1]),
    c("GeomTile", "GeomText", "GeomText", "GeomText", "GeomText")
  )
  expect_equal(
    p1$labels,
    list(
      x = "Target", y = "Prediction",
      fill = "N", label = "N"
    )
  )

  expect_equal(
    p1$scales$scales[[1]]$limits,
    c(2.0, 4.8)
  )
  p1_darkest <- plot_confusion_matrix(cm[["Confusion Matrix"]][[1]], darkness = 1.0)
  expect_equal(
    p1_darkest$scales$scales[[1]]$limits,
    c(2.0, 4.0)
  )

  expect_equal(
    p1$mapping,
    structure(list(x = ~ .data$Target, y = ~ .data$Prediction, fill = ~ .data$N),
      class = "uneval"
    )
  )

  expect_true(!p1$guides$fill[[1]])

  expect_error(
    xpectr::strip_msg(plot_confusion_matrix(cm[["Confusion Matrix"]][[1]], darkness = 1.1)),
    xpectr::strip("1 assertions failed:\n * Variable 'darkness': Element 1 is not <= 1."),
    fixed = TRUE
  )
  expect_error(
    xpectr::strip_msg(plot_confusion_matrix(cm[["Confusion Matrix"]][[1]], darkness = -.1)),
    xpectr::strip("1 assertions failed:\n * Variable 'darkness': Element 0 is not >= 0."),
    fixed = TRUE
  )
})

test_that("plot_confusion_matrix() with multiclass conf mat returns expected plots", {

  # Note: These are just initial tests
  # There's probably a high number of errors it won't catch

  # TODO Check out https://github.com/r-lib/vdiffr
  # It may make testing easier and better

  xpectr::set_test_seed(1)
  targets <- c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 2, 2, 2, 2, 2, 2)
  predictions <- c(1, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 2, 2, 1, 2)

  # Create confusion matrix with default metrics
  cm <- confusion_matrix(targets, predictions)
  conf_mat <- cm[["Confusion Matrix"]][[1]]
  conf_mat[["N"]] <- round(conf_mat[["N"]] * 100 + runif(n = nrow(conf_mat), 0, 100))

  p1 <- plot_confusion_matrix(conf_mat) # , font_vertical = font(nudge_x = -.84))

  expect_equal(p1$data$Target, structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L),
    .Label = c("0", "1", "2"), class = "factor"
  ))
  expect_equal(p1$data$Prediction, structure(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
    .Label = c("0", "1", "2"), class = "factor"
  ))
  expect_equal(p1$data$N, c(427L, 237L, 57L, 291L, 420L, 90L, 194L, 266L, 363L))
  expect_equal(p1$data$N_text, as.character(c(427L, 237L, 57L, 291L, 420L, 90L, 194L, 266L, 363L)))
  expect_equal(p1$data$Normalized, c(
    18.2089552238806, 10.1066098081023, 2.43070362473348, 12.409381663113,
    17.910447761194, 3.83795309168444, 8.272921108742, 11.3432835820896,
    15.4797441364606
  ))
  expect_equal(p1$data$Normalized_text, c(
    "18.2%", "10.1%", "2.4%", "12.4%", "17.9%", "3.8%", "8.3%",
    "11.3%", "15.5%"
  ))
  expect_equal(p1$data$Class_N, c(721L, 721L, 721L, 801L, 801L, 801L, 823L, 823L, 823L))
  expect_equal(p1$data$Class_Percentage, c(
    59.2233009708738, 32.871012482663, 7.90568654646325, 36.3295880149813,
    52.4344569288389, 11.2359550561798, 23.5722964763062, 32.3207776427704,
    44.1069258809235
  ))
  expect_equal(p1$data$Class_Percentage_text, c(
    "59.2%", "32.9%", "7.9%", "36.3%",
    "52.4%", "11.2%", "23.6%", "32.3%",
    "44.1%"
  ))
  expect_equal(p1$data$Prediction_N, c(912L, 923L, 510L, 912L, 923L, 510L, 912L, 923L, 510L))
  expect_equal(p1$data$Prediction_Percentage, c(
    46.8201754385965, 25.6771397616468, 11.1764705882353, 31.9078947368421,
    45.5037919826652, 17.6470588235294, 21.2719298245614, 28.819068255688,
    71.1764705882353
  ))
  expect_equal(p1$data$Prediction_Percentage_text, c(
    "46.8%", "25.7%", "11.2%", "31.9%",
    "45.5%", "17.6%", "21.3%", "28.8%",
    "71.2%"
  ))

  expect_equal(length(p1$layers), 5)
  expect_equal(
    sapply(p1$layers, function(x) class(x$geom)[1]),
    c("GeomTile", "GeomText", "GeomText", "GeomText", "GeomText")
  )
  expect_equal(
    p1$labels,
    list(
      x = "Target", y = "Prediction",
      fill = "N", label = "N"
    )
  )

  expect_equal(
    p1$scales$scales[[1]]$limits,
    c(57, 575)
  )
  p1_darkest <- plot_confusion_matrix(conf_mat, darkness = 1.0)
  expect_equal(
    p1_darkest$scales$scales[[1]]$limits,
    c(57, 427)
  )

  expect_equal(
    p1$mapping,
    structure(list(x = ~ .data$Target, y = ~ .data$Prediction, fill = ~ .data$N),
      class = "uneval"
    )
  )

  expect_true(!p1$guides$fill[[1]])
})

test_that("font() gets updated correctly", {
  f_1 <- font(
    size = 9,
    color = "yellow",
    alpha = 0.3,
    nudge_x = 0.7,
    nudge_y = 0.2,
    angle = 91,
    family = "mono",
    fontface = "bold",
    hjust = 0.3,
    vjust = 1.9,
    lineheight = 9,
    digits = -3,
    prefix = "no_",
    suffix = "_on"
  )

  f_2 <- font(
    size = 2,
    color = "red",
    alpha = 0.9,
    nudge_x = -2.0,
    nudge_y = -0.3,
    angle = NULL,
    family = NULL,
    fontface = "plain",
    hjust = -2.3,
    vjust = 3.4,
    lineheight = 2,
    digits = 2,
    prefix = "ha_",
    suffix = "_ah"
  )

  expect_equal(
    f_1,
    list(
      size = 9, color = "yellow", alpha = 0.3, nudge_x = 0.7,
      nudge_y = 0.2, angle = 91, family = "mono", fontface = "bold",
      hjust = 0.3, vjust = 1.9, lineheight = 9, digits = -3, prefix = "no_",
      suffix = "_on"
    )
  )
  expect_equal(
    f_2,
    list(
      size = 2, color = "red", alpha = 0.9, nudge_x = -2, nudge_y = -0.3,
      angle = NULL, family = NULL, fontface = "plain", hjust = -2.3,
      vjust = 3.4, lineheight = 2, digits = 2, prefix = "ha_",
      suffix = "_ah"
    )
  )

  f_1_upd_1 <- update_font_setting(
    f_1,
    f_2
  )
  expect_equal(f_1, f_1_upd_1)

  f_1_upd_2 <- update_font_setting(
    f_1,
    f_2,
    initial_vals = list(
      size = function(x) {
        x + 2
      },
      color = function(x) {
        paste0("light", x)
      },
      alpha = function(x) {
        x * 0.5
      },
      nudge_x = function(x) {
        x * 2
      },
      nudge_y = function(x) {
        x / 2
      },
      angle = function(x) {
        x + 360
      },
      family = function(x) {
        paste0("mac_", x)
      },
      fontface = function(x) {
        paste0("way_too_", x)
      },
      hdjust = function(x) {
        abs(x)
      },
      vdjust = function(x) {
        -abs(x)
      },
      lineheight = function(x) {
        1.2
      },
      digits = function(x) {
        ifelse(x < 0, -1, x)
      },
      prefix = function(x) {
        paste0("prefix_", x)
      },
      suffix = function(x) {
        paste0(x, "_suffix")
      }
    )
  )

  expect_equal(
    f_1_upd_2,
    list(
      size = 11, color = "lightyellow", alpha = 0.15, nudge_x = 1.4,
      nudge_y = 0.1, angle = 451, family = "mac_mono", fontface = "way_too_bold",
      hjust = 0.3, vjust = 1.9, lineheight = 1.2, digits = -1,
      prefix = "prefix_no_", suffix = "_on_suffix"
    )
  )

  # Replace the NULLs in f_2 with the values from f_1
  f_2_upd_1 <- update_font_setting(
    f_2,
    f_1,
    initial_vals = list(
      angle = function(x) {
        x * 2
      }
    )
  )

  expect_equal(f_2_upd_1$angle, 91 * 2)
  expect_equal(f_2_upd_1$family, "mono")
})
