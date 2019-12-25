# plot functions

# Plot the confusion matrix object
# Modified version of plot by
#   Antoine Sachet at https://stackoverflow.com/a/53612391/11832955
# params:
#   conf_mat: Confusion matrix tibble
#   palette: Color scheme.
#     Passed directly to palette in ggplot2::scale_fill_distiller
#   theme_fn: The theme to apply
#   place_x_axis_above: Add x-axis info on top and reverse levels such that
#     the "correct" diagonal goes from top left to bottom right.
plot_confusion_matrix <- function(conf_mat,
                                  targets_col = "Target",
                                  predictions_col = "Prediction",
                                  counts_col = "N",
                                  palette = "Greens",
                                  theme_fn = ggplot2::theme_light,
                                  place_x_axis_above = TRUE,
                                  rotate_y=TRUE,
                                  add_normalized = TRUE,
                                  add_horizontal_percentage = TRUE,
                                  add_vertical_percentage = TRUE,
                                  digits=2,
                                  font_size=4,
                                  font_color="black",
                                  darkness=0.8){

  if (darkness<0 || darkness > 1){
    stop("'darkness' must be between 0 and 1")
  }

  # Extract needed columns
  cm <- tibble::tibble("Target" = factor(as.character(conf_mat[[targets_col]])),
                       "Prediction" = factor(as.character(conf_mat[[predictions_col]])),
                       "N" = as.integer(conf_mat[[counts_col]]))

  # Normalize the counts
  if (isTRUE(add_normalized)){
    cm[["Normalized"]] <- round(100 * (cm[["N"]] / sum(cm[["N"]])),
                                digits = digits)
    cm[["Label"]] <- paste0(cm[["N"]], "\n", cm[["Normalized"]], "%")
  } else {
    cm[["Label"]] <- as.character(cm[["N"]])
  }

  # Calculate vertical percentages
  if (isTRUE(add_vertical_percentage)){
    vertical_sums <- cm %>%
      dplyr::group_by(Target) %>%
      dplyr::summarize(Class_N = sum(N))
    cm <- cm %>%
      dplyr::left_join(vertical_sums, by = "Target") %>%
      dplyr::mutate(Class_Percentage = paste0(
        "-- ", round(100 * (N / Class_N), digits = digits), "% --"))
  }

  # Calculate horizontal percentages
  if (isTRUE(add_horizontal_percentage)){
    horizontal_sums <- cm %>%
      dplyr::group_by(Prediction) %>%
      dplyr::summarize(Prediction_N = sum(N))
    cm <- cm %>%
      dplyr::left_join(horizontal_sums, by = "Prediction") %>%
      dplyr::mutate(Prediction_Percentage = paste0(
        "-- ", round(100 * (N / Prediction_N), digits = digits), "% --"))
  }

  # To avoid the extremely dark colors
  # where the black font does not work that well
  # We add a bit to the range, so our max N
  # will not appear to be the extreme
  min_N <- min(cm$N)
  max_N <- max(cm$N)
  range_N <- max_N - min_N
  color_limits <- c(min_N, max_N + 10 * (1 - darkness) * (range_N / 5))

  # Create plot
  pl <- cm %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$Target,
                                 y = .data$Prediction,
                                 fill = .data$N)) +
    ggplot2::labs(x = "Target", y = "Prediction") +
    ggplot2::geom_tile() +
    theme_fn() +
    ggplot2::coord_equal() +
    # Add fill colors that differ by N
    ggplot2::scale_fill_distiller(palette = palette,
                                  direction = 1,
                                  limits = color_limits) +
    # Remove the guide
    ggplot2::guides(fill = F) +
    # Add count and percentages labels to middle of tiles
    ggplot2::geom_text(ggplot2::aes(label = Label),
                       size = font_size, color = font_color) +
    # Rotate y-axis text
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = ifelse(isTRUE(rotate_y), 90, 0),
                                                       hjust = ifelse(isTRUE(rotate_y), 0.5, 1),
                                                       vjust = ifelse(isTRUE(rotate_y), 0.5, 0)))

  # Place x-axis text on top of the plot
  if (isTRUE(place_x_axis_above)){
    pl <- pl +
      scale_x_discrete(position = "top",
                       limits = rev(levels(cm$Target)))
  }

  # Add horizontal percentages
  if (isTRUE(add_horizontal_percentage)){
    pl <- pl + ggplot2::geom_text(ggplot2::aes(label = Prediction_Percentage),
                                  color = font_color,
                                  nudge_y = -0.42, size = font_size * 0.7)
  }

  # Add vertical percentages
  if (isTRUE(add_vertical_percentage)){
    pl <- pl + ggplot2::geom_text(ggplot2::aes(label = Class_Percentage),
                                  color = font_color, angle = 90,
                                  nudge_x = -0.42, size = font_size * 0.7)
  }

  pl

}


# Plot density plot for metric
# params:
#   data: Random evaluations data frame TODO This works with other dataframes as well!!
#   metric: Name of metric column in data to plot
#   fill: Color of the plotted distribution
#   alpha: How transparent the distribution should be (0 - 1)
#   theme_fn: The theme to apply
#   xlim: Limits for the x-axis. Can be set to NULL.
plot_metric_density <- function(data,
                                metric = "Overall Accuracy",
                                fill = "pink",
                                alpha = 0.4,
                                theme_fn = ggplot2::theme_light,
                                xlim = NULL) {

  # Add ` ` around the metric name
  # if it's not already there
  if (substr(metric, 1, 1) != "`")
    metric <- paste0("`", metric, "`")

  # Create and return a density plot
  data %>%
    ggplot2::ggplot(ggplot2::aes_string(x = metric)) +
    ggplot2::geom_density(fill = fill, alpha = alpha) +
    ggplot2::coord_cartesian(xlim=xlim) +
    theme_fn() +
    ggplot2::labs(y = "Density")
}
