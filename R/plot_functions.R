# plot functions

# Plot the confusion matrix object
# Modified version of plot by
#   Antoine Sachet at https://stackoverflow.com/a/53612391/11832955
# params:
#   conf_mat: Confusion matrix tibble
#   palette: Color scheme.
#     Passed directly to palette in ggplot2::scale_fill_distiller
#   theme_fn: The theme to apply
plot_confusion_matrix <- function(conf_mat,
                                  palette = "Greens",
                                  theme_fn = ggplot2::theme_light){

  conf_mat %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$Target, y = .data$Prediction, fill = .data$N)) +
    ggplot2::geom_tile() +
    theme_fn() +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_distiller(palette = palette, direction = 1) +
    ggplot2::guides(fill = F) + # remove legend
    ggplot2::geom_text(ggplot2::aes(label = .data$N), color = "black")
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
