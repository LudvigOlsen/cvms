# plot functions

#' @title Density plot for a metric
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a ggplot2 object with a density plot for one of the columns in the passed data frame.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family plotting functions
#' @param Data frame with a metric column to create density plot for.
#' @param metric Name of the metric column in \code{data} to plot. (Character)
#' @param fill Color of the plotted distribution.
#' @param alpha Transparency of the distribution (0 - 1).
#' @param xlim Limits for the x-axis. Can be set to NULL.
#'
#'  E.g. \code{c(0, 1)}.
#' @param theme_fn The ggplot2 theme function to apply.
#' @return
#'  ggplot2 object.
plot_metric_density <- function(data,
                                metric = "Overall Accuracy",
                                fill = "pink",
                                alpha = 0.4,
                                theme_fn = ggplot2::theme_light,
                                xlim = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, col.names = "unique",
                               add = assert_collection)
  checkmate::assert_string(x = metric, min.chars = 1,
                           add = assert_collection)
  checkmate::assert_string(x = fill, null.ok = TRUE, # TODO test NULL works?
                           add = assert_collection)
  checkmate::assert_number(x = alpha, lower = 0, upper = 1,
                           add = assert_collection)
  checkmate::assert_function(x = theme_fn,
                             add = assert_collection)
  checkmate::assert_numeric(x = xlim, null.ok = TRUE,
                            add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Add ` ` around the metric name
  # if it's not already there
  if (substr(metric, 1, 1) != "`") {
    metric <- paste0("`", metric, "`")
  }

  # Create and return a density plot
  data %>%
    ggplot2::ggplot(ggplot2::aes_string(x = metric)) +
    ggplot2::geom_density(fill = fill, alpha = alpha) +
    ggplot2::coord_cartesian(xlim = xlim) +
    theme_fn() +
    ggplot2::labs(y = "Density")
}
