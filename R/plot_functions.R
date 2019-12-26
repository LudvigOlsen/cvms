# plot functions

#' @title Create a list of font settings for plots
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a list of font settings for plotting with cvms plotting functions.
#'
#'  NOTE: This is very experimental and will likely change.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family plotting functions
#' @param size Font size.
#' @param color Color of the text. As passed to
#'  \code{\link[ggplot2:geom_text]{ggplot2::geom_text}}.
#' @param nudge_x,nudge_y Nudge on x/y axes. As passed to
#'  \code{\link[ggplot2:geom_text]{ggplot2::geom_text}}.
#' @param angle Rotation angle. As passed to
#'  \code{\link[ggplot2:geom_text]{ggplot2::geom_text}}.
#' @param digits Number of digits to round to. If negative, no rounding will take place.
#' @param prefix A string prefix.
#' @param suffix A string suffix.
font <- function(size = NULL,
                 color = NULL,
                 nudge_x = NULL,
                 nudge_y = NULL,
                 angle = NULL,
                 digits = NULL,
                 prefix = NULL,
                 suffix = NULL) {

  # TODO Could this inherit from ggplot2::element_text?

  list(
    "size" = size,
    "color" = color,
    "nudge_x" = nudge_x,
    "nudge_y" = nudge_y,
    "angle" = angle,
    "digits" = digits,
    "prefix" = prefix,
    "suffix" = suffix
  )
}

update_font_setting <- function(settings, defaults, initial_vals=NULL){

  # If defaults not provided,
  # here are some reasonable backup defaults
  backup_defaults <-
    font(
      size = 4,
      color = "black",
      nudge_x = 0,
      nudge_y = 0,
      angle = 0,
      digits = -1,
      prefix = "",
      suffix = ""
    )

  new_settings <- list()
  for (opt in names(backup_defaults)){

    if (is.null(settings[[opt]])){

      if (opt %in% names(defaults) &&
          !is.null(defaults[[opt]])){
        new_settings[[opt]] <- defaults[[opt]]
      } else {
        new_settings[[opt]] <- backup_defaults[[opt]]
      }

    } else {
      new_settings[[opt]] <- settings[[opt]]
    }

    # Apply initial values
    if (!is.null(initial_vals) && opt %in% names(initial_vals)){
      new_settings[[opt]] <- initial_vals[[opt]](new_settings[[opt]])
    }

  }

  new_settings
}

preprocess_numeric <- function(vec, settings){
  # Don't round if digits is negative
  if (settings[["digits"]] >= 0){
    vec <- round(vec, settings[["digits"]])
  }
  paste0(settings[["prefix"]], vec, settings[["suffix"]])
}

#' @title Plot a confusion matrix
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a ggplot2 object representing a confusion matrix with counts,
#'  percentages overall, horizontal percentages and vertical percentages.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family plotting functions
#' @param conf_matrix Confusion matrix tibble with each combination of targets and predictions along with their counts.
#'
#'  E.g. for a binary classification:
#'
#'  \tabular{rrr}{
#'   \strong{Target} \tab \strong{Prediction} \tab \strong{N} \cr
#'   class_1 \tab class_1 \tab 5 \cr
#'   class_1 \tab class_2 \tab 9 \cr
#'   class_2 \tab class_1 \tab 3 \cr
#'   class_2 \tab class_2 \tab 2 \cr
#'  }
#'
#'  As created with the various evaluation functions in \code{cvms}, like
#'  \code{\link[cvms:evaluate]{evaluate()}}.
#' @param targets_col Name of column with target levels.
#' @param predictions_col Name of column with prediction levels.
#' @param counts_col Name of column with a count for each combination of the target and prediction levels.
#' @param add_normalized Normalize the counts to percentages and add to the middle of the tiles.
#' @param add_horizontal_percentage Add the horizontal percentages, i.e. how big a part of its row the tile makes up.
#' @param add_vertical_percentage Add the vertical percentages, i.e. how big a part of its column the tile makes up.
#' @param counts_on_top Switch the counts and normalized counts, such that the counts are on top. (Logical)
#' @param rotate_y_text Whether to rotate the y-axis text to be vertical instead of horizontal.
#' @param font_counts List of font settings for the counts. Can be provided with font().
#' @param font_normalized List of font settings for the normalized counts. Can be provided with font().
#' @param font_horizontal List of font settings for the horizontal percentages. Can be provided with font().
#' @param font_vertical List of font settings for the vertical percentages. Can be provided with font().
#' @param palette Color scheme. Passed directly to \code{palette} in
#'  \code{\link[ggplot2:scale_fill_distiller]{ggplot2::scale_fill_distiller}}.
#' @param darken How dark the darkest colors should be, between 0 and 1, where 1 is darkest.
#'
#'  Technically, a lower value increases the upper limit in
#'  \code{\link[ggplot2:scale_fill_distiller]{ggplot2::scale_fill_distiller}}.
#' @param theme_fn The ggplot2 theme function to apply.
#' @param place_x_axis_above Move the x-axis text to the top and reverse the levels such that
#     the "correct" diagonal goes from top left to bottom right. (Logical)
#' @details
#'  Inspired by Antoine Sachet at https://stackoverflow.com/a/53612391/11832955
#' @return
#'  A ggplot2 object representing a confusion matrix. Color intensity depends on the counts.
#' @examples
#' # Attach cvms
#' library(cvms)
#'
#' # Two classes
#'
#' # Create targets and predictions
#' targets <- c(0,1,0,1,0,1,0,1,0,1,0,1)
#' predictions <- c(1,1,0,0,0,1,1,1,0,1,0,0)
#'
#' # Create confusion matrix with default metrics
#' cm <- confusion_matrix(targets, predictions)
#' cm[["Confusion Matrix"]]
#'
#' plot_confusion_matrix(cm[["Confusion Matrix"]])
#'
#' # Three (or more) classes
#'
#' # Create targets and predictions
#' targets <- c(0,1,2,1,0,1,2,1,0,1,2,1,0)
#' predictions <- c(2,1,0,2,0,1,1,2,0,1,2,0,2)
#'
#' # Create confusion matrix with default metrics
#' cm <- confusion_matrix(targets, predictions)
#' cm[["Confusion Matrix"]]
#'
#' plot_confusion_matrix(cm[["Confusion Matrix"]])
plot_confusion_matrix <- function(conf_matrix,
                                  targets_col = "Target",
                                  predictions_col = "Prediction",
                                  counts_col = "N",
                                  add_counts = TRUE,
                                  add_normalized = TRUE,
                                  add_horizontal_percentage = TRUE,
                                  add_vertical_percentage = TRUE,
                                  counts_on_top = FALSE, # TODO
                                  palette = "Greens",
                                  theme_fn = ggplot2::theme_light,
                                  place_x_axis_above = TRUE,
                                  rotate_y_text = TRUE,
                                  font_counts = font(),
                                  font_normalized = font(),
                                  font_horizontal = font(),
                                  font_vertical = font(),
                                  darkness = 0.8){

  if (darkness<0 || darkness > 1){
    stop("'darkness' must be between 0 and 1")
  }

  # Update font settings


  font_counts <- update_font_setting(font_counts, defaults = list(
    "size" = ifelse(isTRUE(counts_on_top), 4.25, 3), "digits" = -1
  ), initial_vals = list(
    "nudge_y" = function(x) {
      x + dplyr::case_when(!isTRUE(counts_on_top) &&
                             isTRUE(add_normalized) ~ -0.15,
                           TRUE ~ 0)
    }
    )
  )

  font_normalized <- update_font_setting(font_normalized, defaults = list(
    "size" = ifelse(!isTRUE(counts_on_top), 4.25, 3), "suffix" = "%", "digits" = 2),
    initial_vals = list(
      "nudge_y" = function(x) {
        x + dplyr::case_when(isTRUE(counts_on_top) &&
                               isTRUE(add_counts) ~ -0.15,
                             TRUE ~ 0)
      }
    )
  )

  font_horizontal <- update_font_setting(font_horizontal, defaults = list(
    "size" = 2.5, "prefix" = "-- ", "suffix" = "% --", "digits" = 2
  ), initial_vals = list(
    "nudge_y" = function(x){x - 0.42}
  ))

  font_vertical <- update_font_setting(font_vertical, defaults = list(
    "size" = 2.5, "prefix" = "-- ", "suffix" = "% --", "digits" = 2
  ), initial_vals = list(
    "nudge_x" = function(x){x - 0.42},
    "angle" = function(x){x + 90}
  ))

  # Extract needed columns
  cm <- tibble::tibble("Target" = factor(as.character(conf_matrix[[targets_col]])),
                       "Prediction" = factor(as.character(conf_matrix[[predictions_col]])),
                       "N" = as.integer(conf_matrix[[counts_col]]))
  cm[["Normalized"]] <- 100 * (cm[["N"]] / sum(cm[["N"]]))

  # Prepare text versions of the numerics
  cm[["N_text"]] <- preprocess_numeric(cm[["N"]], font_counts)
  cm[["Normalized_text"]] <- preprocess_numeric(cm[["Normalized"]], font_normalized)

  # Calculate vertical percentages
  if (isTRUE(add_vertical_percentage)){
    vertical_sums <- cm %>%
      dplyr::group_by(Target) %>%
      dplyr::summarize(Class_N = sum(N))
    cm <- cm %>%
      dplyr::left_join(vertical_sums, by = "Target") %>%
      dplyr::mutate(Class_Percentage = 100 * (N / Class_N),
                    Class_Percentage_text = preprocess_numeric(
                      Class_Percentage, font_vertical))
  }

  # Calculate horizontal percentages
  if (isTRUE(add_horizontal_percentage)){
    horizontal_sums <- cm %>%
      dplyr::group_by(Prediction) %>%
      dplyr::summarize(Prediction_N = sum(N))
    cm <- cm %>%
      dplyr::left_join(horizontal_sums, by = "Prediction") %>%
      dplyr::mutate(Prediction_Percentage = 100 * (N / Prediction_N),
                    Prediction_Percentage_text = preprocess_numeric(
                      Prediction_Percentage, font_horizontal))
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
    ggplot2::labs(x = "Target",
                  y = "Prediction",
                  fill = "N",
                  label = "N") +
    ggplot2::geom_tile() +
    theme_fn() +
    ggplot2::coord_equal() +
    # Add fill colors that differ by N
    ggplot2::scale_fill_distiller(palette = palette,
                                  direction = 1,
                                  limits = color_limits) +
    # Remove the guide
    ggplot2::guides(fill = F) +
    # Rotate y-axis text
    ggplot2::theme(axis.text.y = ggplot2::element_text(angle = ifelse(isTRUE(rotate_y_text), 90, 0),
                                                       hjust = ifelse(isTRUE(rotate_y_text), 0.5, 1),
                                                       vjust = ifelse(isTRUE(rotate_y_text), 0.5, 0)))

  if (isTRUE(add_counts)){
    pl <- pl +
      # Add count labels to middle of tiles
      ggplot2::geom_text(ggplot2::aes(label = .data$N_text),
                         nudge_x = font_counts[["nudge_x"]],
                         nudge_y = font_counts[["nudge_y"]],
                         angle = font_counts[["angle"]],
                         size = font_counts[["size"]],
                         color = font_counts[["color"]])
  }

  if (isTRUE(add_normalized)){
    pl <- pl +
      # Add count and percentages labels to middle of tiles
      ggplot2::geom_text(ggplot2::aes(label = .data$Normalized_text),
                         nudge_x = font_normalized[["nudge_x"]],
                         nudge_y = font_normalized[["nudge_y"]],
                         angle = font_normalized[["angle"]],
                         size = font_normalized[["size"]],
                         color = font_normalized[["color"]])
  }


  # Place x-axis text on top of the plot
  if (isTRUE(place_x_axis_above)){
    pl <- pl +
      ggplot2::scale_x_discrete(position = "top",
                                limits = rev(levels(cm$Target)))
  }

  # Add horizontal percentages
  if (isTRUE(add_horizontal_percentage)){
    pl <- pl + ggplot2::geom_text(ggplot2::aes(label = Prediction_Percentage_text),
                                  nudge_x = font_horizontal[["nudge_x"]],
                                  nudge_y = font_horizontal[["nudge_y"]],
                                  angle = font_horizontal[["angle"]],
                                  size = font_horizontal[["size"]],
                                  color = font_horizontal[["color"]])
  }

  # Add vertical percentages
  if (isTRUE(add_vertical_percentage)){
    pl <- pl + ggplot2::geom_text(ggplot2::aes(label = Class_Percentage_text),
                                  nudge_x = font_vertical[["nudge_x"]],
                                  nudge_y = font_vertical[["nudge_y"]],
                                  angle = font_vertical[["angle"]],
                                  size = font_vertical[["size"]],
                                  color = font_vertical[["color"]])
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
