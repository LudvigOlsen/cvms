
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
#' @param counts_col Name of column with a count for each combination
#'  of the target and prediction levels.
#' @param add_counts Add the counts to the middle of the tiles. (Logical)
#' @param add_normalized Normalize the counts to percentages and
#'  add to the middle of the tiles. (Logical)
#' @param add_horizontal_percentage Add the horizontal percentages,
#'  i.e. how big a part of its row the tile makes up. (Logical)
#' @param add_vertical_percentage Add the vertical percentages,
#'  i.e. how big a part of its column the tile makes up. (Logical)
#' @param counts_on_top Switch the counts and normalized counts,
#'  such that the counts are on top. (Logical)
#' @param rotate_y_text Whether to rotate the y-axis text to
#'  be vertical instead of horizontal. (Logical)
#' @param font_counts List of font settings for the counts.
#'  Can be provided with \code{\link[cvms:font]{font()}}.
#' @param font_normalized List of font settings for the normalized counts.
#'  Can be provided with \code{\link[cvms:font]{font()}}.
#' @param font_horizontal List of font settings for the horizontal percentages.
#'  Can be provided with \code{\link[cvms:font]{font()}}.
#' @param font_vertical List of font settings for the vertical percentages.
#'  Can be provided with \code{\link[cvms:font]{font()}}.
#' @param digits Number of digits to round to (percentages only).
#'  Set to a negative number for no rounding.
#'
#'  Can be set for each font individually via the
#'  \code{font_counts}, \code{font_normalized}, \code{font_horizontal} and \code{font_vertical} arguments.
#' @param palette Color scheme. Passed directly to \code{palette} in
#'  \code{\link[ggplot2:scale_fill_distiller]{ggplot2::scale_fill_distiller}}.
#' @param darkness How dark the darkest colors should be, between 0 and 1, where 1 is darkest.
#'
#'  Technically, a lower value increases the upper limit in
#'  \code{\link[ggplot2:scale_fill_distiller]{ggplot2::scale_fill_distiller}}.
#' @param theme_fn The ggplot2 theme function to apply.
#' @param place_x_axis_above Move the x-axis text to the top and reverse the levels such that
#     the "correct" diagonal goes from top left to bottom right. (Logical)
#' @details
#'  Inspired by Antoine Sachet's answer at https://stackoverflow.com/a/53612391/11832955
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
#' plot_confusion_matrix(cm[["Confusion Matrix"]][[1]])
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
#' plot_confusion_matrix(cm[["Confusion Matrix"]][[1]])
plot_confusion_matrix <- function(conf_matrix,
                                  targets_col = "Target",
                                  predictions_col = "Prediction",
                                  counts_col = "N",
                                  add_counts = TRUE,
                                  add_normalized = TRUE,
                                  add_horizontal_percentage = TRUE,
                                  add_vertical_percentage = TRUE,
                                  counts_on_top = FALSE,
                                  palette = "Greens",
                                  theme_fn = ggplot2::theme_light,
                                  place_x_axis_above = TRUE,
                                  rotate_y_text = TRUE,
                                  digits = 1,
                                  font_counts = font(),
                                  font_normalized = font(),
                                  font_horizontal = font(),
                                  font_vertical = font(),
                                  darkness = 0.8){

  #### Check inputs ####

  if (darkness<0 || darkness > 1){
    stop("'darkness' must be between 0 and 1")
  }

  stopifnot(
    is.character(targets_col), length(targets_col) == 1,
    is.character(predictions_col), length(predictions_col) == 1,
    is.character(counts_col), length(counts_col) == 1,
    is_logical_scalar_not_na(add_counts),
    is_logical_scalar_not_na(add_normalized),
    is_logical_scalar_not_na(add_horizontal_percentage),
    is_logical_scalar_not_na(add_vertical_percentage),
    is_logical_scalar_not_na(counts_on_top),
    is_logical_scalar_not_na(place_x_axis_above),
    is_logical_scalar_not_na(rotate_y_text),
    is_wholenumber_(digits)
  )

  #### Update font settings ####

  font_top_size <- 4.3
  font_bottom_size <- 2.8

  # Font for counts
  font_counts <- update_font_setting(font_counts, defaults = list(
    "size" = ifelse(isTRUE(counts_on_top), font_top_size, font_bottom_size), "digits" = -1
  ), initial_vals = list(
    "nudge_y" = function(x) {
      x + dplyr::case_when(!isTRUE(counts_on_top) &&
                             isTRUE(add_normalized) ~ -0.16,
                           TRUE ~ 0)
    }
  )
  )

  # Font for normalized counts
  font_normalized <- update_font_setting(font_normalized, defaults = list(
    "size" = ifelse(!isTRUE(counts_on_top), font_top_size, font_bottom_size),
    "suffix" = "%", "digits" = digits),
    initial_vals = list(
      "nudge_y" = function(x) {
        x + dplyr::case_when(isTRUE(counts_on_top) &&
                               isTRUE(add_counts) ~ -0.16,
                             TRUE ~ 0)
      }
    )
  )

  # Font for horizontal percentages
  font_horizontal <- update_font_setting(font_horizontal, defaults = list(
    "size" = 2.35, "prefix" = "-- ", "suffix" = "% --",
    "digits" = digits, "alpha" = 0.85
  ), initial_vals = list(
    "nudge_y" = function(x){x - 0.43}
  ))

  # Font for vertical percentages
  font_vertical <- update_font_setting(font_vertical, defaults = list(
    "size" = 2.35, "prefix" = "-- ", "suffix" = "% --",
    "digits" = digits, "alpha" = 0.85
  ), initial_vals = list(
    "nudge_x" = function(x){x + 0.43},
    "angle" = function(x){x + 90}
  ))

  #### Prepare dataset ####

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
      dplyr::group_by(.data$Target) %>%
      dplyr::summarize(Class_N = sum(.data$N))
    cm <- cm %>%
      dplyr::left_join(vertical_sums, by = "Target") %>%
      dplyr::mutate(Class_Percentage = 100 * (.data$N / .data$Class_N),
                    Class_Percentage_text = preprocess_numeric(
                      .data$Class_Percentage, font_vertical))
  }

  # Calculate horizontal percentages
  if (isTRUE(add_horizontal_percentage)){
    horizontal_sums <- cm %>%
      dplyr::group_by(.data$Prediction) %>%
      dplyr::summarize(Prediction_N = sum(.data$N))
    cm <- cm %>%
      dplyr::left_join(horizontal_sums, by = "Prediction") %>%
      dplyr::mutate(Prediction_Percentage = 100 * (.data$N / .data$Prediction_N),
                    Prediction_Percentage_text = preprocess_numeric(
                      .data$Prediction_Percentage, font_horizontal))
  }

  #### Prepare for plotting ####

  # To avoid the extremely dark colors
  # where the black font does not work that well
  # We add a bit to the range, so our max N
  # will not appear to be the extreme
  min_N <- min(cm$N)
  max_N <- max(cm$N)
  range_N <- max_N - min_N
  color_limits <- c(min_N, max_N + 10 * (1 - darkness) * (range_N / 5))

  ##### Create plot ####

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
                         size = font_counts[["size"]],
                         color = font_counts[["color"]],
                         alpha = font_counts[["alpha"]],
                         nudge_x = font_counts[["nudge_x"]],
                         nudge_y = font_counts[["nudge_y"]],
                         angle = font_counts[["angle"]],
                         family = font_counts[["family"]],
                         fontface = font_counts[["fontface"]],
                         hjust = font_counts[["hjust"]],
                         vjust = font_counts[["vjust"]],
                         lineheight = font_counts[["lineheight"]]
      )
  }

  if (isTRUE(add_normalized)){
    pl <- pl +
      # Add count and percentages labels to middle of tiles
      ggplot2::geom_text(ggplot2::aes(label = .data$Normalized_text),
                         size = font_normalized[["size"]],
                         color = font_normalized[["color"]],
                         alpha = font_normalized[["alpha"]],
                         nudge_x = font_normalized[["nudge_x"]],
                         nudge_y = font_normalized[["nudge_y"]],
                         angle = font_normalized[["angle"]],
                         family = font_normalized[["family"]],
                         fontface = font_normalized[["fontface"]],
                         hjust = font_normalized[["hjust"]],
                         vjust = font_normalized[["vjust"]],
                         lineheight = font_normalized[["lineheight"]]
      )
  }


  # Place x-axis text on top of the plot
  if (isTRUE(place_x_axis_above)){
    pl <- pl +
      ggplot2::scale_x_discrete(position = "top",
                                limits = rev(levels(cm$Target)))
  }

  # Add horizontal percentages
  if (isTRUE(add_horizontal_percentage)){
    pl <- pl + ggplot2::geom_text(ggplot2::aes(label = .data$Prediction_Percentage_text),
                                  size = font_horizontal[["size"]],
                                  color = font_horizontal[["color"]],
                                  alpha = font_horizontal[["alpha"]],
                                  nudge_x = font_horizontal[["nudge_x"]],
                                  nudge_y = font_horizontal[["nudge_y"]],
                                  angle = font_horizontal[["angle"]],
                                  family = font_horizontal[["family"]],
                                  fontface = font_horizontal[["fontface"]],
                                  hjust = font_horizontal[["hjust"]],
                                  vjust = font_horizontal[["vjust"]],
                                  lineheight = font_horizontal[["lineheight"]])
  }

  # Add vertical percentages
  if (isTRUE(add_vertical_percentage)){
    pl <- pl + ggplot2::geom_text(ggplot2::aes(label = .data$Class_Percentage_text),
                                  size = font_vertical[["size"]],
                                  color = font_vertical[["color"]],
                                  alpha = font_vertical[["alpha"]],
                                  nudge_x = font_vertical[["nudge_x"]],
                                  nudge_y = font_vertical[["nudge_y"]],
                                  angle = font_vertical[["angle"]],
                                  family = font_vertical[["family"]],
                                  fontface = font_vertical[["fontface"]],
                                  hjust = font_vertical[["hjust"]],
                                  vjust = font_vertical[["vjust"]],
                                  lineheight = font_vertical[["lineheight"]])
  }

  pl

}
