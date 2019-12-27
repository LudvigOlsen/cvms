
#' @title Plot a confusion matrix
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a ggplot2 object representing a confusion matrix with counts,
#'  percentages overall, row percentages and column percentages.
#'
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
#' @param add_row_percentages Add the row percentages,
#'  i.e. how big a part of its row the tile makes up. (Logical)
#'
#'  By default, the row percentage is placed to the right of the tile, rotated 90 degrees.
#' @param add_col_percentages Add the column percentages,
#'  i.e. how big a part of its column the tile makes up. (Logical)
#'
#'  By default, the row percentage is placed at the bottom of the tile.
#' @param counts_on_top Switch the counts and normalized counts,
#'  such that the counts are on top. (Logical)
#' @param rotate_y_text Whether to rotate the y-axis text to
#'  be vertical instead of horizontal. (Logical)
#' @param font_counts List of font settings for the counts.
#'  Can be provided with \code{\link[cvms:font]{font()}}.
#' @param font_normalized List of font settings for the normalized counts.
#'  Can be provided with \code{\link[cvms:font]{font()}}.
#' @param font_row_percentages List of font settings for the row percentages.
#'  Can be provided with \code{\link[cvms:font]{font()}}.
#' @param font_col_percentages List of font settings for the column percentages.
#'  Can be provided with \code{\link[cvms:font]{font()}}.
#' @param digits Number of digits to round to (percentages only).
#'  Set to a negative number for no rounding.
#'
#'  Can be set for each font individually via the
#'  \code{font_counts}, \code{font_normalized}, \code{font_row_percentages} and \code{font_col_percentages} arguments.
#' @param palette Color scheme. Passed directly to \code{palette} in
#'  \code{\link[ggplot2:scale_fill_distiller]{ggplot2::scale_fill_distiller}}.
#' @param tile_border_color Color of the tile borders. Passed as \emph{\code{colour}} to
#' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#' @param tile_border_size Size of the tile borders. Passed as \emph{\code{size}} to
#' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#' @param tile_border_linetype Linetype for the tile borders. Passed as \emph{\code{linetype}} to
#' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
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
#'
#'  By default, each tile has the normalized count
#'  (overall percentage) and count in the middle, the
#'  column percentage at the bottom, and the
#'  row percentage to the right and rotated 90 degrees.
#'
#'  In the "correct" diagonal (upper left to bottom right),
#'  the column percentages are the class-level sensitivity scores,
#'  while the row percentages are the class-level positive prediction values.
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
#'
#' # Add prefix to row and column percentages
#'
#' plot_confusion_matrix(cm[["Confusion Matrix"]][[1]],
#'                       font_row_percentages = font(prefix = "\U2195 "),
#'                       font_col_percentages = font(prefix = "\U2195 "))
#'
#' # Counts only
#' plot_confusion_matrix(cm[["Confusion Matrix"]][[1]],
#'                       add_normalized = FALSE,
#'                       add_row_percentages = FALSE,
#'                       add_col_percentages = FALSE)
plot_confusion_matrix <- function(conf_matrix,
                                  targets_col = "Target",
                                  predictions_col = "Prediction",
                                  counts_col = "N",
                                  add_counts = TRUE,
                                  add_normalized = TRUE,
                                  add_row_percentages = TRUE,
                                  add_col_percentages = TRUE,
                                  counts_on_top = FALSE,
                                  palette = "Greens",
                                  theme_fn = ggplot2::theme_light,
                                  place_x_axis_above = TRUE,
                                  rotate_y_text = TRUE,
                                  digits = 1,
                                  font_counts = font(),
                                  font_normalized = font(),
                                  font_row_percentages = font(),
                                  font_col_percentages = font(),
                                  tile_border_color = NA,
                                  tile_border_size = 0.1,
                                  tile_border_linetype = "solid",
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
    is_logical_scalar_not_na(add_row_percentages),
    is_logical_scalar_not_na(add_col_percentages),
    is_logical_scalar_not_na(counts_on_top),
    is_logical_scalar_not_na(place_x_axis_above),
    is_logical_scalar_not_na(rotate_y_text),
    is_wholenumber_(digits)
  )

  #### Update font settings ####

  font_top_size <- 4.3
  font_bottom_size <- 2.8
  big_counts <- isTRUE(counts_on_top) || !isTRUE(add_normalized)

  # Font for counts
  font_counts <- update_font_setting(font_counts, defaults = list(
    "size" = ifelse(isTRUE(big_counts), font_top_size, font_bottom_size), "digits" = -1
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
    "size" = ifelse(!isTRUE(big_counts), font_top_size, font_bottom_size),
    "suffix" = "%", "digits" = digits),
    initial_vals = list(
      "nudge_y" = function(x) {
        x + dplyr::case_when(isTRUE(counts_on_top) &&
                               isTRUE(add_counts) ~ -0.16,
                             TRUE ~ 0)
      }
    )
  )

  # Font for row percentages
  font_row_percentages <- update_font_setting(font_row_percentages, defaults = list(
    "size" = 2.35, "prefix" = "",
    "suffix" = "%", "fontface" = "italic",
    "digits" = digits, "alpha" = 0.85
  ), initial_vals = list(
    "nudge_x" = function(x){x + 0.43},
    "angle" = function(x){x + 90}
  ))

  # Font for column percentages
  font_col_percentages <- update_font_setting(font_col_percentages, defaults = list(
    "size" = 2.35, "prefix" = "",
    "suffix" = "%", "fontface" = "italic",
    "digits" = digits, "alpha" = 0.85
  ), initial_vals = list(
    "nudge_y" = function(x){x - 0.43}
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

  # Calculate column percentages
  if (isTRUE(add_col_percentages)){
    column_sums <- cm %>%
      dplyr::group_by(.data$Target) %>%
      dplyr::summarize(Class_N = sum(.data$N))
    cm <- cm %>%
      dplyr::left_join(column_sums, by = "Target") %>%
      dplyr::mutate(Class_Percentage = 100 * (.data$N / .data$Class_N),
                    Class_Percentage_text = preprocess_numeric(
                      .data$Class_Percentage, font_col_percentages))
  }

  # Calculate row percentages
  if (isTRUE(add_row_percentages)){
    row_sums <- cm %>%
      dplyr::group_by(.data$Prediction) %>%
      dplyr::summarize(Prediction_N = sum(.data$N))
    cm <- cm %>%
      dplyr::left_join(row_sums, by = "Prediction") %>%
      dplyr::mutate(Prediction_Percentage = 100 * (.data$N / .data$Prediction_N),
                    Prediction_Percentage_text = preprocess_numeric(
                      .data$Prediction_Percentage, font_row_percentages))
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
    ggplot2::geom_tile(colour = tile_border_color,
                       size = tile_border_size,
                       linetype = tile_border_linetype) +
    theme_fn() +
    ggplot2::coord_equal() +
    # Add fill colors that differ by N
    ggplot2::scale_fill_distiller(palette = palette,
                                  direction = 1,
                                  limits = color_limits) +
    # Remove the guide
    ggplot2::guides(fill = F) +
    # Rotate y-axis text
    ggplot2::theme(axis.text.y = ggplot2::element_text(
      angle = ifelse(isTRUE(rotate_y_text), 90, 0),
      hjust = ifelse(isTRUE(rotate_y_text), 0.5, 1),
      vjust = ifelse(isTRUE(rotate_y_text), 0.5, 0)))


  ##### Add numbers to plot ####

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

  # Add row percentages
  if (isTRUE(add_row_percentages)){
    pl <- pl + ggplot2::geom_text(ggplot2::aes(label = .data$Prediction_Percentage_text),
                                  size = font_row_percentages[["size"]],
                                  color = font_row_percentages[["color"]],
                                  alpha = font_row_percentages[["alpha"]],
                                  nudge_x = font_row_percentages[["nudge_x"]],
                                  nudge_y = font_row_percentages[["nudge_y"]],
                                  angle = font_row_percentages[["angle"]],
                                  family = font_row_percentages[["family"]],
                                  fontface = font_row_percentages[["fontface"]],
                                  hjust = font_row_percentages[["hjust"]],
                                  vjust = font_row_percentages[["vjust"]],
                                  lineheight = font_row_percentages[["lineheight"]])
  }

  # Add column percentages
  if (isTRUE(add_col_percentages)){
    pl <- pl +
      ggplot2::geom_text(ggplot2::aes(label = .data$Class_Percentage_text),
                         size = font_col_percentages[["size"]],
                         color = font_col_percentages[["color"]],
                         alpha = font_col_percentages[["alpha"]],
                         nudge_x = font_col_percentages[["nudge_x"]],
                         nudge_y = font_col_percentages[["nudge_y"]],
                         angle = font_col_percentages[["angle"]],
                         family = font_col_percentages[["family"]],
                         fontface = font_col_percentages[["fontface"]],
                         hjust = font_col_percentages[["hjust"]],
                         vjust = font_col_percentages[["vjust"]],
                         lineheight = font_col_percentages[["lineheight"]])
  }

  pl

}
