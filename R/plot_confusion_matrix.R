

#   __________________ #< e5efa5aa075de36169902b4bf6e14ce8 ># __________________
#   Plot a confusion matrix                                                 ####


#' @title Plot a confusion matrix
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a \code{\link[ggplot2:ggplot]{ggplot2}} object representing a confusion matrix with counts,
#'  overall percentages, row percentages and column percentages.
#'
#'  The confusion matrix can be created with \code{\link[cvms:evaluate]{evaluate()}}. See \code{Examples}.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family plotting functions
#' @param conf_matrix Confusion matrix tibble with each combination of
#' targets and predictions along with their counts.
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
#'
#'  \strong{Note}: If you supply the results from \code{\link[cvms:evaluate]{evaluate()}}
#'  or \code{\link[cvms:confusion_matrix]{confusion_matrix()}} directly,
#'  the confusion matrix tibble is extracted automatically, if possible.
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
#' @param add_arrows Add the arrows to the row and col percentages. (Logical)
#'
#'  Note: Adding the arrows requires the \code{rsvg} and \code{ggimage} packages.
#' @param add_zero_shading Add image of skewed lines to zero-tiles. (Logical)
#'
#'  Note: Adding the zero-shading requires the \code{rsvg} and \code{ggimage} packages.
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
#' @param arrow_size Size of arrow icons. (Numeric)
#'
#'  Is divided by \code{sqrt(nrow(conf_matrix))} and passed on
#'  to \code{\link[ggimage:geom_icon]{ggimage::geom_icon()}}.
#' @param arrow_nudge_from_text Distance from the percentage text to the arrow. (Numeric)
#' @param digits Number of digits to round to (percentages only).
#'  Set to a negative number for no rounding.
#'
#'  Can be set for each font individually via the \code{font_*} arguments.
#' @param palette Color scheme. Passed directly to \code{palette} in
#'  \code{\link[ggplot2:scale_fill_distiller]{ggplot2::scale_fill_distiller}}.
#'
#'  Try these palettes: \code{"Greens"}, \code{"Oranges"},
#'  \code{"Greys"}, \code{"Purples"}, \code{"Reds"},
#'  as well as the default \code{"Blues"}.
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
#' @param theme_fn The \code{ggplot2} theme function to apply.
#' @param place_x_axis_above Move the x-axis text to the top and reverse the levels such that
#'  the "correct" diagonal goes from top left to bottom right. (Logical)
#' @details
#'  Inspired by Antoine Sachet's answer at https://stackoverflow.com/a/53612391/11832955
#' @return
#'  A \code{ggplot2} object representing a confusion matrix. Color intensity depends on the counts.
#'
#'  By default, each tile has the normalized count
#'  (overall percentage) and count in the middle, the
#'  column percentage at the bottom, and the
#'  row percentage to the right and rotated 90 degrees.
#'
#'  In the "correct" diagonal (upper left to bottom right, by default),
#'  the column percentages are the class-level sensitivity scores,
#'  while the row percentages are the class-level positive predictive values.
#' @examples
#' \donttest{
#' # Attach cvms
#' library(cvms)
#' library(ggplot2)
#'
#' # Two classes
#'
#' # Create targets and predictions data frame
#' data <- data.frame(
#'   "target" = c("A", "B", "A", "B", "A", "B",
#'                "A", "B", "A", "B", "A", "B"),
#'   "prediction" = c("B", "B", "A", "A", "A", "B",
#'                    "B", "B", "A", "B", "A", "A"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Evaluate predictions and create confusion matrix
#' eval <- evaluate(
#'   data = data,
#'   target_col = "target",
#'   prediction_cols = "prediction",
#'   type = "binomial"
#' )
#'
#' # Inspect confusion matrix tibble
#' eval[["Confusion Matrix"]][[1]]
#'
#' # Plot confusion matrix
#' # Supply confusion matrix tibble directly
#' plot_confusion_matrix(eval[["Confusion Matrix"]][[1]])
#' # Plot first confusion matrix in evaluate() output
#' plot_confusion_matrix(eval)
#'
#' # Three (or more) classes
#'
#' # Create targets and predictions data frame
#' data <- data.frame(
#'   "target" = c("A", "B", "C", "B", "A", "B", "C",
#'                "B", "A", "B", "C", "B", "A"),
#'   "prediction" = c("C", "B", "A", "C", "A", "B", "B",
#'                    "C", "A", "B", "C", "A", "C"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Evaluate predictions and create confusion matrix
#' eval <- evaluate(
#'   data = data,
#'   target_col = "target",
#'   prediction_cols = "prediction",
#'   type = "multinomial"
#' )
#'
#' # Inspect confusion matrix tibble
#' eval[["Confusion Matrix"]][[1]]
#'
#' # Plot confusion matrix
#' # Supply confusion matrix tibble directly
#' plot_confusion_matrix(eval[["Confusion Matrix"]][[1]])
#' # Plot first confusion matrix in evaluate() output
#' plot_confusion_matrix(eval)
#'
#' # Counts only
#' plot_confusion_matrix(
#'   eval[["Confusion Matrix"]][[1]],
#'   add_normalized = FALSE,
#'   add_row_percentages = FALSE,
#'   add_col_percentages = FALSE
#' )
#'
#' # Change color palette to green
#' # Change theme to \code{theme_light}.
#' plot_confusion_matrix(
#'   eval[["Confusion Matrix"]][[1]],
#'   palette = "Greens",
#'   theme_fn = ggplot2::theme_light
#' )
#'
#' # The output is a ggplot2 object
#' # that you can add layers to
#' # Here we change the axis labels
#' plot_confusion_matrix(eval[["Confusion Matrix"]][[1]]) +
#'   ggplot2::labs(x = "True", y = "Guess")
#' }
plot_confusion_matrix <- function(conf_matrix,
                                  targets_col = "Target",
                                  predictions_col = "Prediction",
                                  counts_col = "N",
                                  add_counts = TRUE,
                                  add_normalized = TRUE,
                                  add_row_percentages = TRUE,
                                  add_col_percentages = TRUE,
                                  add_arrows = TRUE,
                                  add_zero_shading = TRUE,
                                  counts_on_top = FALSE,
                                  palette = "Blues",
                                  theme_fn = ggplot2::theme_minimal,
                                  place_x_axis_above = TRUE,
                                  rotate_y_text = TRUE,
                                  digits = 1,
                                  font_counts = font(),
                                  font_normalized = font(),
                                  font_row_percentages = font(),
                                  font_col_percentages = font(),
                                  arrow_size = 0.048,
                                  arrow_nudge_from_text = 0.065,
                                  tile_border_color = NA,
                                  tile_border_size = 0.1,
                                  tile_border_linetype = "solid",
                                  darkness = 0.8) {

  if (length(intersect(class(conf_matrix), c("cfm_results", "eval_results"))) > 0 &&
      "Confusion Matrix" %in% colnames(conf_matrix) &&
      nrow(conf_matrix) > 0){
    if (nrow(conf_matrix) > 1){
      warning(paste0("'conf_matrix' has more than one row. Extracting first confu",
                     "sion matrix with 'conf_matrix[['Confusion Matrix']][[1]]'."))
    }
    conf_matrix <- conf_matrix[["Confusion Matrix"]][[1]]
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(
    x = conf_matrix,
    any.missing = FALSE,
    min.rows = 4,
    min.cols = 3,
    col.names = "named",
    add = assert_collection
  )
  # String
  checkmate::assert_string(x = targets_col, min.chars = 1, add = assert_collection)
  checkmate::assert_string(x = predictions_col, min.chars = 1, add = assert_collection)
  checkmate::assert_string(x = counts_col, min.chars = 1, add = assert_collection)
  checkmate::assert_string(x = tile_border_linetype, na.ok = TRUE, add = assert_collection)
  checkmate::assert_string(x = tile_border_color, na.ok = TRUE, add = assert_collection)
  checkmate::assert_string(x = palette, na.ok = TRUE, null.ok = TRUE, add = assert_collection)

  # Flag
  checkmate::assert_flag(x = add_counts, add = assert_collection)
  checkmate::assert_flag(x = add_normalized, add = assert_collection)
  checkmate::assert_flag(x = add_row_percentages, add = assert_collection)
  checkmate::assert_flag(x = add_col_percentages, add = assert_collection)
  checkmate::assert_flag(x = add_zero_shading, add = assert_collection)
  checkmate::assert_flag(x = counts_on_top, add = assert_collection)
  checkmate::assert_flag(x = place_x_axis_above, add = assert_collection)
  checkmate::assert_flag(x = rotate_y_text, add = assert_collection)

  # Function
  checkmate::assert_function(x = theme_fn, add = assert_collection)

  # Number
  checkmate::assert_number(x = darkness, lower = 0, upper = 1, add = assert_collection)
  checkmate::assert_number(x = tile_border_size, lower = 0, add = assert_collection)
  checkmate::assert_number(x = arrow_size, lower = 0, add = assert_collection)
  checkmate::assert_number(x = arrow_nudge_from_text, lower = 0, add = assert_collection)
  checkmate::assert_count(x = digits, add = assert_collection)

  # List
  checkmate::assert_list(x = font_counts, names = "named", add = assert_collection)
  checkmate::assert_list(x = font_normalized, names = "named", add = assert_collection)
  checkmate::assert_list(x = font_row_percentages, names = "named", add = assert_collection)
  checkmate::assert_list(x = font_col_percentages, names = "named", add = assert_collection)

  checkmate::reportAssertions(assert_collection)

  # Names
  checkmate::assert_names(
    x = colnames(conf_matrix),
    must.include = c(targets_col, predictions_col, counts_col),
    add = assert_collection
  )
  available_font_settings <- names(font())
  checkmate::assert_names(
    x = names(font_counts),
    subset.of = available_font_settings,
    add = assert_collection
  )
  checkmate::assert_names(
    x = names(font_normalized),
    subset.of = available_font_settings,
    add = assert_collection
  )
  checkmate::assert_names(
    x = names(font_row_percentages),
    subset.of = available_font_settings,
    add = assert_collection
  )
  checkmate::assert_names(
    x = names(font_col_percentages),
    subset.of = available_font_settings,
    add = assert_collection
  )

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # When 'rsvg' is missing
  user_has_rsvg <- requireNamespace("rsvg", quietly = TRUE)
  user_has_ggimage <- requireNamespace("ggimage", quietly = TRUE)
  use_ggimage <- all(user_has_rsvg, user_has_ggimage)
  if (isTRUE(use_ggimage)){
    if (!isTRUE(user_has_ggimage))
      warning("'ggimage' is missing. Will not plot arrows and zero-shading.")
    if (!isTRUE(user_has_rsvg))
      warning("'rsvg' is missing. Will not plot arrows and zero-shading.")
    add_arrows <- FALSE
    add_zero_shading <- FALSE
  }

  #### Update font settings ####

  font_top_size <- 4.3
  font_bottom_size <- 2.8
  big_counts <- isTRUE(counts_on_top) || !isTRUE(add_normalized)

  # Font for counts
  font_counts <- update_font_setting(font_counts, defaults = list(
    "size" = ifelse(isTRUE(big_counts), font_top_size, font_bottom_size), "digits" = -1
  ), initial_vals = list(
    "nudge_y" = function(x) {
      x + dplyr::case_when(
        !isTRUE(counts_on_top) &&
          isTRUE(add_normalized) ~ -0.16,
        TRUE ~ 0
      )
    }
  ))

  # Font for normalized counts
  font_normalized <- update_font_setting(font_normalized,
    defaults = list(
      "size" = ifelse(!isTRUE(big_counts), font_top_size, font_bottom_size),
      "suffix" = "%", "digits" = digits
    ),
    initial_vals = list(
      "nudge_y" = function(x) {
        x + dplyr::case_when(
          isTRUE(counts_on_top) &&
            isTRUE(add_counts) ~ -0.16,
          TRUE ~ 0
        )
      }
    )
  )

  # Font for row percentages
  font_row_percentages <- update_font_setting(font_row_percentages, defaults = list(
    "size" = 2.35, "prefix" = "",
    "suffix" = "%", "fontface" = "italic",
    "digits" = digits, "alpha" = 0.85
  ), initial_vals = list(
    "nudge_x" = function(x) {
      x + 0.41
    },
    "angle" = function(x) {
      x + 90
    }
  ))

  # Font for column percentages
  font_col_percentages <- update_font_setting(font_col_percentages, defaults = list(
    "size" = 2.35, "prefix" = "",
    "suffix" = "%", "fontface" = "italic",
    "digits" = digits, "alpha" = 0.85
  ), initial_vals = list(
    "nudge_y" = function(x) {
      x - 0.41
    }
  ))

  # Arrow icons
  arrow_icons <- list("up" = get_figure_path("caret_up_sharp.svg"),
                      "down" = get_figure_path("caret_down_sharp.svg"),
                      "left" = get_figure_path("caret_back_sharp.svg"),
                      "right" = get_figure_path("caret_forward_sharp.svg"))

  # Scale arrow size
  arrow_size <- arrow_size / sqrt(nrow(conf_matrix))

  #### Prepare dataset ####

  # Extract needed columns
  cm <- tibble::tibble(
    "Target" = factor(as.character(conf_matrix[[targets_col]])),
    "Prediction" = factor(as.character(conf_matrix[[predictions_col]])),
    "N" = as.integer(conf_matrix[[counts_col]])
  )
  cm[["Normalized"]] <- 100 * (cm[["N"]] / sum(cm[["N"]]))

  # Prepare text versions of the numerics
  cm[["N_text"]] <- preprocess_numeric(cm[["N"]], font_counts)
  cm[["Normalized_text"]] <- preprocess_numeric(cm[["Normalized"]], font_normalized)

  # Add icons depending on where the tile will be in the image
  cm <- set_arrows(cm, place_x_axis_above = place_x_axis_above,
                   icons = arrow_icons)

  if (isTRUE(use_ggimage) && isTRUE(add_zero_shading)){
    # Add image path for skewed lines for when there's an N=0
    cm[["image_skewed_lines"]] <- ifelse(cm[["N"]] == 0,
                                         get_figure_path("skewed_lines.svg"),
                                         get_figure_path("empty_square.svg"))
  }

  # Calculate column percentages
  if (isTRUE(add_col_percentages)) {
    column_sums <- cm %>%
      dplyr::group_by(.data$Target) %>%
      dplyr::summarize(Class_N = sum(.data$N))
    cm <- cm %>%
      dplyr::left_join(column_sums, by = "Target") %>%
      dplyr::mutate(
        Class_Percentage = 100 * (.data$N / .data$Class_N),
        Class_Percentage_text = preprocess_numeric(
          .data$Class_Percentage, font_col_percentages
        )
      )
  }

  # Calculate row percentages
  if (isTRUE(add_row_percentages)) {
    row_sums <- cm %>%
      dplyr::group_by(.data$Prediction) %>%
      dplyr::summarize(Prediction_N = sum(.data$N))
    cm <- cm %>%
      dplyr::left_join(row_sums, by = "Prediction") %>%
      dplyr::mutate(
        Prediction_Percentage = 100 * (.data$N / .data$Prediction_N),
        Prediction_Percentage_text = preprocess_numeric(
          .data$Prediction_Percentage, font_row_percentages
        )
      )
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
    ggplot2::ggplot(ggplot2::aes(
      x = .data$Target,
      y = .data$Prediction,
      fill = .data$N
    )) +
    ggplot2::labs(
      x = "Target",
      y = "Prediction",
      fill = "N",
      label = "N"
    ) +
    ggplot2::geom_tile(
      colour = tile_border_color,
      size = tile_border_size,
      linetype = tile_border_linetype
    ) +
    theme_fn() +
    ggplot2::coord_equal() +
    # Add fill colors that differ by N
    ggplot2::scale_fill_distiller(
      palette = palette,
      direction = 1,
      limits = color_limits
    ) +
    # Remove the guide
    ggplot2::guides(fill = F) +
    ggplot2::theme(
      # Rotate y-axis text
      axis.text.y = ggplot2::element_text(
        angle = ifelse(isTRUE(rotate_y_text), 90, 0),
        hjust = ifelse(isTRUE(rotate_y_text), 0.5, 1),
        vjust = ifelse(isTRUE(rotate_y_text), 0.5, 0)
      ),
      # Add margin to axis labels
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 6, 0, 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(0, 0, 6, 0)),
      axis.title.x.bottom = ggplot2::element_text(margin = ggplot2::margin(6, 0, 0, 0))
    )

  if (isTRUE(use_ggimage) &&
      isTRUE(add_zero_shading) &&
      any(cm[["N"]] == 0)){
    pl <- pl + ggimage::geom_image(
      ggplot2::aes(image=.data$image_skewed_lines),
      by = "height", size = 0.90/sqrt(nrow(cm)))
  }

  ##### Add numbers to plot ####

  if (isTRUE(add_counts)) {
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

  if (isTRUE(add_normalized)) {
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
  if (isTRUE(place_x_axis_above)) {
    pl <- pl +
      ggplot2::scale_x_discrete(
        position = "top",
        limits = rev(levels(cm$Target))
      )
  }

  # Add row percentages
  if (isTRUE(add_row_percentages)) {
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
      lineheight = font_row_percentages[["lineheight"]]
    )
  }

  # Add column percentages
  if (isTRUE(add_col_percentages)) {
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
        lineheight = font_col_percentages[["lineheight"]]
      )
  }

  #### Add arrow icons ####

  if (isTRUE(use_ggimage) &&
      isTRUE(add_col_percentages) &&
      isTRUE(add_arrows)){
    pl <- pl +
      ggimage::geom_image(
        ggplot2::aes(image = .data$down_icon),
        by = "height",
        size = arrow_size,
        nudge_x = font_col_percentages[["nudge_x"]],
        nudge_y = font_col_percentages[["nudge_y"]] - arrow_nudge_from_text
      ) +
      ggimage::geom_image(
        ggplot2::aes(image = .data$up_icon),
        by = "height",
        size = arrow_size,
        nudge_x = font_col_percentages[["nudge_x"]],
        nudge_y = font_col_percentages[["nudge_y"]] +
          arrow_nudge_from_text - (arrow_size/2)
      )
  }

  if (isTRUE(use_ggimage) &&
      isTRUE(add_row_percentages) &&
      isTRUE(add_arrows)){
    pl <- pl +
      ggimage::geom_image(
        ggplot2::aes(image = .data$right_icon),
        by = "height",
        size = arrow_size,
        nudge_x = font_row_percentages[["nudge_x"]] +
          arrow_nudge_from_text - (arrow_size / 2),
        nudge_y = font_row_percentages[["nudge_y"]]
      ) +
      ggimage::geom_image(
        ggplot2::aes(image = .data$left_icon),
        by = "height",
        size = arrow_size,
        nudge_x = font_row_percentages[["nudge_x"]] - arrow_nudge_from_text,
        nudge_y = font_row_percentages[["nudge_y"]]
      )
  }

  pl
}
