

#   __________________ #< e5efa5aa075de36169902b4bf6e14ce8 ># __________________
#   Plot a confusion matrix                                                 ####


#' @title Plot a confusion matrix
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a \code{\link[ggplot2:ggplot]{ggplot2}} object representing a confusion matrix with counts,
#'  overall percentages, row percentages and column percentages. An extra row and column with sum tiles and the
#'  total count can be added.
#'
#'  The confusion matrix can be created with \code{\link[cvms:evaluate]{evaluate()}}. See \code{`Examples`}.
#'
#'  While this function is intended to be very flexible (hence the large number of arguments),
#'  the defaults should work in most cases for most users. See the \code{Examples}.
#'
#'  \strong{NEW}: Our
#'  \href{https://huggingface.co/spaces/ludvigolsen/plot_confusion_matrix}{
#'  \strong{Plot Confusion Matrix} web application}
#'  allows using this function without code. Select from multiple design templates
#'  or make your own.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family plotting functions
#' @param conf_matrix Confusion matrix \code{tibble} with each combination of
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
#'  An additional \code{`sub_col`} column (\code{character}) can be specified
#'  as well. Its content will replace the bottom text (`counts` by default or
#'  `normalized` when \code{`counts_on_top`} is enabled).
#'
#'  \strong{Note}: If you supply the results from \code{\link[cvms:evaluate]{evaluate()}}
#'  or \code{\link[cvms:confusion_matrix]{confusion_matrix()}} directly,
#'  the confusion matrix \code{tibble} is extracted automatically, if possible.
#' @param target_col Name of column with target levels.
#' @param prediction_col Name of column with prediction levels.
#' @param counts_col Name of column with a count for each combination
#'  of the target and prediction levels.
#' @param sub_col Name of column with text to replace the bottom text
#'  (`counts` by default or `normalized` when \code{`counts_on_top`} is enabled).
#'
#'  It simply replaces the text, so all settings will still be called
#'  e.g. \code{`font_counts`} etc. When other settings make it so, that no
#'  bottom text is displayed (e.g. \code{`add_counts` = FALSE}),
#'  this text is not displayed either.
#' @param class_order Names of the classes in \code{`conf_matrix`} in the desired order.
#'  When \code{NULL}, the classes are ordered alphabetically.
#' @param add_sums Add tiles with the row/column sums. Also adds a total count tile. (Logical)
#'
#'  The appearance of these tiles can be specified in \code{`sums_settings`}.
#'
#'  Note: Adding the sum tiles with a palette requires the \code{ggnewscale} package.
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
#' @param add_3d_effect Add tile overlay with a very slight 3D effect.
#'  This helps separate tiles with the same intensities.
#'
#'  Note: Due to some image distortion issues in \code{`ggimage`}, this
#'  may not work properly in all cases.
#' @param diag_percentages_only Whether to only have row and column percentages in the diagonal tiles. (Logical)
#' @param rm_zero_percentages Whether to remove row and column percentages when the count is \code{0}. (Logical)
#' @param rm_zero_text Whether to remove counts and normalized percentages when the count is \code{0}. (Logical)
#' @param counts_on_top Switch the counts and normalized counts,
#'  such that the counts are on top. (Logical)
#' @param rotate_y_text Whether to rotate the y-axis text to
#'  be vertical instead of horizontal. (Logical)
#' @param font_counts \code{list} of font settings for the counts.
#'  Can be provided with \code{\link[cvms:font]{font()}}.
#' @param font_normalized \code{list} of font settings for the normalized counts.
#'  Can be provided with \code{\link[cvms:font]{font()}}.
#' @param font_row_percentages \code{list} of font settings for the row percentages.
#'  Can be provided with \code{\link[cvms:font]{font()}}.
#' @param font_col_percentages \code{list} of font settings for the column percentages.
#'  Can be provided with \code{\link[cvms:font]{font()}}.
#' @param intensity_by The measure that should control the color intensity of the tiles.
#'  Either \code{`counts`}, \code{`normalized`} or one of \code{`log counts`,
#'  `log2 counts`, `log10 counts`, `arcsinh counts`}.
#'
#'  For `normalized`, the color limits become \code{0-100} (except when
#'  \code{`intensity_lims`} are specified), why the intensities
#'  can better be compared across plots.
#'
#'  For the `log*` and `arcsinh` versions, the log/arcsinh transformed counts are used.
#'
#'  \strong{Note}: In `log*` transformed counts, 0-counts are set to `0`, why they
#'  won't be distinguishable from 1-counts.
#' @param intensity_lims A specific range of values for the color intensity of
#'  the tiles. Given as a numeric vector with \code{c(min, max)}.
#'
#'  This allows having the same intensity scale across plots for better comparison
#'  of prediction sets.
#' @param intensity_beyond_lims What to do with values beyond the
#'  \code{`intensity_lims`}. One of \code{"truncate", "grey"}.
#' @param arrow_size Size of arrow icons. (Numeric)
#'
#'  Is divided by \code{sqrt(nrow(conf_matrix))} and passed on
#'  to \code{\link[ggimage:geom_icon]{ggimage::geom_icon()}}.
#' @param arrow_nudge_from_text Distance from the percentage text to the arrow. (Numeric)
#' @param digits Number of digits to round to (percentages only).
#'  Set to a negative number for no rounding.
#'
#'  Can be set for each font individually via the \code{font_*} arguments.
#' @param palette Color scheme. Passed directly to \code{`palette`} in
#'  \code{\link[ggplot2:scale_fill_distiller]{ggplot2::scale_fill_distiller}}.
#'
#'  Try these palettes: \code{"Greens"}, \code{"Oranges"},
#'  \code{"Greys"}, \code{"Purples"}, \code{"Reds"},
#'  as well as the default \code{"Blues"}.
#'
#'  Alternatively, pass a named list with limits of a custom gradient as e.g.
#'  \code{`list("low"="#B1F9E8", "high"="#239895")`}. These are passed to
#'  \code{\link[ggplot2:scale_fill_gradient]{ggplot2::scale_fill_gradient}}.
#' @param tile_border_color Color of the tile borders. Passed as \emph{\code{`colour`}} to
#' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#' @param tile_border_size Size of the tile borders. Passed as \emph{\code{`size`}} to
#' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#' @param tile_border_linetype Linetype for the tile borders. Passed as \emph{\code{`linetype`}} to
#' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#' @param sums_settings A list of settings for the appearance of the sum tiles.
#'  Can be provided with \code{\link[cvms:sum_tile_settings]{sum_tile_settings()}}.
#' @param darkness How dark the darkest colors should be, between \code{0} and \code{1}, where \code{1} is darkest.
#'
#'  Technically, a lower value increases the upper limit in
#'  \code{\link[ggplot2:scale_fill_distiller]{ggplot2::scale_fill_distiller}}.
#' @param theme_fn The \code{ggplot2} theme function to apply.
#' @param place_x_axis_above Move the x-axis text to the top and reverse the levels such that
#'  the "correct" diagonal goes from top left to bottom right. (Logical)
#' @details
#'  Inspired by Antoine Sachet's answer at https://stackoverflow.com/a/53612391/11832955
#' @return
#'  A \code{ggplot2} object representing a confusion matrix.
#'  Color intensity depends on either the counts (default) or the overall percentages.
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
#'   "target" = c("A", "B", "A", "B", "A", "B", "A", "B",
#'                "A", "B", "A", "B", "A", "B", "A", "A"),
#'   "prediction" = c("B", "B", "A", "A", "A", "B", "B", "B",
#'                    "B", "B", "A", "B", "A", "A", "A", "A"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Evaluate predictions and create confusion matrix
#' evaluation <- evaluate(
#'   data = data,
#'   target_col = "target",
#'   prediction_cols = "prediction",
#'   type = "binomial"
#' )
#'
#' # Inspect confusion matrix tibble
#' evaluation[["Confusion Matrix"]][[1]]
#'
#' # Plot confusion matrix
#' # Supply confusion matrix tibble directly
#' plot_confusion_matrix(evaluation[["Confusion Matrix"]][[1]])
#' # Plot first confusion matrix in evaluate() output
#' plot_confusion_matrix(evaluation)
#'
#' # Add sum tiles
#' plot_confusion_matrix(evaluation, add_sums = TRUE)
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
#' evaluation <- evaluate(
#'   data = data,
#'   target_col = "target",
#'   prediction_cols = "prediction",
#'   type = "multinomial"
#' )
#'
#' # Inspect confusion matrix tibble
#' evaluation[["Confusion Matrix"]][[1]]
#'
#' # Plot confusion matrix
#' # Supply confusion matrix tibble directly
#' plot_confusion_matrix(evaluation[["Confusion Matrix"]][[1]])
#' # Plot first confusion matrix in evaluate() output
#' plot_confusion_matrix(evaluation)
#'
#' # Add sum tiles
#' plot_confusion_matrix(evaluation, add_sums = TRUE)
#'
#' # Counts only
#' plot_confusion_matrix(
#'   evaluation[["Confusion Matrix"]][[1]],
#'   add_normalized = FALSE,
#'   add_row_percentages = FALSE,
#'   add_col_percentages = FALSE
#' )
#'
#' # Change color palette to green
#' # Change theme to `theme_light`.
#' plot_confusion_matrix(
#'   evaluation[["Confusion Matrix"]][[1]],
#'   palette = "Greens",
#'   theme_fn = ggplot2::theme_light
#' )
#'
#' # Change colors palette to custom gradient
#' # with a different gradient for sum tiles
#' plot_confusion_matrix(
#'   evaluation[["Confusion Matrix"]][[1]],
#'   palette = list("low" = "#B1F9E8", "high" = "#239895"),
#'   sums_settings = sum_tile_settings(
#'     palette = list("low" = "#e9e1fc", "high" = "#BE94E6")
#'   ),
#'   add_sums = TRUE
#' )
#'
#' # The output is a ggplot2 object
#' # that you can add layers to
#' # Here we change the axis labels
#' plot_confusion_matrix(evaluation[["Confusion Matrix"]][[1]]) +
#'   ggplot2::labs(x = "True", y = "Guess")
#'
#' # Replace the bottom tile text
#' # with some information
#' # First extract confusion matrix
#' # Then add new column with text
#' cm <- evaluation[["Confusion Matrix"]][[1]]
#' cm[["Trials"]] <- c(
#'   "(8/9)", "(3/9)", "(1/9)",
#'   "(3/9)", "(7/9)", "(4/9)",
#'   "(1/9)", "(2/9)", "(8/9)"
#'  )
#'
#' # Now plot with the `sub_col` argument specified
#' plot_confusion_matrix(cm, sub_col="Trials")
#'
#' }
plot_confusion_matrix <- function(conf_matrix,
                                  target_col = "Target",
                                  prediction_col = "Prediction",
                                  counts_col = "N",
                                  sub_col = NULL,
                                  class_order = NULL,
                                  add_sums = FALSE,
                                  add_counts = TRUE,
                                  add_normalized = TRUE,
                                  add_row_percentages = TRUE,
                                  add_col_percentages = TRUE,
                                  diag_percentages_only = FALSE,
                                  rm_zero_percentages = TRUE,
                                  rm_zero_text = TRUE,
                                  add_zero_shading = TRUE,
                                  add_3d_effect = TRUE,
                                  add_arrows = TRUE,
                                  counts_on_top = FALSE,
                                  palette = "Blues",
                                  intensity_by = "counts",
                                  intensity_lims = NULL,
                                  intensity_beyond_lims = "truncate",
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
                                  sums_settings = sum_tile_settings(),
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
  checkmate::assert_string(x = target_col, min.chars = 1, add = assert_collection)
  checkmate::assert_string(x = prediction_col, min.chars = 1, add = assert_collection)
  checkmate::assert_string(x = counts_col, min.chars = 1, add = assert_collection)
  checkmate::assert_string(x = sub_col, min.chars = 1, add = assert_collection, null.ok = TRUE)
  checkmate::assert_string(x = tile_border_linetype, na.ok = TRUE, add = assert_collection)
  checkmate::assert_string(x = tile_border_color, na.ok = TRUE, add = assert_collection)
  checkmate::assert_character(x = class_order, null.ok = TRUE, any.missing = FALSE, add = assert_collection)
  checkmate::assert_string(x = intensity_by, add = assert_collection)
  checkmate::assert_string(x = intensity_beyond_lims, add = assert_collection)
  checkmate::assert_choice(x = intensity_beyond_lims, choices = c("truncate", "grey"), add = assert_collection)
  checkmate::assert_string(x = sums_settings[["intensity_by"]], null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(x = sums_settings[["intensity_beyond_lims"]], null.ok = TRUE, add = assert_collection)
  checkmate::assert_choice(x = sums_settings[["intensity_beyond_lims"]], choices = c("truncate", "grey"), null.ok = TRUE, add = assert_collection)


  checkmate::assert(
    checkmate::check_string(x = palette, na.ok = TRUE, null.ok = TRUE),
    checkmate::check_list(x = palette, types = "character", names = "unique")
  )
  if (is.list(palette)){
    checkmate::assert_names(
      x = names(palette),
      must.include = c("high", "low"),
      add = assert_collection
    )
  }

  # Flag
  checkmate::assert_flag(x = add_sums, add = assert_collection)
  checkmate::assert_flag(x = add_counts, add = assert_collection)
  checkmate::assert_flag(x = add_normalized, add = assert_collection)
  checkmate::assert_flag(x = add_row_percentages, add = assert_collection)
  checkmate::assert_flag(x = add_col_percentages, add = assert_collection)
  checkmate::assert_flag(x = add_zero_shading, add = assert_collection)
  checkmate::assert_flag(x = counts_on_top, add = assert_collection)
  checkmate::assert_flag(x = place_x_axis_above, add = assert_collection)
  checkmate::assert_flag(x = rotate_y_text, add = assert_collection)
  checkmate::assert_flag(x = diag_percentages_only, add = assert_collection)
  checkmate::assert_flag(x = rm_zero_percentages, add = assert_collection)
  checkmate::assert_flag(x = rm_zero_text, add = assert_collection)
  checkmate::assert_flag(x = add_3d_effect, add = assert_collection)

  # Function
  checkmate::assert_function(x = theme_fn, add = assert_collection)

  # Number
  checkmate::assert_number(x = darkness, lower = 0, upper = 1, add = assert_collection)
  checkmate::assert_number(x = tile_border_size, lower = 0, add = assert_collection)
  checkmate::assert_number(x = arrow_size, lower = 0, add = assert_collection)
  checkmate::assert_number(x = arrow_nudge_from_text, lower = 0, add = assert_collection)
  checkmate::assert_count(x = digits, add = assert_collection)
  checkmate::assert_numeric(
    x = intensity_lims,
    len = 2,
    null.ok = TRUE,
    any.missing = FALSE,
    unique = TRUE,
    sorted = TRUE,
    finite = TRUE,
    add = assert_collection
  )

  # List
  checkmate::assert_list(x = font_counts, names = "named", add = assert_collection)
  checkmate::assert_list(x = font_normalized, names = "named", add = assert_collection)
  checkmate::assert_list(x = font_row_percentages, names = "named", add = assert_collection)
  checkmate::assert_list(x = font_col_percentages, names = "named", add = assert_collection)
  checkmate::assert_list(x = sums_settings, names = "named", add = assert_collection)
  checkmate::reportAssertions(assert_collection)

  # Names
  checkmate::assert_names(
    x = colnames(conf_matrix),
    must.include = c(target_col, prediction_col, counts_col),
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
  checkmate::assert_names(
    x = intensity_by,
    subset.of = c("counts", "normalized", "log counts", "log2 counts", "log10 counts", "arcsinh counts"),
    add = assert_collection
  )
  if (!is.null(sums_settings[["intensity_by"]])){
    checkmate::assert_names(
      x = sums_settings[["intensity_by"]],
      subset.of = c("counts", "normalized", "log counts", "log2 counts", "log10 counts", "arcsinh counts"),
      add = assert_collection
    )
  }

  if (!is.null(class_order)){
    classes_in_data <- unique(conf_matrix[[target_col]])
    if (length(classes_in_data) != length(class_order) ||
        length(setdiff(classes_in_data, class_order)) != 0){
      assert_collection$push(
        "when 'class_order' is specified, it must contain the same levels as 'conf_matrix'.")
    }
  } else {
    class_order <- sort(unique(conf_matrix[[target_col]]))
  }

  # Check `sum_settings` `palette`
  if (!is.null(sums_settings[["palette"]])) {
    checkmate::assert(
      checkmate::check_string(x = sums_settings[["palette"]], na.ok = TRUE),
      checkmate::check_list(
        x = sums_settings[["palette"]],
        types = "character",
        names = "unique"
      )
    )
    if (is.list(sums_settings[["palette"]])) {
      checkmate::assert_names(
        x = names(sums_settings[["palette"]]),
        must.include = c("high", "low"),
        add = assert_collection
      )
    }

    if (palettes_are_equal(palette, sums_settings[["palette"]])) {
      assert_collection$push("'palette' and 'sums_settings[['palette']]' cannot be the same palette.")
    }
  }

  # Check that N are (>= 0)
  checkmate::assert_numeric(
    x = conf_matrix[[counts_col]],
    lower = 0,
    add = assert_collection
  )

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # When 'rsvg', 'ggimage' or 'ggnewscale' is missing
  user_has_rsvg <- requireNamespace("rsvg", quietly = TRUE)
  user_has_ggimage <- requireNamespace("ggimage", quietly = TRUE)
  user_has_ggnewscale <- requireNamespace("ggnewscale", quietly = TRUE)
  use_ggimage <- all(user_has_rsvg, user_has_ggimage)
  if (!isTRUE(use_ggimage)){
    if (!isTRUE(user_has_ggimage))
      warning("'ggimage' is missing. Will not plot arrows and zero-shading.")
    if (!isTRUE(user_has_rsvg))
      warning("'rsvg' is missing. Will not plot arrows and zero-shading.")
    add_arrows <- FALSE
    add_zero_shading <- FALSE
  }
  if (isTRUE(add_sums) &&
      !isTRUE(user_has_ggnewscale) &&
      is.null(sums_settings[["tile_fill"]])){
    warning("'ggnewscale' is missing. Will not use palette for sum tiles.")
    sums_settings[["tile_fill"]] <- "#F5F5F5"
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

  #### Update sum tile settings ####

  if (isTRUE(add_sums)) {

    if (!is.list(palette) &&
        palette == "Greens" &&
        is.null(sums_settings[["palette"]])) {
      sums_settings[["palette"]] <- "Blues"
    }

    # Defaults are defined in update_sum_tile_settings()
    sums_settings <- update_sum_tile_settings(
      settings = sums_settings,
      defaults = list()
    )

  }

  #### Prepare arrow images ####

  # Arrow icons
  arrow_icons <- list("up" = get_figure_path("caret_up_sharp.svg"),
                      "down" = get_figure_path("caret_down_sharp.svg"),
                      "left" = get_figure_path("caret_back_sharp.svg"),
                      "right" = get_figure_path("caret_forward_sharp.svg"))

  # Scale arrow size
  arrow_size <- arrow_size / sqrt(nrow(conf_matrix) + as.integer(isTRUE(add_sums)))

  #### Prepare dataset ####

  # Extract needed columns
  cm <- tibble::tibble(
    "Target" = factor(as.character(conf_matrix[[target_col]]), levels = class_order),
    "Prediction" = factor(as.character(conf_matrix[[prediction_col]]), levels = class_order),
    "N" = as.integer(conf_matrix[[counts_col]])
  )
  cm <- calculate_normalized(cm)

  # Prepare text versions of the numerics
  cm[["N_text"]] <- preprocess_numeric(cm[["N"]], font_counts, rm_zero_text = rm_zero_text)
  cm[["Normalized_text"]] <- preprocess_numeric(
    cm[["Normalized"]],
    font_normalized,
    rm_zero_text = rm_zero_text,
    rm_zeroes_post_rounding = FALSE # Only remove where N==0
  )

  # Set color intensity metric
  cm <- set_intensity(cm, intensity_by)

  # Get min and max intensity scores and their range
  # We need to do this before adding sums
  intensity_measures <- get_intensity_range(
    data = cm,
    intensity_by = intensity_by,
    intensity_lims = intensity_lims
  )

  # Handle intensities outside of the allow range¨
  cm$Intensity <- handle_beyond_intensity_limits(
    intensities = cm$Intensity,
    intensity_range = intensity_measures,
    intensity_beyond_lims = intensity_beyond_lims
  )

  # Add icons depending on where the tile will be in the image
  cm <- set_arrows(cm, place_x_axis_above = place_x_axis_above,
                   icons = arrow_icons)

  if (isTRUE(use_ggimage) &&
      isTRUE(add_zero_shading)){
    # Add image path for skewed lines for when there's an N=0
    cm[["image_skewed_lines"]] <- ifelse(cm[["N"]] == 0,
                                         get_figure_path("skewed_lines.svg"),
                                         get_figure_path("empty_square.svg"))
  }

  if (isTRUE(use_ggimage) &&
      isTRUE(add_3d_effect)){
    # Add image path with slight 3D effect
    cm[["image_3d"]] <- get_figure_path("square_overlay.png")
  }

  # Calculate column sums
  if (isTRUE(add_col_percentages) || isTRUE(add_sums)) {
    column_sums <- cm %>%
      dplyr::group_by(.data$Target) %>%
      dplyr::summarize(Class_N = sum(.data$N))
  }

  # Calculate row sums
  if (isTRUE(add_col_percentages) || isTRUE(add_sums)) {
    row_sums <- cm %>%
      dplyr::group_by(.data$Prediction) %>%
      dplyr::summarize(Prediction_N = sum(.data$N))
  }

  # Calculate column percentages
  if (isTRUE(add_col_percentages)) {
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
    cm <- cm %>%
      dplyr::left_join(row_sums, by = "Prediction") %>%
      dplyr::mutate(
        Prediction_Percentage = 100 * (.data$N / .data$Prediction_N),
        Prediction_Percentage_text = preprocess_numeric(
          .data$Prediction_Percentage, font_row_percentages
        )
      )
  }

  # Signal that current rows are not for the sum tiles
  cm[["is_sum"]] <- FALSE

  # Add sums
  if (isTRUE(add_sums)){

    # Create data frame with data for
    # the sum column, sum row, and total counts tile
    column_sums[["Prediction"]] <- "Total"
    row_sums[["Target"]] <-  "Total"
    column_sums <- dplyr::rename(column_sums, N = "Class_N")
    row_sums <- dplyr::rename(row_sums, N = "Prediction_N")
    column_sums <- calculate_normalized(column_sums)
    row_sums <- calculate_normalized(row_sums)
    total_count <- dplyr::tibble(
      "Target" = "Total", "Prediction" = "Total",
      "N" = sum(cm$N), "Normalized" = 100
    )
    sum_data <- dplyr::bind_rows(column_sums, row_sums, total_count)

    # Prepare text versions of the numerics
    sum_data[["N_text"]] <- preprocess_numeric(sum_data[["N"]], font_counts)
    sum_data[["Normalized_text"]] <- preprocess_numeric(sum_data[["Normalized"]], font_normalized)

    # Set total counts tile text
    sum_data[nrow(sum_data), "N_text"] <- ifelse(
      isTRUE(counts_on_top),
      as.character(sum_data[nrow(sum_data), "N"]),
      ""
    )
    sum_data[nrow(sum_data), "Normalized_text"] <- ifelse(
      isTRUE(counts_on_top), "", paste0(sum_data[nrow(sum_data), "N"]))

    sums_intensity_by <- sums_settings[["intensity_by"]]
    if (is.null(sums_intensity_by)) sums_intensity_by <- intensity_by

    sums_intensity_beyond_lims <- sums_settings[["intensity_beyond_lims"]]
    if (is.null(sums_intensity_beyond_lims)) sums_intensity_beyond_lims <- intensity_beyond_lims

    # Set color intensity metric
    sum_data <- set_intensity(sum_data, sums_intensity_by)

    # Get min and max intensity scores and their range
    # We need to do this before adding sum_data
    sums_intensity_measures <- get_intensity_range(
      data = head(sum_data, nrow(sum_data) - 1),
      intensity_by = sums_intensity_by,
      intensity_lims = sums_settings[["intensity_lims"]]
    )

    # Handle intensities outside of the allow range
    sum_data[1:(nrow(sum_data) - 1), "Intensity"] <- handle_beyond_intensity_limits(
      intensities = sum_data[1:(nrow(sum_data) - 1), "Intensity"]$Intensity,
      intensity_range=sums_intensity_measures,
      intensity_beyond_lims=sums_intensity_beyond_lims
    )

    # Get color limits
    sums_color_limits <- get_color_limits(sums_intensity_measures, darkness)

    sum_data[["image_skewed_lines"]] <- get_figure_path("empty_square.svg")

    # Set flag for whether a row is a total score
    sum_data[["is_sum"]] <- TRUE

    # Combine cm and sum_data
    cm <-  dplyr::bind_rows(cm, sum_data)

    # Set arrow icons to empty square image
    cm[cm[["Target"]] == "Total" | cm[["Prediction"]] == "Total",] <- empty_tile_percentages(
      cm[cm[["Target"]] == "Total" | cm[["Prediction"]] == "Total",])

    # Set class order and labels
    if (isTRUE(place_x_axis_above)){
      class_order <- c("Total", class_order)
    } else {
      class_order <- c(class_order, "Total")
    }
    class_labels <- class_order
    class_labels[class_labels == "Total"] <- sums_settings[["label"]]
    cm[["Target"]] <- factor(cm[["Target"]], levels = class_order, labels = class_labels)
    cm[["Prediction"]] <- factor(cm[["Prediction"]], levels = class_order, labels = class_labels)
  }

  # If sub column is specified

  if (!is.null(sub_col)) {
    # We overwrite the Normalized/N text with the sub column
    if (isTRUE(big_counts)) {
      text_removed <- cm[["Normalized_text"]] == ""
      cm[["Normalized_text"]] <- ""
      cm[seq_len(nrow(conf_matrix)), "Normalized_text"] <-
        as.character(conf_matrix[[sub_col]])
      cm[text_removed, "Normalized_text"] <- ""
    } else {
      text_removed <- cm[["N_text"]] == ""
      cm[["N_text"]] <- ""
      cm[seq_len(nrow(conf_matrix)), "N_text"] <-
        as.character(conf_matrix[[sub_col]])
      cm[text_removed, "N_text"] <- ""
    }
  }

  # Remove percentages outside the diagonal
  if (isTRUE(diag_percentages_only)) {
    cm[cm[["Target"]] != cm[["Prediction"]],] <- empty_tile_percentages(
      cm[cm[["Target"]] != cm[["Prediction"]],])
  }
  if (isTRUE(rm_zero_percentages)){
    # Remove percentages when the count is 0
    cm[cm[["N"]] == 0,] <- empty_tile_percentages(cm[cm[["N"]] == 0,])
  }

  #### Prepare for plotting ####

  # To avoid the extremely dark colors
  # where the black font does not work that well
  # We add a bit to the range, so our max intensity
  # will not appear to be the extreme
  color_limits <- get_color_limits(intensity_measures, darkness)

  #### Create plot ####

  # Create plot
  pl <- cm %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$Target,
      y = .data$Prediction,
      fill = .data$Intensity
    )) +
    ggplot2::labs(
      x = "Target",
      y = "Prediction",
      fill = ifelse(grepl("counts", intensity_by), "N", "Normalized"),
      label = "N"
    ) +
    ggplot2::geom_tile(
      colour = tile_border_color,
      linewidth = tile_border_size,
      linetype = tile_border_linetype,
      show.legend = FALSE
    ) +
    theme_fn() +
    ggplot2::coord_equal()


  # Add fill colors that differ by N
  if (is.list(palette)) {
    pl <- pl +
      ggplot2::scale_fill_gradient(low = palette[["low"]],
                                   high = palette[["high"]],
                                   limits = color_limits)
  } else {
    pl <- pl +
      ggplot2::scale_fill_distiller(palette = palette,
                                    direction = 1,
                                    limits = color_limits)
  }

  pl <- pl +
    # Remove the guide
    ggplot2::guides(fill = "none") +
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

  #### Add sum tiles ####

  if (isTRUE(add_sums)){
    if (is.null(sums_settings[["tile_fill"]])){
      pl <- pl +
        ggnewscale::new_scale_fill() +
        ggplot2::geom_tile(
          data = cm[cm[["is_sum"]] & cm[["Target"]] != cm[["Prediction"]],],
          mapping = ggplot2::aes(fill = .data$Intensity),
          colour = sums_settings[["tile_border_color"]],
          linewidth = sums_settings[["tile_border_size"]],
          linetype = sums_settings[["tile_border_linetype"]],
          show.legend = FALSE
        )

      if (is.list(sums_settings[["palette"]])) {
        pl <- pl +
          ggplot2::scale_fill_gradient(low = sums_settings[["palette"]][["low"]],
                                       high = sums_settings[["palette"]][["high"]],
                                       limits = sums_color_limits)
      } else {
        pl <- pl +
          ggplot2::scale_fill_distiller(palette = sums_settings[["palette"]],
                                        direction = 1,
                                        limits = sums_color_limits)
      }

    } else {
      pl <- pl +
        ggplot2::geom_tile(
          data = cm[cm[["is_sum"]] & cm[["Target"]] != cm[["Prediction"]],],
          fill = sums_settings[["tile_fill"]],
          colour = sums_settings[["tile_border_color"]],
          linewidth = sums_settings[["tile_border_size"]],
          linetype = sums_settings[["tile_border_linetype"]],
          show.legend = FALSE
        )
    }

    # Add special total count tile
    pl <- pl +
      ggplot2::geom_tile(
        data = cm[cm[["is_sum"]] & cm[["Target"]] == cm[["Prediction"]],],
        colour = sums_settings[["tc_tile_border_color"]],
        linewidth = sums_settings[["tc_tile_border_size"]],
        linetype = sums_settings[["tc_tile_border_linetype"]],
        fill = sums_settings[["tc_tile_fill"]]
      )
  }

  #### Add zero shading ####

  if (isTRUE(use_ggimage) &&
      isTRUE(add_zero_shading) &&
      any(cm[["N"]] == 0)){
    pl <- pl + ggimage::geom_image(
      ggplot2::aes(image = .data$image_skewed_lines),
      by = "height", size = 0.90 / sqrt(nrow(cm)))
  }

  #### Add 3D effect ####

  if (isTRUE(use_ggimage) &&
      isTRUE(add_3d_effect)){
    pl <- pl + ggimage::geom_image(
      ggplot2::aes(image = .data$image_3d),
      by = "width",
      # TODO: Size changes by number of classes and doesn't fit with the
      # squares always which simply isn't useful
      # So either we find the perfect scaling factor (the -0.01 thing here
      # but the current doesn't work)
      # or ggimage needs to be fixed
      size = 1.0 / sqrt(nrow(cm)) - 0.01 - (0.01 * as.integer(nrow(cm) <= 3)),
      nudge_x = 0.001, nudge_y = -0.001
    )
  }

  #### Add numbers to plot ####

  if (isTRUE(add_counts)) {
    # Preset the arguments for the geom_text function to avoid repetition
    text_geom <- purrr::partial(
      ggplot2::geom_text,
      size = font_counts[["size"]],
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

    # Add count labels to middle of the regular tiles
    pl <- pl +
      text_geom(
        data = cm[!cm[["is_sum"]], ],
        ggplot2::aes(label = .data$N_text),
        color = font_counts[["color"]]
      )

    # Add count labels to middle of the sum tiles
    if (isTRUE(add_sums)){
      tmp_color <- ifelse(is.null(sums_settings[["font_color"]]),
                          font_counts[["color"]],
                          sums_settings[["font_color"]])
      tmp_tc_color <- ifelse(is.null(sums_settings[["tc_font_color"]]),
                             font_counts[["color"]],
                             sums_settings[["tc_font_color"]])

      # Add count labels to middle of sum tiles
      pl <- pl +
        text_geom(
          data = cm[cm[["is_sum"]] & cm[["Target"]] != cm[["Prediction"]], ],
          ggplot2::aes(label = .data$N_text),
          color = tmp_color
        )

      # Add count label to middle of total count tile
      pl <- pl +
        text_geom(
          data = cm[cm[["is_sum"]] & cm[["Target"]] == cm[["Prediction"]], ],
          ggplot2::aes(label = .data$N_text),
          color = tmp_tc_color
        )
    }
  }

  if (isTRUE(add_normalized)) {
    # Preset the arguments for the geom_text function to avoid repetition
    text_geom <- purrr::partial(
      ggplot2::geom_text,
      size = font_normalized[["size"]],
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

    # Add percentage labels to middle of the regular tiles
    pl <- pl +
      text_geom(
        data = cm[!cm[["is_sum"]], ],
        ggplot2::aes(label = .data$Normalized_text),
        color = font_normalized[["color"]]
      )

    if (isTRUE(add_sums)){
      # Get font color for sum tiles
      # If not set, we use the same as for the other tiles
      tmp_color <- ifelse(is.null(sums_settings[["font_color"]]),
                          font_normalized[["color"]],
                          sums_settings[["font_color"]])
      tmp_tc_color <- ifelse(is.null(sums_settings[["tc_font_color"]]),
                             font_normalized[["color"]],
                             sums_settings[["tc_font_color"]])

      # Add percentage labels to middle of sum tiles
      pl <- pl +
        text_geom(
          data = cm[cm[["is_sum"]] & cm[["Target"]] != cm[["Prediction"]], ],
          ggplot2::aes(label = .data$Normalized_text),
          color = tmp_color
        )

      # Add percentage label to middle of total count tile
      pl <- pl +
        text_geom(
          data = cm[cm[["is_sum"]] & cm[["Target"]] == cm[["Prediction"]], ],
          ggplot2::aes(label = .data$Normalized_text),
          color = tmp_tc_color
        )
    }
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
