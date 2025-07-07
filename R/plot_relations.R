#'
#' plot_correlations <- function(...){
#'   plot_class_relations(..., intensity_lims=c(-1,1))
#' }
#'
#' # TODO: Add "values" to dynamic_...
#' # TODO: Allow separate border colors around max correlations in x/y dim
#' # TODO: Make example with clustering proportions (like Farhad's usecase)
#' # TODO: Add sub_col font_sub settings arg
#' # TODO: add show_value_col argument to allow specifying combinations to show text for
#'
#'
#' #   __________________ #< e5efa5aa075de36169902b4bf6e14ce8 ># __________________
#' #   Plot a confusion matrix                                                 ####
#'
#' #' @title Plot a matrix of relations between classes
#' #' @description
#' #'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#' #'
#' #'  Creates a \code{\link[ggplot2:ggplot]{ggplot2}} object representing a confusion matrix with counts,
#' #'  overall percentages, row percentages and column percentages. An extra row and column with sum tiles and the
#' #'  total count can be added.
#' #'
#' #'  The confusion matrix can be created with \code{\link[cvms:evaluate]{evaluate()}}. See \code{`Examples`}.
#' #'
#' #'  While this function is intended to be very flexible (hence the large number of arguments),
#' #'  the defaults should work in most cases for most users. See the \code{Examples}.
#' #' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' #' @export
#' #' @family plotting functions
#' #' @param relations_matrix Class relations matrix \code{tibble} with values (e.g. correlations) for all
#' #'  each combinations of \code{x-classes} and \code{y-classes}. Classes can be
#' #'  different for the two axes.
#' #'
#' #'  E.g.:
#' #'
#' #'  \tabular{rrr}{
#' #'   \strong{X} \tab \strong{Y} \tab \strong{Val} \cr
#' #'   class_1 \tab class_1 \tab 0.8 \cr
#' #'   class_1 \tab class_2 \tab 0.2 \cr
#' #'   class_2 \tab class_1 \tab -0.4 \cr
#' #'   class_2 \tab class_2 \tab 1.0 \cr
#' #'  }
#' #'
#' #'  An additional \code{`sub_col`} column (\code{character}) can be specified
#' #'  as well. Its content will be placed at the bottom of tiles.
#' #' @param x_class_col Name of column with class names for the x-axis.
#' #'  This column is used as a character vector (converted if needed).
#' #' @param y_class_col Name of column with class names for the y-axis.
#' #'  This column is used as a character vector (converted if needed).
#' #' @param value_col Name of column with a values describing the class relations.
#' #' @param sub_col Name of column with text to add to the bottom of tiles.
#' #'
#' #'  The text is added with the \code{`font_sub`} settings.
#' #' @param show_text_col Name of logical column indicating whether to show
#' #'  the text in a combination's tile.
#' #' @param x_class_order,y_class_order Names of the classes in \code{`x/y_class_col`} in the desired order.
#' #'  When \code{NULL}, the classes are ordered alphabetically.
#' #' @param add_values Add the values to the middle of the tiles. (Logical)
#' #' @param add_row_percentages Add the row percentages,
#' #'  i.e. how big a part of its row the tile makes up. (Logical)
#' #'
#' #'  By default, the row percentage is placed to the right of the tile, rotated 90 degrees.
#' #' @param add_col_percentages Add the column percentages,
#' #'  i.e. how big a part of its column the tile makes up. (Logical)
#' #'
#' #'  By default, the row percentage is placed at the bottom of the tile.
#' #' @param add_arrows Add the arrows to the row and col percentages. (Logical)
#' #'
#' #'  Note: Adding the arrows requires the \code{rsvg} and \code{ggimage} packages.
#' #' @param add_zero_shading Add image of skewed lines to zero-tiles. (Logical)
#' #'
#' #'  Note: Adding the zero-shading requires the \code{rsvg} and \code{ggimage} packages.
#' #'
#' #'  Note: For large matrices, this can be very slow. Consider turning
#' #'  off until the final plotting.
#' #' @param amount_3d_effect Amount of 3D effect (tile overlay) to add.
#' #'  Passed as whole number from \code{0} (no effect) up to \code{6} (biggest effect).
#' #'  This helps separate tiles with the same intensities.
#' #'
#' #'  Note: The overlay may not fit the tiles in many-class cases that haven't been tested.
#' #'  If the boxes do not overlap properly, simply turn it off.
#' #'
#' #'  Note: For large matrices, this can be very slow. Consider turning
#' #'  off until the final plotting.
#' #' @param rm_zero_percentages Whether to remove row and column percentages when the value is \code{0}. (Logical)
#' #' @param rm_zero_text Whether to remove text for \code{0}-values. (Logical)
#' #' @param rotate_y_text Whether to rotate the y-axis text to
#' #'  be vertical instead of horizontal. (Logical)
#' #' @param font_values \code{list} of font settings for the values.
#' #'  Can be provided with \code{\link[cvms:font]{font()}}.
#' #' @param font_subs \code{list} of font settings for the sub text (see \code{`sub_col`}).
#' #'  Can be provided with \code{\link[cvms:font]{font()}}.
#' #' @param font_row_percentages \code{list} of font settings for the row percentages.
#' #'  Can be provided with \code{\link[cvms:font]{font()}}.
#' #' @param font_col_percentages \code{list} of font settings for the column percentages.
#' #'  Can be provided with \code{\link[cvms:font]{font()}}.
#' #' @param intensity_lims A specific range of values for the color intensity of
#' #'  the tiles. Given as a numeric vector with \code{c(min, max)}.
#' #'
#' #'  This allows having the same intensity scale across plots for better comparison
#' #'  of prediction sets.
#' #' @param intensity_beyond_lims What to do with values beyond the
#' #'  \code{`intensity_lims`}. One of \code{"truncate", "grey"}.
#' #' @param dynamic_font_colors A list of settings for using dynamic font colors
#' #'  based on the value of the counts/normalized. Allows changing the font colors
#' #'  when the background tiles are too dark, etc.
#' #'  Can be provided with \code{\link[cvms:dynamic_font_color_settings]{dynamic_font_color_settings()}}.
#' #'
#' #'  Individual thresholds can be set for the different fonts/values via the
#' #'  \code{`font_*`} arguments. Specifying colors in these arguments will overwrite
#' #'  this argument (for the specific font only).
#' #'
#' #'  Specifying colors for specific fonts overrides the "all" values for those fonts.
#' #' @param arrow_size Size of arrow icons. (Numeric)
#' #'
#' #'  Is divided by \code{sqrt(nrow(relations_matrix))} and passed on
#' #'  to \code{\link[ggimage:geom_icon]{ggimage::geom_icon()}}.
#' #' @param arrow_color Color of arrow icons. One of \code{"black", "white"}.
#' #' @param arrow_nudge_from_text Distance from the percentage text to the arrow. (Numeric)
#' #' @param digits Number of digits to round to values to.
#' #'  Set to a negative number for no rounding.
#' #'
#' #'  Can be set for each font individually via the \code{font_*} arguments.
#' #' @param palette Color scheme. Passed directly to \code{`palette`} in
#' #'  \code{\link[ggplot2:scale_fill_distiller]{ggplot2::scale_fill_distiller}}.
#' #'
#' #'  Try these palettes: \code{"Greens"}, \code{"Oranges"},
#' #'  \code{"Greys"}, \code{"Purples"}, \code{"Reds"},
#' #'  as well as the default \code{"Blues"}.
#' #'
#' #'  Alternatively, pass a named list with limits of a custom gradient as e.g.
#' #'  \code{`list("low"="#B1F9E8", "high"="#239895")`}. These are passed to
#' #'  \code{\link[ggplot2:scale_fill_gradient]{ggplot2::scale_fill_gradient}}.
#' #' @param tile_border_color Color of the tile borders. Passed as \emph{\code{`colour`}} to
#' #' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#' #' @param tile_border_size Size of the tile borders. Passed as \emph{\code{`size`}} to
#' #' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#' #' @param tile_border_linetype Linetype for the tile borders. Passed as \emph{\code{`linetype`}} to
#' #' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#' #' @param darkness How dark the darkest colors should be, between \code{0} and \code{1}, where \code{1} is darkest.
#' #'
#' #'  Technically, a lower value increases the upper limit in
#' #'  \code{\link[ggplot2:scale_fill_distiller]{ggplot2::scale_fill_distiller}}.
#' #' @param theme_fn The \code{ggplot2} theme function to apply.
#' #' @param place_x_axis_above Move the x-axis text to the top and reverse the levels. (Logical)
#' #' @return
#' #'  A \code{ggplot2} object representing the class relations (like a correlation matrix plot).
#' #'  Color intensity depends on either the values.
#' plot_class_relations <- function(relations_matrix,
#'   x_class_col,
#'   y_class_col,
#'   value_col,
#'   sub_col = NULL,
#'   show_text_col = NULL,
#'   add_aggs = FALSE,
#'   # Like sums but you can specify the stats? E.g. average/sum/max/min/abs(max)?
#'   x_class_order = NULL,
#'   y_class_order = NULL,
#'   add_values = TRUE,
#'   add_row_percentages = FALSE,
#'   add_col_percentages = FALSE,
#'   # diag_percentages_only = FALSE, # classes can differ
#'   # diag_only = FALSE,
#'   rm_zero_percentages = TRUE,
#'   rm_zero_text = TRUE,
#'
#'   # Expensive with high dims
#'   add_zero_shading = FALSE,
#'   amount_3d_effect = 0,
#'
#'   add_arrows = TRUE,
#'   palette = "Blues",
#'   intensity_lims = NULL,
#'   intensity_beyond_lims = "truncate",
#'   theme_fn = ggplot2::theme_minimal,
#'   place_x_axis_above = FALSE,
#'   rotate_y_text = FALSE,
#'   digits = 2,
#'   font_values = font(),
#'   font_subs = font(),
#'   font_row_percentages = font(),
#'   font_col_percentages = font(),
#'   dynamic_font_colors = dynamic_font_color_settings(),
#'   arrow_size = 0.048,
#'   arrow_color = "black",
#'   arrow_nudge_from_text = 0.065,
#'   tile_border_color = "#fff",
#'   tile_border_size = 1,
#'   tile_border_linetype = "solid",
#'   aggs_settings = list(),
#'   # TODO make helper function
#'   darkness = 0.8) {
#'
#'   # Check arguments ####
#'   assert_collection <- checkmate::makeAssertCollection()
#'   checkmate::assert_data_frame(
#'     x = relations_matrix,
#'     any.missing = FALSE,
#'     min.rows = 1,
#'     min.cols = 3,
#'     col.names = "named",
#'     add = assert_collection
#'   )
#'   # String
#'   checkmate::assert_string(x = x_class_col, min.chars = 1, add = assert_collection)
#'   checkmate::assert_string(x = y_class_col, min.chars = 1, add = assert_collection)
#'   checkmate::assert_string(x = value_col, min.chars = 1, add = assert_collection)
#'   checkmate::assert_string(x = sub_col, min.chars = 1, add = assert_collection, null.ok = TRUE)
#'   checkmate::assert_string(x = tile_border_linetype, na.ok = TRUE, add = assert_collection)
#'   checkmate::assert_string(x = tile_border_color, na.ok = TRUE, add = assert_collection)
#'   checkmate::assert_character(x = x_class_order, null.ok = TRUE, any.missing = FALSE, add = assert_collection)
#'   checkmate::assert_character(x = y_class_order, null.ok = TRUE, any.missing = FALSE, add = assert_collection)
#'   checkmate::assert_string(x = intensity_beyond_lims, add = assert_collection)
#'   checkmate::assert_choice(x = intensity_beyond_lims, choices = c("truncate", "grey"), add = assert_collection)
#'   checkmate::assert_string(x = aggs_settings[["intensity_beyond_lims"]], null.ok = TRUE, add = assert_collection)
#'   checkmate::assert_choice(x = aggs_settings[["intensity_beyond_lims"]], choices = c("truncate", "grey"), null.ok = TRUE, add = assert_collection)
#'   checkmate::assert_choice(x = arrow_color, choices = c("black", "white"),add = assert_collection)
#'
#'   checkmate::assert(
#'     checkmate::check_string(x = palette, na.ok = TRUE, null.ok = TRUE),
#'     checkmate::check_list(x = palette, types = "character", names = "unique")
#'   )
#'   if (is.list(palette)){
#'     checkmate::assert_names(
#'       x = names(palette),
#'       must.include = c("high", "low"),
#'       subset.of = c("high", "mid", "low"),
#'       add = assert_collection
#'     )
#'   }
#'
#'   # Flag
#'   checkmate::assert_flag(x = add_aggs, add = assert_collection)
#'   checkmate::assert_flag(x = add_values, add = assert_collection)
#'   checkmate::assert_flag(x = add_row_percentages, add = assert_collection)
#'   checkmate::assert_flag(x = add_col_percentages, add = assert_collection)
#'   checkmate::assert_flag(x = add_zero_shading, add = assert_collection)
#'   checkmate::assert_flag(x = place_x_axis_above, add = assert_collection)
#'   checkmate::assert_flag(x = rotate_y_text, add = assert_collection)
#'   # checkmate::assert_flag(x = diag_percentages_only, add = assert_collection)
#'   checkmate::assert_flag(x = rm_zero_percentages, add = assert_collection)
#'   checkmate::assert_flag(x = rm_zero_text, add = assert_collection)
#'   checkmate::assert_choice(x = amount_3d_effect, choices = 0:6, add = assert_collection)
#'
#'   # Function
#'   checkmate::assert_function(x = theme_fn, add = assert_collection)
#'
#'   # Number
#'   checkmate::assert_number(x = darkness, lower = 0, upper = 1, add = assert_collection)
#'   checkmate::assert_number(x = tile_border_size, lower = 0, add = assert_collection)
#'   checkmate::assert_number(x = arrow_size, lower = 0, add = assert_collection)
#'   checkmate::assert_number(x = arrow_nudge_from_text, lower = 0, add = assert_collection)
#'   checkmate::assert_count(x = digits, add = assert_collection)
#'   checkmate::assert_numeric(
#'     x = intensity_lims,
#'     len = 2, #TODO should this have a midpoint option? Perhaps base intensity on abs value around midpoint when specified?
#'     null.ok = TRUE,
#'     any.missing = FALSE,
#'     unique = TRUE,
#'     sorted = TRUE,
#'     finite = TRUE,
#'     add = assert_collection
#'   )
#'
#'   # List
#'   checkmate::assert_list(x = font_values, names = "named", add = assert_collection)
#'   checkmate::assert_list(x = font_subs, names = "named", add = assert_collection)
#'   checkmate::assert_list(x = font_row_percentages, names = "named", add = assert_collection)
#'   checkmate::assert_list(x = font_col_percentages, names = "named", add = assert_collection)
#'   checkmate::assert_list(x = aggs_settings, names = "named", add = assert_collection)
#'   checkmate::assert_list(x = dynamic_font_colors, names = "named", null.ok = TRUE, add = assert_collection)
#'   checkmate::reportAssertions(assert_collection)
#'
#'   # Names
#'   checkmate::assert_names(
#'     x = colnames(relations_matrix),
#'     must.include = c(x_class_col, y_class_col, value_col),
#'     add = assert_collection
#'   )
#'   available_font_settings <- names(font())
#'   checkmate::assert_names(
#'     x = names(font_values),
#'     subset.of = available_font_settings,
#'     add = assert_collection
#'   )
#'   checkmate::assert_names(
#'     x = names(font_subs),
#'     subset.of = available_font_settings,
#'     add = assert_collection
#'   )
#'   checkmate::assert_names(
#'     x = names(font_row_percentages),
#'     subset.of = available_font_settings,
#'     add = assert_collection
#'   )
#'   checkmate::assert_names(
#'     x = names(font_col_percentages),
#'     subset.of = available_font_settings,
#'     add = assert_collection
#'   )
#'   checkmate::assert_names(
#'     x = names(dynamic_font_colors),
#'     subset.of = names(dynamic_font_color_settings()),
#'     add = assert_collection
#'   )
#'
#'   # Convert x and y classes to character
#'   relations_matrix[[x_class_col]] <- as.character(relations_matrix[[x_class_col]])
#'   relations_matrix[[y_class_col]] <- as.character(relations_matrix[[y_class_col]])
#'
#'   if (!is.null(x_class_order)) {
#'     x_classes_in_data <- unique(relations_matrix[[x_class_col]])
#'
#'     if (length(x_classes_in_data) != length(x_class_order) ||
#'         length(setdiff(x_classes_in_data, x_class_order)) != 0) {
#'       assert_collection$push(
#'         "when 'x_class_order' is specified, it must contain the same levels as 'relations_matrix[[x_class_col]]'."
#'       )
#'     }
#'   } else {
#'     x_class_order <- sort(unique(relations_matrix[[x_class_col]]))
#'   }
#'
#'   if (!is.null(y_class_order)) {
#'     y_classes_in_data <- unique(relations_matrix[[y_class_col]])
#'
#'     if (length(y_classes_in_data) != length(y_class_order) ||
#'         length(setdiff(y_classes_in_data, y_class_order)) != 0) {
#'       assert_collection$push(
#'         "when 'y_class_order' is specified, it must contain the same levels as 'relations_matrix[[y_class_col]]'."
#'       )
#'     }
#'   } else {
#'     y_class_order <- sort(unique(relations_matrix[[y_class_col]]))
#'   }
#'
#'   # Check `sum_settings` `palette`
#'   if (!is.null(aggs_settings[["palette"]])) {
#'     checkmate::assert(
#'       checkmate::check_string(x = aggs_settings[["palette"]], na.ok = TRUE),
#'       checkmate::check_list(
#'         x = aggs_settings[["palette"]],
#'         types = "character",
#'         names = "unique"
#'       )
#'     )
#'     if (is.list(aggs_settings[["palette"]])) {
#'       checkmate::assert_names(
#'         x = names(aggs_settings[["palette"]]),
#'         must.include = c("high", "low"),
#'         subset.of = c("high", "mid", "low"),
#'         add = assert_collection
#'       )
#'     }
#'
#'     if (palettes_are_equal(palette, aggs_settings[["palette"]])) {
#'       assert_collection$push("'palette' and 'aggs_settings[['palette']]' cannot be the same palette.")
#'     }
#'
#'   }
#'
#'   checkmate::assert_numeric(
#'     x = relations_matrix[[value_col]],
#'     add = assert_collection
#'   )
#'
#'   checkmate::reportAssertions(assert_collection)
#'   # End of argument checks ####
#'
#'   # When 'rsvg', 'ggimage' or 'ggnewscale' is missing
#'   img_pkg_checks <- check_gg_image_packages(
#'     add_arrows=add_arrows,
#'     add_zero_shading=add_zero_shading
#'   )
#'   user_has_rsvg <- img_pkg_checks[["user_has_rsvg"]]
#'   user_has_ggimage <- img_pkg_checks[["user_has_ggimage"]]
#'   user_has_ggnewscale <- img_pkg_checks[["user_has_ggnewscale"]]
#'   use_ggimage <- img_pkg_checks[["use_ggimage"]]
#'   add_arrows <- img_pkg_checks[["add_arrows"]]
#'   add_zero_shading <- img_pkg_checks[["add_zero_shading"]]
#'
#'   if (isTRUE(add_aggs) &&
#'       !isTRUE(user_has_ggnewscale) &&
#'       is.null(aggs_settings[["tile_fill"]])){
#'     warning("'ggnewscale' is missing. Will not use palette for agg tiles.")
#'     aggs_settings[["tile_fill"]] <- "#F5F5F5"
#'   }
#'
#'   #### Update font settings ####
#'
#'   font_top_size <- 4.3
#'   font_bottom_size <- 2.8
#'   big_vals <- TRUE # always put on top (but keep logic for now)
#'   values_on_top <- TRUE
#'   colors_specified <- list()
#'
#'   # Font for values
#'   colors_specified[["values"]] <- !is.null(font_values[["color"]])
#'   font_values <- update_font_setting(
#'     font_values,
#'     defaults = list(
#'       "size" = font_top_size,
#'       "digits" = digits
#'     ),
#'     initial_vals = list(
#'       "nudge_y" = function(x) {
#'         x + dplyr::case_when(!isTRUE(values_on_top) &&
#'             isTRUE(add_normalized) ~ -0.16,
#'           TRUE ~ 0)
#'       }
#'     )
#'   )
#'
#'   # Font for sub texts
#'   colors_specified[["subs"]] <- !is.null(font_subs[["color"]])
#'   font_subs <- update_font_setting(
#'     font_subs,
#'     defaults = list(
#'       "size" = font_bottom_size,
#'       "suffix" = "%",
#'       "digits" = digits
#'     ),
#'     initial_vals = list(
#'       "nudge_y" = function(x) {
#'         x + dplyr::case_when(isTRUE(values_on_top) &&
#'             isTRUE(add_values) ~ -0.16, TRUE ~ 0)
#'       }
#'     )
#'   )
#'
#'   # Font for row percentages
#'   colors_specified[["row_percentages"]] <- !is.null(font_row_percentages[["color"]])
#'   font_row_percentages <- update_font_setting(
#'     font_row_percentages,
#'     defaults = list(
#'       "size" = 2.35,
#'       "prefix" = "",
#'       "suffix" = "%",
#'       "fontface" = "italic",
#'       "digits" = digits,
#'       "alpha" = 0.85
#'     ),
#'     initial_vals = list(
#'       "nudge_x" = function(x) {
#'         x + 0.41
#'       },
#'       "angle" = function(x) {
#'         x + 90
#'       }
#'     )
#'   )
#'
#'   # Font for column percentages
#'   colors_specified[["col_percentages"]] <- !is.null(font_col_percentages[["color"]])
#'   font_col_percentages <- update_font_setting(
#'     font_col_percentages,
#'     defaults = list(
#'       "size" = 2.35,
#'       "prefix" = "",
#'       "suffix" = "%",
#'       "fontface" = "italic",
#'       "digits" = digits,
#'       "alpha" = 0.85
#'     ),
#'     initial_vals = list(
#'       "nudge_y" = function(x) {
#'         x - 0.41
#'       }
#'     )
#'   )
#'
#'   #### Update agg tile settings ####
#'
#'   if (isTRUE(add_aggs)) {
#'
#'     if (!is.list(palette) &&
#'         palette == "Greens" &&
#'         is.null(aggs_settings[["palette"]])) {
#'       aggs_settings[["palette"]] <- "Blues"
#'     }
#'
#'     # Defaults are defined in update_sum_tile_settings()
#'     aggs_settings <- update_sum_tile_settings(
#'       settings = aggs_settings,
#'       defaults = list()
#'     )
#'
#'   }
#'
#'   #### Prepare dataset ####
#'
#'   # Extract needed columns
#'   cm <- tibble::tibble(
#'     "x" = factor(as.character(relations_matrix[[x_class_col]]), levels = x_class_order[x_class_order %in% unique(relations_matrix[[x_class_col]])]),
#'     "y" = factor(as.character(relations_matrix[[y_class_col]]), levels = y_class_order[y_class_order %in% unique(relations_matrix[[y_class_col]])]),
#'     "N" = as.numeric(relations_matrix[[value_col]])
#'   )
#'
#'   # Prepare text versions of the numerics
#'   cm[["N_text"]] <- preprocess_numeric(cm[["N"]], font_values, rm_zero_text = rm_zero_text)
#'
#'   # Calculate column sums
#'   if (isTRUE(add_col_percentages) || isTRUE(add_aggs)) {
#'     column_aggs <- cm %>%
#'       dplyr::group_by(.data$x) %>%
#'       dplyr::summarize(x_N = aggs_settings[["fn"]](.data$N))
#'   }
#'
#'   # Calculate row sums
#'   if (isTRUE(add_col_percentages) || isTRUE(add_aggs)) {
#'     row_aggs <- cm %>%
#'       dplyr::group_by(.data$y) %>%
#'       dplyr::summarize(y_N = aggs_settings[["fn"]](.data$N))
#'   }
#'
#'   # Calculate column percentages
#'   if (isTRUE(add_col_percentages)) {
#'     cm <- cm %>%
#'       dplyr::left_join(column_aggs, by = "x") %>%
#'       dplyr::mutate(
#'         x_percentage = 100 * (.data$N / .data$x_N),
#'         x_percentage_text = preprocess_numeric(
#'           .data$x_percentage, font_col_percentages
#'         )
#'       )
#'   }
#'
#'   # Calculate row percentages
#'   if (isTRUE(add_row_percentages)) {
#'     cm <- cm %>%
#'       dplyr::left_join(row_aggs, by = "y") %>%
#'       dplyr::mutate(
#'         y_percentage = 100 * (.data$N / .data$y_N),
#'         y_percentage_text = preprocess_numeric(
#'           .data$y_percentage, font_row_percentages
#'         )
#'       )
#'   }
#'
#'   # Reuse intensity functions where "counts" is the equivalent
#'   intensity_by <- "counts"
#'
#'   # Set color intensity metric
#'   cm <- set_intensity(data = cm, intensity_by = intensity_by)
#'
#'   # Get min and max intensity scores and their range
#'   # We need to do this before adding aggs
#'   intensity_measures <- get_intensity_range(
#'     data = cm,
#'     intensity_by = intensity_by,
#'     intensity_lims = intensity_lims
#'   )
#'
#'   # Handle intensities outside of the allow range
#'   cm$Intensity <- handle_beyond_intensity_limits(
#'     intensities = cm$Intensity,
#'     intensity_range = intensity_measures,
#'     intensity_beyond_lims = intensity_beyond_lims
#'   )
#'
#'   # Calculate number of tiles to size by
#'   num_y_classes <- length(unique(cm[["y"]])) + as.integer(isTRUE(add_aggs))
#'
#'   # Prepare dynamic font / arrow color settings
#'   all_dynamic_font_color_settings <- NULL
#'   if (!is.null(dynamic_font_colors[["threshold"]])){
#'     all_dynamic_font_color_settings <- c(dynamic_font_colors, value_col = "N")
#'     all_dynamic_font_color_settings[["already_specified_for"]] <- colors_specified
#'   }
#'
#'   # Arrow icons
#'   arrow_icons <- make_arrow_paths(arrow_color)
#'
#'   # Scaling arrow size
#'   arrow_size <- arrow_size / num_y_classes
#'
#'   # Add icons depending on where the tile will be in the image
#'   cm <- set_arrows(
#'     cm=cm,
#'     col_names = c("x","y"),
#'     place_x_axis_above = place_x_axis_above,
#'     icons = arrow_icons,
#'     arrow_color=arrow_color,
#'     all_dynamic_font_color_settings=all_dynamic_font_color_settings
#'   )
#'
#'   if (isTRUE(use_ggimage) &&
#'       isTRUE(add_zero_shading)){
#'     # Add image path for skewed lines for when there's an N=0
#'     cm[["image_skewed_lines"]] <- ifelse(cm[["N"]] == 0,
#'       get_figure_path("skewed_lines.svg"),
#'       get_figure_path("empty_square.svg"))
#'   }
#'
#'   # Signal that current rows are not for the sum tiles
#'   cm[["is_agg"]] <- FALSE
#'
#'   # Add sums
#'   if (isTRUE(add_aggs)){
#'
#'     # Create data frame with data for
#'     # the agg column, agg row, and total agg tile
#'     column_aggs[["y"]] <- "Total"
#'     row_aggs[["x"]] <-  "Total"
#'     column_aggs <- dplyr::rename(column_aggs, N = "x_N")
#'     row_aggs <- dplyr::rename(row_aggs, N = "y_N")
#'     total_count <- dplyr::tibble(
#'       "x" = "Total",
#'       "y" = "Total",
#'       "N" = aggs_settings[["total_fn"]](cm$N)
#'     )
#'     aggs_data <- dplyr::bind_rows(column_aggs, row_aggs, total_count)
#'
#'     # Prepare text versions of the numerics
#'     aggs_data[["N_text"]] <- preprocess_numeric(aggs_data[["N"]], font_values)
#'
#'     # Set total agg tile text
#'     aggs_data[nrow(aggs_data), "N_text"] <- as.character(aggs_data[nrow(aggs_data), "N"])
#'
#'     # The aggregation is called N, so "counts" work here
#'     aggs_intensity_by <- intensity_by
#'
#'     aggs_intensity_beyond_lims <- aggs_settings[["intensity_beyond_lims"]]
#'     if (is.null(aggs_intensity_beyond_lims)) aggs_intensity_beyond_lims <- intensity_beyond_lims
#'
#'     # Set color intensity metric
#'     aggs_data <- set_intensity(aggs_data, aggs_intensity_by)
#'
#'     # Get min and max intensity scores and their range
#'     # We need to do this before adding aggs_data
#'     aggs_intensity_measures <- get_intensity_range(
#'       data = head(aggs_data, nrow(aggs_data) - 1),
#'       intensity_by = aggs_intensity_by,
#'       intensity_lims = aggs_settings[["intensity_lims"]]
#'     )
#'
#'     # Handle intensities outside of the allow range
#'     aggs_data[1:(nrow(aggs_data) - 1), "Intensity"] <- handle_beyond_intensity_limits(
#'       intensities = aggs_data[1:(nrow(aggs_data) - 1), "Intensity"]$Intensity,
#'       intensity_range=aggs_intensity_measures,
#'       intensity_beyond_lims=aggs_intensity_beyond_lims
#'     )
#'
#'     # Get color limits
#'     aggs_color_limits <- get_color_limits(aggs_intensity_measures, darkness)
#'
#'     aggs_data[["image_skewed_lines"]] <- get_figure_path("empty_square.svg")
#'
#'     # Set flag for whether a row is a total score
#'     aggs_data[["is_agg"]] <- TRUE
#'
#'     # Combine cm and aggs_data
#'     cm <-  dplyr::bind_rows(cm, aggs_data)
#'
#'     # Set arrow icons to empty square image
#'     cm[cm[["x"]] == "Total" | cm[["y"]] == "Total",] <- empty_tile_percentages(
#'       cm[cm[["x"]] == "Total" | cm[["y"]] == "Total",])
#'
#'     # Set class order and labels
#'     if (isTRUE(place_x_axis_above)) {
#'       x_class_order <- c("Total", x_class_order)
#'       y_class_order <- c("Total", y_class_order)
#'     } else {
#'       x_class_order <- c(x_class_order, "Total")
#'       y_class_order <- c(y_class_order, "Total")
#'     }
#'
#'     x_class_labels <- x_class_order
#'     y_class_labels <- y_class_order
#'     x_class_labels[x_class_labels == "Total"] <- aggs_settings[["label"]]
#'     y_class_labels[y_class_labels == "Total"] <- aggs_settings[["label"]]
#'
#'     cm[["x"]] <- factor(
#'       cm[["x"]],
#'       levels = x_class_order[x_class_order %in% unique(cm[["x"]])],
#'       labels = x_class_labels
#'     )
#'     cm[["y"]] <- factor(
#'       cm[["y"]],
#'       levels = class_order[class_order %in% unique(cm[["y"]])],
#'       labels = y_class_labels
#'     )
#'   }
#'
#'   # Assign 3D effect image
#'   cm <- add_3d_path(cm = cm,
#'     amount_3d_effect = amount_3d_effect,
#'     use_ggimage = use_ggimage)
#'
#'
#'   # If sub column is specified
#'
#'   # TODO: Use "sub_text" instead?
#'   if (!is.null(sub_col)) {
#'     # We overwrite the Normalized/N text with the sub column
#'     if (isTRUE(big_vals)) {
#'       text_removed <- cm[["Normalized_text"]] == ""
#'       cm[["Normalized_text"]] <- ""
#'       cm[seq_len(nrow(relations_matrix)), "Normalized_text"] <-
#'         as.character(relations_matrix[[sub_col]])
#'       cm[text_removed, "Normalized_text"] <- ""
#'     } else {
#'       text_removed <- cm[["N_text"]] == ""
#'       cm[["N_text"]] <- ""
#'       cm[seq_len(nrow(relations_matrix)), "N_text"] <-
#'         as.character(relations_matrix[[sub_col]])
#'       cm[text_removed, "N_text"] <- ""
#'     }
#'   }
#'
#'   # # Remove percentages outside the diagonal
#'   # if (isTRUE(diag_percentages_only)) {
#'   #   cm[as.character(cm[["x"]]) != as.character(cm[["y"]]),] <- empty_tile_percentages(
#'   #     cm[as.character(cm[["x"]]) != as.character(cm[["y"]]),])
#'   # }
#'
#'   # Remove percentages when the count is 0
#'   if (isTRUE(rm_zero_percentages)){
#'     cm[cm[["N"]] == 0,] <- empty_tile_percentages(cm[cm[["N"]] == 0,])
#'   }
#'
#'   #### Prepare for plotting ####
#'
#'   # To avoid the extremely dark colors
#'   # where the black font does not work that well
#'   # We add a bit to the range, so our max intensity
#'   # will not appear to be the extreme
#'   color_limits <- get_color_limits(intensity_measures, darkness)
#'
#'   #### Create plot ####
#'
#'   # Create plot
#'   pl <- cm %>%
#'     ggplot2::ggplot(ggplot2::aes(
#'       x = .data$x,
#'       y = .data$y,
#'       fill = .data$Intensity
#'     )) +
#'     ggplot2::labs(
#'       x = "x",
#'       y = "y",
#'       fill = "N",
#'       label = "N"
#'     ) +
#'     ggplot2::geom_tile(
#'       colour = tile_border_color,
#'       linewidth = tile_border_size,
#'       linetype = tile_border_linetype,
#'       show.legend = FALSE
#'     ) +
#'     theme_fn() +
#'     ggplot2::coord_equal()
#'
#'
#'   # Add fill colors that differ by N
#'   if (is.list(palette)) {
#'     pl <- pl +
#'       ggplot2::scale_fill_gradient(low = palette[["low"]],
#'         high = palette[["high"]],
#'         limits = color_limits)
#'   } else {
#'     pl <- pl +
#'       ggplot2::scale_fill_distiller(palette = palette,
#'         direction = 1,
#'         limits = color_limits)
#'   }
#'
#'   pl <- pl +
#'     # Remove the guide
#'     ggplot2::guides(fill = "none") +
#'     ggplot2::theme(
#'       # Rotate y-axis text
#'       axis.text.y = ggplot2::element_text(
#'         angle = ifelse(isTRUE(rotate_y_text), 90, 0),
#'         hjust = ifelse(isTRUE(rotate_y_text), 0.5, 1),
#'         vjust = ifelse(isTRUE(rotate_y_text), 0.5, 0)
#'       ),
#'       # Add margin to axis labels
#'       axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 6, 0, 0)),
#'       axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(0, 0, 6, 0)),
#'       axis.title.x.bottom = ggplot2::element_text(margin = ggplot2::margin(6, 0, 0, 0))
#'     )
#'
#'   #### Add sum tiles ####
#'
#'   if (isTRUE(add_aggs)){
#'     if (is.null(aggs_settings[["tile_fill"]])){
#'       pl <- pl +
#'         ggnewscale::new_scale_fill() +
#'         ggplot2::geom_tile(
#'           data = cm[cm[["is_agg"]] & as.character(cm[["x"]]) != as.character(cm[["y"]]),],
#'           mapping = ggplot2::aes(fill = .data$Intensity),
#'           colour = aggs_settings[["tile_border_color"]],
#'           linewidth = aggs_settings[["tile_border_size"]],
#'           linetype = aggs_settings[["tile_border_linetype"]],
#'           show.legend = FALSE
#'         )
#'
#'       # TODO: scale_fill_gradient2 allows setting midpoint color
#'
#'       if (is.list(aggs_settings[["palette"]])) {
#'         pl <- pl +
#'           ggplot2::scale_fill_gradient(low = aggs_settings[["palette"]][["low"]],
#'                                        high = aggs_settings[["palette"]][["high"]],
#'                                        limits = aggs_color_limits)
#'       } else {
#'         pl <- pl +
#'           ggplot2::scale_fill_distiller(palette = aggs_settings[["palette"]],
#'                                         direction = 1,
#'                                         limits = aggs_color_limits)
#'       }
#'
#'     } else {
#'       pl <- pl +
#'         ggplot2::geom_tile(
#'           data = cm[cm[["is_agg"]] & as.character(cm[["x"]]) !=as.character(cm[["y"]]),],
#'           fill = aggs_settings[["tile_fill"]],
#'           colour = aggs_settings[["tile_border_color"]],
#'           linewidth = aggs_settings[["tile_border_size"]],
#'           linetype = aggs_settings[["tile_border_linetype"]],
#'           show.legend = FALSE
#'         )
#'     }
#'
#'     # Add special total count tile
#'     pl <- pl +
#'       ggplot2::geom_tile(
#'         data = cm[cm[["is_agg"]] & as.character(cm[["x"]]) == as.character(cm[["y"]]),],
#'         colour = aggs_settings[["tc_tile_border_color"]],
#'         linewidth = aggs_settings[["tc_tile_border_size"]],
#'         linetype = aggs_settings[["tc_tile_border_linetype"]],
#'         fill = aggs_settings[["tc_tile_fill"]]
#'       )
#'   }
#'
#'   #### Add zero shading ####
#'
#'   if (isTRUE(use_ggimage) &&
#'       isTRUE(add_zero_shading) &&
#'       any(cm[["N"]] == 0)){
#'     pl <- pl + ggimage::geom_image(
#'       ggplot2::aes(image = .data$image_skewed_lines),
#'       by = "height", size = 0.90 / num_y_classes)
#'   }
#'
#'   #### Add 3D effect ####
#'
#'   pl <- add_3d_overlay_geom(
#'     pl = pl,
#'     num_rows = num_y_classes,
#'     amount_3d_effect = amount_3d_effect,
#'     use_ggimage = use_ggimage
#'   )
#'
#'   #### Add numbers to plot ####
#'
#'   if (isTRUE(add_values)) {
#'
#'     pl <- add_geom_text(
#'       pl=pl,
#'       data=cm[!cm[["is_agg"]], ],
#'       values_col="N",
#'       values_label_col="N_text",
#'       font_settings=font_values,
#'       all_dynamic_font_color_settings=all_dynamic_font_color_settings,
#'       dynamic_font_name="values"
#'     )
#'
#'     # Add count labels to middle of the sum tiles
#'     if (isTRUE(add_aggs)){
#'
#'       font_aggs <- font_values
#'       font_aggs[["color"]] <- font_values[["color"]]
#'       aggs_all_dynamic_font_color_settings <- NULL
#'       if (!is.null(aggs_settings[["font_values_color"]])){
#'         font_aggs[["color"]] <- aggs_settings[["font_values_color"]]
#'       } else if (!is.null(aggs_settings[["dynamic_font_colors"]][["threshold"]])){
#'         aggs_all_dynamic_font_color_settings <- aggs_settings[["dynamic_font_colors"]]
#'         aggs_all_dynamic_font_color_settings <- c(
#'           aggs_all_dynamic_font_color_settings,
#'           value_col = "N"
#'         )
#'       }
#'
#'       # Add count labels to middle of sum tiles
#'       pl <- add_geom_text(
#'         pl=pl,
#'         data=cm[cm[["is_agg"]] & as.character(cm[["x"]]) != as.character(cm[["y"]]), ],
#'         values_col="N",
#'         values_label_col="N_text",
#'         font_settings=font_aggs,
#'         all_dynamic_font_color_settings=aggs_all_dynamic_font_color_settings,
#'         dynamic_font_name="values"
#'       )
#'
#'       font_sum_tc <- font_values
#'       font_sum_tc[["color"]] <- ifelse(
#'         is.null(aggs_settings[["tc_font_color"]]),
#'         font_values[["color"]],
#'         aggs_settings[["tc_font_color"]]
#'       )
#'
#'       # Add count label to middle of total count tile
#'       # Add count labels to middle of sum tiles
#'       pl <- add_geom_text(
#'         pl=pl,
#'         data = cm[cm[["is_agg"]] & as.character(cm[["x"]]) == as.character(cm[["y"]]), ],
#'         values_col="N",
#'         values_label_col="N_text",
#'         font_settings=font_sum_tc
#'       )
#'
#'     }
#'
#'   }
#'
#'   # Place x-axis text on top of the plot
#'   if (isTRUE(place_x_axis_above)) {
#'     pl <- pl +
#'       ggplot2::scale_x_discrete(
#'         position = "top",
#'         limits = rev(levels(cm$x))
#'       )
#'   }
#'
#'   # Add row percentages
#'   if (isTRUE(add_row_percentages)) {
#'
#'     # Add row percentage labels to tiles
#'     pl <- add_geom_text(
#'       pl=pl,
#'       data=cm,
#'       values_col="y_percentage",
#'       values_label_col="y_percentage_text",
#'       font_settings=font_row_percentages,
#'       all_dynamic_font_color_settings=all_dynamic_font_color_settings,
#'       dynamic_font_name="row_percentages"
#'     )
#'
#'   }
#'
#'   # Add column percentages
#'   if (isTRUE(add_col_percentages)) {
#'
#'     # Add column percentage labels to tiles
#'     pl <- add_geom_text(
#'       pl=pl,
#'       data=cm,
#'       values_col="x_percentage",
#'       values_label_col="x_percentage_text",
#'       font_settings=font_col_percentages,
#'       all_dynamic_font_color_settings=all_dynamic_font_color_settings,
#'       dynamic_font_name="col_percentages"
#'     )
#'   }
#'
#'   #### Add arrow icons ####
#'
#'   if (isTRUE(use_ggimage) &&
#'       isTRUE(add_col_percentages) &&
#'       isTRUE(add_arrows)){
#'     pl <- pl +
#'       ggimage::geom_image(
#'         ggplot2::aes(image = .data$down_icon),
#'         by = "height",
#'         size = arrow_size,
#'         nudge_x = font_col_percentages[["nudge_x"]],
#'         nudge_y = font_col_percentages[["nudge_y"]] - arrow_nudge_from_text
#'       ) +
#'       ggimage::geom_image(
#'         ggplot2::aes(image = .data$up_icon),
#'         by = "height",
#'         size = arrow_size,
#'         nudge_x = font_col_percentages[["nudge_x"]],
#'         nudge_y = font_col_percentages[["nudge_y"]] +
#'           arrow_nudge_from_text - (arrow_size/2)
#'       )
#'   }
#'
#'   if (isTRUE(use_ggimage) &&
#'       isTRUE(add_row_percentages) &&
#'       isTRUE(add_arrows)){
#'     pl <- pl +
#'       ggimage::geom_image(
#'         ggplot2::aes(image = .data$right_icon),
#'         by = "height",
#'         size = arrow_size,
#'         nudge_x = font_row_percentages[["nudge_x"]] +
#'           arrow_nudge_from_text - (arrow_size / 2),
#'         nudge_y = font_row_percentages[["nudge_y"]]
#'       ) +
#'       ggimage::geom_image(
#'         ggplot2::aes(image = .data$left_icon),
#'         by = "height",
#'         size = arrow_size,
#'         nudge_x = font_row_percentages[["nudge_x"]] - arrow_nudge_from_text,
#'         nudge_y = font_row_percentages[["nudge_y"]]
#'       )
#'   }
#'
#'   pl
#' }
