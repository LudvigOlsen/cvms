# Plotting utilities


##  .................. #< c9ae6c32edab68c9106ff11fd591ae8a ># ..................
##  Fonts and text                                                          ####


#' @title Create a list of font settings for plots
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a list of font settings for plotting with cvms plotting functions.
#'
#'  Some arguments can take either the value to use directly
#'  OR a function that takes one argument (vector with the
#'  values to set a font for; e.g., the counts, percentages, etc.)
#'  and returns the value(s) to use for each element. Such a
#'  function could for instance specify different font
#'  colors for different background intensities.
#'
#'  NOTE: This is experimental and could change.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family plotting functions
#' @param size,color,alpha,nudge_x,nudge_y,angle,family,fontface,hjust,vjust,lineheight Either the value to pass directly to
#'  \code{\link[ggplot2:geom_text]{ggplot2::geom_text}} or a function that takes in the values (e.g., counts, percentages, etc.)
#'  and returns a vector of values to pass to \code{\link[ggplot2:geom_text]{ggplot2::geom_text}}.
#' @param digits Number of digits to round to. If negative, no rounding will take place.
#' @param prefix A string prefix.
#' @param suffix A string suffix.
#' @return List of settings.
font <- function(size = NULL,
                 color = NULL,
                 alpha = NULL,
                 nudge_x = NULL,
                 nudge_y = NULL,
                 angle = NULL,
                 family = NULL,
                 fontface = NULL,
                 hjust = NULL,
                 vjust = NULL,
                 lineheight = NULL,
                 digits = NULL,
                 prefix = NULL,
                 suffix = NULL) {

  # TODO Could this inherit from ggplot2::element_text?

  list(
    "size" = size,
    "color" = color,
    "alpha" = alpha,
    "nudge_x" = nudge_x,
    "nudge_y" = nudge_y,
    "angle" = angle,
    "family" = family,
    "fontface" = fontface,
    "hjust" = hjust,
    "vjust" = vjust,
    "lineheight" = lineheight,
    "digits" = digits,
    "prefix" = prefix,
    "suffix" = suffix
  )
}

update_font_setting <- function(settings, defaults, initial_vals = NULL) {

  # If defaults not provided,
  # here are some reasonable backup defaults
  backup_defaults <-
    font(
      size = 4,
      color = "black",
      alpha = 1.0,
      nudge_x = 0,
      nudge_y = 0,
      angle = 0,
      family = "",
      fontface = "plain",
      hjust = 0.5,
      vjust = 0.5,
      lineheight = 1.2,
      digits = -1,
      prefix = "",
      suffix = ""
    )

  update_settings_object(
    settings = settings,
    defaults = defaults,
    backup_defaults = backup_defaults,
    initial_vals = initial_vals
  )
}

preprocess_numeric <- function(vec, settings, rm_zero_text=FALSE, rm_zeroes_post_rounding=TRUE) {

  # Find the pre-rounding 0s or NaNs
  is_zero <- vec == 0 | is.na(vec)

  # Don't round if digits is negative
  if (settings[["digits"]] >= 0) {
    vec <- round(vec, settings[["digits"]])
  }
  out <- paste0(settings[["prefix"]], vec, settings[["suffix"]])

  # Remove text for zeroes
  # Potentially including elements zero after rounding
  if (isTRUE(rm_zero_text)){
    if (isTRUE(rm_zeroes_post_rounding)){
      out[vec == 0 | is.na(vec)] <- ""
    } else {
      out[is_zero] <- ""
    }
  }
  out
}

#' @title Create a list of dynamic font color settings for plots
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a list of dynamic font color settings for plotting with \code{cvms} plotting functions.
#'
#'  Specify separate colors below and above a given value threshold.
#'
#'  NOTE: This is experimental and will likely change.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family plotting functions
#' @param threshold The threshold at which the color changes.
#' @param by The value to check against \code{`threshold`}.
#'  One of \{\code{`counts`}, \code{`normalized`}\}.
#' @param all Set same color settings for all fonts at once.
#'  Takes a character vector with two hex code strings (low, high).
#'  Example: `c('#000', '#fff')`.
#' @param counts,normalized,row_percentages,col_percentages Set color settings for the individual font.
#'  Takes a character vector with two hex code strings (low, high).
#'  Example: `c('#000', '#fff')`.
#'
#'  Specifying colors for specific fonts overrides the settings specified in
#'  \code{`all`} (for those fonts only).
#' @param invert_arrows String specifying when to invert the color of the arrow icons based on the threshold.
#'  One of \{\code{`below`}, \code{`at_and_above`}\} (or \code{NULL} for no dynamical arrow colors).
#' @return List of settings.
dynamic_font_color_settings <- function(threshold = NULL,
                                        by = "counts",
                                        all = NULL,
                                        counts = NULL,
                                        normalized = NULL,
                                        row_percentages = NULL,
                                        col_percentages = NULL,
                                        invert_arrows = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_number(x = threshold, null.ok = TRUE, add = assert_collection)
  checkmate::assert_choice(
    x = by,
    choices = c("counts", "normalized"),
    add = assert_collection
  )
  checkmate::assert_character(
    x = all,
    min.chars = 1,
    len = 2,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_character(
    x = counts,
    min.chars = 1,
    len = 2,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_character(
    x = normalized,
    min.chars = 1,
    len = 2,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_character(
    x = row_percentages,
    min.chars = 1,
    len = 2,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_character(
    x = col_percentages,
    min.chars = 1,
    len = 2,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_choice(
    x = invert_arrows,
    choices = c("below", "at_and_above"),
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  list(
    "threshold" = threshold,
    "by" = by,
    "all" = all,
    "counts" = counts,
    "normalized" = normalized,
    "row_percentages" = row_percentages,
    "col_percentages" = col_percentages,
    "invert_arrows" = invert_arrows
  )

}

# TODO: Document and export!
create_dynamic_font_setting <- function(low_color, high_color, threshold){
  function(values){
    ifelse(values >= threshold, high_color, low_color)
  }
}

##  .................. #< 9aec0fe5634b9f4a907cfad120a085af ># ..................
##  Sum tile settings                                                       ####


#' @title Create a list of settings for the sum tiles in plot_confusion_matrix()
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a list of settings for plotting the column/row sums
#'  in \code{\link[cvms:plot_confusion_matrix]{plot_confusion_matrix()}}.
#'
#'  The \code{`tc_`} in the arguments refers to the \strong{total count} tile.
#'
#'  NOTE: This is very experimental and will likely change.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family plotting functions
#' @param palette Color scheme to use for sum tiles.
#'  Should be different from the \code{`palette`} used for the regular tiles.
#'
#'  Passed directly to \code{`palette`} in
#'  \code{\link[ggplot2:scale_fill_distiller]{ggplot2::scale_fill_distiller}}.
#'
#'  Try these palettes: \code{"Greens"}, \code{"Oranges"}, \code{"Greys"},
#'  \code{"Purples"}, \code{"Reds"}, and \code{"Blues"}.
#'
#'  Alternatively, pass a named list with limits of a custom gradient as e.g.
#'  \code{`list("low"="#e9e1fc", "high"="#BE94E6")`}. These are passed to
#'  \code{\link[ggplot2:scale_fill_gradient]{ggplot2::scale_fill_gradient}}.
#'
#'  Note: When \code{`tile_fill`} is specified, the \code{`palette`} is \strong{ignored}.
#' @param label The label to use for the sum column and the sum row.
#' @param font_counts_color,font_normalized_color Color of the text in the tiles with the column and row sums.
#'  Either the value directly passed to \code{\link[ggplot2:geom_text]{ggplot2::geom_text}} or
#'  a function that take in the values (e.g., counts, percentages, etc.)
#'  and returns a vector of values to pass to \code{\link[ggplot2:geom_text]{ggplot2::geom_text}}.
#' @param dynamic_font_colors A list of settings for using dynamic font colors
#'  based on the value of the counts/normalized. Allows changing the font colors
#'  when the background tiles are too dark, etc.
#'  Can be provided with \code{\link[cvms:dynamic_font_color_settings]{
#'  dynamic_font_color_settings(threshold =, by =, all =, counts =, normalized =)}}.
#'
#'  Individual thresholds can be set for the different fonts/values via the
#'  \code{`font_*_color`} arguments. Specifying colors in these arguments will overwrite
#'  this argument (for the specific font only).
#'
#'  Specifying colors for specific fonts overrides the "all" values for those fonts.
#' @param tc_font_color Color of the text in the total count tile.
#' @param tc_tile_fill,tile_fill Specific background color for the tiles. Passed as \emph{\code{`fill`}} to
#' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#'
#'  If specified, the \code{`palette`} is ignored.
#' @param tc_tile_border_color,tile_border_color Color of the tile borders. Passed as \emph{\code{`colour`}} to
#' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#' @param tc_tile_border_size,tile_border_size Size of the tile borders. Passed as \emph{\code{`size`}} to
#' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#' @param tc_tile_border_linetype,tile_border_linetype Linetype for the tile borders. Passed as \emph{\code{`linetype`}} to
#' \code{\link[ggplot2:geom_tile]{ggplot2::geom_tile}}.
#' @param font_color Deprecated.
#' @inheritParams plot_confusion_matrix
#' @return List of settings.
sum_tile_settings <- function(palette = NULL,
                              label = NULL,
                              tile_fill = NULL,
                              font_counts_color = NULL,
                              font_normalized_color = NULL,
                              dynamic_font_colors = dynamic_font_color_settings(),
                              tile_border_color = NULL,
                              tile_border_size = NULL,
                              tile_border_linetype = NULL,
                              tc_tile_fill = NULL,
                              tc_font_color = NULL,
                              tc_tile_border_color = NULL,
                              tc_tile_border_size = NULL,
                              tc_tile_border_linetype = NULL,
                              intensity_by = NULL,
                              intensity_lims = NULL,
                              intensity_beyond_lims = NULL,
                              font_color = deprecated()) {

  # Check deprecated font color argument
  if (!rlang::is_missing(font_color)) {
    deprecate_warn(
      "1.8.0", "cvms::sum_tile_settings(font_color =)",
      "cvms::sum_tile_settings(font_counts_color=)"
    )
    font_counts_color <- font_color
    font_normalized_color <- font_color
  }

  list(
    "palette" = palette,
    "label" = label,
    "tile_fill" = tile_fill,
    "font_counts_color" = font_counts_color,
    "font_normalized_color" = font_normalized_color,
    "dynamic_font_colors" = dynamic_font_colors,
    "tile_border_color" = tile_border_color,
    "tile_border_size" = tile_border_size,
    "tile_border_linetype" = tile_border_linetype,
    "tc_tile_fill" = tc_tile_fill,
    "tc_font_color" = tc_font_color,
    "tc_tile_border_color" = tc_tile_border_color,
    "tc_tile_border_size" = tc_tile_border_size,
    "tc_tile_border_linetype" = tc_tile_border_linetype,
    "intensity_by"= intensity_by,
    "intensity_lims" = intensity_lims,
    "intensity_beyond_lims" = intensity_beyond_lims
  )
}

update_sum_tile_settings <- function(settings, defaults, initial_vals = NULL) {

  # If defaults not provided,
  # here are some reasonable backup defaults
  backup_defaults <-
    sum_tile_settings(
      palette = "Greens",
      label = "\u2211",
      tile_fill = NULL,
      font_counts_color = NULL,
      font_normalized_color = NULL,
      dynamic_font_colors = dynamic_font_color_settings(),
      tile_border_color = NA,
      tile_border_size = 0.1,
      tile_border_linetype = "solid",
      tc_tile_fill = "#F5F5F5",
      tc_font_color = NULL,
      tc_tile_border_color = NA,
      tc_tile_border_size = 0.1,
      tc_tile_border_linetype = "solid",
      intensity_by = NULL,
      intensity_lims = NULL,
      intensity_beyond_lims = NULL
    )

  update_settings_object(
    settings = settings,
    defaults = defaults,
    backup_defaults = backup_defaults,
    initial_vals = initial_vals
  )
}

sort_palette <- function(palette) {
  if (is.character(palette))
    return(sort(palette))
  palette[order(names(palette))]
}

palettes_are_equal <- function(p1, p2) {
  p1 <- sort_palette(p1)
  p2 <- sort_palette(p2)
  if (is.list(p1)) {
    if (!is.list(p2))
      return(FALSE)
    if (!all(names(p1) == names(p2)))
      return(FALSE)
    return(identical(c(p1[["low"]], p1[["high"]]), c(p2[["low"]], p2[["high"]])))
  }
  p1 == p2
}


##  .................. #< 7306b5e303b38ee92abe09fbcde5fc29 ># ..................
##  Images                                                                  ####


# Get figure path from either /inst/images/ or /images/
# Stuff in /inst/ is put in root folder on build
get_figure_path <- function(fig_name, inst_dir = "images", pgk_name = "cvms") {
  dir_path <- system.file(inst_dir, package = pgk_name)
  fig_path <- paste0(dir_path, "/", fig_name)
  if (file.exists(fig_path))
    return(fig_path)
  warning("Could not find figure.")
  invisible()
}


##  .................. #< cfe54766be9f8b8abf2439453a3d2e82 ># ..................
##  Basics                                                                  ####


calculate_normalized <- function(data){
  sum_ <- sum(data[["N"]])
  if (sum_ == 0){
    sum_ <- 1
  }
  data[["Normalized"]] <- 100 * (data[["N"]] / sum_)
  data
}

set_intensity <- function(data, intensity_by) {
  if (grepl("counts", intensity_by)) {
    counts <- data[["N"]]

    if (intensity_by == "log counts") {
      counts[counts != 0] <- log(counts[counts != 0])
    } else if (intensity_by == "log2 counts") {
      counts[counts != 0] <- log2(counts[counts != 0])
    } else if (intensity_by == "log10 counts") {
      counts[counts != 0] <- log10(counts[counts != 0])
    } else if (intensity_by == "arcsinh counts") {
      counts <- asinh(counts)
    }

    data[["Intensity"]] <- counts

  } else if (intensity_by == "normalized") {
    data[["Intensity"]] <- data[["Normalized"]]
  } else if (intensity_by == "row_percentages") {
    if (!("Prediction_Percentage" %in% colnames(data))){
      stop("Cannot set intensity by `row_percentages` when `add_row_percentages=FALSE`.")
    }
    data[["Intensity"]] <- data[["Prediction_Percentage"]]
  } else if (intensity_by == "col_percentages") {
    if (!("Class_Percentage" %in% colnames(data))){
      stop("Cannot set intensity by `col_percentages` when `add_col_percentages=FALSE`.")
    }
    data[["Intensity"]] <- data[["Class_Percentage"]]
  }
  data
}

get_intensity_range <- function(data, intensity_by, intensity_lims) {
  # Get min and max intensity scores
  if (!is.null(intensity_lims)) {
    min_intensity <- as.double(intensity_lims[[1]])
    max_intensity <- as.double(intensity_lims[[2]])
  } else if (grepl("counts", intensity_by)) {
    min_intensity <- as.double(min(data$Intensity))
    max_intensity <- as.double(max(data$Intensity))
    if (min_intensity == max_intensity && min_intensity == 0) {
      # When all are 0, make sure all get lowest value in palette
      max_intensity <- 1
    }
  } else {
    min_intensity <- 0.
    max_intensity <- 100.
  }
  range_intensity <- max_intensity - min_intensity
  list("min" = min_intensity,
       "max" = max_intensity,
       "range" = range_intensity)
}

handle_beyond_intensity_limits <- function(intensities, intensity_range, intensity_beyond_lims){
  if (intensity_beyond_lims == "truncate"){
    dplyr::case_when(
      intensities > intensity_range[["max"]] ~ intensity_range[["max"]],
      intensities < intensity_range[["min"]] ~ intensity_range[["min"]],
      TRUE ~ intensities
    )
  } else {
    dplyr::case_when(
      intensities > intensity_range[["max"]] ~ Inf,
      intensities < intensity_range[["min"]] ~ -Inf,
      TRUE ~ intensities
    )
  }
}

get_color_limits <- function(intensity_measures, darkness){
  # To avoid the extremely dark colors
  # where the black font does not work that well
  # We add a bit to the range, so our max intensity
  # will not appear to be the extreme
  c(intensity_measures[["min"]],
    intensity_measures[["max"]] + 10 * (1 - darkness) * (
      intensity_measures[["range"]] / 5)
  )
}

update_settings_object <- function(settings, defaults, backup_defaults, initial_vals){
  new_settings <- list()
  for (opt in names(backup_defaults)) {
    if (is.null(settings[[opt]])) {
      if (opt %in% names(defaults) &&
          !is.null(defaults[[opt]])) {
        new_settings[[opt]] <- defaults[[opt]]
      } else {
        new_settings[[opt]] <- backup_defaults[[opt]]
      }
    } else {
      new_settings[[opt]] <- settings[[opt]]
    }

    # Apply initial values
    if (!is.null(initial_vals) && opt %in% names(initial_vals)) {
      new_settings[[opt]] <- initial_vals[[opt]](new_settings[[opt]])
    }
  }

  new_settings
}


# If setting is a function, call it on the values and the return the outputs
# If setting is a values, return the value
interpret_font_setting <- function(font_settings, arg_name, values) {

  if (!(arg_name %in% names(font_settings))) {
    stop(paste0("`", arg_name, "` not in the font settings."))
  }

  setting <- font_settings[[arg_name]]

  # If a function, we call it on the values
  # Otherwise it's assumed to be valid a value
  if (is.function(setting)) {
    setting <- setting(values)
  }

  setting
}

interpret_all_font_settings <- function(font_settings, values) {
  purrr::map(names(font_settings), .f =  ~ {
    interpret_font_setting(
      font_settings = font_settings,
      arg_name = .x,
      values = values
    )
  }) %>% setNames(names(font_settings))
}


add_geom_text <- function(
    pl,
    data,
    values_col,
    values_label_col,
    font_settings,
    allowed_static_setting_names=c(
      "size",
      "alpha",
      "nudge_x",
      "nudge_y",
      "angle",
      "family",
      "fontface",
      "hjust",
      "vjust",
      "lineheight",
      "color"
    ),
    add_settings = list(),
    all_dynamic_font_color_settings=NULL,
    dynamic_font_name=NULL
) {
  # Interpret font settings for counts
  font_settings <- font_settings[names(font_settings) %in% allowed_static_setting_names]
  font_interps <- interpret_all_font_settings(font_settings, data[[values_col]])

  # Get dynamic font color settings for this font
  if (!is.null(dynamic_font_name) &&

      # Ensure it's not specified via `font_*`
      # For sum tiles, we skip this check ("already_specified_for" is not in the list)
      (!("already_specified_for" %in% names(all_dynamic_font_color_settings)) ||
      !all_dynamic_font_color_settings[["already_specified_for"]][[dynamic_font_name]]) &&

      # Colors must be specified for 'all' or the specific font
      !(is.null(all_dynamic_font_color_settings[["all"]]) &&
      is.null(all_dynamic_font_color_settings[[dynamic_font_name]]))){

    # Get colors for either the specific font
    if (!is.null(all_dynamic_font_color_settings[[dynamic_font_name]])){
      font_interps[["color"]] <- create_dynamic_font_setting(
        all_dynamic_font_color_settings[[dynamic_font_name]][[1]],
        all_dynamic_font_color_settings[[dynamic_font_name]][[2]],
        threshold = all_dynamic_font_color_settings[["threshold"]]
      )(data[[all_dynamic_font_color_settings[["value_col"]]]])

    # Or from 'all' specification
    } else if (!(is.null(all_dynamic_font_color_settings[["all"]]))){
      font_interps[["color"]] <- create_dynamic_font_setting(
        all_dynamic_font_color_settings[["all"]][[1]],
        all_dynamic_font_color_settings[["all"]][[2]],
        threshold = all_dynamic_font_color_settings[["threshold"]]
      )(data[[all_dynamic_font_color_settings[["value_col"]]]])
    }

  }

  # Add count labels to middle of the regular tiles
  pl <- pl +
    purrr::exec(
      ggplot2::geom_text,
      data = data,
      mapping = ggplot2::aes(label = !!as.name(values_label_col)),
      !!!font_interps,
      !!!add_settings
    )

  pl

}

add_3d_overlay_geom <- function(pl, num_rows, amount_3d_effect, use_ggimage){
  if (isTRUE(use_ggimage) &&
      amount_3d_effect > 0) {
    overlay_size_subtract <- 0.043 / 2 ^ (num_rows - 2)
    overlay_size_subtract <-
      dplyr::case_when(num_rows >= 5 ~ overlay_size_subtract + 0.002,
        TRUE ~ overlay_size_subtract)

    pl <- pl + ggimage::geom_image(
      ggplot2::aes(image = .data$image_3d),
      by = "width",
      size = 1.0 / num_rows - overlay_size_subtract
    )
  }

  pl
}

check_gg_image_packages <- function(add_arrows, add_zero_shading){
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

  list(
    "user_has_rsvg" = user_has_rsvg,
    "user_has_ggimage" = user_has_ggimage,
    "user_has_ggnewscale" = user_has_ggnewscale,
    "use_ggimage" = use_ggimage,
    "add_arrows" = add_arrows,
    "add_zero_shading" = add_zero_shading
  )
}

make_arrow_paths <- function(arrow_color){
  arrow_icons <- list(
    "up" = get_figure_path(paste0("caret_up_sharp_", arrow_color, ".svg")),
    "down" = get_figure_path(paste0("caret_down_sharp_", arrow_color, ".svg")),
    "left" = get_figure_path(paste0("caret_back_sharp_", arrow_color, ".svg")),
    "right" = get_figure_path(paste0("caret_forward_sharp_", arrow_color, ".svg"))
  )
  arrow_icons
}

add_3d_path <- function(cm, use_ggimage, amount_3d_effect){
  # Assign 3D effect image
  if (isTRUE(use_ggimage) &&
      amount_3d_effect > 0){
    # Add image path with slight 3D effect
    if (FALSE){  # Debugging
      cm[["image_3d"]] <- get_figure_path("square_overlay_bordered.png")
    } else {
      cm[["image_3d"]] <- get_figure_path(paste0("square_overlay_", amount_3d_effect, ".png"))
    }
  }
  cm
}
