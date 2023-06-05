# Plotting utilities


##  .................. #< c9ae6c32edab68c9106ff11fd591ae8a ># ..................
##  Fonts and text                                                          ####


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
#' @param size,color,alpha,nudge_x,nudge_y,angle,family,fontface,hjust,vjust,lineheight As passed to
#'  \code{\link[ggplot2:geom_text]{ggplot2::geom_text}}.
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
#'  Note: When \code{`tile_fill`} is specified, the \code{`palette`} is \strong{ignored}.
#' @param label The label to use for the sum column and the sum row.
#' @param tc_font_color,font_color Color of the text in the tiles with the column and row sums.
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
#' @return List of settings.
sum_tile_settings <- function(palette = NULL,
                              label = NULL,
                              tile_fill = NULL,
                              font_color = NULL,
                              tile_border_color = NULL,
                              tile_border_size = NULL,
                              tile_border_linetype = NULL,
                              tc_tile_fill = NULL,
                              tc_font_color = NULL,
                              tc_tile_border_color = NULL,
                              tc_tile_border_size = NULL,
                              tc_tile_border_linetype = NULL) {
  list(
    "palette" = palette,
    "label" = label,
    "tile_fill" = tile_fill,
    "font_color" = font_color,
    "tile_border_color" = tile_border_color,
    "tile_border_size" = tile_border_size,
    "tile_border_linetype" = tile_border_linetype,
    "tc_tile_fill" = tc_tile_fill,
    "tc_font_color" = tc_font_color,
    "tc_tile_border_color" = tc_tile_border_color,
    "tc_tile_border_size" = tc_tile_border_size,
    "tc_tile_border_linetype" = tc_tile_border_linetype
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
      font_color = NULL,
      tile_border_color = NA,
      tile_border_size = 0.1,
      tile_border_linetype = "solid",
      tc_tile_fill = "#F5F5F5",
      tc_font_color = NULL,
      tc_tile_border_color = NA,
      tc_tile_border_size = 0.1,
      tc_tile_border_linetype = "solid"
    )

  update_settings_object(
    settings = settings,
    defaults = defaults,
    backup_defaults = backup_defaults,
    initial_vals = initial_vals
  )
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

set_intensity <- function(data, intensity_by){
  if (intensity_by == "counts") {
    data[["Intensity"]] <- data[["N"]]
  } else {
    data[["Intensity"]] <- data[["Normalized"]]
  }
  data
}

get_intensity_range <- function(data, intensity_by){
  # Get min and max intensity scores
  if (intensity_by == "counts"){
    min_intensity <- min(data$N)
    max_intensity <- max(data$N)
    if (min_intensity == max_intensity && min_intensity == 0){
      # When all are 0, make sure all get lowest value in palette
      max_intensity <- 1
    }
  } else {
    min_intensity <- 0
    max_intensity <- 100
  }
  range_intensity <- max_intensity - min_intensity
  list(
    "min" = min_intensity,
    "max" = max_intensity,
    "range" = range_intensity
  )
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
