# Plotting utilities

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

update_font_setting <- function(settings, defaults, initial_vals=NULL){

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
