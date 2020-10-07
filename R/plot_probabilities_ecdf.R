

#   __________________ #< 5e5bf705e0a39e257a1c53917eeec836 ># __________________
#   Plot probabilities ECDF                                                 ####



##  .................. #< 2fc0b5f9735b66cde5a1690750fa0e63 ># ..................
##  Documentation                                                           ####


# TODO We probably want to avoid using obs_id_col unless we specifically want the
# ecdf to be calculated on averages?


#' @title Plot ECDF for the predicted probabilities
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Plots the empirical cumulative distribution function (ECDF) for the
#'  probabilities of either the target classes or the predicted classes.
#'
#'  Creates a \code{\link[ggplot2:ggplot]{ggplot2}} with the \code{\link[ggplot2:stat_ecdf]{stat_ecdf()}} geom.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @keywords internal
#' @family plotting functions
#' @param data \code{data.frame} with probabilities, target classes and (optional) predicted classes.
#'  Can also include observation identifiers and a grouping variable.
#'
#'  Example for binary classification:
#'
#'  \tabular{rrrrr}{
#'   \strong{Classifier} \tab \strong{Observation} \tab \strong{Probability} \tab \strong{Target} \tab \strong{Prediction}  \cr
#'   SVM \tab 1 \tab 0.3 \tab cl_1 \tab cl_1 \cr
#'   SVM \tab 2 \tab 0.7 \tab cl_1 \tab cl_2 \cr
#'   NB \tab 1 \tab 0.2 \tab cl_2 \tab cl_1 \cr
#'   NB \tab 2 \tab 0.8 \tab cl_2 \tab cl_2 \cr
#'   ... \tab ... \tab ... \tab ... \tab ... \cr
#'  }
#'
#'  Example for multiclass classification:
#'
#'  \tabular{rrrrrrr}{
#'   \strong{Classifier} \tab \strong{Observation} \tab \strong{cl_1} \tab \strong{cl_2} \tab \strong{cl_3} \tab \strong{Target} \tab \strong{Prediction}  \cr
#'   SVM \tab 1 \tab 0.2 \tab 0.1 \tab 0.7 \tab cl_1 \tab cl_3 \cr
#'   SVM \tab 2 \tab 0.3 \tab 0.5 \tab 0.2 \tab cl_1 \tab cl_2 \cr
#'   NB \tab 1 \tab 0.8 \tab 0.1 \tab 0.1 \tab cl_2 \tab cl_1 \cr
#'   NB \tab 2 \tab 0.1 \tab 0.6 \tab 0.3 \tab cl_3 \tab cl_2 \cr
#'   ... \tab ... \tab ... \tab ... \tab ... \tab ... \tab ... \cr
#'  }
#'
#'  As created with the various validation functions in \code{cvms}, like
#'  \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}}.
#' @param predicted_class_col Name of column with predicted classes.
#'
#'  This is required when \code{probability_of = "prediction"}.
#' @param obs_id_col Name of column with observation identifiers for averaging the
#'  predicted probabilities per observation before computing the ECDF (\emph{when deemed meaningful}).
#'  When \code{NULL}, each row is an observation.
#' @param probability_of Whether to plot the ECDF for the probabilities of the
#'  target classes (\code{"target"}) or the predicted classes (\code{"prediction"}).
#'
#'  For each row, we extract the probability of either the
#'  \emph{target class} or the \emph{predicted class}. Both are useful
#'  to plot, as they show the behavior of the classifier in a way a confusion matrix doesn't.
#'  One classifier might be very certain in its predictions (whether wrong or right), whereas
#'  another might be less certain.
#' @param ecdf_settings Named list of arguments for \code{\link[ggplot2:stat_ecdf]{ggplot2::stat_ecdf()}}.
#'
#'   The \code{mapping} argument is set separately.
#'
#'   Any argument not in the list will use the default value set by \code{cvms}.
#'
#'   Defaults: \code{list(geom = "smooth", pad = FALSE)}.
#'
#'   Common changes are to set \code{`geom = "step"`} and/or \code{`pad = TRUE`}.
#' @param xlim Limits for the x-scale.
#' @inheritParams plot_probabilities
#' @details
#'  TODO
#' @return
#'  A \code{ggplot2} object with a faceted line plot. TODO
#' @examples
#' \donttest{
#' # Attach cvms
#' library(cvms)
#' library(ggplot2)
#' library(dplyr)
#'
#' #
#' # Multiclass
#' #
#'
#' # TODO: Go through and rewrite comments and code!
#'
#' # Plot probabilities of target classes
#' # From repeated cross-validation of three classifiers
#'
#' # plot_probabilities_ecdf(
#' #   data = predicted.musicians,
#' #   target_col = "Target",
#' #   probability_cols = c("A", "B", "C", "D"),
#' #   predicted_class_col = "Predicted Class",
#' #   group_col = "Classifier",
#' #   probability_of = "target"
#' # )
#'
#' # Plot probabilities of predicted classes
#' # From repeated cross-validation of three classifiers
#'
#' # plot_probabilities_ecdf(
#' #   data = predicted.musicians,
#' #   target_col = "Target",
#' #   probability_cols = c("A", "B", "C", "D"),
#' #   predicted_class_col = "Predicted Class",
#' #   group_col = "Classifier",
#' #   probability_of = "prediction"
#' # )
#'
#' #
#' # Binary
#' #
#'
#' # Filter the predicted.musicians dataset
#' # binom_data <- predicted.musicians %>%
#' #   dplyr::filter(
#' #     Target %in% c("A", "B")
#' #   ) %>%
#' #   # "B" is the second class alphabetically
#' #   dplyr::rename(Probability = B) %>%
#' #   dplyr::mutate(`Predicted Class` = ifelse(
#' #     Probability > 0.5, "B", "A")) %>%
#' #   dplyr::select(-c("A","C","D"))
#'
#' # Plot probabilities of predicted classes
#' # From repeated cross-validation of three classifiers
#'
#' # plot_probabilities_ecdf(
#' #   data = binom_data,
#' #   target_col = "Target",
#' #   probability_cols = "Probability",
#' #   predicted_class_col = "Predicted Class",
#' #   group_col = "Classifier",
#' #   probability_of = "target"
#' # )
#'
#' # plot_probabilities_ecdf(
#' #   data = binom_data,
#' #   target_col = "Target",
#' #   probability_cols = "Probability",
#' #   predicted_class_col = "Predicted Class",
#' #   group_col = "Classifier",
#' #   probability_of = "prediction",
#' #   xlim = c(0.5, 1)
#' # )
#'
#' }
plot_probabilities_ecdf <- function(data,
                                    target_col,
                                    probability_cols,
                                    predicted_class_col = NULL,
                                    obs_id_col = NULL,
                                    group_col = NULL,
                                    probability_of = "target",
                                    positive = 2,
                                    theme_fn = ggplot2::theme_minimal,
                                    color_scale = ggplot2::scale_colour_brewer(palette = "Dark2"),
                                    apply_facet = length(probability_cols) > 1,
                                    add_caption = TRUE,
                                    ecdf_settings = list(),
                                    facet_settings = list(),
                                    xlim = c(0, 1)) {

  call_plot_probabilities_ecdf_(
    data = data,
    target_col = target_col,
    probability_cols = probability_cols,
    predicted_class_col = predicted_class_col,
    obs_id_col = obs_id_col,
    group_col = group_col,
    probability_of = probability_of,
    positive = positive,
    theme_fn = theme_fn,
    color_scale = color_scale,
    apply_facet = apply_facet,
    add_caption = add_caption,
    ecdf_settings = ecdf_settings,
    facet_settings = facet_settings,
    xlim = xlim
  )

}


##  .................. #< 1608c527f9dfe05bb86186c816a3aeeb ># ..................
##  Implementation                                                          ####


call_plot_probabilities_ecdf_ <- function(data,
                                     target_col,
                                     probability_cols,
                                     predicted_class_col,
                                     obs_id_col,
                                     group_col,
                                     probability_of,
                                     positive,
                                     theme_fn,
                                     color_scale,
                                     apply_facet,
                                     add_caption,
                                     ecdf_settings,
                                     facet_settings,
                                     xlim){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()

  ## Data Frame ####
  checkmate::assert_data_frame(
    x = data,
    any.missing = FALSE,
    min.rows = 2,
    min.cols = 2,
    col.names = "named",
    add = assert_collection
  )

  ## List ####

  aapply(
    checkmate::assert_list,
    . ~ ecdf_settings +
      facet_settings,
    any.missing = FALSE,
    names = "unique"
  )

  ## Strings ####
  checkmate::assert_string(x = target_col, add = assert_collection)
  checkmate::assert_string(
    x = predicted_class_col,
    null.ok = TRUE,
    min.chars = 1,
    add = assert_collection
  )
  checkmate::assert_string(
    x = obs_id_col,
    null.ok = TRUE,
    min.chars = 1,
    add = assert_collection
  )
  checkmate::assert_string(
    x = group_col,
    null.ok = TRUE,
    min.chars = 1,
    add = assert_collection
  )
  checkmate::assert_string(x = probability_of, add = assert_collection)
  checkmate::assert_character(
    x = probability_cols,
    any.missing = FALSE,
    min.len = 1,
    names = "unnamed",
    unique = TRUE,
    add = assert_collection
  )

  ## Flag ####
  checkmate::assert_flag(x = add_caption, add = assert_collection)
  checkmate::assert_flag(x = apply_facet, add = assert_collection)

  ## Number ####
  checkmate::assert_numeric(
    x = xlim,
    len = 2,
    sorted = TRUE,
    any.missing = FALSE,
    names = "unnamed",
    unique = TRUE,
    add = assert_collection
  )

  # Check 'positive'
  checkmate::assert(
    checkmate::check_integerish(
      positive,
      lower = 1,
      upper = 2,
      any.missing = FALSE,
      len = 1
    ),
    checkmate::check_string(positive, min.chars = 1)
  )

  ## Function ####
  checkmate::assert_function(x = theme_fn, add = assert_collection)

  ## Additional checks ####

  # Color scale
  if ("Scale" %ni% class(color_scale)) {
    assert_collection$push("'color_scale' must be a 'Scale' object.")
  }

  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(
    x = colnames(data),
    must.include = c(
      target_col,
      probability_cols,
      predicted_class_col,
      obs_id_col,
      group_col
    ),
    what = "colnames",
    add = assert_collection
  )
  checkmate::assert_names(
    x = probability_of,
    subset.of = c("target", "prediction"),
    add = assert_collection
  )

  # Check probabilities sum to 1
  if (length(probability_cols)>1 &&
      !rows_sum_to(data[, probability_cols], sum_to = 1, digits = 5)) {
    assert_collection$push(
      paste0(
        "when 'probability_cols' has length > 1, probability columns m",
        "ust sum to 1 row-wise."
      )
    )
  }

  checkmate::reportAssertions(assert_collection)

  ## Column types ####

  checkmate::assert(
    checkmate::check_character(x = data[[target_col]], any.missing = FALSE),
    checkmate::check_factor(x = data[[target_col]], min.levels = 2, any.missing = FALSE)
  )
  checkmate::assert(
    checkmate::check_character(x = data[[predicted_class_col]], any.missing = FALSE, null.ok = TRUE),
    checkmate::check_factor(x = data[[predicted_class_col]], any.missing = FALSE)
  )
  checkmate::assert_data_frame(
    x = data[, probability_cols],
    types = "double",
    add = assert_collection
  )

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####


  #### Update settings lists ####

  # Update setting lists
  ecdf_settings <- update_hyperparameters(
    size = 0.5,
    geom = "smooth",
    pad = FALSE,
    hyperparameters = ecdf_settings
  )
  facet_settings <- update_hyperparameters(
    hyperparameters = facet_settings
  )

  #### Prepare dataset ####

  data <- dplyr::ungroup(data)
  data <- base_select(data, cols = c(
    group_col, obs_id_col, target_col,
    probability_cols, predicted_class_col))

  if (is.null(obs_id_col)){
    obs_id_col <- create_tmp_name(data, "Observation")
    data <- data %>%
      dplyr::group_by_at(group_col) %>%
      dplyr::mutate(!!obs_id_col := seq_len(dplyr::n()))
  }

  # Ensure ID column is factor
  data[[obs_id_col]] <- as.factor(data[[obs_id_col]])

  # Create tmp column names
  prob_of_col <- create_tmp_name(data, ".probability_of")
  avg_prob_col <- create_tmp_name(data, name = ".avg_probability")
  rank_col <- create_tmp_name(data, ".observation_rank")

  # Prepare extraction of probabilities
  if (probability_of == "target"){
    of_col <- target_col
    cat_levels <- levels_as_characters(data[[of_col]], drop_unused = TRUE)
  } else if (probability_of == "prediction"){
    of_col <- predicted_class_col
    # Make sure there are all the cat_levels for binomial case
    cat_levels <- union(
      levels_as_characters(data[[predicted_class_col]], drop_unused = TRUE),
      levels_as_characters(data[[target_col]], drop_unused = TRUE)
    )
  }

  # Extract probabilities of target/prediction
  data[[prob_of_col]] <- extract_probabilities_of(
    data = data,
    probability_cols = probability_cols,
    of_col = of_col,
    cat_levels = cat_levels,
    positive = positive)

  # Remove probability cols
  data <- base_deselect(data, cols = probability_cols)

  # Order by IDs' average probability
  data <- add_id_aggregates(
    data = data,
    group_col = group_col,
    obs_id_col = obs_id_col,
    of_col = of_col,
    prob_of_col = prob_of_col,
    order = "identity",
    apply_facet = apply_facet,
    rank_col_name = rank_col,
    avg_prob_col = avg_prob_col
  )

  # Create group_col if none (simplifies code a lot)
  remove_legend <- FALSE
  if (is.null(group_col)){
    group_col <- create_tmp_name(data, "Group")
    data[[group_col]] <- factor(".tmp")
    remove_legend <- TRUE
  }

  #### Create plot ####

  pl <- data %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes_string(
        x = avg_prob_col)
    ) +
    color_scale

  # Group plot
  if (!is.null(group_col)){
    pl <- pl +
      ggplot2::aes_string(color = group_col, group = group_col)
  }

  # Add ecdf plot
  pl <- pl +
    do.call(ggplot2::stat_ecdf, ecdf_settings)

  # Add faceting
  if (isTRUE(apply_facet)){
    pl <- pl +
      do.call(
        ggplot2::facet_wrap,
        c(
          facets = reformulate(paste0("`", of_col, "`")),
          facet_settings
        )
      )
  }

  # Tweak layout
  pl <- pl +
    ggplot2::coord_cartesian(xlim = xlim) +
    theme_fn() +
    ggplot2::theme(
      # Add margin to axis labels
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 6, 0, 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(0, 0, 6, 0)),
      axis.title.x.bottom = ggplot2::element_text(margin = ggplot2::margin(6, 0, 0, 0)),
      plot.caption = ggplot2::element_text(margin = ggplot2::margin(10, 0, 0, 0)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(size = 0.25)
    )

  # Prepare for y-axis label
  y_lab_prob_of <- dplyr::case_when(
    probability_of == "target" ~ "Target",
    probability_of == "prediction" ~ "Predicted",
    TRUE ~ ""
  )

  # Create caption text
  # caption <- caption_probability_plot_(
  #   add_caption = add_caption,
  #   probability_of = probability_of,
  #   add_points = add_points,
  #   add_hlines = add_hlines,
  #   apply_facet = apply_facet,
  #   remove_legend = remove_legend,
  #   group_col = group_col,
  #   lines_are_averages = lines_are_averages,
  #   smoothe = smoothe,
  #   smoothe_settings = smoothe_settings,
  #   str_width = 70
  # )

  caption <- paste0("Empirical Cumulative Distribution Function.\n",
                    "The percentage of probabilities below or equal to x.")

  # Add labels to axes
  pl <- pl +
    ggplot2::labs(x = paste0("Probability of ", y_lab_prob_of, " Class"),
                  y = paste0("% less than or equal to"),
                  caption = caption) +
    ggplot2::theme(plot.caption.position = "plot")


  # Remove legend if no groups
  # TODO What happens with only 1 group?
  if (isTRUE(remove_legend)){
    pl <- pl +
      ggplot2::theme(legend.position = "none")
  }

  pl
}
