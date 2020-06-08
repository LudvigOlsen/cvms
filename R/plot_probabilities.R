
# TODO Add setting to order probabilities left, right, happy face, sad face (align_max = "left/center/right")
# Add this reordering to the groupdata2 rearrange function and use that?

# TODO Add option to plot the distribution of class probabilities per observation and overall
#  - start by arranging by each of the probability cols from A-D

# TODO When hlines have same score, make sure each line has 1/(num overlapping lines) of the color on x-axis
# so we can see them all (must be possible)


#   __________________ #< c57c79b191073c821289f43e8a78cd1a ># __________________
#   Plot probabilities                                                      ####


#' @title Plot predicted probabilities
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Creates a \code{\link[ggplot2:ggplot]{ggplot2}} line plot object with the probabilities
#'  of either the target classes or the predicted classes.
#'
#'  The observations are ordered by the highest probability.
#'
#'  TODO line geom: average probability per observation
#'
#'  TODO points geom: actual probabilities per observation
#'
#'  The meaning of the \strong{horizontal lines} depend on the settings.
#'  These are either \emph{recall} scores, \emph{precision} scores,
#'  or \emph{accuracy} scores, depending on the \code{`probability_of`}
#'  and \code{`apply_facet`} arguments.
#'
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
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
#'  You can have multiple rows per observation ID per group. If, for instance, we
#'  have run repeated cross-validation of 3 classifiers, we would have one predicted probability
#'  per fold column per classifier.
#'
#'  As created with the various validation functions in \code{cvms}, like
#'  \code{\link[cvms:cross_validate_fn]{cross_validate_fn()}}.
#' @param targets_col Name of column with target levels.
#' @param probability_cols Name of columns with predicted probabilities.
#'
#'  For \strong{binary} classification, this should be \strong{one column} with the probability of the
#'  \strong{second class} (alphabetically).
#'
#'  For \strong{multiclass} classification, this should be \strong{one column per class}.
#'  These probabilities must sum to \code{1} row-wise.
#' @param predicted_class_col Name of column with predicted classes.
#'
#'  This is required when \code{probability_of = "prediction"} and/or \code{add_hlines = TRUE}.
#' @param obs_id_col Name of column with observation identifiers for grouping the \strong{x-axis}.
#'  When \code{NULL}, each row is an observation.
#'
#'  Use case: when you have multiple predicted probabilities per observation by a classifier
#'  (e.g. from repeated cross-validation).
#'
#'  Can also be a grouping variable that you wish to aggregate.
#' @param group_col Name of column with groups. The plot elements
#'  are split by these groups and can be identified by their color.
#'
#'  E.g. the \emph{classifier} responsible for the prediction.
#'
#'  \strong{N.B.} With more than \strong{\code{8}} groups,
#'  the default \code{`color_scale`} might run out of colors.
#' @param probability_of Whether to plot the probabilities of the
#'  target classes (\code{"target"}) or the predicted classes (\code{"prediction"}).
#'
#'  For each row, we extract the probability of either the
#'  \emph{target class} or the \emph{predicted class}. Both are useful
#'  to plot, as they show the behavior of the classifier in a way a confusion matrix doesn't.
#'  One classifier might be very certain in its predictions (whether wrong or right), whereas
#'  another might be less certain.
#' @param order How to order of the the probabilities. (Character)
#'
#'   One of: \code{"descending"}, \code{"ascending"}, and \code{"centered"}.
#' @param color_scale \code{ggplot2} color scale object for adding discrete colors to the plot.
#'
#'  E.g. the output of
#'  \code{\link[ggplot2:scale_colour_brewer]{ggplot2::scale_colour_brewer()}} or
#'  \code{\link[ggplot2:scale_colour_viridis_d]{ggplot2::scale_colour_viridis_d()}}.
#'
#'  \strong{N.B.} The number of colors in the object's palette should be at least the same as
#'  the number of groups in the \code{`group_col`} column.
#' @param add_points Add a point for each predicted probability.
#'  These are grouped on the x-axis by the \code{`obs_id_col`} column. (Logical)
#' @param add_hlines Add horizontal lines. (Logical)
#'
#'  The meaning of these lines depends on the \code{`probability_of`}
#'  and \code{`apply_facet`} arguments:
#'
#'  \tabular{rrr}{
#'   \strong{\code{apply_facet}} \tab \strong{\code{probability_of}} \tab \strong{Metric} \cr
#'   \code{FALSE} \tab \code{"target"} \tab \strong{Accuracy} \cr
#'   \code{FALSE} \tab \code{"prediction"} \tab \strong{Accuracy} \cr
#'   \code{TRUE} \tab \code{"target"} \tab \strong{Recall / Sensitivity} \cr
#'   \code{TRUE} \tab \code{"prediction"} \tab \strong{Precision / PPV} \cr
#'  }
#'
#' @param apply_facet Whether to use
#'  \code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap()}}. (Logical)
#' @param smoothe Whether to use \code{\link[ggplot2:geom_smooth]{ggplot2::geom_smooth()}} instead of
#'  \code{\link[ggplot2:geom_line]{ggplot2::geom_line()}}.
#'  This also adds a \code{95\%} confidence interval by default.
#'
#'  Settings can be passed via the \code{`smoothe_settings`} argument.
#' @param theme_fn The \code{ggplot2} theme function to apply.
#' @param line_settings Named list of arguments for \code{\link[ggplot2:geom_line]{ggplot2::geom_line()}}.
#'
#'   The \code{mapping} argument is set separately.
#'
#'   Any argument not in the list will use its default value.
#'
#'   Default: \code{list(size = 0.5)}
#'
#'   \strong{N.B.} Ignored when \code{smoothe = TRUE}.
#' @param smoothe_settings Named list of arguments for \code{\link[ggplot2:geom_smooth]{ggplot2::geom_smooth()}}.
#'
#'   The \code{mapping} argument is set separately.
#'
#'   Any argument not in the list will use its default value.
#'
#'   Default: \code{list(size = 0.5, alpha = 0.18, level = 0.95, se = TRUE)}
#'
#'   \strong{N.B.} Only used when \code{smoothe = TRUE}.
#' @param point_settings Named list of arguments for \code{\link[ggplot2:geom_point]{ggplot2::geom_point()}}.
#'
#'   The \code{mapping} argument is set separately.
#'
#'   Any argument not in the list will use its default value.
#'
#'   Default: \code{list(size = 0.1, alpha = 0.4)}
#' @param hline_settings Named list of arguments for \code{\link[ggplot2:geom_hline]{ggplot2::geom_hline()}}.
#'
#'   The \code{mapping} argument is set separately.
#'
#'   Any argument not in the list will use its default value.
#'
#'   Default: \code{list(size = 0.35, alpha = 0.5)}
#' @param facet_settings Named list of arguments for \code{\link[ggplot2:facet_wrap]{ggplot2::facet_wrap()}}.
#'
#'   The \code{facets} argument is set separately.
#'
#'   Any argument not in the list will use its default value.
#'
#'   Commonly set arguments are \code{nrow} and \code{ncol}.
#' @param ylim Limits for the y-scale.
#' @param verbose Whether to message the meaning of the horizontal lines. (Logical)
#'
#'  This might be used to add additional explanations in a later version.
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
#' # Plot probabilities of target classes
#' # From repeated cross-validation of three classifiers
#'
#' plot_probabilities(
#'   data = predicted.musicians,
#'   target_col = "Target",
#'   probability_cols = c("A", "B", "C", "D"),
#'   predicted_class_col = "Predicted Class",
#'   group = "Classifier",
#'   obs_id_col = "ID",
#'   probability_of = "target"
#' )
#'
#' # Plot probabilities of predicted classes
#' # From repeated cross-validation of three classifiers
#'
#' plot_probabilities(
#'   data = predicted.musicians,
#'   target_col = "Target",
#'   probability_cols = c("A", "B", "C", "D"),
#'   predicted_class_col = "Predicted Class",
#'   group = "Classifier",
#'   obs_id_col = "ID",
#'   probability_of = "prediction"
#' )
#'
#' # Center probabilities
#'
#' plot_probabilities(
#'   data = predicted.musicians,
#'   target_col = "Target",
#'   probability_cols = c("A", "B", "C", "D"),
#'   predicted_class_col = "Predicted Class",
#'   group = "Classifier",
#'   obs_id_col = "ID",
#'   probability_of = "prediction",
#'   order = "centered"
#' )
#'
#' #
#' # Binary
#' #
#'
#' # Filter the predicted.musicians dataset
#' binom_data <- predicted.musicians %>%
#'   dplyr::filter(Target %in% c("A", "B")) %>%
#'   # "B" is the second class alphabetically
#'   dplyr::rename(Probability = B) %>%
#'   dplyr::select(-c("A","C","D"))
#'
#' # Plot probabilities of predicted classes
#' # From repeated cross-validation of three classifiers
#'
#' plot_probabilities(
#'   data = binom_data,
#'   target_col = "Target",
#'   probability_cols = "Probability",
#'   predicted_class_col = "Predicted Class",
#'   group = "Classifier",
#'   obs_id_col = "ID",
#'   probability_of = "target"
#' )
#'
#' # Without faceting
#' # Horizontal lines become accuracies
#' plot_probabilities(
#'   data = binom_data,
#'   target_col = "Target",
#'   probability_cols = "Probability",
#'   predicted_class_col = "Predicted Class",
#'   group = "Classifier",
#'   obs_id_col = "ID",
#'   probability_of = "target",
#'   apply_facet = FALSE
#' )
#'
#' }
plot_probabilities <- function(data,
                               target_col,
                               probability_cols,
                               predicted_class_col = NULL,
                               obs_id_col = NULL,
                               group_col = NULL,
                               probability_of = "target",
                               order = "descending",
                               theme_fn = ggplot2::theme_minimal,
                               color_scale = ggplot2::scale_colour_brewer(palette = "Dark2"),
                               apply_facet = TRUE,
                               smoothe = FALSE,
                               add_points = !is.null(obs_id_col),
                               add_hlines = TRUE,
                               line_settings = list(),
                               smoothe_settings = list(),
                               point_settings = list(),
                               hline_settings = list(),
                               facet_settings = list(),
                               ylim = c(0, 1),
                               verbose = TRUE) {

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
    . ~ line_settings + smoothe_settings +
      point_settings + hline_settings +
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
  checkmate::assert_string(x = order, add = assert_collection)
  checkmate::assert_character(
    x = probability_cols,
    any.missing = FALSE,
    min.len = 1,
    names = "unnamed",
    unique = TRUE,
    add = assert_collection
  )

  ## Flag ####
  checkmate::assert_flag(x = add_points, add = assert_collection)
  checkmate::assert_flag(x = add_hlines, add = assert_collection)
  checkmate::assert_flag(x = apply_facet, add = assert_collection)

  ## Number ####
  checkmate::assert_numeric(
    x = ylim,
    len = 2,
    sorted = TRUE,
    any.missing = FALSE,
    names = "unnamed",
    unique = TRUE,
    add = assert_collection
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
  checkmate::assert_names(
    x = order,
    subset.of = c("descending", "ascending", "centered"),
    add = assert_collection
  )

  if (isTRUE(add_hlines) && is.null(predicted_class_col)) {
    assert_collection$push("When 'add_hlines' is TRUE, 'predicted_class_col' cannot be 'NULL'.")
  }

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
  line_settings <- update_hyperparameters(
    size = 0.5,
    hyperparameters = line_settings
  )
  smoothe_settings <- update_hyperparameters(
    alpha = 0.18,
    size = 0.5,
    level = 0.95,
    se = TRUE,
    hyperparameters = smoothe_settings
  )
  point_settings <- update_hyperparameters(
    size = 0.1,
    alpha = 0.4,
    hyperparameters = point_settings
  )
  hline_settings <- update_hyperparameters(
    size = 0.35,
    alpha = 0.5,
    hyperparameters = hline_settings
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
    cat_levels <- levels_as_characters(data[[of_col]])
  } else if (probability_of == "prediction"){
    of_col <- predicted_class_col
    # Make sure there are all the cat_levels for binomial case
    cat_levels <- union(
      levels_as_characters(data[[predicted_class_col]]),
      levels_as_characters(data[[target_col]])
    )
  }

  # Extract probabilities of target/prediction
  data[[prob_of_col]] <- extract_probabilities_of(
    data = data,
    probability_cols = probability_cols,
    of_col = of_col,
    cat_levels = cat_levels)

  # Remove probability cols
  data <- base_deselect(data, cols = probability_cols)

  # Order by IDs' average probability
  data <- add_id_aggregates(
    data = data,
    group_col = group_col,
    obs_id_col = obs_id_col,
    of_col = of_col,
    prob_of_col = prob_of_col,
    order = order,
    rank_col_name = rank_col,
    avg_prob_col = avg_prob_col
  )

  # Calculate horizontal lines
  # When 'probability_of' is 'target', these are recalls
  # When 'probability_of' is 'prediction', these are precisions
  # When 'apply_facet' is FALSE, these are accuracies
  if (isTRUE(add_hlines)) {
    if (isTRUE(apply_facet)){
      hline_by <- c(group_col, of_col)
    } else {
      hline_by <- group_col
    }
    hlines <- data %>%
      dplyr::group_by_at(hline_by) %>%
      dplyr::summarise(
        hline = mean(!!as.name(target_col) == !!as.name(predicted_class_col))
      )
    # Add scores to data
    data <- data %>%
      dplyr::left_join(hlines, by = hline_by)
  }

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
        x = rank_col, y = avg_prob_col)
    ) +
    color_scale

  # Group plot
  if (!is.null(group_col)){
    pl <- pl +
      ggplot2::aes_string(color = group_col, group = group_col)
  }

  # Add horizontal lines (recall or precision)
  if (isTRUE(add_hlines)){

    call_geom_hline <- function(...){
      ggplot2::geom_hline(ggplot2::aes_string(yintercept = "hline", color = group_col), ...)
    }

    pl <- pl +
      do.call(call_geom_hline, hline_settings)
  }

  # Add points
  if (isTRUE(add_points)){

    call_geom_point <- function(...){
      ggplot2::geom_point(ggplot2::aes_string(y = prob_of_col), ...)
    }

    pl <- pl +
      do.call(call_geom_point, point_settings)
  }

  # Add average probability lines
  if (isTRUE(smoothe)) {
    # Add geom_smooth with user's settings
    pl <- pl +
      do.call(ggplot2::geom_smooth, smoothe_settings)
  } else {
    pl <- pl +
      do.call(ggplot2::geom_line, line_settings)
  }

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
    ggplot2::coord_cartesian(ylim = ylim) +
    theme_fn() +
    ggplot2::theme(
      # Add margin to axis labels
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 6, 0, 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(0, 0, 6, 0)),
      axis.title.x.bottom = ggplot2::element_text(margin = ggplot2::margin(6, 0, 0, 0))
    )

  # Prepare for y-axis label
  y_lab_prob_of <- dplyr::case_when(
    probability_of == "target" ~ "Target",
    probability_of == "prediction" ~ "Predicted",
    TRUE ~ ""
  )

  # Add labels to axes
  pl <- pl +
    ggplot2::labs(x = paste0("Observations (", order, ")"),
                  y = paste0("Probability of ", y_lab_prob_of, " Class"))

  # Remove legend if no groups
  if (isTRUE(remove_legend)){
    pl <- pl +
      ggplot2::theme(legend.position = "none")
  }

  #### Explain plot ####

  if (isTRUE(verbose)){

    # Horizontal lines
    if (isTRUE(add_hlines)){
      hlines_are <- dplyr::case_when(
        !isTRUE(apply_facet) ~ "accuracy scores",
        probability_of == "target" ~ "recall/sensitivity scores",
        probability_of == "prediction" ~ "precision/PPV scores",
        TRUE ~ ""
      )
      message(paste0("Horizontal lines are ", hlines_are, "."))
    }

    # TODO Add one for confidence intervals when added
  }

  pl
}

#### Helpers ####

add_id_aggregates <- function(data, group_col, obs_id_col, of_col, prob_of_col,
                              order, rank_col_name = ".observation_rank",
                              avg_prob_col = ".avg_probability"){

  if (order == "descending") {
    arrange_fn <- function(data, col){dplyr::arrange(data, dplyr::desc(!!as.name(col)), .by_group = TRUE)}
  } else if (order == "ascending") {
    arrange_fn <- function(data, col){dplyr::arrange(data, !!as.name(col), .by_group = TRUE)}
  } else if (order == "centered") {
    arrange_fn <- function(data, col){groupdata2::center_max(data = data, col = col)}
  }

  # Order by IDs' average probability
  id_aggregates <- data %>%
    dplyr::group_by_at(c(group_col, of_col, obs_id_col)) %>%
    dplyr::summarise(!!avg_prob_col := mean(!!as.name(prob_of_col))) %>%
    dplyr::group_by_at(c(group_col, of_col)) %>%
    arrange_fn(col = avg_prob_col) %>%
    dplyr::group_by_at(c(group_col, of_col)) %>%
    dplyr::mutate(!!rank_col_name := seq_len(dplyr::n())) %>%
    dplyr::ungroup()

  # Add padding when centered
  if (order == "centered"){
    obs_per_group <- id_aggregates %>%
      dplyr::group_by_at(group_col) %>%
      dplyr::count(!!as.name(of_col))

    tmp_index <- create_tmp_name(id_aggregates, name = "max_val_index")
    max_val_indices_per_group <- id_aggregates %>%
      dplyr::group_by_at(c(group_col, of_col)) %>%
      dplyr::mutate(!!tmp_index := seq_len(dplyr::n())) %>%
      dplyr::filter(
        !!as.name(avg_prob_col) == max(!!as.name(avg_prob_col))
      ) %>%
      base_select(cols = c(group_col, of_col, tmp_index))

    tmp_max_ind <- create_tmp_name(id_aggregates, name = ".__max_ind__")
    tmp_pad_left <- create_tmp_name(id_aggregates, name = "padding_left")
    obs_per_group <- obs_per_group %>%
      dplyr::left_join(max_val_indices_per_group, by = c(group_col, of_col)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!tmp_max_ind := max(!!as.name(tmp_index)),
                    !!tmp_pad_left := max(!!as.name(tmp_max_ind)) - !!as.name(tmp_index)) %>%
      base_select(cols = c(group_col, of_col, tmp_pad_left))

    id_aggregates <- id_aggregates %>%
      dplyr::left_join(obs_per_group, by = c(group_col, of_col)) %>%
      dplyr::mutate(!!rank_col_name := !!as.name(rank_col_name) + !!as.name(tmp_pad_left))

    if (min(id_aggregates[[rank_col_name]]) > 0){
      id_aggregates[[rank_col_name]] <- id_aggregates[[rank_col_name]] - min(id_aggregates[[rank_col_name]])
    }

  }

  # Add ranks and aggregates to 'data'
  data <- id_aggregates %>%
    dplyr::right_join(data, by = c(group_col, of_col, obs_id_col))

  data
}

# # TODO Is this a reasonable approach to calculating CIs?
# Naaaaaaah doesn't seem so. Not without a lot more data points at least
# calc_lower_ci <- function(vec, level = .95){
#   quantile(vec, probs = 1-(level)/2)
# }
#
# calc_upper_ci <- function(vec, level = .95){
#   quantile(vec, probs = 1-(1-level)/2)
# }
#

