
# TODO Add setting to order probabilities left, right, happy face, sad face
# Add this reordering to the groupdata2 rearrange function and use that?

# TODO Add option to plot the distribution of class probabilities per observation and overall
#  - start by arranging by each of the probability cols from A-D

# TODO Option to add CI to lines instead of points (E.g. if there are a LOT of observations per id)

# Horizontal Lines:
# When 'probability_of' is 'target', these are recalls
# When 'probability_of' is 'prediction', these are precisions
# When 'apply_facet' is FALSE, these are accuracies
#
#
# binom_data <- predicted.musicians %>%
#   dplyr::filter(Target %in% c("A", "B")) %>%
#   dplyr::rename(Probability = B) %>%
#   dplyr::select(-c("A","C","D"))
#
# plot_probabilities(
#   data = binom_data,
#   target_col = "Target",
#   probability_cols = "Probability",
#   predicted_class_col = "Predicted Class",
#   group = "Classifier",
#   obs_id_col = "ID",
#   probability_of = "target"
# )
#
#
# plot_probabilities(
#   data = predicted.musicians,
#   target_col = "Target",
#   probability_cols = c("A", "B", "C", "D"),
#   predicted_class_col = "Predicted Class",
#   group = "Classifier",
#   obs_id_col = "ID",
#   probability_of = "target"
# )
#
plot_probabilities <- function(data,
                               target_col,
                               probability_cols,
                               predicted_class_col = NULL,
                               obs_id_col = NULL,
                               group_col = NULL,
                               probability_of = "target", # or prediction
                               theme_fn = ggplot2::theme_minimal,
                               color_scale = ggplot2::scale_colour_brewer(palette = "Dark2"),
                               add_points = ifelse(is.null(obs_id_col), FALSE, TRUE),
                               add_hlines = TRUE, # TODO Requires predicted_class_col
                               point_size = 0.1,
                               point_alpha = 0.4,
                               hline_size = 0.2,
                               hline_alpha = 0.5,
                               apply_facet = TRUE,
                               facet_nrow = NULL,
                               facet_ncol = NULL,
                               facet_strip_position = "top",
                               ylim = c(0,1),
                               verbose = TRUE) {


  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = group_col, null.ok = TRUE, add = assert_collection)

  if (isTRUE(add_hlines) && is.null(predicted_class_col)){
    assert_collection$push(
      "When 'add_sensitivities' is TRUE, 'predicted_class_col' cannot be 'NULL'."
    )
  }

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

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
    rank_col_name = rank_col
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
      dplyr::summarise(hline = mean(!!as.name(target_col) == !!as.name(predicted_class_col)))
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
        x = rank_col, y = "avg_probability")
    ) +
    color_scale

  # Group plot
  if (!is.null(group_col)){
    pl <- pl +
      ggplot2::aes_string(color = group_col, group = group_col)
  }

  # Add horizontal lines (recall or precision)
  if (isTRUE(add_hlines)){
    pl <- pl +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = hline,
                     color = !!as.name(group_col)),
        size = hline_size, alpha = hline_alpha)
  }

  # Add points
  if (isTRUE(add_points)){
    pl <- pl +
      ggplot2::geom_point(ggplot2::aes(y = !!as.name(prob_of_col)),
                          size = point_size, alpha = point_alpha)
  }

  y_lab_prob_of <- dplyr::case_when(
    probability_of == "target" ~ "Target",
    probability_of == "prediction" ~ "Predicted",
    TRUE ~ ""
  )

  pl <- pl +
    ggplot2::stat_summary(fun = mean, geom = "line")    # fun.max = max,fun.min = min, could be sd or 95% CI?

  if (isTRUE(apply_facet)){
    pl <- pl +
      ggplot2::facet_wrap(
        reformulate(paste0("`", of_col, "`")),
        nrow = facet_nrow,
        ncol = facet_ncol,
        strip.position = facet_strip_position
      )
  }

  pl <- pl +
    ggplot2::coord_cartesian(ylim = ylim) +
    ggplot2::labs(x = "Observation Rank",
                  y = paste0("Probability of ", y_lab_prob_of, " Class")) +
    theme_fn() +
    ggplot2::theme(
      # Add margin to axis labels
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(0, 6, 0, 0)),
      axis.title.x.top = ggplot2::element_text(margin = ggplot2::margin(0, 0, 6, 0)),
      axis.title.x.bottom = ggplot2::element_text(margin = ggplot2::margin(6, 0, 0, 0))
    )

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
                              rank_col_name = ".observation_rank"){

  # Order by IDs' average probability
  id_aggregates <- data %>%
    dplyr::group_by_at(c(group_col, of_col, obs_id_col)) %>%
    dplyr::summarise(avg_probability = mean(!!as.name(prob_of_col))) %>%
    dplyr::group_by_at(c(group_col, of_col)) %>%
    dplyr::arrange(dplyr::desc(.data$avg_probability), .by_group = TRUE) %>%
    dplyr::mutate(!!rank_col_name := seq_len(dplyr::n())) %>%
    dplyr::ungroup()

  # Add ranks and aggregates to 'data'
  data <- id_aggregates %>%
    dplyr::right_join(data, by = c(group_col, of_col, obs_id_col))

  data
}
