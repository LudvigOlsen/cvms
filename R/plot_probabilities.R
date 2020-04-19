
# TODO Add setting to order probabilities left, right, happy face, sad face
# Add this reordering to the groupdata2 rearrange function and use that?

# TODO Allow turning off facet grid, so you would just have one plot with prob of target

plot_probabilities <- function(data, target_col, probability_cols, id_col = NULL, group_col = NULL,
                               theme_fn = ggplot2::theme_minimal,
                               add_points = ifelse(is.null(id_col), FALSE, TRUE)){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = group_col, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  data <- dplyr::ungroup(data)
  data <- base_select(data, cols = c(
    group_col, id_col, target_col, probability_cols))

  if (is.null(id_col)){
    id_col <- create_tmp_name(data, "Observation")
    data <- data %>%
      dplyr::group_by_at(group_col) %>%
      dplyr::mutate(!!id_col := seq_len(dplyr::n()))
  }

  # Ensure ID column is factor
  data[[id_col]] <- as.factor(data[[id_col]])

  # Name for target probability column
  target_prob_col <- create_tmp_name(data, "probability_of_target")

  # Multinomial
  if (length(probability_cols) > 1) {
    # TODO This is used both here and in most_challenging
    # Convert to helper function

    # Extract probability of target class
    target_index_map <-
      as.list(setNames(seq_along(probability_cols), probability_cols))
    target_indices <- purrr::map_int(
      data[[target_col]],
      .f = function(x) {
        target_index_map[[x]]
      }
    )
    data[[target_prob_col]] <- as.data.frame(data[, probability_cols])[
      cbind(seq_along(target_indices), target_indices)]

  } else {
    # Binomial

    cat_levels <- levels_as_characters(data[[target_col]])
    if (length(cat_levels) != 2) {
      stop("When 'probability_cols' has length 1, the 'target_col' column should have 2 levels.")
    }

    positive <- cat_levels[2]

    # Probability of target
    data[[target_prob_col]] <-
      ifelse(data[[target_col]] == positive,
             data[[probability_cols]],
             1 - data[[probability_cols]])

  }

  # Remove probability cols
  data <- base_deselect(data, cols = probability_cols)

  # Order by IDs' average probability
  id_aggregates <- data %>%
    dplyr::group_by_at(c(group_col, target_col, id_col)) %>%
    dplyr::summarise(avg_prob = mean(!!as.name(target_prob_col))) %>%
    dplyr::group_by_at(c(group_col, target_col)) %>%
    dplyr::arrange(dplyr::desc(.data$avg_prob), .by_group = TRUE) %>%
    dplyr::mutate(.observation_rank = seq_len(dplyr::n())) %>%
    dplyr::ungroup()

  data <- id_aggregates %>%
    dplyr::right_join(data, by = c(group_col, target_col, id_col))

  # Create plot
  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = .observation_rank, y = avg_prob))

  # Group plot
  if (!is.null(group_col)){
    pl <- pl + ggplot2::aes(color = !!as.name(group_col), group = !!as.name(group_col))
  }

  # Add points
  if (isTRUE(add_points)){
    pl <- pl +
      ggplot2::geom_point(ggplot2::aes(y = !!as.name(target_prob_col)),
                          size = 0.1, alpha = 0.4)
  }

  pl <- pl +
    ggplot2::stat_summary(fun = mean, geom = "line") +    # fun.max = max,fun.min = min, could be sd or 95% CI?
    ggplot2::facet_grid(reformulate(target_col)) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    theme_fn() +
    ggplot2::labs(x = "Observation Rank", y = "Probability of Target Class")

  pl
}

# plot_probabilities(
#   data = predicted.musicians,
#   target_col = "Target",
#   probability_cols = c("A", "B", "C", "D")# ,
#   id_cl = "ID"
# )
