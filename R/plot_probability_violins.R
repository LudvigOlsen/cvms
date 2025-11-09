# str_wrap_class = TRUE, #  with the name mapping this can be done outside the function (Part of demo?)
# str_wrap_width = 15,


# Making new class name mapping
# nm <- as.list(levels(data$class))
# names(nm) <- c(<<new names>>) # could be paste or formatting step

# plot_probability_violins <- function(
#   data,
#   switch_faceting = FALSE,
#   apply_facet = TRUE,
#   probability_of = "prediction",
#   class_name_mapping = NULL,
#   ylim = c("auto", 1) # Does this make sense when probability_of = "target" ??
#
# ) {
#
#   data <- data %>%
#     tidyr::gather(key = "Class", value = "Prob", 6:9) %>%
#     dplyr::mutate(Class = factor(.data$Class),
#                   Target = factor(.data$Target),
#                   `Predicted Class` = factor(.data$`Predicted Class`))
#
#   if (probability_of == "prediction") {
#     data <- data %>%
#       dplyr::filter(Class == .data$`Predicted Class`)
#
#     # Make sure there are all the cat_levels for binomial case
#     cat_levels <- sort(union(
#       levels_as_characters(data[["Predicted Class"]], drop_unused = TRUE),
#       levels_as_characters(data[["Target"]], drop_unused = TRUE)
#     ))
#   } else {
#     # TODO Is this the right implementation (look at other plot functions)
#     # Also, is this a meaningful plot?
#     data <- data %>%
#       dplyr::filter(.data$Class == .data$Target)
#
#     cat_levels <- levels_as_characters(data[["Target"]], drop_unused = TRUE, sort_levels=TRUE)
#   }
#
#   if (is.character(ylim) && probability_of == "prediction"){
#     # Since the largest probability will always be >= 1/C
#     # We don't need to show below that
#     ylim <- c(1/length(cat_levels), as.numeric(ylim[[2]]))
#   }
#
#   # Rename all three class variables with the name map
#   # TODO Check is list with list(new_name = old_name, )
#   # Also check all are there? Or does it work with fewer/too many?
#   if (!is.null(class_name_mapping)){
#     levels(data$Class) <- class_name_mapping
#     levels(data$Target) <- class_name_mapping
#     levels(data$`Predicted Class`) <- class_name_mapping
#   }
#
#   if (!isTRUE(switch_faceting)) {
#     pl <- data %>%
#       ggplot2::ggplot(ggplot2::aes(y = .data$Prob, x = .data$Class, fill = .data$Class))
#
#     if (isTRUE(apply_facet)){
#       pl <- pl + ggplot2::facet_wrap(. ~ .data$Classifier)
#     }
#   } else {
#     pl <- data %>%
#       ggplot2::ggplot(ggplot2::aes(y = .data$Prob, x = .data$Classifier, fill = .data$Classifier))
#
#     if (isTRUE(apply_facet)){
#       pl <- pl + ggplot2::facet_wrap(. ~ .data$Class)
#     }
#   }
#
#   pl +
#     ggplot2::coord_cartesian(ylim = ylim) +
#     see::geom_violinhalf(color = NA, alpha = 0.8) +
#     ggplot2::scale_fill_brewer(palette = "Dark2") +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(axis.text.x = ggplot2::element_text(
#       angle = 45,
#       vjust = 1,
#       hjust = 1
#     )) +
#     ggplot2::theme(legend.position = "none")
#
# }

# plot_probability_violins(predicted.musicians, switch_faceting = T,
#                          #probability_of = "target",
#                          apply_facet = F, # For apply_facet = F the switch_faceting name isn't obvious for changing between classes and classifiers
#                          class_name_mapping = list("AAAAA" = "A", "BBBBB" = "B",
#                                                    "CCCCC"="C", "DDDDD" = "D"))
# plot_probability_violins(predicted.musicians, switch_faceting = T)
#
# predicted.musicians %>%
#   tidyr::gather(key="Class", value="Prob", 6:9) %>%
#   dplyr::filter(Class == `Predicted Class`) %>%
#   ggplot(aes(y = Prob, x = Classifier, fill = Classifier)) +
#   coord_cartesian(ylim = c(0, 1)) +
#   see::geom_violinhalf() +
#   facet_wrap(. ~ Class) +
#   ggplot2::scale_fill_brewer(palette = "Dark2") +
#   theme_minimal()
#
# predicted.musicians %>%
#   tidyr::gather(key="Class", value="Prob", 6:9) %>%
#   dplyr::filter(Class == `Predicted Class`) %>%
#   ggplot(aes(y = Prob, x = Classifier, fill = Classifier)) +
#   coord_cartesian(ylim = c(0, 1)) +
#   see::geom_violinhalf() +
#   #facet_wrap(. ~ Classifier) +
#   ggplot2::scale_fill_brewer(palette = "Dark2") +
#   theme_minimal()
#
#
#
# plot_probabilities(
#   data = predicted.musicians,
#   target_col = "Target",
#   probability_cols = c("A", "B", "C", "D"),
#   predicted_class_col = "Predicted Class",
#   group_col = "Classifier",
#   obs_id_col = "ID",
#   probability_of = "target"
# )
#
# plot_probabilities_ecdf(
#   data = predicted.musicians,
#   target_col = "Target",
#   probability_cols = c("A", "B", "C", "D"),
#   predicted_class_col = "Predicted Class",
#   group_col = "Classifier",
#   probability_of = "target"
# )
