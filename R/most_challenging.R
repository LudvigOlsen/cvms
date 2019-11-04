# TODO Perhaps rename now that it is based on a threshold instead of a relative comparison?
# threshold percentage it should be wrongly predicted to be included
most_challenging <- function(data,
                             obs_col = "Observation",
                             targets_col = "Target",
                             predictions_col = "Predicted Class", # TODO Should depend on family?
                             family = "binomial",
                             threshold = 0.85){

  # If the dataset is grouped, we need the indices and keys for the groups
  # so we can evaluate group wise
  # grouping_factor <- dplyr::group_indices(data)
  grouping_keys <- dplyr::group_keys(data)

  ### CLASSIFICATION

  if (family %in% c("binomial", "multinomial")){
    # Ensure same type
    data[[targets_col]] <- as.character(data[[targets_col]])
    data[[predictions_col]] <- as.character(data[[predictions_col]])

    data[["Correct"]] <- data[[predictions_col]] == data[[targets_col]]
    data[["Incorrect"]] <- data[[predictions_col]] != data[[targets_col]]

    by_observation <- data %>%
      dplyr::group_by_at(c(colnames(grouping_keys), obs_col)) %>%
      dplyr::summarise(Correct = sum(.data$Correct),
                       Incorrect = sum(.data$Incorrect),
                       `Percentage Correct` = Correct/dplyr::n())

    return(
      by_observation[by_observation[["Percentage Correct"]] < (1-threshold),]
    )

  } else if (family == "gaussian"){
    stop("Not yet implemented!")
  }

}
