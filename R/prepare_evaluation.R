# Preparing data for evaluation

prepare_id_level_evaluation <- function(data,
                                        target_col,
                                        prediction_cols,
                                        family,
                                        id_col,
                                        id_method,
                                        groups_col,
                                        cutoff = NULL,
                                        apply_softmax = length(prediction_cols) > 1,
                                        new_prediction_col_name = "prediction") {

  # Prepare data for id evaluation

  if (is.null(id_col)){
    stop("'id_col' was NULL.")
  }

  if (!is.character(id_col)) {
    stop("'id_col' must be either the name of a column in 'data' or NULL.")
  }
  if (id_col %ni% colnames(data)) {
    stop(paste0("could not find 'id_col', ", id_col, ", in 'data'."))
  }

  if (isTRUE(apply_softmax)) {
    if (length(prediction_cols) < 2) {
      stop("can only apply softmax when there are more than one 'prediction_cols'.")
    }
    data <- softmax(data, cols = prediction_cols)
  }

  if (family == "binomial"){
    if (is.null(cutoff)){
      stop("when 'family' is 'binomial', 'cutoff' must be numeric between 0 and 1.")
    }
    if (is.null(cutoff)){
      stop("when 'family' is 'binomial', 'cutoff' must be numeric between 0 and 1.")
    }
  }

  num_groups <- length(unique(data[[groups_col]]))

  # Add actual class
  id_classes <-
    extract_id_classes(
      data = data,
      groups_col = groups_col,
      id_col = id_col,
      target_col = target_col
    )

  if (id_method == "mean") {

    data_for_id_evaluation <- data %>%
      dplyr::group_by(!!as.name(groups_col), !!as.name(id_col)) %>%
      dplyr::summarise_at(dplyr::vars(prediction_cols), .funs = mean) %>%
      dplyr::left_join(id_classes, by = c(id_col, groups_col)) %>%
      dplyr::ungroup()

  } else if (id_method == "majority"){

    ## By majority vote
    # If multiple classes share the majority, they also share the probability! #mustShare!

    if (family == "multinomial"){

      data[["predicted_class_index"]] <- data %>%
        dplyr::select(dplyr::one_of(prediction_cols)) %>%
        argmax()
      data[["predicted_class"]] <- purrr::map_chr(
        data[["predicted_class_index"]],
        .f = function(x){prediction_cols[[x]]})

      data_majority_count_by_id <- data %>%
        dplyr::group_by(!!as.name(groups_col), !!as.name(id_col)) %>%
        dplyr::count(.data$predicted_class) %>%
        dplyr::left_join(id_classes, by = c(id_col, groups_col)) %>%
        dplyr::ungroup()

      # In case not all classes were predicted
      # We add them with NA values
      # TODO Add unit test to make sure this part works !!!! !!!! !!!!
      classes_to_add <- setdiff(prediction_cols, # use for testing: c(prediction_cols, "cl_7"),
                                data_majority_count_by_id[["predicted_class"]])

      if (length(classes_to_add) > 0){
        na_cols <- dplyr::bind_rows(setNames(rep(
          list(rep(NA, num_groups * nlevels(factor(data_majority_count_by_id[[id_col]])))),
          length(classes_to_add)), classes_to_add))
      }

      # Spread to probability tibble with NAs where NO majority was
      majority_vote_probabilities <- data_majority_count_by_id %>%
        tidyr::spread(key = "predicted_class", value = "n")


      if (length(classes_to_add) > 0){
        majority_vote_probabilities <- majority_vote_probabilities %>%
          dplyr::bind_cols(na_cols) %>%
          dplyr::select(dplyr::one_of(
            groups_col, id_col, target_col, prediction_cols))
      }

      # Set NAs to 0
      majority_vote_probabilities[is.na(majority_vote_probabilities)] <- 0

      # Make all the majority votes ~10000
      # so they will be extremely close to 1 when applying softmax
      # then we will apply softmax in a later step
      majority_vote_probabilities <- majority_vote_probabilities %>%
        dplyr::mutate_at(prediction_cols, function(x){(x/(x+1e-30))*1e+4})

      data_for_id_evaluation <- majority_vote_probabilities %>%
        dplyr::ungroup()

    } else if (family == "binomial"){

      if (length(prediction_cols)>1){
        stop("when 'family' is 'binomial', length of 'prediction_cols' should be 1.")
      }

      data[["predicted_class"]] <- ifelse(data[[prediction_cols]] > cutoff, 1, 0)

      # Create majority count by id
      data_for_id_evaluation <- data %>%
        dplyr::group_by(!!as.name(groups_col), !!as.name(id_col)) %>%
        dplyr::summarise(mean_prediction = mean(.data$predicted_class)) %>%
        dplyr::mutate(mean_prediction = dplyr::case_when(
          mean_prediction > cutoff ~ 1-1e-40, # Almost 1
          mean_prediction == cutoff ~ cutoff, # split decision (unlikely to happen)
          mean_prediction < cutoff ~ 1e-40 # Almost 0
        )) %>%
        dplyr::rename_at("mean_prediction", ~prediction_cols) %>%
        dplyr::left_join(id_classes, by = c(id_col, groups_col)) %>%
        dplyr::ungroup()

    } else {
      stop(paste0("family ", family, " not currently supported for majority vote aggregated ID evaluation."))
    }
  }

  if (family == "multinomial") {

    data_for_id_evaluation <- prepare_multinomial_evaluation(
      data_for_id_evaluation,
      target_col = target_col,
      prediction_cols = prediction_cols,
      apply_softmax = TRUE,
      new_prediction_col_name = new_prediction_col_name
    )

  }

  data_for_id_evaluation

}

prepare_multinomial_evaluation <- function(data,
                                           target_col,
                                           prediction_cols,
                                           apply_softmax,
                                           new_prediction_col_name){

  # Split data into probabilities and the rest
  col_split <- extract_and_remove_probability_cols(data = data,
                                                   prediction_cols = prediction_cols)
  predicted_probabilities <- col_split[["predicted_probabilities"]]
  data <- col_split[["data"]]

  if (isTRUE(apply_softmax)){
    predicted_probabilities <- softmax(predicted_probabilities)
  }

  data[[new_prediction_col_name]] <- predicted_probabilities %>%
    nest_rowwise()

  # TODO Do we need to do anything to the dependent column? E.g. make numeric or check against names in prediction_cols?
  # TODO What if the classes are numeric, then what will prediction_cols contain?

  if (length(setdiff(levels_as_characters(data[[target_col]]), prediction_cols)) > 0){
    stop("Not all levels in 'target_col' was found in 'prediction_cols'.")
  }

  data

}

extract_and_remove_probability_cols <- function(data, prediction_cols) {
  # Split data into probabilities and the rest
  predicted_probabilities <- data %>%
    dplyr::select(dplyr::one_of(prediction_cols))
  data <- data %>%
    dplyr::select(-dplyr::one_of(prediction_cols))

  # Test that the probability columns are numeric
  if (any(!sapply(predicted_probabilities, is.numeric))) {
    stop("the prediction columns must be numeric.")
  }

  list("data" = data,
       "predicted_probabilities" = predicted_probabilities)
}

extract_id_classes <- function(data, groups_col, id_col, target_col){

  id_classes <- data %>%
    dplyr::select(!!as.name(groups_col), !!as.name(id_col), !!as.name(target_col)) %>%
    dplyr::distinct()

  # Make sure the targets are constant within IDs
  check_constant_targets_within_ids(distinct_data = id_classes,
                                    groups_col = groups_col,
                                    id_col = id_col)

  id_classes
}

check_constant_targets_within_ids <- function(distinct_data, groups_col, id_col){

  # Checks that there's only one unique value for each ID (per group)

  counts <- distinct_data %>%
    dplyr::group_by(!!as.name(groups_col), !!as.name(id_col)) %>%
    dplyr::summarize(n = dplyr::n())

  if (any(counts$n > 1)){
    stop(paste0("The targets must be constant within the IDs with the current ID method. ",
                "These IDs had more than one unique value in the target column: ",
                paste0(counts %>%
                         dplyr::filter(.data$n>1) %>%
                         dplyr::pull(!!as.name(id_col)),
                       collapse = ", "),"."))
  }
}
