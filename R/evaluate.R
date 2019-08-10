# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


# TODO Find another name, as evaluate is already in generics::evaluate
# Isn't there an appropriate synonym?
evaluate <- function(data,
                     dependent_col,
                     prediction_cols,
                     id_col = NULL,
                     id_method = "mean", # or majority
                     model = NULL,
                     type = "linear_regression",
                     apply_softmax = TRUE,
                     cutoff = 0.5,
                     positive = 2
){

  # Test if family is allowed
  stopifnot(type %in% c("gaussian", "gaussian_regression", "linear_regression",
                        "binomial", "binomial_classification", "binary_classification",
                        "multinomial", "multinomial_classification", "multiclass_classification")) #"multilabel"))

  # Convert families to the internally used
  if (type %in% c("gaussian", "gaussian_regression", "linear_regression")){
    family <- "gaussian"}
  if (type %in% c("binomial", "binomial_classification", "binary_classification")){
    family <- "binomial"}
  if (type %in% c("multinomial", "multinomial_classification", "multiclass_classification")){
    family <- "multinomial"}

  # Check the passed arguments TODO Add more checks
  check_args_evaluate(data = data,
                      dependent_col = dependent_col,
                      prediction_cols = prediction_cols,
                      id_col = id_col,
                      model = model,
                      type = type,
                      apply_softmax = apply_softmax,
                      cutoff = cutoff,
                      positive = positive)

  model_specifics <- list(
    model_formula = "",
    family = family,
    REML = FALSE,
    link = NULL,
    cutoff = cutoff,
    positive = positive,
    model_verbose = FALSE
  ) %>%
    basics_update_model_specifics()

  # Create temporary prediction column name
  local_tmp_predictions_col_var <- create_tmp_var(data, "tmp_predictions_col")

  if (!is.null(id_col)) {
    data_for_id_evaluation <- prepare_id_level_evaluation(
      data = data,
      dependent_col = dependent_col,
      prediction_cols = prediction_cols,
      family = family,
      id_col = id_col,
      id_method = id_method,
      apply_softmax = FALSE,
      new_prediction_col_name = local_tmp_predictions_col_var
    ) %>% dplyr::ungroup()

  }

  if (family == "multinomial"){

    data <- prepare_multinomial_evaluation(data = data,
                                           dependent_col = dependent_col,
                                           prediction_cols = prediction_cols,
                                           apply_softmax = apply_softmax,
                                           new_prediction_col_name = local_tmp_predictions_col_var
    )

    prediction_cols <- local_tmp_predictions_col_var

  } else {
    if (length(prediction_cols) > 1) {
      stop(paste0("'prediction_cols' must have length 1 when family is '", family, "'."))
    }
  }

  evaluations <- run_evaluate_wrapper(
      data = data,
      type = family,
      predictions_col = prediction_cols,
      targets_col = dependent_col,
      models = model,
      model_specifics = model_specifics
    )

  if (!is.null(id_col)){
    id_evaluations <- run_evaluate_wrapper(
      data = data_for_id_evaluation,
      type = family,
      predictions_col = prediction_cols,
      targets_col = dependent_col,
      models = model,
      model_specifics = model_specifics
    )

    return(
      list("Evaluation" = evaluations,
           "ID_evaluation" = id_evaluations)
    )
  } else {
    return(evaluations)
  }

}


run_evaluate_wrapper <- function(
  data, type, predictions_col, targets_col, models,
  fold_info_cols = NULL, model_specifics) {

  if (is.null(fold_info_cols)){

    # Add fold columns. They are not really used but are currently required.
    local_tmp_fold_col_var <- create_tmp_var(data, "fold_column")
    local_tmp_rel_fold_col_var <- create_tmp_var(data, "rel_fold")
    local_tmp_abs_fold_col_var <- create_tmp_var(data, "abs_fold")
    data[[local_tmp_fold_col_var]] <- 1
    data[[local_tmp_rel_fold_col_var]] <- 1
    data[[local_tmp_abs_fold_col_var]] <- 1

    fold_info_cols = list(rel_fold = local_tmp_rel_fold_col_var,
                          abs_fold = local_tmp_abs_fold_col_var,
                          fold_column = local_tmp_fold_col_var)
  }

  evaluations <- internal_evaluate(data = data,
                                   type = type,
                                   predictions_col = predictions_col,
                                   targets_col = targets_col,
                                   models = models,
                                   fold_info_cols = fold_info_cols,
                                   model_specifics = model_specifics)

  evaluations

}

prepare_id_level_evaluation <- function(data,
                                        dependent_col,
                                        prediction_cols,
                                        family,
                                        id_col,
                                        id_method,
                                        apply_softmax = length(prediction_cols) > 1,
                                        new_prediction_col_name = "prediction") {

  # Prepare data for id evaluation

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

  # Add actual class
  id_classes <- data %>%
    dplyr::select(!!as.name(id_col), !!as.name(dependent_col)) %>%
    dplyr::distinct()

  if (id_method == "mean") {

    data_for_id_evaluation <- data %>%
      dplyr::group_by(!!as.name(id_col)) %>%
      dplyr::summarise_at(dplyr::vars(prediction_cols), .funs = mean) %>%
      dplyr::left_join(id_classes, by = id_col)

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
        dplyr::group_by(!!as.name(id_col)) %>%
        dplyr::count(.data$predicted_class)

      data_majority_count_by_id <- data_majority_count_by_id %>%
        dplyr::left_join(id_classes, by = id_col)

      # In case not all classes were predicted
      # We add them with NA values
      # TODO Add unit test to make sure this part works !!!! !!!! !!!!
      classes_to_add <- setdiff(prediction_cols,
                                data_majority_count_by_id[["predicted_class"]])

      if (length(classes_to_add) > 0){
        na_cols <- dplyr::bind_rows(setNames(rep(
          list(rep(NA, nlevels(factor(data_majority_count_by_id[[id_col]])))),
          length(classes_to_add)), classes_to_add))
      }

      # Spread to probability tibble with NAs where NO majority was
      majority_vote_probabilities <- data_majority_count_by_id %>%
        tidyr::spread(key = "predicted_class", value = "n")


      if (length(classes_to_add) > 0){
        majority_vote_probabilities <- majority_vote_probabilities %>%
          dplyr::bind_cols(na_cols) %>%
          dplyr::select(dplyr::one_of(id_col, prediction_cols))
      }

      # Set NAs to 0
      majority_vote_probabilities[is.na(majority_vote_probabilities)] <- 0

      # Make all the majority votes ~10000
      # so they will be extremely close to 1 when applying softmax
      # then we will apply softmax in a later step
      majority_vote_probabilities <- majority_vote_probabilities %>%
        dplyr::mutate_at(prediction_cols, function(x){(x/(x+1e-30))*1e+4})

      data_for_id_evaluation <- majority_vote_probabilities

    } else if (family == "binomial"){
      stop("not yet implemented")
    } else {
      stop(paste0("family ", family, " not currently supported for ID evaluation."))
    }
  }

  if (family == "multinomial") {

    data_for_id_evaluation <- prepare_multinomial_evaluation(
      data_for_id_evaluation,
      dependent_col = dependent_col,
      prediction_cols = prediction_cols,
      apply_softmax = TRUE,
      new_prediction_col_name = new_prediction_col_name
    )

  }

  data_for_id_evaluation

}

prepare_multinomial_evaluation <- function(data,
                                           dependent_col,
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
    nest_probabilities_rowwise()

  # TODO Do we need to do anything to the dependent column? E.g. make numeric or check against names in prediction_cols?
  # TODO What if the classes are numeric, then what will prediction_cols contain?

  if (length(setdiff(levels_as_characters(data[[dependent_col]]), prediction_cols)) > 0){
    stop("Not all levels in 'dependent_col' was found in 'prediction_cols'.")
  }

  data

}

internal_evaluate <- function(data,
                              type = "gaussian",
                              predictions_col = "prediction",
                              targets_col = "target",
                              fold_info_cols = list(rel_fold = "rel_fold",
                                                    abs_fold = "abs_fold",
                                                    fold_column = "fold_column"),
                              models = NULL,
                              model_specifics = list()) {


  stopifnot(type %in% c("linear_regression", "gaussian", "binomial", "multinomial")) #, "multiclass", "multilabel"))
  if (type == "linear_regression") type <- "gaussian"

  # data is a table with predictions, targets and folds
  # predictions can be values, logits, or classes, depending on evaluation type

  if (type == "gaussian") {
    results <- linear_regression_eval(
      data,
      models = models,
      predictions_col = predictions_col,
      targets_col = targets_col,
      fold_info_cols = fold_info_cols,
      model_specifics = model_specifics)

  } else if (type == "binomial"){

    results <- binomial_classification_eval(
      data,
      predictions_col = predictions_col,
      targets_col = targets_col,
      fold_info_cols = fold_info_cols,
      models = models,
      cutoff = model_specifics[["cutoff"]],
      positive = model_specifics[["positive"]])

  } else if (type == "multinomial"){

    results <- multinomial_classification_eval(
      data,
      predictions_col = predictions_col,
      targets_col = targets_col,
      id_col = id_col,
      fold_info_cols = fold_info_cols,
      models = models)

  }


  return(results)
}

check_args_evaluate <- function(data,
                                dependent_col,
                                prediction_cols,
                                id_col,
                                model,
                                type,
                                apply_softmax,
                                cutoff,
                                positive){

  # TODO Add more checks !!

  # Check columns
  # dependent_col
  if (!is.character(dependent_col)) {
    stop("'dependent_col' must be name of the dependent column in 'data'.")
  }
  if (dependent_col %ni% colnames(data)){
    stop(paste0("the 'dependent_col', ",dependent_col,", was not found in 'data'"))
  }

  # prediction_cols
  if (!is.character(dependent_col)) {
    stop("'prediction_cols' must be name(s) of the prediction column(s) in 'data'.")
  }
  if (length(setdiff(prediction_cols, colnames(data))) > 0) {
    stop("not all names in 'prediction_cols' was found in 'data'.")
  }

  # id_col
  if (!is.null(id_col) && !is.character(id_col)) {
    stop("'id_col' must be either a column name or NULL.")
  }
  if (type == "gaussian" && !is.null(id_col)) {
    warning(paste0("'id_col' is ignored when type is '", type, "'."))
  }
  if (id_col %ni% colnames(data)) {
    stop(paste0("the 'id_col', ", id_col, ", was not found in 'data'."))
  }

  # softmax
  if (!is.logical(apply_softmax) || is.na(apply_softmax)){
    stop("'apply_softmax' must be logical (TRUE/FALSE).")
  }

  # TODO add for rest of args

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
