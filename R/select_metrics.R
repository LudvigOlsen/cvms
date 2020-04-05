
#' @title Select columns with evaluation metrics and model definitions.
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  When reporting results, we might not want all
#'  the nested tibbles and process information columns.
#'  This function selects the evaluation metrics and model formulas only.
#'
#'  If an expected column is not in the \code{results} tibble, it is simply ignored.
#' @return
#'  The results tibble with only the metric and model definition columns.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param results Results tibble. E.g. from
#'  \code{\link[cvms:cross_validate]{cross_validate()}} or \code{\link[cvms:evaluate]{evaluate()}}.
#' @param include_definitions Whether to include the \code{Dependent},
#'  \code{Fixed} and (possibly) \code{Random} and \code{HParams} columns. (Logical)
#' @param additional_includes Names of additional columns to select. (Character)
select_metrics <- function(results, include_definitions = TRUE,
                           additional_includes = NULL) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(
    x = results,
    min.rows = 1, min.cols = 1,
    col.names = "named",
    add = assert_collection
  )
  checkmate::assert_flag(x = include_definitions, add = assert_collection)
  checkmate::assert_character(x = additional_includes, null.ok = TRUE, add = assert_collection)
  if (!is.null(additional_includes)) {
    checkmate::reportAssertions(assert_collection)
    checkmate::assert_names(
      x = colnames(results),
      must.include = additional_includes,
      what = "colnames"
    )
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  model_formula_cols <- c("Dependent", "Fixed", "Random", "HParams")
  model_formula_cols <- intersect(model_formula_cols, colnames(results))

  # TODO What about "Convergence Warnings"? People should be aware of this!

  metric_cols <- c(
    # Gaussian
    "RMSE", "MAE",
    "NRMSE(RNG)", "NRMSE(IQR)", "NRMSE(STD)", "NRMSE(AVG)",         # TODO Order the new metrics meaningfully!
    "RSE", "RRSE", "RAE", "RMSLE", "MALE",
    "MAPE", "MSE", "TAE", "TSE",
    "r2m", "r2c",
    # Multi- and binomial
    "Overall Accuracy",
    "Balanced Accuracy", "Weighted Balanced Accuracy",
    "Accuracy", "Weighted Accuracy",
    "F1", "Weighted F1",
    "Sensitivity", "Weighted Sensitivity",
    "Specificity", "Weighted Specificity",
    "Pos Pred Value", "Weighted Pos Pred Value",
    "Neg Pred Value", "Weighted Neg Pred Value",
    "AUC", "Lower CI", "Upper CI",
    "Kappa", "Weighted Kappa",
    "MCC",
    "Detection Rate", "Weighted Detection Rate",
    "Detection Prevalence", "Weighted Detection Prevalence",
    "Prevalence", "Weighted Prevalence",
    "False Neg Rate", "Weighted False Neg Rate",
    "False Pos Rate", "Weighted False Pos Rate",
    "False Discovery Rate", "Weighted False Discovery Rate",
    "False Omission Rate", "Weighted False Omission Rate",
    "Threat Score", "Weighted Threat Score",
    # All
    "AIC", "AICc", "BIC"
  )

  metric_cols <- add_additional_colnames(metric_cols, additional_includes)
  if (isTRUE(include_definitions)) {
    metric_cols <- add_additional_colnames(metric_cols, model_formula_cols)
    if ("Fixed" %in% metric_cols){
      metric_cols <- c("Fixed", metric_cols[metric_cols != "Fixed"])
    }
  }
  metric_cols <- dplyr::intersect(metric_cols, colnames(results))

  # Return the specified columns
  base_select(results, cols = c(metric_cols))
}

add_additional_colnames <- function(metric_cols, additional_includes) {
  if (!is.null(additional_includes)) {
    metric_cols <- c(metric_cols, additional_includes)
  }
  metric_cols
}
