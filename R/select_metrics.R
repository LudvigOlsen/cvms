
#' @title Select columns with evaluation metrics and model definitions.
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("maturing")}
#'
#'  When reporting results, we might not want all
#'  the nested tibbles and process information columns.
#'  This function selects the evaluation metrics and model formulas only.
#' @return
#'  The results tibble with only metric and model definition columns.
#' @details The first element in the \code{Family} column is used to identify the relevant columns.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @param results Results tibble from \code{\link{cross_validate}()} or \code{\link{validate}()}.
#' @param include_definitions Whether to include the \code{Dependent},
#'  \code{Fixed} and (possibly) \code{Random} columns. (Logical)
#' @param additional_includes Names of additional columns to select. (Character)
select_metrics <- function(results, include_definitions = TRUE,
                           additional_includes = NULL){

  # TODO Add checks of results input

  if ("Random" %in% colnames(results)){
    model_formula_cols <- c("Dependent", "Fixed", "Random")
  } else {
    model_formula_cols <- c("Dependent", "Fixed")
  }

  if ("Family" %ni% colnames(results)){
    stop("results must contain the column Family.")
  }

  if (length(setdiff(additional_includes, colnames(results))) != 0){
    stop("not all columns in 'additional_includes' were found in 'results'.")
  }

  # What about "Convergence Warnings"? People should be aware of this!

  if (results[["Family"]][[1]] == "gaussian"){

    metric_cols <- c("RMSE", "MAE", "NRMSE", "RMSEIQR", "RMSESTD", # TODO Order the new metrics meaningfully!
                     "RAE", "RRSE", "MAPE", "MSE", "TAE", "TSE",
                     "r2m", "r2c", "AIC", "AICc", "BIC")

    metric_cols <- add_additional_colnames(metric_cols, additional_includes)
    metric_cols <- dplyr::intersect(metric_cols, colnames(results))

  } else if (results[["Family"]][[1]] == "binomial"){

    metric_cols <- c("Balanced Accuracy", "Accuracy",
                     "F1", "Sensitivity", "Specificity", "Pos Pred Value",
                     "Neg Pred Value", "AUC", "Lower CI", "Upper CI", "Kappa",
                     "MCC", "Detection Rate", "Detection Prevalence", "Prevalence",
                     "False Neg Rate", "False Pos Rate", "False Discovery Rate",
                     "False Omission Rate", "Threat Score",
                     "AIC", "AICc", "BIC")

    metric_cols <- add_additional_colnames(metric_cols, additional_includes)
    metric_cols <- dplyr::intersect(metric_cols, colnames(results))

  } else if (results[["Family"]][[1]] == "multinomial"){

    metric_cols <- c("Overall Accuracy",
                     "Balanced Accuracy", "Weighted Balanced Accuracy",
                     "Accuracy", "Weighted Accuracy",
                     "F1", "Weighted F1",
                     "Sensitivity", "Weighted Sensitivity",
                     "Specificity", "Weighted Specificity",
                     "Pos Pred Value", "Weighted Pos Pred Value",
                     "Neg Pred Value", "Weighted Neg Pred Value",
                     "AUC",
                     "Kappa", "Weighted Kappa",
                     "MCC", "Weighted MCC",
                     "Detection Rate", "Weighted Detection Rate",
                     "Detection Prevalence", "Weighted Detection Prevalence",
                     "Prevalence", "Weighted Prevalence",
                     "False Neg Rate", "Weighted False Neg Rate",
                     "False Pos Rate", "Weighted False Pos Rate",
                     "False Discovery Rate", "Weighted False Discovery Rate",
                     "False Omission Rate", "Weighted False Omission Rate",
                     "Threat Score", "Weighted Threat Score",
                     "AIC", "AICc", "BIC"
                     )

    metric_cols <- add_additional_colnames(metric_cols, additional_includes)
    metric_cols <- dplyr::intersect(metric_cols, colnames(results))

  } else {
    stop(paste0("Family, ", results[["Family"]], ", not currently supported."))
  }

  # Return the specified columns
  if (isTRUE(include_definitions)){
    return(base_select(results, cols = c(metric_cols, model_formula_cols)))
  } else {
    return(base_select(results, cols = c(metric_cols)))
  }

}

add_additional_colnames <- function(metric_cols, additional_includes){
  if (!is.null(additional_includes)){
    metric_cols <- c(metric_cols, additional_includes)
  }
  metric_cols
}
