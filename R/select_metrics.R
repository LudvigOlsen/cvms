
#' @title Select columns with evaluation metrics and model definitions.
#' @description When reporting results, we may not want all
#'  the nested tibbles and process information columns.
#'  This function selects the evaluation metrics and model formulas only.
#' @return
#'  The results tibble with only metric and model definition columns.
#' @details The \code{Family} column is used to identify relevant columns.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @param results Results tibble from cross_validate() or validate().
select_metrics <- function(results){

  # TODO Add checks of results input

  if ("Random" %in% colnames(results)){
    model_formula_cols <- c("Dependent", "Fixed", "Random")
  } else {
    model_formula_cols <- c("Dependent", "Fixed")
  }

  # What about "Convergence Warnings"? People should be aware of this!

  if (results[["Family"]] == "gaussian"){

    metric_cols <- c("RMSE", "MAE", "r2m", "r2c", "AIC", "AICc", "BIC")

    return( dplyr::select(results, dplyr::one_of(metric_cols), dplyr::one_of(model_formula_cols)))

  } else if (results[["Family"]] == "binomial"){

    metric_cols <- c("Balanced Accuracy","F1","Sensitivity","Specificity","Pos Pred Value",
                     "Neg Pred Value","AUC","Lower CI","Upper CI","Kappa",
                     "MCC","Detection Rate","Detection Prevalence","Prevalence")

    return( dplyr::select(results, dplyr::one_of(metric_cols), dplyr::one_of(model_formula_cols)))

  } else {
    stop(paste0("Family, ",results[["Family"]],", not currently supported."))
  }

}
