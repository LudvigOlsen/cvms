#' @title Summarize metrics with common descriptors
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Summarizes all numeric columns. Counts the NAs and Infs in the columns.
#' @param data Data frame with numeric columns to summarize.
#' @param cols Names of columns to summarize. Non-numeric columns are ignored.
#' @param na.rm Whether to remove NAs before summarizing. (logical)
#' @param inf.rm Whether to remove Infs before summarizing. (logical)
#' @return tibble where each row is a descriptor of the column.
#'
#'  The \strong{Measure} column contains the name of the descriptor.
#'
#'  The \strong{NAs} row is a count of \code{NA}s in the column.
#'
#'  The \strong{INFs} row is a count of \code{Inf}s in the column.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
summarize_metrics <- function(data, cols = NULL, na.rm = TRUE, inf.rm = TRUE){

  # Subset the chosen columns
  if (!is.null(cols)){
    data <- data %>%
      base_select(cols = cols)
  }

  # Select only numeric columns
  data <- data %>%
    dplyr::select_if(is.numeric)

  # Start by replacing INF with NA
  # We keep the infs as well, as we wish to distinguish between them in the summary
  inf_replacement <- replace_inf_with_na(data)
  data_no_infs <- inf_replacement[["metric_cols"]]
  rows_with_infs <- inf_replacement[["rows_with_infs"]]

  # TODO Test this - what is the effect when there are actual Infs in data?
  if (isTRUE(inf.rm)){
    data <- data_no_infs
  }

  # Summarize the metrics with a range of functions
  # Note: This may be better solveable with pivot_* from tidyr, when it is on CRAN
  # This isn't exactly pretty.

  descriptors <- list(
    "Mean" = mean,
    "Median" = median,
    "SD" = sd,
    "IQR" = IQR,
    "Max" = max,
    "Min" = min,
    "NAs" = function(x, na.rm){sum(is.na(x))}
  )

  summarized_metrics <- dplyr::bind_rows(
    plyr::ldply(names(descriptors), function(descr){
      d_fn <- descriptors[[descr]]
      data %>%
        dplyr::summarize_all(.funs = list( ~ d_fn(., na.rm = na.rm))) %>%
        dplyr::mutate(Measure = descr)
    }),
    rows_with_infs %>%
      dplyr::summarize_all(.funs = list(~sum(is.infinite(.)))) %>%
      dplyr::mutate(Measure = "INFs")
  ) %>%
    position_first(col = "Measure")

  # Remove the INFs from the NAs count
  if(nrow(rows_with_infs) > 0){
    summarized_metrics <- subtract_inf_count_from_na_count(summarized_metrics)
  }

  summarized_metrics
}

replace_inf_with_na <- function(metric_cols){

  # Get rows with INFs
  metric_cols_with_infs <- metric_cols[is.infinite(rowSums(metric_cols)),]

  # Replace infs with NA
  if(nrow(metric_cols_with_infs) > 0){
    metric_cols <- do.call(
      data.frame,
      c(
        lapply(metric_cols,
               function(x)
                 replace(x, is.infinite(x), NA)),
        check.names = FALSE,
        fix.empty.names = FALSE,
        stringsAsFactors = FALSE
      )
    )
  }

  list("metric_cols" = metric_cols,
       "rows_with_infs" = metric_cols_with_infs)
}


