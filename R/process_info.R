

#   __________________ #< 4e68876e2e7c5f3f8233d3c5777dfc01 ># __________________
#   Process information objects                                             ####


##  .................. #< 727b76c292437a1b4f3c58ec8a43ee9d ># ..................
##  Binomial                                                                ####


process_info_binomial <- function(data, targets_col, predictions_col, id_col, cat_levels, positive, cutoff, locale=NULL){
  if (is.null(locale)) locale <- Sys.getlocale(category="LC_ALL")
  target_summary <- describe_categorical(data[[targets_col]], classes=cat_levels)
  predictions_summary <- describe_numeric(data[[predictions_col]])
  l <- list(
    "Target Column" = targets_col,
    "Prediction Column" = predictions_col,
    "ID Column" = id_col,
    "Family" = "Binomial",
    "Classes" = cat_levels,
    "Positive Class" = positive,
    "Cutoff" = cutoff,
    "Target Summary" = target_summary,
    "Probability Summary" = predictions_summary,
    "Locale" = locale
  )

  structure(l, class = "cvms_process_info_binomial")
}

print.cvms_process_info_binomial <- function(x, ...){
  cat(as.character(x))
}

as.character.cvms_process_info_binomial <- function(x, ...){
  cat_levels <- x[["Classes"]]
  cutoff <- x[["Cutoff"]]
  paste0(
    "---",
    "\nProcess Information",
    "\n---",
    "\nTarget column: ", x[["Target Column"]],
    "\nPrediction column: ", x[["Prediction Column"]],
    ifelse(!is.null(x[["ID Column"]]), paste0("\nID column: ", x[["ID Column"]]), ""),
    "\nFamily / type: ", x[["Family"]],
    "\nClasses: ", paste0(cat_levels, collapse = ", "),
    "\nPositive class: ", x[["Positive Class"]],
    "\nCutoff: ", cutoff,
    "\nProbabilities are of class: ", cat_levels[[2]],
    "\nProbabilities < ", cutoff, " are considered: ", cat_levels[[1]],
    "\nProbabilities >= ", cutoff, " are considered: ", cat_levels[[2]],
    "\nLocale used when sorting class levels (LC_ALL): \n  ", x[["Locale"]],
    "\nTarget counts: ", categorical_description_as_character(x[["Target Summary"]]),
    "\nProbability summary: ", numeric_description_as_character(x[["Probability Summary"]]),
    "\n---"
  )
}


##  .................. #< 2b0bd2c6fd20767770a15db54e22c6ec ># ..................
##  Multinomial                                                             ####


process_info_multinomial <- function(
  data,
  targets_col,
  prediction_cols,
  pred_class_col,
  id_col,
  cat_levels,
  apply_softmax,
  locale = NULL) {
  if (is.null(locale)) locale <- Sys.getlocale(category="LC_ALL")
  target_summary <- describe_categorical(data[[targets_col]], classes=cat_levels)
  predicted_class_summary <- describe_categorical(data[[pred_class_col]], classes=cat_levels)
  l <- list(
    "Target Column" = targets_col,
    "Prediction Columns" = prediction_cols,
    "ID Column" = id_col,
    "Family" = "Multinomial",
    "Classes" = cat_levels,
    "Softmax Applied" = apply_softmax,
    "Target Summary" = target_summary,
    "Prediction Summary" = predicted_class_summary,
    "Locale" = locale
  )

  structure(l, class = "cvms_process_info_multinomial")
}

print.cvms_process_info_multinomial <- function(x, ...){
  cat(as.character(x))
}

as.character.cvms_process_info_multinomial <- function(x, ...){
  # Collapse and shorten if necessary
  # E.g. in case of 100s of classes
  pred_cols_str <- paste0(x[["Prediction Columns"]], collapse = ", ")
  pred_cols_str <- shorten_string(pred_cols_str, 60)
  classes_str <- paste0(x[["Classes"]], collapse = ", ")
  classes_str <- shorten_string(classes_str, 71)
  # Create statement
  paste0(
    "---",
    "\nProcess Information",
    "\n---",
    "\nTarget column: ", x[["Target Column"]],
    "\nPrediction columns: ", pred_cols_str,
    ifelse(!is.null(x[["ID Column"]]), paste0("\nID column: ", x[["ID Column"]]), ""),
    "\nFamily / type: ", x[["Family"]],
    "\nClasses: ", classes_str,
    ifelse(!is.null(x[["Softmax Applied"]]),
           paste0("\nSoftmax: ", ifelse(isTRUE(x[["Softmax Applied"]]), "Applied", "Not applied")),
           ""),
    "\nTarget counts: ", categorical_description_as_character(x[["Target Summary"]], lim=59),
    "\nPrediction counts: ", categorical_description_as_character(x[["Prediction Summary"]], lim=55),
    "\nLocale (LC_ALL): \n  ", x[["Locale"]],
    "\n---"
  )
}


##  .................. #< a3ccd3cb85c17fe725e2313cb75fdb1f ># ..................
##  Gaussian                                                                ####


process_info_gaussian <- function(data, targets_col, predictions_col, id_col, locale=NULL){
  if (is.null(locale)) locale <- Sys.getlocale(category="LC_ALL")
  target_summary <- describe_numeric(data[[targets_col]])
  predictions_summary <- describe_numeric(data[[predictions_col]])
  l <- list(
    "Target Column" = targets_col,
    "Prediction Column" = predictions_col,
    "ID Column" = id_col,
    "Family" = "Gaussian",
    "Target Summary" = target_summary,
    "Prediction Summary" = predictions_summary,
    "Locale" = locale
  )

  structure(l, class = "cvms_process_info_gaussian")
}

print.cvms_process_info_gaussian <- function(x, ...){
  cat(as.character(x))
}

as.character.cvms_process_info_gaussian <- function(x, ...){
  paste0(
    "---",
    "\nProcess Information",
    "\n---",
    "\nTarget column: ", x[["Target Column"]],
    "\nPrediction column: ", x[["Prediction Column"]],
    ifelse(!is.null(x[["ID Column"]]), paste0("\nID column: ", x[["ID Column"]]), ""),
    "\nFamily / type: ", x[["Family"]],
    "\nTarget summary: ", numeric_description_as_character(x[["Target Summary"]]),
    "\nPrediction summary: ", numeric_description_as_character(x[["Prediction Summary"]]),
    "\nLocale (LC_ALL): \n  ", x[["Locale"]],
    "\n---"
  )
}


##  .................. #< 60cfc78f594e5611a6eaaf34a2b212ae ># ..................
##  Utilities                                                               ####


shorten_string <- function(x, lim=60){
  if (nchar(x) > lim)
    paste0(substr(x, 1, lim-3), "...")
  else
    x
}

describe_numeric <- function(v, na.rm=FALSE){
  list(
    "Mean" = mean(v, na.rm=na.rm),
    "Median" = median(v, na.rm=na.rm),
    "Range" = range(v, na.rm=na.rm),
    "SD" = sd(v, na.rm=na.rm),
    "IQR" = tryCatch(IQR(v, na.rm=na.rm), error = function(e){
      # As the only one, IQR throws error on NAs
      if (grep("missing values and NaN", as.character(e))){
        return(NA)
      } else {
        stop(e)
      }
    })
  )
}

numeric_description_as_character <- function(x){
  paste0(
    "mean: ", round(x[["Mean"]], digits = 3),
    ", median: ", round(x[["Median"]], digits = 3),
    ", range: [", round(x[["Range"]][[1]], digits = 3), ", ", round(x[["Range"]][[2]], digits = 3),
    "], SD: ", round(x[["SD"]], digits = 3),
    ", IQR: ", round(x[["IQR"]], digits = 3)
  )
}

describe_categorical <- function(v, classes, na.rm=FALSE){
  # Count the classes in v
  class_counts <- table(v)
  available_classes <- names(class_counts)
  class_counts <- setNames(as.numeric(class_counts), nm = available_classes)

  # Find and add the classes that are not in v
  # but that we know should be there (with a zero-count)
  missing_classes <- setdiff(classes, available_classes)
  missing_counts <- setNames(rep(0, length(missing_classes)), nm = missing_classes)
  class_counts <- c(class_counts, missing_counts)

  # Order by class name
  class_counts <- class_counts[order(names(class_counts))]

  list(
    "Total" = length(v),
    "Class Counts" = class_counts
  )
}

categorical_description_as_character <- function(x, lim=59){
  paste0(
    "total=",
    shorten_string(paste0(
      x[["Total"]], ", ",
        paste0(names(x[["Class Counts"]]), "=",
        unname(x[["Class Counts"]]),
        collapse = ", ")
     ),
    lim = lim)
  )
}
