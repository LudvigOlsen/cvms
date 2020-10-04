

#   __________________ #< 1f97b5a554f2c90b5fb34e9263640d1e ># __________________
#   %c%                                                                     ####


# Get all lists in a list with a certain name
# Use: list_of_lists %c% 'list_name'
`%c%` <- function(x, n) lapply(x, `[[`, n)
# From http://stackoverflow.com/questions/5935673/accessing-same-named-list-elements-of-the-list-of-lists-in-r/5936077#5936077


#   __________________ #< 5cbdba0ee926baf31ee42f080b77e671 ># __________________
#   Not in                                                                  ####


# Not in
`%ni%` <- function(x, table) {
  return(!(x %in% table))
}


#   __________________ #< 5f6cc138effcec1c38b9c839ca82e7b3 ># __________________
#   Default link                                                            ####


default_link <- function(link, family) {
  if (is.null(link)) {
    if (family == "gaussian") {
      return("identity")
    }
    if (family == "binomial") {
      return("logit")
    }
  } else {
    return(link)
  }
}


#   __________________ #< e80ab4e0dd1d3c57d56cf90275aa92a7 ># __________________
#   Default control                                                         ####


default_control <- function(control, family, link) {
  if (is.null(control)) {

    # Note that gaussian models with alternative link functions are run with glmer
    if (family == "gaussian") {
      if (link == "identity") {
        return(lme4::lmerControl())
      }
      return(lme4::glmerControl())
    }
    if (family == "binomial") {
      return(lme4::glmerControl())
    }
  } else {
    return(control)
  }
}


#   __________________ #< ccbe15a8878f3d8218c2e424a909275c ># __________________
#   Extract y                                                               ####


## For validate_single and cross_validate_single

# Extract y_column from model
extract_y <- function(formula) {
  splits <- unlist(strsplit(formula, "\\s*~"))
  if (length(splits) < 2) {
    return(NULL)
  }
  return(splits[1])
}


#   __________________ #< 517875dddfa5a45e2c867e81d4befc9b ># __________________
#   Contains random effects                                                 ####


# Check if there are random effects
# returns TRUE or FALSE
rand_effects <- function(formula) {
  checkmate::assert_formula(x = formula)
  length(lme4::findbars(as.formula(formula))) > 0
}


#   __________________ #< 906a28d545390fb07c5d16881cc97441 ># __________________
#   Count convergence warnings                                              ####


count_convergence_warnings <- function(convergences) { # "Yes" or "No"

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_names(x = unique(convergences), subset.of = c("Yes", "No"),
                           add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Count the convergence warnings
  conv_warns <- as.integer(table(convergences)["No"])
  if (is.na(conv_warns)) {
    conv_warns <- 0
  }
  return(conv_warns)
}


#   __________________ #< be89ced97b64d9a7be007ba2e3563657 ># __________________
#   Count NULLs in list                                                     ####


count_nulls_in_list <- function(l) {
  plyr::llply(l, function(e) {
    ifelse(is.null(e), 1, 0)
  }) %>%
    unlist() %>%
    sum()
}


#   __________________ #< 1d6a9508c761951daebbd027350a8fc6 ># __________________
#   Contains NA                                                             ####


contains_na <- function(v) {
  sum(is.na(v)) > 0
}


#   __________________ #< 4d1587dec3d7591a40303b1f8036da6c ># __________________
#   Model specifics                                                         ####


##  .................. #< 6937cf08013702a27f0b30b8be2ffb3c ># ..................
##  Check argument in model specifics                                       ####


check_argument_in_model_specifics <- function(var_name, model_specifics) {
  if (var_name %ni% names(model_specifics)) {
    stop(paste0(var_name, " is a required named argument in model_specifics. Be sure to name arguments."))
  }
}


##  .................. #< 801facabefc7b3dc6baaf1d237a0cbbd ># ..................
##  Replace argument in model specifics                                     ####


replace_argument_in_model_specifics_if_null <- function(var_name, model_specifics, new_value, err = TRUE) {
  if (is.null(model_specifics[[var_name]])) {
    if (isTRUE(err)) {
      stop(paste0(
        var_name, " was NULL in model_specifics. Remember to name arguments, i.e. ",
        var_name, " = ??? when passing model_specifics."
      ))
    }

    model_specifics[[var_name]] <- new_value
  }

  model_specifics
}


##  .................. #< 655f3906f21d09e04f33d1f9b0ff9e2e ># ..................
##  Stop if argument is not NULL                                            ####


stop_if_argument_not_null <- function(var_name, model_specifics) {
  if (!is.null(model_specifics[[var_name]])) {
    stop(paste0("'", var_name, "' was not NULL."))
  }
}


##  .................. #< 8775bf89b75dcd59fc75163facaa790c ># ..................
##  Stop if argument is NULL                                                ####


stop_if_argument_is_null <- function(var_name, model_specifics) {
  if (is.null(model_specifics[[var_name]])) {
    stop(paste0("'", var_name, "' was NULL."))
  }
}


##  .................. #< 20d2e13ffbfb93367bf608621ab54e35 ># ..................
##  Stop if argument is not function                                        ####


stop_if_argument_is_not_function <- function(var_name, model_specifics) {
  if (!is.function(model_specifics[[var_name]])) {
    stop(paste0("'", var_name, "' was not a function."))
  }
}


#   __________________ #< fd70347a80fefc16b9fcb36be1a7bacd ># __________________
#   Nest                                                                    ####


##  .................. #< cc603e8af54e30fbe46101ad771bf59e ># ..................
##  Nest results                                                            ####


nest_results <- function(results) {

  # Make results into a tibble
  iter_results <- tibble::as_tibble(results)
  rownames(iter_results) <- NULL
  iter_results <- iter_results %>%
    dplyr::group_nest(.key = "results")

  iter_results
}


##  .................. #< 19ced9d10d623ea16ce750532c85a6e2 ># ..................
##  Nest models                                                             ####


nest_models <- function(model_coefs) {
  # Make tidied models into a tibble
  iter_models <- tibble::as_tibble(model_coefs)
  if ("p.value" %ni% colnames(iter_models)) {
    iter_models[["p.value"]] <- NA
  }
  iter_models <- iter_models %>%
    dplyr::group_nest(.key = "Coefficients")

  iter_models
}


#   __________________ #< 01cf8e744b13a5a37da4e7aabb51ba2f ># __________________
#   Levels as characters                                                    ####


levels_as_characters <- function(col, drop_unused = TRUE) {
  fcol <- factor(col)
  if (isTRUE(drop_unused)){
    fcol <- droplevels(fcol)
  }
  levs <- levels(fcol)

  cat_levels <- plyr::llply(seq_len(length(levs)), function(i) {
    as.character(levs[i])
  }) %>% unlist()

  cat_levels
}


#   __________________ #< 57f5f08470806ab94489d36b30390b30 ># __________________
#   Return if not null for named list                                       ####


assign_if_not_null_named_lists <- function(var, var_name, list_name) {
  if (is.null(var)) {
    stop(paste0(
      var_name,
      " is NULL. The arguments in the ",
      list_name,
      " list must be named."
    ))
  }
  var
}


#   __________________ #< d686a6f089e1f4cd1b8369b7910aff50 ># __________________
#   Remove from colnames                                                    ####


# Removes pattern from all column names
remove_from_colnames <- function(data, pattern) {
  colnames(data) <- colnames(data) %>%
    tibble::enframe(name = NULL) %>%
    dplyr::mutate(colname = stringr::str_remove_all(.data$value, pattern)) %>%
    dplyr::pull(.data$colname)

  return(data)
}


#   __________________ #< f2a32a42588a251e082196931502c8a4 ># __________________
#   Create folds map                                                        ####


# Returns list with folds_map and n_folds
create_folds_map <- function(data, fold_cols) {

  # Create a map of number of folds per fold column
  # The range tells what fold column a specific fold belongs to.
  folds_map <- plyr::llply(seq_len(length(fold_cols)), function(fold_column) {
    nlevels(data[[ fold_cols[[fold_column]] ]])
  }) %>%
    unlist() %>%
    tibble::enframe(name = "fold_col", value = "num_folds")

  # Create ranges
  first_start <- folds_map$num_folds[[1]]
  folds_map <- folds_map %>%
    dplyr::mutate(
      end_ = cumsum(.data$num_folds),
      start_ = .data$end_ - (first_start - 1)
    )

  # Calculate number of folds
  n_folds <- sum(folds_map$num_folds)

  # Expand ranges to long format
  folds_map_expanded <-
    plyr::ldply(seq_len(length(fold_cols)), function(fold_column) {
      data.frame(
        "fold_col_idx" = fold_column,
        "fold_col_name" = factor(fold_cols[[fold_column]]),
        abs_fold = c(folds_map[["start_"]][[fold_column]]:folds_map[["end_"]][[fold_column]]),
        rel_fold = c(1:folds_map[["num_folds"]][[fold_column]]),
        stringsAsFactors = FALSE
      )
    }) %>% dplyr::as_tibble()

  return(
    list(
      "folds_map" = folds_map_expanded,
      "n_folds" = n_folds
    )
  )
}


#   __________________ #< 156466a5561a5e81e9b2330957bc7617 ># __________________
#   Create fold and fold column map                                         ####


# Creates data frame with existing combinations of fold column, abs_fold and rel_fold
# For adding the info to other data frames via joins
create_fold_and_fold_column_map <- function(data, fold_info_cols) {
  tibble::tibble(
    "fold_column" = data[[fold_info_cols[["fold_column"]]]],
    "abs_fold" = data[[fold_info_cols[["abs_fold"]]]],
    "rel_fold" = data[[fold_info_cols[["rel_fold"]]]]
  ) %>%
    dplyr::distinct()
}


#   __________________ #< 0b7162d59e8eca41362f7f09292860c9 ># __________________
#   R version                                                               ####

##  .................. #< 52bf2e9f3679f411f75cb7daee2c2e20 ># ..................
##  Skip test if R is too old                                               ####


# Skips testthat test, if the R version is below 3.6.0
# WHY? Due to the change in the random sampling generator
# tests fail on R versions below 3.6.0.
# It is possible to fix this by using the old generator for
# unit tests, but that would take a long time to convert,
# and most likely the code works the same on v3.5
skip_test_if_old_R_version <- function(min_R_version = "3.6") {
  if (getRversion()$minor < strsplit(min_R_version, ".", fixed = TRUE)[[1]][[2]]) {
    testthat::skip(message = paste0("Skipping test as R version is < ", min_R_version, "."))
  }
}


#   __________________ #< 53855d1cfe9c14e0344914fd9330ac28 ># __________________
#   Numeric argument checks                                                 ####


##  .................. #< 4e5c8f3958897e1d03a927512991a7c5 ># ..................
##  Is wholenumber                                                          ####


is_wholenumber_ <- function(n) {

  # If n is a whole number
  # .. return TRUE
  # else
  # .. return FALSE

  return(floor(n) == n)
}


##  .................. #< 1049ff3ccadbbbeb0d80e6e693f6a45b ># ..................
##  Argument is wholenumber                                                 ####


arg_is_wholenumber_ <- function(n) {

  # Checks if n is a whole number of either
  # type integer or numeric
  # Returns TRUE if yes, else FALSE

  # If n is an integer, return TRUE
  # else check if it is a numeric
  # .. if yes, check if it is a whole number
  # .... if yes, return TRUE
  # .... if no, return FALSE
  # .. if not a numeric
  # .... return FALSE

  if (is.integer(n)) {
    return(TRUE)
  } else if (is.numeric(n)) {
    if (is_wholenumber_(n)) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}


##  .................. #< 42c968eb4750bcefe6ccb9b7d207cbc7 ># ..................
##  Argument is number                                                      ####


arg_is_number_ <- function(n) {

  # Checks if n is either an integer or a numeric
  # Returns TRUE if yes, FALSE if no

  if (is.integer(n) || is.numeric(n)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


#   __________________ #< b60c9996af128a33785d5e44a03d9942 ># __________________
#   Is between                                                              ####


is_between_ <- function(x, a, b) {

  # Checks if x is between a and b

  x > a & x < b
}


#   __________________ #< 7b5741e261cb48ace03223ef51752445 ># __________________
#   Check rows sum to                                                       ####


rows_sum_to <- function(data, sum_to = 1, digits = 5){
  !any(round(rowSums(data), digits = digits) != 1)
}


#   __________________ #< b3810924eabe73aa9d64767d421a7bf3 ># __________________
#   Tidyr legacy functions                                                  ####


##  .................. #< 42f80740a58172561302fd2b8c82d3af ># ..................
##  Tidyr check if new interface                                            ####


# https://tidyr.tidyverse.org/dev/articles/in-packages.html
tidyr_new_interface <- function() {
  utils::packageVersion("tidyr") > "0.8.99"
}


##  .................. #< cd15671468ed3de2653ba6e1532bcf0e ># ..................
##  Legacy nest function                                                    ####


# As tidyr v1.0.0 has breaking changes
# to nest (and unnest!), we make sure it's compatible for now
# TODO replace nest_legacy with the new nest syntax within the
# code, once people have moved to that.
legacy_nest <- function(...) {
  if (tidyr_new_interface()) {
    tidyr::nest_legacy(...)
  } else {
    tidyr::nest(...)
  }
}


##  .................. #< 65355f4d719ea07c474f69fceb2d1ef2 ># ..................
##  Legacy unnest                                                           ####

# TODO Change to new unnest
legacy_unnest <- function(...) {
  if (tidyr_new_interface()) {
    tidyr::unnest_legacy(...)
  } else {
    tidyr::unnest(...)
  }
}


#   __________________ #< 0837fb70595155c5978fe8e8d395a60f ># __________________
#   Keras check availability                                                ####


# Keras check
# testthat utilty for skipping tests when Keras isn't available
# skip_if_no_keras <- function(version = NULL) {
#   if (!keras::is_keras_available(version))
#     testthat::skip("Required keras version not available for testing")
# }


#   __________________ #< 34f98b837e25f69c5864c60ed9f33172 ># __________________
#   Reposition column                                                       ####


# Wraps tibble::add_column
reposition_column <- function(data, col, .before = NULL, .after = NULL) {
  col_values <- data[[col]]
  data[[col]] <- NULL
  data %>%
    tibble::add_column(!!(col) := col_values, .before = .before, .after = .after)
}


#   __________________ #< 6032a716799351f7bff557c8549a4f2e ># __________________
#   Argument not used                                                       ####


arg_not_used <- function(arg, arg_name, family, current_fn, message_fn = message) {
  if (!is.null(arg)) {
    message_fn(paste0("'", arg_name, "' was not used for ", family, " version of ", current_fn, "()."))
  }
}


#   __________________ #< dfbf897de4d46ecd0a6ee611b1071454 ># __________________
#   *_validate_list arguments                                                    ####


##  .................. #< e014ad880a793bc44844d33ac4c58e6a ># ..................
##  Check fold column factor                                                ####


check_fold_col_factor <- function(data, fold_cols) {
  # Check that the fold column(s) is/are factor(s)
  if (length(fold_cols) == 1) {
    if (fold_cols %ni% names(data)) {
      stop(paste0("'", fold_cols, "' not found in 'data'."))
    }
    stopifnot(is.factor(data[[fold_cols]]))
  } else {
    fcols <- data %>%
      base_select(cols = fold_cols) %>%
      sapply(is.factor)
    if (FALSE %in% fcols) {
      stop("At least one of the fold columns is not a factor.")
    }
  }
}


##  .................. #< 4c648a271eca3ea75643e6f9ed5de670 ># ..................
##  Check partitions column factor                                          ####


check_partitions_col_factor <- function(data, partitions_col) {
  # Check that the fold column(s) is/are factor(s)
  if (length(partitions_col) == 1) {
    if (partitions_col %ni% names(data)) {
      stop(paste0("'", partitions_col, "' not found in 'data'."))
    }
    stopifnot(is.factor(data[[partitions_col]]))
  } else {
    stop("only one 'partitions_col' can currently be used.")
    # fcols <- data %>% dplyr::select(dplyr::one_of(fold_cols)) %>%
    #   sapply(is.factor)
    # if (FALSE %in% fcols) {stop("At least one of the fold columns is not a factor.")}
  }
}


#   __________________ #< 0bd4ddb34d78b1fab5185a8b71182444 ># __________________
#   Check metrics list                                                      ####


# Check metrics argument
check_metrics_list <- function(metrics) {
  if (!(is.list(metrics) || metrics == "all")) {
    stop("'metrics' must be either a list or the string 'all'.")
  }

  if (is.list(metrics) && length(metrics) > 0) {
    if (!rlang::is_named(metrics)) {
      stop("when 'metrics' is a non-empty list, it must be a named list.")
    }
  }
}


#   __________________ #< 028221cdde609cc4255b7e576b1e3c00 ># __________________
#   Create warnings and messages tibble                                     ####


# Creates initial warnings and messages tibble
# cols: Message, Type, Function
create_warnings_and_messages_tibble <- function(warnings, messages, fn) {
  tibble::tibble(
    "Message" = warnings,
    "Type" = "warning"
  ) %>%
    dplyr::bind_rows(tibble::tibble(
      "Message" = messages,
      "Type" = "message"
    )) %>%
    dplyr::mutate(Function = fn)
}



#   __________________ #< 440b147b963f8a7fd202661bfc3b068e ># __________________
#   Create message for side effects                                         ####

append_to_message <- function(msg, .var = NULL, .msg = NULL, ignore_var_null = TRUE, ignore_empty = TRUE){

  if (is.null(.var) && isTRUE(ignore_var_null)){
    return(msg)
  }

  if (checkmate::test_string(.var) && .var == "" && isTRUE(ignore_empty)){
    return(msg)
  }

  paste0(
    msg, "\n",
    .msg,
    .var
  )
}

create_message <- function(m, caller, formula = NULL, fold_col = NULL, fold = NULL,
                           hyperparameters = NULL, note = NULL, why = NULL){
  msg <- paste(
    "---",
    paste0(caller, ": ", m),
    sep = "\n")

  msg <- append_to_message(msg, .var = why, .msg = "Why: ")
  msg <- append_to_message(msg, .var = note, .msg = "Note: ")

  if (!is.null(formula) || !is.null(fold_col) ||
      !is.null(fold) || !is.null(hyperparameters)){

    msg <- append_to_message(msg, .msg = "For:", ignore_var_null = FALSE)
    msg <- append_to_message(msg, .var = formula, .msg = "Formula: ")
    msg <- append_to_message(msg, .var = fold_col, .msg = "Fold column: ")
    msg <- append_to_message(msg, .var = fold, .msg = "Fold: ")
    msg <- append_to_message(msg, .var = paste_hparams(hyperparameters), .msg = "Hyperparameters: ")

  }

  msg <- append_to_message(msg, .msg = "", ignore_var_null = FALSE, ignore_empty = FALSE)

  msg
}




#   __________________ #< 71c73c7cedb289ef6c3dd17503736847 ># __________________
#   Convert to tibble                                                       ####


# Wraps dplyr::as_tibble()
# If x is NULL, returns NULL
to_tibble <- function(x, x_name, caller = "") {
  if (!is.null(x)) {
    x <- tryCatch(
      {
        dplyr::as_tibble(x)
      },
      error = function(e) {
        stop(paste0(caller, ": Could not convert '", x_name, "' to a tibble."))
      }
    )
  }
  x
}


#   __________________ #< 9845049003389e2dbeab337816f43718 ># __________________
#   Base functions                                                          ####


##  .................. #< ac6d7d82a3b1d45868c25496db71eb0b ># ..................
##  Base rename                                                             ####


base_rename <- function(data, before, after,
                        warn_at_overwrite = FALSE,
                        message_if_identical = FALSE) {

  #
  # Replaces name of column in data frame
  #

  # Check names
  if (!is.character(before) || !is.character(after)) {
    stop("'before' and 'after' must both be of type character.")
  }
  if (length(before) != 1 || length(before) != 1) {
    stop("'before' and 'after' must both have length 1.")
  }

  if (isTRUE(message_if_identical) && before == after) {
    message("'before' and 'after' were identical.")
    return(data)
  }
  # If after is already a column in data
  # remove it, so we don't have duplicate column names
  if (after %in% colnames(data)) {
    if (isTRUE(warn_at_overwrite)) {
      warning("'after' already existed in 'data' and will be replaced.")
    }
    data[[after]] <- NULL
  }
  colnames(data)[names(data) == before] <- after
  data
}


##  .................. #< 2f0cafd5ae90638866abbb82afb50be0 ># ..................
##  Base select                                                             ####


# Cols should be col names
base_select <- function(data, cols) {
  subset(data, select = cols)
}


##  .................. #< 78209f097c80c9562536d81a2fa39dd6 ># ..................
##  Base deselect                                                           ####


# Cols should be col names
base_deselect <- function(data, cols) {
  if (!is.character(cols)) stop("cols must be names")
  base_select(data = data, cols = setdiff(names(data), cols))
}


##  .................. #< cd68f4a19d3a23deb06cfcc85cfcfc8a ># ..................
##  Position first                                                          ####


# Col should be col name
position_first <- function(data, col) {
  if (!checkmate::test_string(x = col))
    stop("col must be name")

  base_select(data = data, cols = c(col, setdiff(names(data), col)))
}


##  .................. #< 7159eb4bd02cd271576699cb2f3e586b ># ..................
##  Position last                                                           ####


position_last <- function(data, col) {
  if (is.numeric(col)) stop("col must be name")

  base_select(data = data, cols = c(setdiff(names(data), col), col))
}


#   __________________ #< 1ef24d7e71e99daa336c2d4294fe1aba ># __________________
#   One-hot encoder function                                                ####


# use_epsilon: Add epsilon to 0s and subtract epsilon from 1s
one_hot_encode <- function(data, col, c_levels = NULL, use_epsilon = FALSE, epsilon = 1e-6) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(x = data, add = assert_collection)
  checkmate::assert_string(x = col, add = assert_collection)
  checkmate::assert_character(x = c_levels, null.ok = TRUE,
                              add = assert_collection)
  checkmate::assert_flag(x = use_epsilon, add = assert_collection)
  checkmate::assert_number(x = epsilon, upper = 0.01, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(x = colnames(data), must.include = col,
                          add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # If not provided, extract categorical levels from col
  if (is.null(c_levels)) {
    c_levels <- unique(data[[col]])
  }

  # Sort so columns will also be sorted
  c_levels <- sort(c_levels)

  # Check that none of the categorical levels already
  # have a column in data
  if (length(intersect(colnames(data), c_levels)) > 0) {
    stop("'data' already includes one or more columns with name of one of the levels.")
  }

  # Initialize zero tibble
  initial_tbl <- matrix(rep(c_levels, each = nrow(data)),
    nrow = nrow(data),
    ncol = length(c_levels)
  ) %>%
    dplyr::as_tibble(.name_repair = "minimal")
  colnames(initial_tbl) <- c_levels

  # Add col with a unique name
  tmp_colname <- create_tmp_name(data, ".__col__")
  initial_tbl[[tmp_colname]] <- data[[col]]

  equal_int <- function(x1, x2) {
    as.integer(x1 == x2)
  }

  # Create dummy variables
  dummies <- initial_tbl %>%
    dplyr::mutate_at(.vars = c_levels, .funs = list(
      ~ equal_int(., !!as.name(tmp_colname))
    )) %>%
    base_deselect(tmp_colname)

  if (isTRUE(use_epsilon)) {
    dummies <- dummies + epsilon - (dummies * 2 * epsilon)
  }

  # Combine and return
  dplyr::bind_cols(data, dummies)
}


#   __________________ #< d1964aaebfb875051d2b33ff5db3383e ># __________________
#   Create tmp name                                                         ####


create_tmp_name <- function(data, name = ".tmp_") {

  # Assert input
  # 'data' can be anything where names() can be used,
  # so we don't add assertions for that
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = name, min.chars = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)

  # Extract non-empty names from 'data'
  data_names <- non_empty_names(data)

  # Append underscore until unique
  while (name %in% data_names) {
    name <- paste0(name, "_")
  }

  name
}


#   __________________ #< bf63eb325847267b713b04037906e09e ># __________________
#   Get non-empty names                                                     ####


# Remove NAs and empty "" names
# Note: Output is always a character vector
# whereas names() usually returns NULL for unnamed objects
non_empty_names <- function(x, always_character = TRUE) {
  ns <- names(x)
  if (is.null(ns) && isTRUE(always_character))
    return(character(0))
  ns <- ns[!is.na(ns)]
  ns[nzchar(ns, keepNA = TRUE)]
}


#   __________________ #< 5189dc5658a43b24ed8273c326c7c50c ># __________________
#   Checkmate apply to multiple arguments                                   ####

# argument apply
# https://github.com/mllg/checkmate/issues/115#issuecomment-331800164
aapply <- function(fun, formula, ..., fixed = list()) {
  fun <- match.fun(fun)
  terms <- terms(formula)
  vnames <- attr(terms, "term.labels")
  ee <- attr(terms, ".Environment")

  dots <- list(...)
  dots$.var.name <- vnames
  dots$x <- unname(mget(vnames, envir = ee))
  .mapply(fun, dots, MoreArgs = fixed)

  invisible(NULL)
}


#   __________________ #< e9e6b48ab92b1896f3cb1fec83dc42e8 ># __________________
#   Set arrow icons for plot_confusion_matrix                               ####


set_arrows <- function(cm, place_x_axis_above, icons,
                       empty_path = get_figure_path("empty_square.svg")){

  # Get the extreme levels
  max_prediction_level <- max(as.character(levels(cm[["Prediction"]])))
  min_prediction_level <- min(as.character(levels(cm[["Prediction"]])))
  max_target_level <- max(as.character(levels(cm[["Target"]])))
  min_target_level <- min(as.character(levels(cm[["Target"]])))

  # Set arrow icon names for all tiles
  cm[["right_icon"]] <- icons[["right"]]
  cm[["left_icon"]] <- icons[["left"]]
  cm[["up_icon"]] <- icons[["up"]]
  cm[["down_icon"]] <- icons[["down"]]

  # Remove arrows where Prediction is extreme level
  cm[cm[["Prediction"]] == max_prediction_level, "up_icon"] <- empty_path
  cm[cm[["Prediction"]] == min_prediction_level, "down_icon"] <- empty_path

  # Remove arrows where Target is extreme level
  if (isTRUE(place_x_axis_above)){
    cm[cm[["Target"]] == max_target_level, "left_icon"] <- empty_path
    cm[cm[["Target"]] == min_target_level, "right_icon"] <- empty_path
  } else {
    cm[cm[["Target"]] == max_target_level, "right_icon"] <- empty_path
    cm[cm[["Target"]] == min_target_level, "left_icon"] <- empty_path
  }

  cm
}


#   __________________ #< dcca4b803d31b98d0906e28484ac92b8 ># __________________
#   Empty percentages cols for confusion matrix plot                        ####

empty_tile_percentages <- function(data){
  cols_to_make_invisible_img <- intersect(
    colnames(data),
    c("right_icon", "left_icon", "up_icon", "down_icon")
  )
  cols_to_make_empty_string <- intersect(
    colnames(data),
    c("Class_Percentage_text", "Prediction_Percentage_text")
  )
  # Set image addresses to empty square image
  data[, cols_to_make_invisible_img] <- get_figure_path("empty_square.svg")

  # Set each cell to empty text string
  data[, cols_to_make_empty_string] <- ""

  # Return data
  data
}


#   __________________ #< 044131f18e1777a3f55f678ac9a6e0e8 ># __________________
#   R cmd check imports                                                     ####


# Never used, but removes R CMD check NOTEs
rcmd_import_handler <- function() {
  lifecycle::deprecate_soft()
  broom.mixed::tidy()
  broom::tidy()
}


get_pkg_version <- function(pkg_name){
  vs <- unlist(packageVersion(pkg_name))
  list("major" = vs[[1]],
       "minor" = vs[[2]],
       "patch" = vs[[3]],
       "dev" = ifelse(length(vs) > 3, vs[[4]], integer(0)))
}

is_tibble_v2 <- function(){
  get_pkg_version("tibble")$major == 2
}

# Currently, lme4 v 1.1.22+ is the newer versions
# This can be updated later
is_newer_lme4 <- function(){
  v <- get_pkg_version("lme4")
  if (v$major < 1) out <- FALSE
  else if (v$minor < 1) out <- FALSE
  else if (v$patch < 22) out <- FALSE
  else out <- TRUE
  out
}

is_dplyr_1 <- function() {
  v <- get_pkg_version("dplyr")
  v$major >= 1 || (v$minor == 8 && v$patch == 99 && v$dev >= 9003)
}

