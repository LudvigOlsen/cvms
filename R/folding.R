
add_fold_ids_ = function(data, col){

  # Takes a dataframe and the column with the unique identifier (e.g. subject, ID, name, etc.)
  # Creates a fold_id column in the dataframe
  # .. So every unique value in col is assigned a number from a list
  # ... ranging from 1 to the count of unique values
  # Returns the dataframe ordered by the fold_id column

  data$fold_id = as.numeric(interaction(data[[col]], drop = T))

  return(data[order(data$fold_id),])

}


folder_ = function(data, k){

  # This creates the list of folds without having to type unique() every time.

  return(caret::createFolds(unique(data), k))

}


create_balanced_folds_ = function(data, cat_col=NULL, id_col=NULL, k=5){

  # Creates balanced folds based on a given column (cat_col),
  # that needs to be somewhat equally represented in all folds
  # If cat_col is NULL, don't make the folds balanced

  if (!is.null(cat_col)){

    # Split data by cat_col
    subsets = split(data, data[[cat_col]], drop=TRUE)

  } else {

    # Split data to 1 list element to have it in the same format as
    # if it was splitted by cat_col
    subsets = split(data, 1, drop=TRUE)

  }

  # Create column fold_id in all subsets
  data_list = plyr::llply(subsets, function(d){add_fold_ids_(d, id_col)})

  # Create lists of folds for all subsets
  fold_lists = plyr::llply(data_list, function(d){folder_(d$fold_id, k)})

  # Return a list with the 2 lists data_list and fold_list
  return(list(data_list=data_list, fold_lists=fold_lists))

}

