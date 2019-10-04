# Evaluates single model object
# and extracts information like coefficients
internal_evaluate_model <- function(model,
                                    train_data,
                                    test_data,
                                    type,
                                    fold_info,
                                    model_specifics = list(),
                                    metrics,
                                    include_fold_columns = TRUE){

  if (type == "gaussian") {
    results <- gaussian_model_eval(
      model = model,
      train_data = train_data,
      test_data = test_data,
      type = type,
      fold_info = fold_info,
      model_specifics = model_specifics,
      metrics = metrics,
      include_fold_columns = include_fold_columns)

  } else if (type == "binomial"){

    stop("NOT YET IMPLEMENTED")
    results <- binomial_model_eval(
      model = model,
      train_data = train_data,
      test_data = test_data,
      type = type,
      fold_info = fold_info,
      model_specifics = model_specifics,
      metrics = metrics,
      include_fold_columns = include_fold_columns)

  } else if (type == "multinomial"){

    stop("NOT YET IMPLEMENTED")
    results <- multinomial_model_eval(
      model = model,
      train_data = train_data,
      test_data = test_data,
      type = type,
      fold_info = fold_info,
      model_specifics = model_specifics,
      metrics = metrics,
      include_fold_columns = include_fold_columns)

  }

  results

}
