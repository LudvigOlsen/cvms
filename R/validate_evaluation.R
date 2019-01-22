validate_evaluation <- function(fold_eval_fn, eval_aggregation_fn,
                                predictions_and_targets, model,
                                model_specifics){

  # Evaluate as single fold
  single_fold_evaluation <- list(fold_eval_fn(predictions_and_targets,
                                         model, fold=1,
                                         model_specifics=model_specifics))

  # Aggregate fold evaluations
  model_evaluation <- eval_aggregation_fn(single_fold_evaluation, 1,
                                          model_specifics=model_specifics)


  model_evaluation

}
