library(cvms)
context("cross_validate_fn_single()")

test_that("gaussian models with cross_validate_single_fn()",{

  # Load data and fold it
  set.seed(1)
  dat <- groupdata2::fold(participant.scores, k = 4,
                          cat_col = 'diagnosis',
                          id_col = 'participant')

  lmer_model_fn <- function(train_set,
                            test_set,
                            fold,
                            model_verbose,
                            model_formula){

    y_col <- extract_y(model_formula)

    model <- run_basic_model(lme4::lmer, model_formula, data = train_set,
                             warn_info=list(model_formula, fold, model_verbose))

    if (is.null(model)){ converged <- FALSE }
    else { converged <- TRUE}

    predictions = stats::predict(model, test_set, allow.new.levels=TRUE)

    predictions_and_observations <- tibble::tibble("true_value"=test_set[[y_col]],
                                                   "prediction"=predictions,
                                                   "fold"=fold,
                                                   "converged"=converged)

    return(list(predictions_and_observations=predictions_and_observations,
                model=model))

  }


  cross_validate_fn_single(dat, lmer_model_fn,
                           fold_evaluation_lm_lmer,
                           eval_aggregation_lm_lmer,
                           eval_model_specifics = list(family="gaussian", link=default_link(NULL, "gaussian")),
                           folds_col =".folds",
                           model_formula="score~diagnosis+(1|session)",
                           model_verbose = FALSE)

})
