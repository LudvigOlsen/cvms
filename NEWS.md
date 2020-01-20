
# cvms 0.3.2.9000

## Breaking changes

* In `cross_validate()` and `validate()`, the `models` argument is renamed to `formulas`. This is a more meaningful name that was recently introduced in `cross_validate_fn()`. For now, the `models` argument is deprecated, will be used instead of `formulas` if specified, and will throw a warning.

* In `cross_validate()` and `validate()`, the `model_verbose` argument is renamed to `verbose`. This is a more meaningful name that was recently introduced in `cross_validate_fn()`. For now, the `model_verbose` argument is deprecated, will be used instead of `verbose` if specified, and will throw a warning.

* In `cross_validate()` and `validate()`, the `link` argument is removed. Consider using `cross_validate_fn()` or `validate_fn()` instead, where you have full control over the prediction type fed to the evaluation.

* `multinomial` `AUC` is calculated with `pROC::multiclass.roc()` instead of in the one-vs-all evaluations. This removes `AUC`, `Lower CI`, and `Upper CI` from the `Class Level Results` and removes `Lower CI` and `Upper CI` from the main output tibble. Also removes option to enable "Weighted AUC", "Weighted Lower CI", and "Weighted Upper CI".

* `multinomial` `AUC` is disabled by default, as it can take a long time to calculate for a large set of classes.

* `ROC` columns now return the `ROC` objects instead of the extracted `sensitivities` and `specificities`, both of which can be extracted from the objects.

* In `cross_validate_fn()`, the `predict_type` argument is removed. You now have to pass a predict function as that is safer and more transparent.

* `validate()` now returns a tibble with the model objects nested in the `Model` column. Previously, it returned a list with the results and models. This allows for easier use in `magrittr` pipelines (`%>%`).

* In `evaluate()`, it's no longer possible to pass model objects. It now only evaluates the predictions. This removes the the `AIC`, `AICc`, `BIC`, `r2m`, and `r2c` metrics.

* In `evaluate()`, `apply_softmax` now defaults to `FALSE`. 
Throws error if probabilities do not add up 1 row-wise (tolerance of 5 decimals) when `type` is `multinomial`.

* In `cross_validate_fn()`, the `AIC`, `AICc`, `BIC`, `r2m`, and `r2c` metrics are now disabled by default in `gaussian`. Only some model types will allow the computation of those metrics, and it is preferable that the user actively makes a choice to include them.

* In `baseline()`, the `AIC`, `AICc`, `BIC`, `r2m`, and `r2c` metrics are now disabled by default in `gaussian`.
It can be unclear whether the IC metrics (computed on the `lm()`/`lmer()` model objects) can be compared to those calculated for a given other model function. To avoid such confusion, it is preferable that the user actively makes a choice to include the metrics. The r-squared metrics will only be non-zero when random effects are passed. Given that we shouldn't use the r-squared metrics for model selection, it makes sense to not have them enabled by default.

* In multinomial `baseline()`, the aggregation approach is changed. The summarized results now properly describe the random evaluations tibble, except for the four new measures `CL_Max`, `CL_Min`, `CL_NAs`, and `CL_INFs`, which describe the class level results. Previously, `NAs` were removed before aggregating the one-vs-all evaluations, meaning that some metric summaries could become inflated if small classes had `NA`s. It was also non-transparent that the `NA`s and `INF`s were counted in the class level results instead of being a count of random evaluations with `NA`s or `INF`s.

## Additions

* `validate_fn()` is added. Validate your custom model function on a test set.

* `confusion_matrix()` is added. Create a confusion matrix and calculate associated metrics from your targets and predictions. 

* `evaluate_residuals()` is added. Calculate common metrics from regression residuals. 

* `summarize_metrics()` is added. Use it summarize the numeric columns in your dataset with a set of common descriptors. Counts the `NA`s and `Inf`s. Used by `baseline()`.

* `example_model_functions()` is added. Contains simple `model_fn` examples that can be used in `cross_validate_fn()` and `validate_fn()` or as starting points.

* `example_predict_functions()` is added. Contains simple `predict_fn` examples that can be used in `cross_validate_fn()` and `validate_fn()` or as starting points.

* `example_preprocess_functions()` is added. Contains simple `preprocess_fn` examples that can be used in `cross_validate_fn()` and `validate_fn()` or as starting points.

* `most_challenging()` is added. Finds the data points that were the most difficult to predict.

* `plot_confusion_matrix()` is added. Creates a ggplot representing a given confusion matrix.

* `font()` is added. Utility for setting font settings (size, color, etc.) in plotting functions. 

* `simplify_formula()` is added. Converts a formula with inline functions to a simple formula 
where all variables are added together (e.g. `y ~ x*z + log(a) + (1|b)` -> `y ~ x + z + a + b`). This is 
useful when passing a formula to `recipes::recipe()`, which doesn't allow the inline functions.

* `gaussian_metrics()`, `binomial_metrics()`, and `multinomial_metrics()` are added. Can be used
to select metrics for the `metrics` argument in many `cvms` functions.

* `wines` dataset is added. Contains a list of wine varieties in an approximately Zipfian distribution.

* `musicians` dataset is added. This has been **generated** for multiclass classification examples.

* `predicted.musicians` dataset is added. This contains cross-validated predictions of the `musicians`
dataset by three algorithms. Can be used to demonstrate working with predictions from repeated 5-fold stratified cross-validation.

* Adds optional `hyperparameter` argument to `cross_validate_fn()`. 
Pass a list of hyperparameters and every combination of these will be cross-validated. 

* Adds optional `preprocess_fn` argument to `cross_validate_fn()`. This can, for instance, be used to standardize the training and test sets within the function. E.g., by extracting the scaling and centering parameters from the training set and apply them to both the training set and the test fold.

* Adds `Preprocess` column to output when `preprocess_fn` is passed. Contains returned parameters (e.g. mean, sd) used in preprocessing.

* Adds `preprocess_once` argument to `cross_validate_fn()`. When preprocessing does not depend on the current formula or hyperparameters, we might as well perform it on each train/test split once, instead of for every model.

* Adds Information Criteria metrics (`AIC`, `AICc`, `BIC`) to the `binomial` and `multinomial` output of some functions (disabled by default). 
These are based on the fitted model objects and will only work for some types of models.

* Adds `NRMSE` and `RMSEIQR` metrics to Gaussian evaluations. `NRMSE` is the RMSE divided by the range (max - min) of the target values. `RMSEIQR` is the RMSE divided by the IQR of the target values.

* Adds `metrics` argument to `baseline()`. Enable the non-default metrics you want a baseline evaluation for.

* Adds `preprocessing` argument to `cross_validate()` and `validate()`. Currently allows "standardize", "scale", "center", and "range". Results will likely not be affected noticeably by the preprocessing.

* Adds `Observation` column in the nested predictions tibble in `cross_validate()`, `cross_validate_fn()`, `validate()`, and `validate_fn()`. These indices can be used to identify which observations are difficult to predict.

## Other changes

* The `metrics` argument now allows setting a boolean for `"all"` inside the list to enable or disable all the metrics. For instance, the following would disable all the metrics except `RMSE`: `metrics = list("all" = FALSE, "RMSE" = TRUE)`.

* `multinomial` evaluation results now contain the `Results` tibble with the results for each fold column. The main metrics are now averages of these fold column results. Previously, they were not aggregated by fold column first. In the unit tests, this has not altered the results, but it is a more correct approach.

* Changes the required arguments in the `predict_fn` function passed to `cross_validate_fn()`.

* Changes the required arguments in the `model_fn` function passed to `cross_validate_fn()`.

* Warnings and messages from `preprocess_fn` are caught and added to `Warnings and Messages`. 
Warnings are counted in `Other Warnings`.

* Nesting is now done with `dplyr::group_nest` instead of `tidyr::nest_legacy` for speed improvements.

* `caret`, `mltools`, and `ModelMetrics` are no longer dependencies. The confusion matrix metrics have instead been implemented in `cvms` (see `confusion_matrix()`).

# cvms 0.3.2

* Fixes bug in `evaluate()`, when used on a grouped data frame. The row order in the output was not guaranteed to fit the grouping keys. 


# cvms 0.3.1

* Fixes documentation in `cross_validate_fn()`. The examples section contained an unreasonable number of mistakes :-)

* In `cross_validate_fn()`, warnings and messages from the predict function are now included in 
`Warnings and Messages`. The warnings are counted in `Other Warnings`.

# cvms 0.3.0

* Breaking change: In `evaluate()`, when `type` is `multinomial`, the output is now a single tibble. The `Class Level Results` are included as a nested tibble.

* Breaking change: In `baseline()`, `lmer` models are now fitted with `REML = FALSE` by default.

* Adds `REML` argument to `baseline()`.

* `cross_validate_fn()` is added. Cross-validate custom model functions.

* Bug fix: the `control` argument in `cross_validate()` was not being used. Now it is.

* In `cross_validate()`, the model is no longer fitted twice when a warning is thrown during fitting.

* Adds `metrics` argument to `cross_validate()` and `validate()`. Allows enabling the regular `Accuracy` metric
in `binomial` or to disable metrics (will currently still be computed but not included in the output).

* `AICc` is now computed with the `MuMIn` package instead of the `AICcmodavg` package, which
is no longer a dependency.

* Adds `lifecycle` badges to the function documentation.

# cvms 0.2.0

* `evaluate()` is added. Evaluate your model's predictions with the same metrics as used in `cross_validate()`.

* Adds `'multinomial'` family/type to `baseline()` and `evaluate()`.

* Adds `multiclass_probability_tibble()` for generating a random probability tibble. 

* Adds `random_effects` argument to `baseline()` for adding random effects to the Gaussian baseline model.

* Adds Zenodo DOI for easier citation.

* In nested confusion matrices, the Reference column is renamed to Target, to use the same naming scheme as in the nested predictions.

# cvms 0.1.2

* Bug fix: p-values are correctly added to the nested coefficients tibble. Adds tests of this table as well.

* Adds extra unit tests to increase code coverage.

* When argument `"model_verbose"` is `TRUE`, the used model function is now messaged instead of printed.

* Adds badges to README, including travis-ci status, AppVeyor status, 
Codecov, min. required R version, CRAN version and monthly CRAN downloads. Note: Zenodo badge will be added post release.

# cvms 0.1.1

* Unit tests have been made compatible with `R v. 3.5`

# cvms 0.1.0

* Adds optional parallelization.

* Results now contain a count of singular fit messages. See `?lme4::isSingular` for more information.

* Argument `"positive"` changes default value to 2. Now takes either 1 or 2 (previously 0 and 1). If your dependent variable has values 0 and 1, 1 is now the positive class by default.

* AUC calculation has changed. Now explicitly sets the direction in `pROC::roc`.

* Unit tests have been updated for the new random sampling generator in `R 3.6.0`. They will NOT run previous versions of R. 

* Adds `baseline()` for creating baseline evaluations.

* Adds `reconstruct_formulas()` for reconstructing formulas based on model definition columns in the results tibble.

* Adds `combine_predictors()` for generating model formulas from a set of fixed effects.

* Adds `select_metrics()` for quickly selecting the metrics and model definition columns.

* Breaking change: Metrics have been rearranged and a few metrics have been added.  

* Breaking change: Renamed argument `folds_col` to `fold_cols` to better fit the new repeated cross-validation option.  

* New: repeated cross-validation.  

* Created package :)  
