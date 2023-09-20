
# cvms 1.6.0.9000

* `plot_confusion_matrix()` now shows total count when `add_normalized=FALSE`. Thanks [@JianWang2016](https://github.com/LudvigOlsen/cvms/issues/36) for reporting the issue. 

# cvms 1.6.0

## `plot_confusion_matrix()`:

* Breaking: Adds slight 3D tile effect to help separate tiles with the same count. 
Not tested in all many-class scenarios.

* Fixes image sizing (arrows and zero-shading) when there are different
numbers of unique classes in targets and predictions.

* Fixes bug with `class_order` argument when there are different
numbers of unique classes in targets and predictions.

# cvms 1.5.0

## `plot_confusion_matrix()`:

 * NEW: We created a [**Plot Confusion Matrix** *web application*](https://huggingface.co/spaces/ludvigolsen/plot_confusion_matrix)!
It allows using `plot_confusion_matrix()` without code. Select from multiple 
design templates or make your own.

* For `(palette=, sums_settings(palette=))` arguments, tile color 
palettes can now be a custom gradient. Simply supply a named list with hex 
colors for "low" and "high" (e.g. `list("low"="#B1F9E8", "high"="#239895")`).

* Adds `intensity_by`, `intensity_lims`, and `intensity_beyond_lims`
arguments to `sum_tile_settings()` to allow setting them separately
for sum tiles.

* Adds `intensity_lims` argument which allows setting a custom 
range for the tile color intensities. Makes it easier to 
compare plots for different prediction sets.

* Adds `intensity_beyond_lims` for specifying how to handle counts / percentages
outside the specified `intensity_lims`. Default is to truncate the intensities.

* Fixes bug where arrow size was not taking `add_sums` into account.

# cvms 1.4.1

## `plot_confusion_matrix()`:

* Adds option to set `intensity_by` to a log/arcsinh transformed version of the counts. 
This adds the options `"log counts"`, `"log2 counts"`, `"log10 counts"`, `"arcsinh counts"` 
to the `intensity_by` argument.

* Fixes bug when `add_sums = TRUE` and `counts_on_top = TRUE`.

* Raises error for negative counts.

* Fixes zero-division when all counts are 0.

* Sets palette colors to lowest value when all counts are 0.

# cvms 1.4.0

* In `plot_confusion_matrix()`, adds `sub_col` argument for passing in text to replace
the bottom text (`counts` by default).

* In `plot_confusion_matrix()`, fixes direction of arrows when `class_order` is specified.

* In `update_hyperparameters()`, allows `hyperparameters` argument to be `NULL`. Thanks [@ggrothendieck](https://github.com/LudvigOlsen/cvms/issues/30) for reporting the issue.

# cvms 1.3.9

* Minor test fix.

# cvms 1.3.8

* In relevant contexts: Informs user *once* about the `positive` argument in `evaluate()` and `cross_validate*()` not affecting the interpretation of probabilities. I, myself, had forgotten about this in a project, so seems useful to remind us all about :-)

* Fixes usage of the `"all"` name in `set_metrics()` after `purrr v1.0.0` update.

# cvms 1.3.7

* Makes testing conditional on the availability of `xpectr`.

* Fixes `tidyselect`-related warnings.

# cvms 1.3.6

* Prepares for `parameters 0.19.0`. Thanks to @strengejacke.

# cvms 1.3.5

* Fixes tests for CRAN.

# cvms 1.3.4

* Fixes tests for CRAN.

* Adds `merDeriv` as suggested package.

# cvms 1.3.3

* Prepares for `parameters 0.15.0`. Thanks to @strengejacke.

# cvms 1.3.2

* Prepares package for `checkmate 2.1.0`.

# cvms 1.3.1

* Replaces deprecated uses of `ggplot2` functions. Now compatible with `ggplot2 3.3.4`.

# cvms 1.3.0

* In order to reduce dependencies, model coefficients are now tidied with the `parameters` package instead of `broom` and `broom.mixed`. Thanks to @IndrajeetPatil for the contributions.

* In `cross_validate()` and `cross_validate_fn()`, fold columns can now have a varying number of folds in repeated cross-validation. Struggling to choose a number of folds? Average over multiple settings.

* In the `Class Level Results` in multinomial evaluations, the nested `Confusion Matrix` and `Results` tibbles are now named with their class to ease extraction and further work with these tibbles. The `Results` tibble further gets a `Class` column. This information might be redundant, but could make life easier.

* Adds vignette: `Multiple-k: Picking the number of folds for cross-validation`.

# cvms 1.2.1

* Fixes bug in `plot_confusion_matrix()`, where tiles with a count > 0 but a rounded percentage of 0 did not have the percentage text. Only tiles with a count of 0 should now be without text.

# cvms 1.2.0

* Breaking change: In `plot_confusion_matrix()`, the `targets_col` and `predictions_col` arguments have been renamed to `target_col` and `prediction_col` to be consistent with `evaluate()`.

* Breaking change: In `evaluate_residuals()`, the `targets_col` and `predictions_col` arguments have been renamed to `target_col` and `prediction_col` to be consistent with `evaluate()`.

* Breaking change: In `process_info_gaussian/binomial/multinomial()`, the `targets_col` argument have been renamed to `target_col` to be consistent with `evaluate()`.

* In `binomial` `most_challenging()`, the probabilities are now properly of the second class alphabetically.

* In `plot_confusion_matrix()`, adds argument `class_order` for manually setting the order of the classes
in the facets.

* In `plot_confusion_matrix()`, tiles with a count of `0` no longer has text in the tile by default.
This adds the `rm_zero_percentages` (for column/row percentage) and `rm_zero_text` (for counts and normalized) arguments. 

* In `plot_confusion_matrix()`, adds optional sum tiles. Enabling this (`add_sums = TRUE`) adds an extra column and
an extra row with the sums. The corner tile contains the total count. This adds the `add_sums` and `sums_settings` arguments. A `sum_tile_settings()` function has been added to control the appearance of these tiles. Thanks to [@MaraAlexeev](https://github.com/LudvigOlsen/cvms/issues/15) for the idea.

* In `plot_confusion_matrix()`, adds option (`intensity_by`) to set the color intensity of the tiles to the overall percentages (`normalized`). 

# cvms 1.1.0

* In `plot_confusion_matrix()`, adds option to only have row and column percentages in the diagonal tiles. Thanks to [@xgirouxb](https://github.com/LudvigOlsen/cvms/issues/12) for the idea.

* Adds `Process` information to output with the settings used. Adds transparency. It has a custom print method, making it easy to read. Underneath it is a list, why all information is available using `$` or similar. In most cases, the `Family` information has been moved into the `Process` object. Thanks to [@daviddalpiaz](https://github.com/LudvigOlsen/cvms/issues/13) for notifying me of the need for more transparency.

* In outputs, the `Family` information is (in most cases) moved into the new `Process` object.

* In `binomial` `evaluate()` and `baseline()`, `Accuracy` is now enabled by default. It is still disabled in `cross_validate*()` functions to guide users away from using it as the main criterion for model selection (as it is well known to many but can be quite bad in cases with imbalanced datasets.)

* Fixes: In binomial evaluation, the probabilities are now properly of the second class alphabetically.
When the target column was a factor where the levels were not in alphabetical order, the second level in that order was used. The levels are now sorted before extraction. Thanks to [@daviddalpiaz](https://github.com/LudvigOlsen/cvms/issues/13) for finding the bug.

* Fixes: In *grouped* multinomial evaluation, when predictions are classes and there are different sets of classes per group, only the classes in the subset are used.

* Fixes: Bug in `ROC` direction parameter being set wrong when `positive` is numeric. In regression tests, the `AUC` scores were *not* impacted.

* Fixes: 2-class `multinomial` evaluation returns all expected metrics.

* In multinomial evaluation, the `Class Level Results` are sorted by the `Class`.

* Imports `broom.mixed` to allow tidying of coefficients from `lme4::lmer` models.

* Exports `process_info_binomial()`, `process_info_multinomial()`, `process_info_gaussian()` constructors to ensure the various methods are available. They are not necessarily intended for external use.

# cvms 1.0.2

* Compatibility with `dplyr` version `1.0.0`. NOTE: this version of `dplyr` slows down some functions in `cvms` significantly, why it might be beneficial not to update before version `1.1.0`, which is supposed to tackle this problem.

# cvms 1.0.1

* `rsvg` and `ggimage` are now only *suggested* and `plot_confusion_matrix()` throws warning if either are not installed.

* Additional input checks for `evaluate()`.

# cvms 1.0.0

## Breaking changes

### Changed arguments 

* In `cross_validate()` and `validate()`, the `models` argument is renamed to `formulas`. This is a more meaningful name that was recently introduced in `cross_validate_fn()`. For now, the `models` argument is deprecated, will be used instead of `formulas` if specified, and will throw a warning.

* In `cross_validate()` and `validate()`, the `model_verbose` argument is renamed to `verbose`. This is a more meaningful name that was recently introduced in `cross_validate_fn()`. For now, the `model_verbose` argument is deprecated, will be used instead of `verbose` if specified, and will throw a warning.

* In `cross_validate()` and `validate()`, the `link` argument is removed. Consider using `cross_validate_fn()` or `validate_fn()` instead, where you have full control over the prediction type fed to the evaluation.

* In `cross_validate_fn()`, the `predict_type` argument is removed. You now have to pass a predict function as that is safer and more transparent.

* In functions with `family`/`type` argument, this argument no longer has a default, forcing the user to specify the family/type of the task. This also means that arguments have been reordered. In general, it is safer to name arguments when passing values to them.

* In `evaluate()`, `apply_softmax` now defaults to `FALSE`. 
Throws error if probabilities do not add up to 1 row-wise (tolerance of 5 decimals) when `type` is `multinomial`.

### Changed metrics

* `multinomial` `MCC` is now the proper multiclass generalization. Previous versions used `macro MCC`. Removes `MCC` from the class level results. Removes the option to enable `Weighted MCC`.

* `multinomial` `AUC` is calculated with `pROC::multiclass.roc()` instead of in the one-vs-all evaluations. This removes `AUC`, `Lower CI`, and `Upper CI` from the `Class Level Results` and removes `Lower CI` and `Upper CI` from the main output tibble. Also removes option to enable "Weighted AUC", "Weighted Lower CI", and "Weighted Upper CI".

* `multinomial` `AUC` is disabled by default, as it can take a long time to calculate for a large set of classes.

* `ROC` columns now return the `ROC` objects instead of the extracted `sensitivities` and `specificities`, both of which can be extracted from the objects.

* In `evaluate()`, it's no longer possible to pass model objects. It now only evaluates the predictions. This removes the the `AIC`, `AICc`, `BIC`, `r2m`, and `r2c` metrics.

* In `cross_validate` and `validate()`, the `r2m`, and `r2c` metrics are now disabled by default in `gaussian`. The r-squared metrics are non-predictive and should not be used for model selection. They can be enabled with `metrics = list("r2m" = TRUE, "r2c" = TRUE)`.

* In `cross_validate_fn()`, the `AIC`, `AICc`, `BIC`, `r2m`, and `r2c` metrics are now disabled by default in `gaussian`. Only some model types will allow the computation of those metrics, and it is preferable that the user actively makes a choice to include them.

* In `baseline()`, the `AIC`, `AICc`, `BIC`, `r2m`, and `r2c` metrics are now disabled by default in `gaussian`.
It can be unclear whether the IC metrics (computed on the `lm()`/`lmer()` model objects) can be compared to those calculated for a given other model function. To avoid such confusion, it is preferable that the user actively makes a choice to include the metrics. The r-squared metrics will only be non-zero when random effects are passed. Given that we shouldn't use the r-squared metrics for model selection, it makes sense to not have them enabled by default.

### Changes in functionality

* `validate()` now returns a tibble with the model objects nested in the `Model` column. Previously, it returned a list with the results and models. This allows for easier use in `magrittr` pipelines (`%>%`).

* In multinomial `baseline()`, the aggregation approach is changed. The summarized results now properly describe the random evaluations tibble, except for the four new measures `CL_Max`, `CL_Min`, `CL_NAs`, and `CL_INFs`, which describe the class level results. Previously, `NAs` were removed before aggregating the one-vs-all evaluations, meaning that some metric summaries could become inflated if small classes had `NA`s. It was also non-transparent that the `NA`s and `INF`s were counted in the class level results instead of being a count of random evaluations with `NA`s or `INF`s.

* `cv_plot()` is removed. It wasn't very useful and has never been developed properly. We aim to provide specialized plotting functions instead.


## Additions

### New functions

* `validate_fn()` is added. Validate your custom model function on a test set.

* `confusion_matrix()` is added. Create a confusion matrix and calculate associated metrics from your targets and predictions. 

* `evaluate_residuals()` is added. Calculate common metrics from regression residuals. 

* `summarize_metrics()` is added. Use it summarize the numeric columns in your dataset with a set of common descriptors. Counts the `NA`s and `Inf`s. Used by `baseline()`.

* `select_definitions()` is added. Select the columns that define the models, such as `Dependent`, `Fixed`, `Random`, and the (unnested) hyperparameters.

* `model_functions()` is added. Contains simple `model_fn` examples that can be used in `cross_validate_fn()` and `validate_fn()` or as starting points.

* `predict_functions()` is added. Contains simple `predict_fn` examples that can be used in `cross_validate_fn()` and `validate_fn()` or as starting points.

* `preprocess_functions()` is added. Contains simple `preprocess_fn` examples that can be used in `cross_validate_fn()` and `validate_fn()` or as starting points.

* `update_hyperparameters()` is added. For managing hyperparameters when writing custom model functions.

* `most_challenging()` is added. Finds the data points that were the most difficult to predict.

* `plot_confusion_matrix()` is added. Creates a `ggplot` representing a given confusion matrix. Thanks to Malte Lau Petersen (@maltelau), Maris Sala (@marissala) and Kenneth Enevoldsen (@KennethEnevoldsen) for feedback.

* `plot_metric_density()` is added. Creates a ggplot density plot for a metric column. 

* `font()` is added. Utility for setting font settings (size, color, etc.) in plotting functions. 

* `simplify_formula()` is added. Converts a formula with inline functions to a simple formula where all variables are added together (e.g. `y ~ x*z + log(a) + (1|b)` -> `y ~ x + z + a + b`). This is useful when passing a formula to `recipes::recipe()`, which doesn't allow the inline functions.

* `gaussian_metrics()`, `binomial_metrics()`, and `multinomial_metrics()` are added. Can be used to select metrics for the `metrics` argument in many `cvms` functions.

* `baseline_gaussian()`, `baseline_binomial()`, `baseline_multinomial()` are added. Simple wrappers for `baseline()` that are easier to use and have simpler help files. `baseline()` has a lot of arguments that are specific to a family, which can be a bit confusing.

### New datasets

* `wines` dataset is added. Contains a list of wine varieties in an approximately Zipfian distribution.

* `musicians` dataset is added. This has been **generated** for multiclass classification examples.

* `predicted.musicians` dataset is added. This contains cross-validated predictions of the `musicians` dataset by three algorithms. Can be used to demonstrate working with predictions from repeated 5-fold stratified cross-validation.

### New metrics

* Adds `NRMSE(RNG)`, `NRMSE(IQR)`, `NRMSE(STD)`, `NRMSE(AVG)` metrics to `gaussian` evaluations. The `RMSE` is normalized by either target range (RNG), target interquartile range (IQR), target standard deviation (STD), or target mean (AVG). Only `NRMSE(IQR)` is enabled by default.

* Adds `RMSLE`, `RAE`, `RSE`, `RRSE`, `MALE`, `MAPE`, `MSE`, `TAE` and `TSE` metrics to `gaussian` evaluations. `RMSLE`, `RAE`, and `RRSE` are enabled by default.

* Adds Information Criterion metrics (`AIC`, `AICc`, `BIC`) to the `binomial` and `multinomial` output of some functions (disabled by default). These are based on the fitted model objects and will only work for some types of models.

* Adds `Positive Class` column to `binomial` evaluations.

### New function arguments

* Adds optional `hyperparameter` argument to `cross_validate_fn()`. 
Pass a list of hyperparameters and every combination of these will be cross-validated. 

* Adds optional `preprocess_fn` argument to `cross_validate_fn()`. This can, for instance, be used to standardize the training and test sets within the function. E.g., by extracting the scaling and centering parameters from the training set and apply them to both the training set and the test fold.

* Adds `Preprocess` column to output when `preprocess_fn` is passed. Contains returned parameters (e.g. mean, sd) used in preprocessing.

* Adds `preprocess_once` argument to `cross_validate_fn()`. When preprocessing does not depend on the current formula or hyperparameters, we might as well perform it on each train/test split once, instead of for every model.

* Adds `metrics` argument to `baseline()`. Enable the non-default metrics you want a baseline evaluation for.

* Adds `preprocessing` argument to `cross_validate()` and `validate()`. Currently allows "standardize", "scale", "center", and "range". Results will likely not be affected noticeably by the preprocessing.

* Adds `add_targets` and `add_predicted_classes` arguments to `multiclass_probability_tibble()`.

* Adds `Observation` column in the nested predictions tibble in `cross_validate()`, `cross_validate_fn()`, `validate()`, and `validate_fn()`. These indices can be used to identify which observations are difficult to predict.

* Adds `SD` column in the nested predictions tibble in `evaluate()` when performing ID aggregated evaluation with `id_method = 'mean'`. This is the standard deviation of the predictions for the ID.


### New vignettes

* Adds vignette: `Cross-validating custom model functions with cvms`

* Adds vignette: `Creating a confusion matrix with cvms`

* Adds vignette: `The available metrics in cvms`

* Adds vignette: `Evaluate by ID/group`


## Other changes

* The `metrics` argument now allows setting a boolean for `"all"` inside the list to enable or disable all the metrics. For instance, the following would disable all the metrics except `RMSE`: `metrics = list("all" = FALSE, "RMSE" = TRUE)`.

* `multinomial` evaluation results now contain the `Results` tibble with the results for each fold column. The main metrics are now averages of these fold column results. Previously, they were not aggregated by fold column first. In the unit tests, this has not altered the results, but it is a more correct approach.

* The prediction column(s) in `evaluate()` must be either numeric or character, depending on the format chosen.

* In `binomial` `evaluate()`, it's now possible to pass predicted classes instead of probabilities. Probabilities
still carry more information though. Both the prediction and target columns must have type character in this format.

* Changes the required arguments in the `predict_fn` function passed to `cross_validate_fn()`.

* Changes the required arguments in the `model_fn` function passed to `cross_validate_fn()`.

* Warnings and messages from `preprocess_fn` are caught and added to `Warnings and Messages`. Warnings are counted in `Other Warnings`.

* Nesting is now done with `dplyr::group_nest` instead of `tidyr::nest_legacy` for speed improvements.

* `caret`, `mltools`, and `ModelMetrics` are no longer dependencies. The confusion matrix metrics have instead been implemented in `cvms` (see `confusion_matrix()`).

* `select_metrics()` now works with a wider range of inputs as it no longer depends on a `Family` column.

* The `Fixed` column in some of the output tibbles have been moved to make it clearer which model was evaluated.

* Better handling of inline functions in formulas.

# cvms 0.3.2

* Fixes bug in `evaluate()`, when used on a grouped data frame. The row order in the output was not guaranteed to fit the grouping keys. 

# cvms 0.3.1

* Fixes documentation in `cross_validate_fn()`. The examples section contained an unreasonable number of mistakes :-)

* In `cross_validate_fn()`, warnings and messages from the predict function are now included in `Warnings and Messages`. The warnings are counted in `Other Warnings`.

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
