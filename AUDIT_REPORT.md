# cvms Package Audit Report

**Package:** cvms v2.0.0 (Cross-Validation for Model Selection)
**Scope:** 63 R source files, 26 test files
**Date:** 2026-03-13

---

## Summary

The codebase is generally well-structured and well-tested. However, this audit identified **6 confirmed bugs**, **numerous edge-case gaps that can produce silently incorrect results**, and **widespread use of deprecated dplyr/tidyr functions** that will cause breakage in future releases.

---

## CONFIRMED BUGS

### 1. `base_rename()` validates the wrong argument (CRITICAL)

**File:** `R/helpers.R:761`

```r
if (length(before) != 1 || length(before) != 1) {
  stop("'before' and 'after' must both have length 1.")
}
```

The second condition checks `length(before)` again instead of `length(after)`. This means:
- A multi-element `after` vector is never caught by this guard.
- Downstream behavior with a multi-element `after` is undefined and could silently corrupt column names.

**Fix:** Change the second `before` to `after`:
```r
if (length(before) != 1 || length(after) != 1) {
```

---

### 2. Missing `na.rm` in cross-validation metric averaging (HIGH)

**File:** `R/cross_validate_list.R:371-373`

```r
average_model_metrics <- current_model_metrics %>%
  base_deselect(cols = fold_info_cols[["rel_fold"]]) %>%
  dplyr::group_by(!!as.name(fold_info_cols[["fold_column"]])) %>%
  dplyr::summarise_all(.funs = ~ mean(.)) %>%
  base_deselect(cols = fold_info_cols[["fold_column"]]) %>%
  dplyr::summarise_all(.funs = ~ mean(.))
```

Neither `mean()` call includes `na.rm = TRUE`. If any fold iteration produces an `NA` metric (e.g., from a non-convergent model), the entire averaged metric becomes `NA`/`NaN` **without any warning**.

This is inconsistent with other evaluation paths:
- `R/evaluate_predictions_gaussian.R:154-156` correctly uses `na.rm = na.rm`.
- `R/evaluate_predictions_binomial.R:336` correctly uses `na.rm = na.rm`.
- `R/confusion_matrix.R:546` correctly uses `na.rm = na.rm`.

The comment at line 186 of the same file even says *"omitting potential NAs from non-converged iterations"*, but the code doesn't implement this.

**Fix:** Add `na.rm = TRUE` (or pass through the `na.rm` parameter) to both `mean()` calls.

---

### 3. `tidy_confusion_matrix()` fails when input is already a tibble (MEDIUM)

**File:** `R/confusion_matrix.R:646-673`

```r
tidy_confusion_matrix <- function(conf_mat, c_levels = NULL) {
  if (!tibble::is_tibble(conf_mat)) {
    conf_mat_df <- dplyr::as_tibble(conf_mat)
  }
  # conf_mat_df is used from here on, but is NEVER defined
  # when conf_mat is already a tibble
  if (nrow(conf_mat_df) == 4) {
    ...
```

If `conf_mat` is already a tibble, `conf_mat_df` is never assigned, causing an "object 'conf_mat_df' not found" error. Currently masked because `create_confusion_matrix()` always returns a `table()` object, but this will break if anyone calls `tidy_confusion_matrix()` with a tibble input.

**Fix:** Add an `else` branch: `else { conf_mat_df <- conf_mat }`.

---

### 4. `sensitivity()` missing input validation (LOW)

**File:** `R/confusion_matrix.R:737-740`

```r
sensitivity <- function(label_counts) {
  # TP / (TP + FN)
  label_counts[["TP"]] / (label_counts[["TP"]] + label_counts[["FN"]])
}
```

Unlike all other metric functions (`specificity`, `prevalence`, `pos_pred_value`, `neg_pred_value`, `detection_rate`, `detection_prevalence`, `threat_score`, `balanced_accuracy`, `accuracy`, `f_score`, `kappa`, `mcc`), `sensitivity()` does **not** call `check_label_counts()`. This also affects `false_neg_rate()` (line 788) which delegates to `sensitivity()`.

**Fix:** Add `check_label_counts(label_counts)` as the first line of `sensitivity()`.

---

### 5. Binomial predict functions hardcode positive class column (HIGH)

**File:** `R/predict_functions.R:82, 108, 134`

```r
# svm_binomial (line 82):
probabilities[[2]]

# naive_bayes (line 108):
predict(..., type = "raw")[, 2]

# randomForest_binomial (line 134):
predict(..., type = "prob")[, 2]
```

All three binomial predict functions hardcode extraction of the **second** column from the probability matrix. This assumes the positive class is always the second alphabetical level (the package default `positive = 2`). If a user sets `positive = 1` (first alphabetical level as positive), these functions still return the probability for the *second* level, producing inverted probability estimates that silently yield wrong AUC, log loss, and all probability-derived metrics.

**Fix:** Pass the `positive` level through to predict functions, or extract the column by name rather than position.

---

### 6. NRMSE, RAE, RSE, and MAPE divide by zero on constant/zero targets (HIGH)

**File:** `R/evaluate_residuals.R:219-234`

```r
nrmse_iqr <- rmse / targets_iqr     # Inf when IQR = 0
nrmse_rng <- rmse / targets_range   # Inf when range = 0
nrmse_std <- rmse / targets_std     # Inf when sd = 0
nrmse_avg <- rmse / targets_mean    # Inf when mean = 0
rae <- tae / sum(abs_targets_centered)     # NaN when all targets identical
rse <- tse / sum(square_targets_centered)  # NaN when all targets identical
ape <- abs(residuals__ / targets)          # Inf when any target = 0
mape <- mean(ape)                          # Inf
```

When target values are constant (all identical) or contain zeros, multiple normalized metrics produce `Inf` or `NaN` with no warning. These values then propagate through fold averaging. The test suite actually expects `NaN` for RMSLE (line 877 of `test_metrics.R`), confirming this is known but unhandled.

**Fix:** Return `NA` with a warning when normalization denominators are zero.

---

## EDGE CASES THAT CAN PRODUCE WRONG/SILENT RESULTS

### 5. Division by zero in metric functions (MEDIUM)

Multiple metric functions can produce `NaN` via division by zero with no warning:

| Function | Condition for division by zero |
|----------|-------------------------------|
| `sensitivity()` | TP + FN = 0 (no actual positives) |
| `specificity()` | TN + FP = 0 (no actual negatives) |
| `pos_pred_value()` | TP + FP = 0 (no positive predictions) |
| `neg_pred_value()` | TN + FN = 0 (no negative predictions) |
| `f_score()` | Both precision and recall are 0 |
| `threat_score()` | TP + FN + FP = 0 |
| `kappa()` | `p_expected = 1` (perfect chance agreement) |

These are all reachable in practice with one-vs-all evaluations where a class has zero support or zero predictions. The `NaN` values then silently propagate through averages. Consider returning `NA` with a warning, or documenting the behavior explicitly.

### 6. `multiclass_mcc()` silently returns 0 for degenerate inputs

**File:** `R/confusion_matrix.R:883-898`

```r
mcc <- sum(cov_ytyp / sqrt(cov_ytyt * cov_ypyp))
if (is.na(mcc)) mcc <- 0
```

When the denominator is 0 (e.g., all predictions are the same class), `NaN` is produced and silently replaced with `0`. An MCC of 0 implies random performance, but the true situation (degenerate classifier) is qualitatively different. A warning would help users understand why their MCC is 0.

### 7. Empty hyperparameter grid produces silent empty results

**File:** `R/computational_grid.R:46-48`

The code checks for `NULL` hyperparameters but not for an empty list (`list()`). Passing `hparams = list()` causes `expand.grid()` to return an empty tibble, resulting in zero models evaluated with no error message.

### 8. Single-observation test folds

**File:** `R/prepare_train_test.R:291-295`

The code checks for `nrow(test_data) == 0` but not for `nrow(test_data) == 1`. A single-observation test fold will:
- Produce a degenerate ROC curve (AUC undefined or misleading)
- Make most classification metrics undefined (only one class in the fold)
- No warning is given

---

## DEPRECATED API USAGE (WILL BREAK IN FUTURE)

72 instances of `TODO`/`FIXME` across 23 files indicate known technical debt. Most critically:

### Deprecated dplyr functions (superseded in dplyr 1.0.0+)

Used across 10+ source files:

| Deprecated Function | Replacement | Files Affected |
|---------------------|-------------|----------------|
| `summarise_all()` / `summarize_all()` | `across()` | `cross_validate_list.R`, `confusion_matrix.R`, `evaluate_predictions_multinomial.R`, `evaluate_predictions_gaussian.R`, `evaluate_predictions_binomial.R`, `baseline_multinomial.R`, `summarize_metrics.R` |
| `mutate_at()` | `mutate(across(...))` | `helpers.R`, `prepare_evaluation.R`, `baseline_multinomial.R`, `combine_predictors.R`, `plot_probability_violins.R` |
| `rename_at()` | `rename_with()` | `prepare_evaluation.R`, `evaluate_predictions_multinomial.R` |
| `select_at()` | `select(all_of(...))` | `baseline_gaussian.R` |
| `group_by_at()` | `group_by(across(...))` | `evaluate.R` |

### Deprecated tidyr functions

**File:** `R/helpers.R:491-512`

The package wraps `tidyr::nest_legacy()` and `tidyr::unnest_legacy()` in helper functions. These have been deprecated since tidyr 1.0.0 (2019). The code even has TODO comments acknowledging this.

### `dplyr::mutate_all()`

**File:** `R/softmax.R:60` - Uses `dplyr::mutate_all(softmax_vector)` which is deprecated.

---

## DESIGN INCONSISTENCIES

### 9. Observation ID convention differs between `cross_validate` and `validate`

- `R/cross_validate_list.R:181`: All observations get positive sequential IDs via `seq_len(nrow(data))`.
- `R/validate_list.R:196-197`: Test observations get positive sequential IDs, but training observations are set to `-1`.

This inconsistency means downstream code that processes observation IDs must handle both conventions, and the `-1` sentinel value is undocumented.

### 10. `model_metric_wrapper()` swallows errors as warnings

**File:** `R/metrics.R:83-105`

```r
error = function(e) {
  if (raise_errors) stop(e)
  if (grepl("no applicable method for", as.character(e), ignore.case = TRUE)) {
    return(NA)
  }
  warning(e)
  NA
}
```

When `raise_errors = FALSE` (the default), model metric calculation errors are downgraded to warnings and `NA` is returned. Combined with the missing `na.rm` in averaging (Bug #2), this means: a metric calculation error in one fold silently produces `NaN` for the entire model's metrics.

### 11. `weighted.mean()` with `na.rm=TRUE` does NOT exclude `NaN`

**File:** `R/confusion_matrix.R:557`, `R/evaluate_predictions_multinomial.R:350`

```r
weighted.mean(., w = support, na.rm = na.rm)
```

R's `weighted.mean()` with `na.rm = TRUE` only removes `NA` values, **not** `NaN`. When metric functions produce `NaN` from division by zero (see Bug #5 and items 7-8), `na.rm = TRUE` does not save you. One `NaN` metric for any class contaminates the entire weighted average, producing `NaN` for the overall result.

### 12. `r.squaredGLMM` warning handling is a no-op

**File:** `R/metrics.R:3-22`

```r
warning = function(w) {
  if (grepl("now calculates a revised statistic", ...)) {
    return(MuMIn::r.squaredGLMM(model_)[1])  # Same call that produced the warning
  } else {
    warning(w)
    return(MuMIn::r.squaredGLMM(model_)[1])  # Same call again
  }
}
```

Both branches of the warning handler call the exact same function that triggered the warning. The "special" branch for the "revised statistic" warning does nothing different from the default branch. The intent was likely to suppress the specific warning, but `return()` inside a `tryCatch` `warning` handler actually does suppress further propagation. However, both branches execute the same code, so the `if/else` is meaningless — it could be replaced with a single `return(MuMIn::r.squaredGLMM(model_)[1])`.

### 13. `argmax` tie-breaking is silent and deterministic

**File:** `R/evaluate_predictions_multinomial.R:492-499`

```r
argmax_row <- function(...) {
  x <- unname(c(...))
  which.max(x)  # Returns FIRST index on ties
}
```

When two or more classes share the highest predicted probability, `which.max()` silently picks the first one (lowest column index, i.e., first alphabetically). No warning is issued. Users have no way to know that ties occurred or that results depend on class name ordering.

### 14. No prediction range validation for binomial models

**File:** `R/run_prediction_process.R:112-136`

Predicted probabilities from custom model functions are never checked to be in `[0, 1]`. Values outside this range (e.g., from a misconfigured predict function returning log-odds instead of probabilities) silently produce nonsensical metrics.

---

## RECOMMENDATIONS

### Immediate fixes (bugs):
1. Fix the `base_rename()` validation typo (`before` -> `after`)
2. Add `na.rm` parameter passthrough to `cross_validate_list.R` averaging
3. Fix `tidy_confusion_matrix()` to handle tibble inputs
4. Add `check_label_counts()` to `sensitivity()`
5. Fix binomial predict functions to respect the `positive` parameter
6. Add zero-denominator guards to NRMSE/RAE/RSE/MAPE calculations

### Short-term (correctness):
7. Add division-by-zero guards or documentation to all metric functions
8. Handle `NaN` (not just `NA`) in `weighted.mean()` calls
9. Warn on single-observation test folds
10. Validate empty hyperparameter grids
11. Validate prediction ranges for binomial models

### Medium-term (maintainability):
12. Replace all deprecated `dplyr` scoped verbs (`*_all`, `*_at`, `*_if`) with `across()`
13. Replace `tidyr::nest_legacy()` / `unnest_legacy()` with current API
14. Address the 72 TODO/FIXME items, especially those noting potential correctness issues
