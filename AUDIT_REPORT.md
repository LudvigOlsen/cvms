# cvms Package Audit Report

**Package:** cvms v2.0.0 (Cross-Validation for Model Selection)
**Scope:** 63 R source files, 26 test files
**Date:** 2026-03-13

---

## Summary

The codebase is generally well-structured and well-tested. However, this audit identified **4 confirmed bugs**, **several edge-case gaps that can produce silently incorrect results**, and **widespread use of deprecated dplyr/tidyr functions** that will cause breakage in future releases.

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

### 11. `r.squaredGLMM` warning handling is a no-op

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

Both branches of the warning handler call the exact same function that triggered the warning. The "special" branch for the "revised statistic" warning does nothing different from the default branch. The intent was likely to suppress the specific warning, but `return()` inside `warning()` doesn't suppress it - the warning was already emitted.

---

## RECOMMENDATIONS

### Immediate fixes (bugs):
1. Fix the `base_rename()` validation typo (`before` -> `after`)
2. Add `na.rm` parameter passthrough to `cross_validate_list.R` averaging
3. Fix `tidy_confusion_matrix()` to handle tibble inputs
4. Add `check_label_counts()` to `sensitivity()`

### Short-term (correctness):
5. Add division-by-zero guards or documentation to metric functions
6. Warn on single-observation test folds
7. Validate empty hyperparameter grids

### Medium-term (maintainability):
8. Replace all deprecated `dplyr` scoped verbs (`*_all`, `*_at`, `*_if`) with `across()`
9. Replace `tidyr::nest_legacy()` / `unnest_legacy()` with current API
10. Address the 72 TODO/FIXME items, especially those noting potential correctness issues
