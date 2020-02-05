
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cvms <a href='https://github.com/LudvigOlsen/cvms'><img src='man/figures/cvms_logo_242x280_250dpi.png' align="right" height="140" /></a>

**Cross-Validation for Model Selection**  
**Authors:** [Ludvig R. Olsen](http://ludvigolsen.dk/) (
<r-pkgs@ludvigolsen.dk> ), Hugh Benjamin Zachariae <br/> **License:**
[MIT](https://opensource.org/licenses/MIT) <br/> **Started:** October
2016

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/cvms)](https://cran.r-project.org/package=cvms)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/cvms)](https://cran.r-project.org/package=cvms)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.5-6666ff.svg)](https://cran.r-project.org/)
[![Codecov test
coverage](https://codecov.io/gh/ludvigolsen/cvms/branch/master/graph/badge.svg)](https://codecov.io/gh/ludvigolsen/cvms?branch=master)
[![Travis build
status](https://travis-ci.org/LudvigOlsen/cvms.svg?branch=master)](https://travis-ci.org/LudvigOlsen/cvms)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/LudvigOlsen/cvms?branch=master&svg=true)](https://ci.appveyor.com/project/LudvigOlsen/cvms)
[![DOI](https://zenodo.org/badge/71063931.svg)](https://zenodo.org/badge/latestdoi/71063931)

## Overview

R package for model evaluation and comparison.

  - **Cross-validate** one or multiple regression or classification
    models with relevant evaluation metrics in a **tidy** format.
  - **Validate** the best model on a test set and compare it to a
    **baseline** evaluation.
  - Perform **hyperparameter tuning** with grid search.
  - **Evaluate** predictions from an external model.
  - Extract the observations that were the **most challenging** to
    predict.

Currently supports regression (`'gaussian'`), binary classification
(`'binomial'`), and (some functions only) multiclass classification
(`'multinomial'`).

### Main functions

| Function              | Description                                                         |
| :-------------------- | :------------------------------------------------------------------ |
| `cross_validate()`    | Cross-validate linear models with `lm()`/`lmer()`/`glm()`/`glmer()` |
| `cross_validate_fn()` | Cross-validate a custom model function                              |
| `validate()`          | Validate linear models with (`lm`/`lmer`/`glm`/`glmer`)             |
| `validate_fn()`       | Validate a custom model function                                    |
| `evaluate()`          | Evaluate predictions with a large set of metrics                    |
| `baseline()`          | Perform baseline evaluations of a dataset                           |

### Evaluation utilities

| Function               | Description                                                     |
| :--------------------- | :-------------------------------------------------------------- |
| `confusion_matrix()`   | Create a confusion matrix from predictions and targets          |
| `evaluate_residuals()` | Evaluate residuals from a regression task                       |
| `most_challenging()`   | Find the observations that were the most challenging to predict |
| `summarize_metrics()`  | Summarize numeric columns with a set of descriptors             |

### Formula utilities

| Function                 | Description                                             |
| :----------------------- | :------------------------------------------------------ |
| `combine_predictors()`   | Generate model formulas from a list of predictors       |
| `reconstruct_formulas()` | Extract formulas from output tibble                     |
| `simplify_formula()`     | Remove inline functions with more from a formula object |

### Plotting utilities

| Function                  | Description                                                                         |
| :------------------------ | :---------------------------------------------------------------------------------- |
| `plot_confusion_matrix()` | Plot a confusion matrix                                                             |
| `plot_metric_density()`   | Create a density plot for a metric column                                           |
| `font()`                  | Set font settings for plotting functions (currently only `plot_confusion_matrix()`) |

### Other utilities

| Function                                                                                             | Description                                              |
| :--------------------------------------------------------------------------------------------------- | :------------------------------------------------------- |
| `select_metrics()`                                                                                   | Select the metric columns from the output                |
| `gaussian_metrics()`<br />`binomial_metrics()`<br />`multinomial_metrics()`                          | Create list of metrics for the common `metrics` argument |
| `example_model_functions()`<br />`example_predict_functions()`<br />`example_preprocess_functions()` | Example functions for `cross_validate_fn()`              |
| `multiclass_probability_tibble()`                                                                    | Generate a multiclass probability tibble                 |

### Datasets

| Name                       | Description                                                                               |
| :------------------------- | :---------------------------------------------------------------------------------------- |
| `participant.scores`       | Made-up experiment data with 10 participants and two diagnoses                            |
| `wines`                    | A list of wine varieties in an approximately Zipfian distribution                         |
| `musicians`                | Made-up data on 60 musicians in 4 groups for multiclass classification                    |
| `predicted.musicians`      | Predictions by 3 classifiers of the 4 classes in the `musicians` dataset                  |
| `precomputed.formulas`     | Fixed effect combinations for model formulas with/without two- and three-way interactions |
| `compatible.formula.terms` | 162,660 pairs of compatible terms for building model formulas with up to 15 fixed effects |

## Table of Contents

  - [cvms](#cvms)
      - [Overview](#overview)
          - [Main functions](#main-functions)
          - [Evaluation utilities](#evaluation-utilities)
          - [Formula utilities](#formula-utilities)
          - [Plotting utilities](#plotting-utilities)
          - [Other utilities](#other-utilities)
          - [Datasets](#datasets)
      - [Important News](#news)
      - [Installation](#installation)
  - [Examples](#examples)
      - [Attach packages](#packages)
      - [Load data](#load-data)
      - [Fold data](#fold)
      - [Cross-validate a single model](#cv-single)
          - [Gaussian](#cv-single-gaussian)
          - [Binomial](#cv-single-binomial)
      - [Cross-validate multiple models](#cv-multi)
          - [Create model formulas](#cv-multi-formulas)
          - [Cross-validate fixed effects models](#cv-multi-fixed)
          - [Cross-validate mixed effects models](#cv-multi-mixed)
      - [Repeated cross-validation](#cv-repeated)
      - [Cross-validating custom model functions](#cv-custom)
          - [SVM](#cv-custom-svm)
          - [Naive Bayes](#cv-custom-naive)
      - [Evaluating predictions](#evaluate)
          - [Multinomial evaluation](#evaluate-multinomial)
      - [Baseline evaluations](#baseline)
          - [Binomial baseline](#baseline-binomial)
          - [Multinomial baseline](#baseline-multinomial)
          - [Gaussian baseline](#baseline-gaussian)
      - [Generate model formulas](#generate-formulas)

## Important News

> Check `NEWS.md` for the full list of changes.

  - Version `1.0.0` contained multiple breaking changes. Please see
    `NEWS.md`. (4th of February 2020)

  - `cv_plot()` has been removed.

  - Fixes bug in `evaluate()`, when used on a grouped data frame. The
    row order in the output was not guaranteed to fit with the grouping
    keys. If you have used `evaluate()` on a grouped data frame, please
    rerun to make sure your results are correct\! (30th of November
    2019)

  - In `cross_validate()` and `validate()`, the `models` argument is
    renamed to `formulas` and the `model_verbose` argument is renamed to
    `verbose`. Further, the `link` argument is hard-deprecated and will
    throw an error if used.

  - `Multinomial` AUC is now calculated with `pROC::multiclass.roc`.

  - `cross_validate_fn()` and `validate_fn()` now take the
    `preprocess_fn`, `preprocess_once`, and `hyperparameters` arguments.
    The `predict_type` argument has been removed.

  - `cross_validate_fn()` and `validate_fn()` are added.
    (Cross-)validate custom model functions.

## Installation

CRAN:

> install.packages(“cvms”)

Development version:

> install.packages(“devtools”)
> 
> devtools::install\_github(“LudvigOlsen/groupdata2”)
> 
> devtools::install\_github(“LudvigOlsen/cvms”)

# Examples

## Attach packages

``` r
library(cvms)
library(groupdata2) # fold() partition()
library(knitr) # kable()
library(dplyr) # %>% arrange()
library(ggplot2)
```

## Load data

The dataset `participant.scores` comes with `cvms`.

``` r
data <- participant.scores
```

## Fold data

Create a grouping factor for subsetting of folds using
`groupdata2::fold()`. Order the dataset by the folds:

``` r
# Set seed for reproducibility
set.seed(7)

# Fold data 
data <- fold(data, k = 4,
             cat_col = 'diagnosis',
             id_col = 'participant') %>% 
  arrange(.folds)

# Show first 15 rows of data
data %>% head(15) %>% kable()
```

| participant | age | diagnosis | score | session | .folds |
| :---------- | --: | --------: | ----: | ------: | :----- |
| 9           |  34 |         0 |    33 |       1 | 1      |
| 9           |  34 |         0 |    53 |       2 | 1      |
| 9           |  34 |         0 |    66 |       3 | 1      |
| 8           |  21 |         1 |    16 |       1 | 1      |
| 8           |  21 |         1 |    32 |       2 | 1      |
| 8           |  21 |         1 |    44 |       3 | 1      |
| 2           |  23 |         0 |    24 |       1 | 2      |
| 2           |  23 |         0 |    40 |       2 | 2      |
| 2           |  23 |         0 |    67 |       3 | 2      |
| 1           |  20 |         1 |    10 |       1 | 2      |
| 1           |  20 |         1 |    24 |       2 | 2      |
| 1           |  20 |         1 |    45 |       3 | 2      |
| 6           |  31 |         1 |    14 |       1 | 2      |
| 6           |  31 |         1 |    25 |       2 | 2      |
| 6           |  31 |         1 |    30 |       3 | 2      |

## Cross-validate a single model

### Gaussian

``` r
CV1 <- cross_validate(data, 
                      formulas = "score~diagnosis",
                      fold_cols = '.folds',
                      family = 'gaussian',
                      REML = FALSE)

# Show results
CV1
#> # A tibble: 1 x 20
#>   Fixed  RMSE   MAE RMSLE   r2m   r2c   AIC  AICc   BIC Predictions Results
#>   <chr> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <list>      <list> 
#> 1 diag…  16.4  13.8 0.474 0.271 0.271  195.  196.  198. <tibble [3… <tibbl…
#> # … with 9 more variables: Coefficients <list>, Folds <int>, `Fold
#> #   Columns` <int>, `Convergence Warnings` <int>, `Singular Fit
#> #   Messages` <int>, `Other Warnings` <int>, `Warnings and Messages` <list>,
#> #   Family <chr>, Dependent <chr>

# Let's take a closer look at the different parts of the output 

# Metrics and formulas
CV1 %>% select_metrics() %>% kable()
```

| Fixed     |     RMSE |      MAE |     RMSLE |      r2m |      r2c |      AIC |     AICc |      BIC | Dependent |
| :-------- | -------: | -------: | --------: | -------: | -------: | -------: | -------: | -------: | :-------- |
| diagnosis | 16.35261 | 13.75772 | 0.4736577 | 0.270991 | 0.270991 | 194.6218 | 195.9276 | 197.9556 | score     |

``` r

# Nested predictions 
# Note that [[1]] picks predictions for the first row
CV1$Predictions[[1]] %>% head() %>% kable()
```

| Fold Column | Fold | Observation | Target | Prediction |
| :---------- | ---: | ----------: | -----: | ---------: |
| .folds      |    1 |           1 |     33 |   51.00000 |
| .folds      |    1 |           2 |     53 |   51.00000 |
| .folds      |    1 |           3 |     66 |   51.00000 |
| .folds      |    1 |           4 |     16 |   30.66667 |
| .folds      |    1 |           5 |     32 |   30.66667 |
| .folds      |    1 |           6 |     44 |   30.66667 |

``` r

# Nested results from the different folds
CV1$Results[[1]] %>% kable()
```

| Fold Column | Fold |     RMSE |      MAE |     RMSLE |       r2m |       r2c |      AIC |     AICc |      BIC |
| :---------- | ---: | -------: | -------: | --------: | --------: | --------: | -------: | -------: | -------: |
| .folds      |    1 | 12.56760 | 10.72222 | 0.3555080 | 0.2439198 | 0.2439198 | 209.9622 | 211.1622 | 213.4963 |
| .folds      |    2 | 16.60767 | 14.77778 | 0.5805901 | 0.2525524 | 0.2525524 | 182.8739 | 184.2857 | 186.0075 |
| .folds      |    3 | 15.97355 | 12.87037 | 0.4767100 | 0.2306104 | 0.2306104 | 207.9074 | 209.1074 | 211.4416 |
| .folds      |    4 | 20.26162 | 16.66049 | 0.4818228 | 0.3568816 | 0.3568816 | 177.7436 | 179.1554 | 180.8772 |

``` r

# Nested model coefficients
# Note that you have the full p-values, 
# but kable() only shows a certain number of digits
CV1$Coefficients[[1]] %>% kable()
```

| Fold Column | Fold | term        |   estimate | std.error |  statistic |   p.value |
| :---------- | ---: | :---------- | ---------: | --------: | ---------: | --------: |
| .folds      |    1 | (Intercept) |   51.00000 |  5.901264 |   8.642216 | 0.0000000 |
| .folds      |    1 | diagnosis   | \-20.33333 |  7.464574 | \-2.723978 | 0.0123925 |
| .folds      |    2 | (Intercept) |   53.33333 |  5.718886 |   9.325826 | 0.0000000 |
| .folds      |    2 | diagnosis   | \-19.66667 |  7.565375 | \-2.599563 | 0.0176016 |
| .folds      |    3 | (Intercept) |   49.77778 |  5.653977 |   8.804030 | 0.0000000 |
| .folds      |    3 | diagnosis   | \-18.77778 |  7.151778 | \-2.625610 | 0.0154426 |
| .folds      |    4 | (Intercept) |   49.55556 |  5.061304 |   9.791065 | 0.0000000 |
| .folds      |    4 | diagnosis   | \-22.30556 |  6.695476 | \-3.331437 | 0.0035077 |

``` r

# Additional information about the model
# and the training process
CV1 %>% select(13:20) %>% kable()
```

| Folds | Fold Columns | Convergence Warnings | Singular Fit Messages | Other Warnings | Warnings and Messages                                                                                                       | Family   | Dependent |
| ----: | -----------: | -------------------: | --------------------: | -------------: | :-------------------------------------------------------------------------------------------------------------------------- | :------- | :-------- |
|     4 |            1 |                    0 |                     0 |              0 | list(`Fold Column` = character(0), Fold = integer(0), Function = character(0), Type = character(0), Message = character(0)) | gaussian | score     |

### Binomial

``` r
CV2 <- cross_validate(data, 
                      formulas = "diagnosis~score",
                      fold_cols = '.folds',
                      family = 'binomial')

# Show results
CV2
#> # A tibble: 1 x 28
#>   Fixed `Balanced Accur…    F1 Sensitivity Specificity `Pos Pred Value`
#>   <chr>            <dbl> <dbl>       <dbl>       <dbl>            <dbl>
#> 1 score            0.736 0.821       0.889       0.583            0.762
#> # … with 22 more variables: `Neg Pred Value` <dbl>, AUC <dbl>, `Lower
#> #   CI` <dbl>, `Upper CI` <dbl>, Kappa <dbl>, MCC <dbl>, `Detection
#> #   Rate` <dbl>, `Detection Prevalence` <dbl>, Prevalence <dbl>,
#> #   Predictions <list>, ROC <list>, `Confusion Matrix` <list>, Results <list>,
#> #   Coefficients <list>, Folds <int>, `Fold Columns` <int>, `Convergence
#> #   Warnings` <int>, `Singular Fit Messages` <int>, `Other Warnings` <int>,
#> #   `Warnings and Messages` <list>, Family <chr>, Dependent <chr>

# Let's take a closer look at the different parts of the output 
# We won't repeat the parts too similar to those in Gaussian

# Metrics
CV2 %>% select(1:10) %>% kable()
```

| Fixed | Balanced Accuracy |        F1 | Sensitivity | Specificity | Pos Pred Value | Neg Pred Value |       AUC |  Lower CI |  Upper CI |
| :---- | ----------------: | --------: | ----------: | ----------: | -------------: | -------------: | --------: | --------: | --------: |
| score |         0.7361111 | 0.8205128 |   0.8888889 |   0.5833333 |      0.7619048 |      0.7777778 | 0.7685185 | 0.5962701 | 0.9407669 |

``` r
CV2 %>% select(11:15) %>% kable()
```

|     Kappa |       MCC | Detection Rate | Detection Prevalence | Prevalence |
| --------: | --------: | -------------: | -------------------: | ---------: |
| 0.4927536 | 0.5048268 |      0.5333333 |                  0.7 |        0.6 |

``` r

# Confusion matrix
CV2$`Confusion Matrix`[[1]] %>% kable()
```

| Fold Column | Prediction | Target | Pos\_0 | Pos\_1 |  N |
| :---------- | :--------- | :----- | :----- | :----- | -: |
| .folds      | 0          | 0      | TP     | TN     |  7 |
| .folds      | 1          | 0      | FN     | FP     |  5 |
| .folds      | 0          | 1      | FP     | FN     |  2 |
| .folds      | 1          | 1      | TN     | TP     | 16 |

``` r

# Plot confusion matrix
plot_confusion_matrix(CV2$`Confusion Matrix`[[1]])
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="644" />

## Cross-validate multiple models

### Create model formulas

``` r
model_formulas <- c("score~diagnosis", "score~age")
mixed_model_formulas <- c("score~diagnosis+(1|session)", "score~age+(1|session)")
```

### Cross-validate fixed effects models

``` r
CV3 <- cross_validate(data, 
                      formulas = model_formulas,
                      fold_cols = '.folds',
                      family = 'gaussian',
                      REML = FALSE)

# Show results
CV3
#> # A tibble: 2 x 20
#>   Fixed  RMSE   MAE RMSLE    r2m    r2c   AIC  AICc   BIC Predictions Results
#>   <chr> <dbl> <dbl> <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <list>      <list> 
#> 1 diag…  16.4  13.8 0.474 0.271  0.271   195.  196.  198. <tibble [3… <tibbl…
#> 2 age    22.4  18.9 0.618 0.0338 0.0338  201.  202.  204. <tibble [3… <tibbl…
#> # … with 9 more variables: Coefficients <list>, Folds <int>, `Fold
#> #   Columns` <int>, `Convergence Warnings` <int>, `Singular Fit
#> #   Messages` <int>, `Other Warnings` <int>, `Warnings and Messages` <list>,
#> #   Family <chr>, Dependent <chr>
```

### Cross-validate mixed effects models

``` r
CV4 <- cross_validate(data, 
                      formulas = mixed_model_formulas,
                      fold_cols = '.folds',
                      family = 'gaussian',
                      REML = FALSE)

# Show results
CV4
#> # A tibble: 2 x 21
#>   Fixed  RMSE   MAE RMSLE    r2m   r2c   AIC  AICc   BIC Predictions Results
#>   <chr> <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <list>      <list> 
#> 1 diag…  7.95  6.41 0.226 0.290  0.811  176.  178.  180. <tibble [3… <tibbl…
#> 2 age   17.5  16.2  0.480 0.0366 0.526  194.  196.  198. <tibble [3… <tibbl…
#> # … with 10 more variables: Coefficients <list>, Folds <int>, `Fold
#> #   Columns` <int>, `Convergence Warnings` <int>, `Singular Fit
#> #   Messages` <int>, `Other Warnings` <int>, `Warnings and Messages` <list>,
#> #   Family <chr>, Dependent <chr>, Random <chr>
```

## Repeated cross-validation

Instead of only dividing our data into folds once, we can do it multiple
times and average the results. As the models can be ranked differently
with different splits, this is generally preferable.

Let’s first add some extra fold columns. We will use the `num_fold_cols`
argument to add 3 *unique* fold columns. We tell `fold()` to keep the
existing fold column and simply add three extra columns. We could also
choose to remove the existing fold column, if, for instance, we were
changing the number of folds (`k`). Note, that the original fold column
will be renamed to `".folds_1"`.

``` r
# Set seed for reproducibility
set.seed(2)

# Fold data 
data <- fold(data, k = 4,
             cat_col = 'diagnosis',
             id_col = 'participant',
             num_fold_cols = 3,
             handle_existing_fold_cols = "keep")

# Show first 15 rows of data
data %>% head(10) %>% kable()
```

| participant | age | diagnosis | score | session | .folds\_1 | .folds\_2 | .folds\_3 | .folds\_4 |
| :---------- | --: | --------: | ----: | ------: | :-------- | :-------- | :-------- | :-------- |
| 10          |  32 |         0 |    29 |       1 | 4         | 4         | 3         | 1         |
| 10          |  32 |         0 |    55 |       2 | 4         | 4         | 3         | 1         |
| 10          |  32 |         0 |    81 |       3 | 4         | 4         | 3         | 1         |
| 2           |  23 |         0 |    24 |       1 | 2         | 3         | 1         | 2         |
| 2           |  23 |         0 |    40 |       2 | 2         | 3         | 1         | 2         |
| 2           |  23 |         0 |    67 |       3 | 2         | 3         | 1         | 2         |
| 4           |  21 |         0 |    35 |       1 | 3         | 2         | 4         | 4         |
| 4           |  21 |         0 |    50 |       2 | 3         | 2         | 4         | 4         |
| 4           |  21 |         0 |    78 |       3 | 3         | 2         | 4         | 4         |
| 9           |  34 |         0 |    33 |       1 | 1         | 1         | 2         | 3         |

Now, let’s cross-validate the four fold columns. We use `paste0()` to
create the four column names:

``` r
CV5 <- cross_validate(data, 
                      formulas = c("diagnosis ~ score", 
                                   "diagnosis ~ score + age"),
                      fold_cols = paste0(".folds_", 1:4),
                      family = 'binomial')

# Show results
CV5
#> # A tibble: 2 x 28
#>   Fixed `Balanced Accur…    F1 Sensitivity Specificity `Pos Pred Value`
#>   <chr>            <dbl> <dbl>       <dbl>       <dbl>            <dbl>
#> 1 score            0.729 0.813       0.875       0.583            0.759
#> 2 scor…            0.545 0.643       0.653       0.438            0.635
#> # … with 22 more variables: `Neg Pred Value` <dbl>, AUC <dbl>, `Lower
#> #   CI` <dbl>, `Upper CI` <dbl>, Kappa <dbl>, MCC <dbl>, `Detection
#> #   Rate` <dbl>, `Detection Prevalence` <dbl>, Prevalence <dbl>,
#> #   Predictions <list>, ROC <list>, `Confusion Matrix` <list>, Results <list>,
#> #   Coefficients <list>, Folds <int>, `Fold Columns` <int>, `Convergence
#> #   Warnings` <int>, `Singular Fit Messages` <int>, `Other Warnings` <int>,
#> #   `Warnings and Messages` <list>, Family <chr>, Dependent <chr>

# Subset of the results per fold for the first model
CV5$Results[[1]] %>% select(1:8) %>% kable()
```

| Fold Column | Balanced Accuracy |        F1 | Sensitivity | Specificity | Pos Pred Value | Neg Pred Value |       AUC |
| :---------- | ----------------: | --------: | ----------: | ----------: | -------------: | -------------: | --------: |
| .folds\_1   |         0.7361111 | 0.8205128 |   0.8888889 |   0.5833333 |      0.7619048 |      0.7777778 | 0.7685185 |
| .folds\_2   |         0.7361111 | 0.8205128 |   0.8888889 |   0.5833333 |      0.7619048 |      0.7777778 | 0.7777778 |
| .folds\_3   |         0.7083333 | 0.7894737 |   0.8333333 |   0.5833333 |      0.7500000 |      0.7000000 | 0.7476852 |
| .folds\_4   |         0.7361111 | 0.8205128 |   0.8888889 |   0.5833333 |      0.7619048 |      0.7777778 | 0.7662037 |

## Cross-validating custom model functions

`cross_validate_fn()` allows us to cross-validate a custom model
function, like a support vector machine or a neural network. It works
with regression (`gaussian`), binary classification (`binomial`), and
multiclass classification (`multinomial`).

It is required to pass a model function and a predict function. Further,
it is possible to pass a preprocessing function and a list of
hyperparameter values to test. You can check the requirements for these
functions at `?cross_validate_fn`.

### SVM

Let’s cross-validate a support-vector machine using the `svm()` function
from the `e1071` package. First, we will create a model function. You
can do anything you want inside it, as long as it takes the arguments
`train_data`, `formula`, and `hyperparameters` and returns a fitted
model object.

``` r
# Create model function
#
# train_data : tibble with the training data
# formula : a formula object
# hyperparameters : a named list of hyparameters

svm_model_fn <- function(train_data, formula, hyperparameters){
  
  # Note that `formula` must be passed first
  # when calling svm(), otherwise it fails
  e1071::svm(formula = formula,
             data = train_data, 
             kernel = "linear",
             type = "C-classification",
             probability = TRUE)
}
```

We also need a predict function. This will usually wrap the
`stats::predict` function. The point is to ensure that the predictions
have the correct format. In this case, we want a single column with the
probability of the positive class.

Note, that you do not need to use the `formula` and `hyperparameters`
arguments within your function.

``` r
# Create predict function
#
# test_data : tibble with the test data
# model : fitted model object
# formula : a formula object
# hyperparameters : a named list of hyparameters

svm_predict_fn <- function(test_data, model, formula, hyperparameters){
  
  # Predict the test set with the model
  predictions <- stats::predict(
    object = model,
    newdata = test_data,
    allow.new.levels = TRUE,
    probability = TRUE)

  # Extract the probabilities
  # Usually the predict function will just 
  # output the probabilities directly
  probabilities <- dplyr::as_tibble(
    attr(predictions, "probabilities")
    )

  # Return second column
  # With probabilities of positive class
  probabilities[[2]]
}
```

With these functions defined, we can cross-validate the support vector
machine:

``` r
# Cross-validate svm_model_fn
CV6 <- cross_validate_fn(data = data,
                         model_fn = svm_model_fn,
                         predict_fn = svm_predict_fn,
                         formulas = c("diagnosis~score", "diagnosis~age"),
                         fold_cols = '.folds_1', 
                         type = 'binomial')
#> Will cross-validate 2 models. This requires fitting 8 model instances.

CV6
#> # A tibble: 2 x 27
#>   Fixed `Balanced Accur…    F1 Sensitivity Specificity `Pos Pred Value`
#>   <chr>            <dbl> <dbl>       <dbl>       <dbl>            <dbl>
#> 1 score            0.653 0.780       0.889       0.417            0.696
#> 2 age              0.458 0.615       0.667       0.25             0.571
#> # … with 21 more variables: `Neg Pred Value` <dbl>, AUC <dbl>, `Lower
#> #   CI` <dbl>, `Upper CI` <dbl>, Kappa <dbl>, MCC <dbl>, `Detection
#> #   Rate` <dbl>, `Detection Prevalence` <dbl>, Prevalence <dbl>,
#> #   Predictions <list>, ROC <list>, `Confusion Matrix` <list>, Results <list>,
#> #   Coefficients <list>, Folds <int>, `Fold Columns` <int>, `Convergence
#> #   Warnings` <int>, `Other Warnings` <int>, `Warnings and Messages` <list>,
#> #   Family <chr>, Dependent <chr>
```

### Naive Bayes

Let’s try with a Naive Bayes classifier as well. First, we will define
the model function:

``` r
# Create model function
#
# train_data : tibble with the training data
# formula : a formula object
# hyperparameters : a named list of hyparameters

nb_model_fn <- function(train_data, formula, hyperparameters){
  e1071::naiveBayes(formula = formula, 
                    data = train_data)
}
```

And the predict function:

``` r
# Create predict function
#
# test_data : tibble with the test data
# model : fitted model object
# formula : a formula object
# hyperparameters : a named list of hyparameters

nb_predict_fn <- function(test_data, model, formula, hyperparameters){
  stats::predict(object = model, 
                 newdata = test_data, 
                 type = "raw", 
                 allow.new.levels = TRUE)[,2]
}
```

With both functions specified, we are ready to cross-validate our naive
Bayes classifier:

``` r
CV7 <- cross_validate_fn(data,
                         model_fn = nb_model_fn,
                         predict_fn = nb_predict_fn,
                         formulas = c("diagnosis~score", "diagnosis~age"),
                         type = 'binomial',
                         fold_cols = '.folds_1')
#> Will cross-validate 2 models. This requires fitting 8 model instances.

CV7
#> # A tibble: 2 x 27
#>   Fixed `Balanced Accur…    F1 Sensitivity Specificity `Pos Pred Value`
#>   <chr>            <dbl> <dbl>       <dbl>       <dbl>            <dbl>
#> 1 score            0.736 0.821       0.889       0.583            0.762
#> 2 age              0.25  0.462       0.5         0                0.429
#> # … with 21 more variables: `Neg Pred Value` <dbl>, AUC <dbl>, `Lower
#> #   CI` <dbl>, `Upper CI` <dbl>, Kappa <dbl>, MCC <dbl>, `Detection
#> #   Rate` <dbl>, `Detection Prevalence` <dbl>, Prevalence <dbl>,
#> #   Predictions <list>, ROC <list>, `Confusion Matrix` <list>, Results <list>,
#> #   Coefficients <list>, Folds <int>, `Fold Columns` <int>, `Convergence
#> #   Warnings` <int>, `Other Warnings` <int>, `Warnings and Messages` <list>,
#> #   Family <chr>, Dependent <chr>
```

## Evaluating predictions

We can also evaluate predictions from a model trained outside `cvms`.
This works with regression (`gaussian`), binary classification
(`binomial`), and multiclass classification (`multinomial`). The
following is an example of multinomial evaluation.

### Multinomial evaluation

We start by creating a dataset with 3 predictors and a target column.
Partition it with `groupdata2::partition()` to create a training set and
a validation set. `multiclass_probability_tibble()` is a simple helper
function for generating random tibbles.

``` r
# Set seed
set.seed(1)

# Create class names
class_names <- paste0("class_", 1:4)

# Create random dataset with 100 observations 
# Partition into training set (75%) and test set (25%)
multiclass_partitions <- multiclass_probability_tibble(
  num_classes = 3, # Here, number of predictors
  num_observations = 100,
  apply_softmax = FALSE,
  FUN = rnorm,
  class_name = "predictor_") %>%
  dplyr::mutate(class = sample(
    class_names,
    size = 100,
    replace = TRUE)) %>%
  partition(p = 0.75,
            cat_col = "class")

# Extract partitions
multiclass_train_set <- multiclass_partitions[[1]]
multiclass_test_set <- multiclass_partitions[[2]]

multiclass_test_set
#> # A tibble: 26 x 4
#>    predictor_1 predictor_2 predictor_3 class  
#>          <dbl>       <dbl>       <dbl> <chr>  
#>  1      1.60         0.158     -0.331  class_1
#>  2     -1.99        -0.180     -0.341  class_1
#>  3      0.418       -0.324      0.263  class_1
#>  4      0.398        0.450      0.136  class_1
#>  5      0.0743       1.03      -1.32   class_1
#>  6      0.738        0.910      0.541  class_2
#>  7      0.576        0.384     -0.0134 class_2
#>  8     -0.305        1.68       0.510  class_2
#>  9     -0.0449      -0.393      1.52   class_2
#> 10      0.557       -0.464     -0.879  class_2
#> # … with 16 more rows
```

Train a multinomial model using the `nnet` package and get the predicted
probabilities:

``` r
# Train multinomial model
multiclass_model <- nnet::multinom(
   "class ~ predictor_1 + predictor_2 + predictor_3",
   data = multiclass_train_set)
#> # weights:  20 (12 variable)
#> initial  value 102.585783 
#> iter  10 value 98.124010
#> final  value 98.114250 
#> converged

# Predict the targets in the test set
predictions <- predict(multiclass_model, 
                       multiclass_test_set,
                       type = "probs") %>%
  dplyr::as_tibble()

# Add the targets
predictions[["target"]] <- multiclass_test_set[["class"]]

head(predictions, 10)
#> # A tibble: 10 x 5
#>    class_1 class_2 class_3 class_4 target 
#>      <dbl>   <dbl>   <dbl>   <dbl> <chr>  
#>  1   0.243   0.214   0.304   0.239 class_1
#>  2   0.136   0.371   0.234   0.259 class_1
#>  3   0.230   0.276   0.264   0.230 class_1
#>  4   0.194   0.218   0.262   0.326 class_1
#>  5   0.144   0.215   0.302   0.339 class_1
#>  6   0.186   0.166   0.241   0.407 class_2
#>  7   0.201   0.222   0.272   0.305 class_2
#>  8   0.117   0.131   0.195   0.557 class_2
#>  9   0.237   0.264   0.215   0.284 class_2
#> 10   0.216   0.310   0.303   0.171 class_2
```

Perform the evaluation. This will create one-vs-all binomial evaluations
and summarize the results:

``` r
# Evaluate predictions
ev <- evaluate(data = predictions,
               target_col = "target",
               prediction_cols = class_names,
               type = "multinomial")

ev
#> # A tibble: 1 x 15
#>   `Overall Accura… `Balanced Accur…    F1 Sensitivity Specificity
#>              <dbl>            <dbl> <dbl>       <dbl>       <dbl>
#> 1            0.154            0.427   NaN       0.143       0.712
#> # … with 10 more variables: `Pos Pred Value` <dbl>, `Neg Pred Value` <dbl>,
#> #   Kappa <dbl>, MCC <dbl>, `Detection Rate` <dbl>, `Detection
#> #   Prevalence` <dbl>, Prevalence <dbl>, Predictions <list>, `Confusion
#> #   Matrix` <list>, `Class Level Results` <list>
```

The class level results (i.e., the one-vs-all evaluations) are also
included and could be reported alongside the above results.

``` r
ev$`Class Level Results`
#> [[1]]
#> # A tibble: 4 x 14
#>   Class `Balanced Accur…      F1 Sensitivity Specificity `Pos Pred Value`
#>   <chr>            <dbl>   <dbl>       <dbl>       <dbl>            <dbl>
#> 1 clas…            0.476 NaN           0           0.952            0    
#> 2 clas…            0.380   0.211       0.286       0.474            0.167
#> 3 clas…            0.474 NaN           0           0.947            0    
#> 4 clas…            0.380   0.211       0.286       0.474            0.167
#> # … with 8 more variables: `Neg Pred Value` <dbl>, Kappa <dbl>, MCC <dbl>,
#> #   `Detection Rate` <dbl>, `Detection Prevalence` <dbl>, Prevalence <dbl>,
#> #   Support <int>, `Confusion Matrix` <list>
```

## Baseline evaluations

While it’s common to find the chance-level baseline analytically (in
classification tasks), it’s often possible to get a better evaluation
than that by chance. Hence, it is useful to check the range of our
metrics when randomly guessing the probabilities.

Usually, we use `baseline()` on our test set at the start of our
modeling process, so we know what level of performance we should beat.

Start by partitioning the dataset:

``` r
# Set seed for reproducibility
set.seed(1)

# Partition the dataset 
partitions <- groupdata2::partition(participant.scores,
                                    p = 0.7,
                                    cat_col = 'diagnosis',
                                    id_col = 'participant',
                                    list_out = TRUE)
train_set <- partitions[[1]]
test_set <- partitions[[2]]
```

### Binomial baseline

Approach: `n` random sets of predictions are evaluated against the
dependent variable in the test set. We also evaluate a set of all `0`s
and a set of all `1`s.

Create the baseline evaluations:

``` r
binomial_baseline <- baseline(test_data = test_set, n = 300, 
         dependent_col = "diagnosis", family = "binomial")

binomial_baseline$summarized_metrics
#> # A tibble: 10 x 15
#>    Measure `Balanced Accur…      F1 Sensitivity Specificity `Pos Pred Value`
#>    <chr>              <dbl>   <dbl>       <dbl>       <dbl>            <dbl>
#>  1 Mean               0.497   0.483       0.482       0.512            0.491
#>  2 Median             0.5     0.5         0.5         0.5              0.5  
#>  3 SD                 0.132   0.154       0.198       0.185            0.150
#>  4 IQR                0.167   0.208       0.333       0.333            0.200
#>  5 Max                0.833   0.857       1           0.833            0.833
#>  6 Min                0.25    0.182       0           0.167            0    
#>  7 NAs                0       4           0           0                0    
#>  8 INFs               0       0           0           0                0    
#>  9 All_0              0.5   NaN           0           1              NaN    
#> 10 All_1              0.5     0.667       1           0                0.5  
#> # … with 9 more variables: `Neg Pred Value` <dbl>, AUC <dbl>, `Lower CI` <dbl>,
#> #   `Upper CI` <dbl>, Kappa <dbl>, MCC <dbl>, `Detection Rate` <dbl>,
#> #   `Detection Prevalence` <dbl>, Prevalence <dbl>
```

On average, we can expect an `F1` score of approximately `0.483`. The
maximum `F1` score achieved by randomly guessing was `0.857` though.
That’s likely because of the small size of the test set, but it
illustrates how such information could be useful in a real-life
scenario.

The `All_1` row shows us that we can achieve an `F1` score of `0.667` by
always predicting `1`. Some model architectures, like neural networks,
have a tendency to always predict the majority class. Such a model is
quite useless of course, why it is good to be aware of the performance
it could achieve. We could also check the confusion matrix for such a
pattern.

``` r
binomial_baseline$random_evaluations
#> # A tibble: 300 x 19
#>    `Balanced Accur…    F1 Sensitivity Specificity `Pos Pred Value`
#>               <dbl> <dbl>       <dbl>       <dbl>            <dbl>
#>  1            0.417 0.462       0.5         0.333            0.429
#>  2            0.667 0.6         0.5         0.833            0.75 
#>  3            0.5   0.571       0.667       0.333            0.5  
#>  4            0.417 0.364       0.333       0.5              0.4  
#>  5            0.583 0.545       0.5         0.667            0.6  
#>  6            0.583 0.545       0.5         0.667            0.6  
#>  7            0.667 0.667       0.667       0.667            0.667
#>  8            0.417 0.364       0.333       0.5              0.4  
#>  9            0.333 0.333       0.333       0.333            0.333
#> 10            0.583 0.545       0.5         0.667            0.6  
#> # … with 290 more rows, and 14 more variables: `Neg Pred Value` <dbl>,
#> #   AUC <dbl>, `Lower CI` <dbl>, `Upper CI` <dbl>, Kappa <dbl>, MCC <dbl>,
#> #   `Detection Rate` <dbl>, `Detection Prevalence` <dbl>, Prevalence <dbl>,
#> #   Predictions <list>, ROC <list>, `Confusion Matrix` <list>, Family <chr>,
#> #   Dependent <chr>
```

We can plot the distribution of `F1` scores from the random evaluations:

``` r
# First, remove the NAs from the F1 column
random_evaluations <- binomial_baseline$random_evaluations
random_evaluations <- random_evaluations[!is.na(random_evaluations$F1),]

# Create density plot for F1
plot_metric_density(data = random_evaluations, 
                    metric = "F1", xlim = c(0, 1))
```

<img src="man/figures/README-unnamed-chunk-25-1.png" width="644" />

### Multinomial baseline

Approach: Creates one-vs-all (binomial) baseline evaluations for `n`
sets of random predictions against the dependent variable, along with
sets of `all class x,y,z,...` predictions.

Create the baseline evaluations:

``` r
multiclass_baseline <- baseline(
  test_data = multiclass_test_set, n = 300,
  dependent_col = "class", family = "multinomial")

# Summarized metrics
multiclass_baseline$summarized_metrics
#> # A tibble: 16 x 13
#>    Measure `Overall Accura… `Balanced Accur…       F1 Sensitivity Specificity
#>    <chr>              <dbl>            <dbl>    <dbl>       <dbl>       <dbl>
#>  1 Mean              0.247            0.498    0.295       0.247       0.749 
#>  2 Median            0.231            0.501    0.294       0.25        0.748 
#>  3 SD                0.0885           0.0589   0.0686      0.0885      0.0296
#>  4 IQR               0.115            0.0851   0.0743      0.136       0.0407
#>  5 Max               0.538            0.688    0.523       0.529       0.847 
#>  6 Min               0.0385           0.358    0.155       0.0357      0.680 
#>  7 NAs               0                0      155           0           0     
#>  8 INFs              0                0        0           0           0     
#>  9 CL_Max           NA                0.902    0.857       1           1     
#> 10 CL_Min           NA                0.214    0.111       0           0.429 
#> 11 CL_NAs           NA                0      197           0           0     
#> 12 CL_INFs          NA                0        0           0           0     
#> 13 All_cl…           0.192            0.5    NaN           0.25        0.75  
#> 14 All_cl…           0.269            0.5    NaN           0.25        0.75  
#> 15 All_cl…           0.269            0.5    NaN           0.25        0.75  
#> 16 All_cl…           0.269            0.5    NaN           0.25        0.75  
#> # … with 7 more variables: `Pos Pred Value` <dbl>, `Neg Pred Value` <dbl>,
#> #   Kappa <dbl>, MCC <dbl>, `Detection Rate` <dbl>, `Detection
#> #   Prevalence` <dbl>, Prevalence <dbl>
```

The `CL_` measures describe the `Class Level Results` (aka. one-vs-all
evaluations). E.g. we have 65 `NA`s in the class-level `F1` results.
When averaged to the random evaluations tibble, there are only 50 `NA`s.
Similarly, one of the classes have a maximum `Balanced Accuracy` score
of `0.778`, while the maximum `Balanced Accuracy` in the random
evaluations is `0.646`.

``` r

# Summarized class level results for class 1
multiclass_baseline$summarized_class_level_results %>% 
  dplyr::filter(Class == "class_1") %>%
  tidyr::unnest(Results)
#> # A tibble: 10 x 13
#>    Class Measure `Balanced Accur…      F1 Sensitivity Specificity
#>    <chr> <chr>              <dbl>   <dbl>       <dbl>       <dbl>
#>  1 clas… Mean               0.501   0.278       0.258      0.743 
#>  2 clas… Median             0.483   0.25        0.2        0.762 
#>  3 clas… SD                 0.113   0.124       0.204      0.0915
#>  4 clas… IQR                0.152   0.182       0.2        0.143 
#>  5 clas… Max                0.857   0.727       1          1     
#>  6 clas… Min                0.214   0.118       0          0.429 
#>  7 clas… NAs                0      69           0          0     
#>  8 clas… INFs               0       0           0          0     
#>  9 clas… All_0              0.5   NaN           0          1     
#> 10 clas… All_1              0.5     0.323       1          0     
#> # … with 7 more variables: `Pos Pred Value` <dbl>, `Neg Pred Value` <dbl>,
#> #   Kappa <dbl>, MCC <dbl>, `Detection Rate` <dbl>, `Detection
#> #   Prevalence` <dbl>, Prevalence <dbl>

# Random evaluations
# Note, that the class level results for each repetition
# is available as well
multiclass_baseline$random_evaluations
#> # A tibble: 300 x 18
#>    Repetition `Overall Accura… `Balanced Accur…      F1 Sensitivity Specificity
#>         <int>            <dbl>            <dbl>   <dbl>       <dbl>       <dbl>
#>  1          1           0.154             0.443 NaN          0.171        0.716
#>  2          2           0.154             0.438 NaN          0.157        0.719
#>  3          3           0.0385            0.361 NaN          0.0357       0.686
#>  4          4           0.231             0.507 NaN          0.271        0.743
#>  5          5           0.269             0.503 NaN          0.25         0.755
#>  6          6           0.192             0.454 NaN          0.179        0.730
#>  7          7           0.269             0.501 NaN          0.25         0.753
#>  8          8           0.269             0.512 NaN          0.264        0.760
#>  9          9           0.269             0.509   0.269      0.264        0.754
#> 10         10           0.346             0.573   0.345      0.364        0.783
#> # … with 290 more rows, and 12 more variables: `Pos Pred Value` <dbl>, `Neg
#> #   Pred Value` <dbl>, Kappa <dbl>, MCC <dbl>, `Detection Rate` <dbl>,
#> #   `Detection Prevalence` <dbl>, Prevalence <dbl>, Predictions <list>,
#> #   `Confusion Matrix` <list>, `Class Level Results` <list>, Family <chr>,
#> #   Dependent <chr>
```

### Gaussian baseline

Approach: The baseline model `(y ~ 1)`, where `1` is simply the
intercept (i.e. mean of `y`), is fitted on `n` random subsets of the
training set and evaluated on the test set. We also perform an
evaluation of the model fitted on the entire training set.

We usually wish to establish whether our predictors add anything useful
to our model. We should thus at least do better than a model without any
predictors.

Create the baseline evaluations:

``` r
gaussian_baseline <- baseline(test_data = test_set, 
                              train_data = train_set,
                              n = 300, 
                              dependent_col = "score", 
                              family = "gaussian")

gaussian_baseline$summarized_metrics
#> # A tibble: 9 x 5
#>   Measure    RMSE    MAE  RMSLE `Training Rows`
#>   <chr>     <dbl>  <dbl>  <dbl>           <dbl>
#> 1 Mean     19.7   15.8   0.557             9.73
#> 2 Median   19.2   15.5   0.548             9   
#> 3 SD        1.10   0.820 0.0229            3.07
#> 4 IQR       0.907  0.111 0.0172            6   
#> 5 Max      25.1   20.2   0.665            15   
#> 6 Min      18.9   15.5   0.541             5   
#> 7 NAs       0      0     0                 0   
#> 8 INFs      0      0     0                 0   
#> 9 All_rows 19.1   15.5   0.543            18
```

The `All_rows` row tells us the performance when fitting the intercept
model on the full training set. It is quite close to the mean of the
random evaluations.

``` r
gaussian_baseline$random_evaluations
#> # A tibble: 300 x 9
#>     RMSE   MAE RMSLE Predictions Coefficients `Training Rows` Family Dependent
#>    <dbl> <dbl> <dbl> <list>      <list>                 <int> <chr>  <chr>    
#>  1  19.9  16.2 0.602 <tibble [1… <tibble [1 …               8 gauss… score    
#>  2  23.8  19.2 0.620 <tibble [1… <tibble [1 …               5 gauss… score    
#>  3  19.2  15.5 0.543 <tibble [1… <tibble [1 …              10 gauss… score    
#>  4  18.9  15.5 0.556 <tibble [1… <tibble [1 …              14 gauss… score    
#>  5  18.9  15.5 0.561 <tibble [1… <tibble [1 …              14 gauss… score    
#>  6  19.2  15.5 0.575 <tibble [1… <tibble [1 …               9 gauss… score    
#>  7  20.2  15.8 0.544 <tibble [1… <tibble [1 …              10 gauss… score    
#>  8  20.0  15.7 0.542 <tibble [1… <tibble [1 …              13 gauss… score    
#>  9  19.5  15.5 0.541 <tibble [1… <tibble [1 …               9 gauss… score    
#> 10  24.2  19.5 0.632 <tibble [1… <tibble [1 …               8 gauss… score    
#> # … with 290 more rows, and 1 more variable: Fixed <chr>
```

Plot the density plot for `RMSE`:

``` r
plot_metric_density(data = gaussian_baseline$random_evaluations,
                    metric = "RMSE")
```

<img src="man/figures/README-unnamed-chunk-30-1.png" width="644" />

In this instance, the `All_rows` row might have been enough, as the
subsets mainly add higher `RMSE` scores.

## Generate model formulas

Instead of manually typing all possible model formulas for a set of
fixed effects (including the possible interactions),
`combine_predictors()` can do it for you (with some constraints).

When including interactions, \>200k formulas have been precomputed for
up to 8 fixed effects, with a maximum interaction size of 3, and a
maximum of 5 fixed effects per formula. It’s possible to further limit
the generated formulas.

We can also append a random effects structure to the generated formulas.

``` r
combine_predictors(dependent = "y",
                   fixed_effects = c("a","b","c"),
                   random_effects = "(1|d)")
#>  [1] "y ~ a + (1|d)"                     "y ~ b + (1|d)"                    
#>  [3] "y ~ c + (1|d)"                     "y ~ a * b + (1|d)"                
#>  [5] "y ~ a * c + (1|d)"                 "y ~ a + b + (1|d)"                
#>  [7] "y ~ a + c + (1|d)"                 "y ~ b * c + (1|d)"                
#>  [9] "y ~ b + c + (1|d)"                 "y ~ a * b * c + (1|d)"            
#> [11] "y ~ a * b + c + (1|d)"             "y ~ a * c + b + (1|d)"            
#> [13] "y ~ a + b * c + (1|d)"             "y ~ a + b + c + (1|d)"            
#> [15] "y ~ a * b + a * c + (1|d)"         "y ~ a * b + b * c + (1|d)"        
#> [17] "y ~ a * c + b * c + (1|d)"         "y ~ a * b + a * c + b * c + (1|d)"
```

If two or more fixed effects should not be in the same formula, like an
effect and its log-transformed version, we can provide them as sublists.

``` r
combine_predictors(dependent = "y",
                   fixed_effects = list("a", list("b","log_b")),
                   random_effects = "(1|d)")
#> [1] "y ~ a + (1|d)"         "y ~ b + (1|d)"         "y ~ log_b + (1|d)"    
#> [4] "y ~ a * b + (1|d)"     "y ~ a * log_b + (1|d)" "y ~ a + b + (1|d)"    
#> [7] "y ~ a + log_b + (1|d)"
```
