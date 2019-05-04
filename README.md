
<!-- README.md is generated from README.Rmd. Please edit that file -->
cvms
====

**Cross-Validation for Model Selection**

R package: Cross-validating gaussian and binomial regression models.

By Ludvig R. Olsen and Benjamin Zachariae,
Cognitive Science, Aarhus University.
Started in Oct. 2016

Contact at: <r-pkgs@ludvigolsen.dk>

Main functions:

-   cross\_validate()
-   cv\_plot()

Installation
------------

Development version:

> install.packages("devtools")
>
> devtools::install\_github("LudvigOlsen/groupdata2")
>
> devtools::install\_github("LudvigOlsen/cvms")

Examples
========

Attach packages
---------------

``` r
library(cvms)
library(groupdata2) # fold()
library(knitr) # kable()
library(dplyr) # %>% arrange()
library(ggplot2)
```

Load data
---------

The dataset participant.scores comes with cvms.

``` r
data <- participant.scores
```

Fold data
---------

Create a grouping factor for subsetting in folds using groupdata2::fold(). Order the dataset by the folds.

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

| participant |  age|  diagnosis|  score|  session| .folds |
|:------------|----:|----------:|------:|--------:|:-------|
| 9           |   34|          0|     33|        1| 1      |
| 9           |   34|          0|     53|        2| 1      |
| 9           |   34|          0|     66|        3| 1      |
| 7           |   43|          1|     11|        1| 1      |
| 7           |   43|          1|     35|        2| 1      |
| 7           |   43|          1|     41|        3| 1      |
| 4           |   21|          0|     35|        1| 2      |
| 4           |   21|          0|     50|        2| 2      |
| 4           |   21|          0|     78|        3| 2      |
| 1           |   20|          1|     10|        1| 2      |
| 1           |   20|          1|     24|        2| 2      |
| 1           |   20|          1|     45|        3| 2      |
| 6           |   31|          1|     14|        1| 2      |
| 6           |   31|          1|     25|        2| 2      |
| 6           |   31|          1|     30|        3| 2      |

Cross-validate a single model
-----------------------------

### Gaussian

``` r
CV1 <- cross_validate(data, "score~diagnosis", 
                     fold_cols = '.folds', 
                     family='gaussian', 
                     REML = FALSE)

# Show results
CV1
#> # A tibble: 1 x 15
#>    RMSE   MAE   r2m   r2c   AIC  AICc   BIC Results Coefficients Folds
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <list>  <list>       <int>
#> 1  16.7  14.0 0.272 0.272  195.  196.  198. <tibbl… <tibble [8 …     4
#> # … with 5 more variables: `Convergence Warnings` <dbl>, Family <chr>,
#> #   Link <chr>, Dependent <chr>, Fixed <chr>
```

### Binomial

``` r
CV2 <- cross_validate(data, "diagnosis~score", 
                     fold_cols = '.folds', 
                     family='binomial')

# Show results
CV2
#> # A tibble: 1 x 23
#>   `Balanced Accur…    F1 Sensitivity Specificity `Pos Pred Value`
#>              <dbl> <dbl>       <dbl>       <dbl>            <dbl>
#> 1            0.708 0.636       0.583       0.833              0.7
#> # … with 18 more variables: `Neg Pred Value` <dbl>, AUC <dbl>, `Lower
#> #   CI` <dbl>, `Upper CI` <dbl>, Kappa <dbl>, `Detection Rate` <dbl>,
#> #   `Detection Prevalence` <dbl>, Prevalence <dbl>, MCC <dbl>,
#> #   Predictions <list>, ROC <list>, Coefficients <list>, Folds <int>,
#> #   `Convergence Warnings` <dbl>, Family <chr>, Link <chr>,
#> #   Dependent <chr>, Fixed <chr>
```

Cross-validate multiple models
------------------------------

### Describe model formulas

``` r
models <- c("score~diagnosis","score~age")
mixed_models <- c("score~diagnosis+(1|session)","score~age+(1|session)")
```

### Cross-validate fixed effects models

``` r
CV3 <- cross_validate(data, models, 
                     fold_cols = '.folds', 
                     family='gaussian', 
                     REML = FALSE)

# Show results
CV3
#> # A tibble: 2 x 15
#>    RMSE   MAE     r2m     r2c   AIC  AICc   BIC Results Coefficients Folds
#>   <dbl> <dbl>   <dbl>   <dbl> <dbl> <dbl> <dbl> <list>  <list>       <int>
#> 1  16.7  14.0 0.272   0.272    195.  196.  198. <tibbl… <tibble [8 …     4
#> 2  19.6  16.2 0.00427 0.00427  202.  203.  205. <tibbl… <tibble [8 …     4
#> # … with 5 more variables: `Convergence Warnings` <dbl>, Family <chr>,
#> #   Link <chr>, Dependent <chr>, Fixed <chr>
```

### Cross-validate mixed effects models

``` r
CV4 <- cross_validate(data, mixed_models, 
                     fold_cols = '.folds', 
                     family='gaussian', 
                     REML = FALSE)

# Show results
CV4
#> # A tibble: 2 x 16
#>    RMSE   MAE     r2m   r2c   AIC  AICc   BIC Results Coefficients Folds
#>   <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <list>  <list>       <int>
#> 1  8.65  6.94 0.290   0.810  175.  178.  180. <tibbl… <tibble [8 …     4
#> 2 13.7  12.3  0.00467 0.488  195.  198.  200. <tibbl… <tibble [8 …     4
#> # … with 6 more variables: `Convergence Warnings` <dbl>, Family <chr>,
#> #   Link <chr>, Dependent <chr>, Fixed <chr>, Random <chr>
```

Plot results
------------

### Gaussian

``` r
cv_plot(CV1, type = "RMSE") +
  theme_bw()
```

![](README-unnamed-chunk-10-1.png)

``` r
cv_plot(CV1, type = "r2") +
  theme_bw()
```

![](README-unnamed-chunk-10-2.png)

``` r
cv_plot(CV1, type = "IC") +
  theme_bw()
```

![](README-unnamed-chunk-10-3.png)

``` r
cv_plot(CV1, type = "coefficients") +
  theme_bw()
```

![](README-unnamed-chunk-10-4.png)

### Binomial

``` r
cv_plot(CV2, type = "ROC") +
  theme_bw()
```

![](README-unnamed-chunk-11-1.png)
