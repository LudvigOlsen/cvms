
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
-   validate()
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
#> # A tibble: 1 x 17
#>    RMSE   MAE   r2m   r2c   AIC  AICc   BIC Predictions Results
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <list>      <list> 
#> 1  16.7  14.0 0.272 0.272  195.  196.  198. <tibble [3… <tibbl…
#> # … with 8 more variables: Coefficients <list>, Folds <int>, `Fold
#> #   Columns` <int>, `Convergence Warnings` <dbl>, Family <chr>,
#> #   Link <chr>, Dependent <chr>, Fixed <chr>

# Let's take a closer look at the different parts of the output 

# Results metrics
CV1 %>% select(1:7) %>% kable()
```

|      RMSE|       MAE|        r2m|        r2c|       AIC|      AICc|       BIC|
|---------:|---------:|----------:|----------:|---------:|---------:|---------:|
|  16.66528|  13.99321|  0.2716369|  0.2716369|  194.6045|  195.9104|  197.9384|

``` r

# Nested predictions 
# Note that [[1]] picks predictions for the first row
CV1$Predictions[[1]] %>% head() %>% kable()
```

| Fold Column |  Fold|  Target|  Prediction|
|:------------|-----:|-------:|-----------:|
| .folds      |     1|      33|          51|
| .folds      |     1|      53|          51|
| .folds      |     1|      66|          51|
| .folds      |     1|      11|          31|
| .folds      |     1|      35|          31|
| .folds      |     1|      41|          31|

``` r

# Nested results from the different folds/models
CV1$Results[[1]] %>% kable()
```

| Fold Column |  Fold|      RMSE|       MAE|        r2m|        r2c|       AIC|      AICc|       BIC|
|:------------|-----:|---------:|---------:|----------:|----------:|---------:|---------:|---------:|
| .folds      |     1|  13.34791|  11.50000|  0.2410542|  0.2410542|  209.5432|  210.7432|  213.0773|
| .folds      |     2|  15.88941|  13.32099|  0.1807444|  0.1807444|  183.4500|  184.8618|  186.5836|
| .folds      |     3|  17.22940|  13.92963|  0.2307319|  0.2307319|  206.9734|  208.1734|  210.5075|
| .folds      |     4|  20.19443|  17.22222|  0.4340170|  0.4340170|  178.4516|  179.8633|  181.5851|

``` r

# Nested model coefficients
# Note that you have the full p-values, 
# but kable() only show a certain number of digits
CV1$Coefficients[[1]] %>% kable()
```

| term        |   estimate|  std.error|  statistic|  p.value|  Fold| Fold Column |
|:------------|----------:|----------:|----------:|--------:|-----:|:------------|
| (Intercept) |   51.00000|   5.849976|   8.717984|        0|     1| .folds      |
| diagnosis   |  -20.00000|   7.399700|  -2.702812|        0|     1| .folds      |
| (Intercept) |   49.77778|   5.797873|   8.585524|        0|     2| .folds      |
| diagnosis   |  -16.11111|   7.669865|  -2.100573|        0|     2| .folds      |
| (Intercept) |   49.55556|   5.545020|   8.936948|        0|     3| .folds      |
| diagnosis   |  -18.42222|   7.013958|  -2.626509|        0|     3| .folds      |
| (Intercept) |   53.33333|   5.147342|  10.361335|        0|     4| .folds      |
| diagnosis   |  -26.66667|   6.809293|  -3.916217|        0|     4| .folds      |

``` r

# Additional information about the model
# and the training process
CV1 %>% select(11:17) %>% kable()
```

|  Folds|  Fold Columns|  Convergence Warnings| Family   | Link     | Dependent | Fixed     |
|------:|-------------:|---------------------:|:---------|:---------|:----------|:----------|
|      4|             1|                     0| gaussian | identity | score     | diagnosis |

### Binomial

``` r
CV2 <- cross_validate(data, "diagnosis~score", 
                     fold_cols = '.folds', 
                     family='binomial')

# Show results
CV2
#> # A tibble: 1 x 24
#>   `Balanced Accur…    F1 Sensitivity Specificity `Pos Pred Value`
#>              <dbl> <dbl>       <dbl>       <dbl>            <dbl>
#> 1            0.708 0.636       0.583       0.833              0.7
#> # … with 19 more variables: `Neg Pred Value` <dbl>, AUC <dbl>, `Lower
#> #   CI` <dbl>, `Upper CI` <dbl>, Kappa <dbl>, MCC <dbl>, `Detection
#> #   Rate` <dbl>, `Detection Prevalence` <dbl>, Prevalence <dbl>,
#> #   Predictions <list>, ROC <list>, Coefficients <list>, Folds <int>,
#> #   `Fold Columns` <int>, `Convergence Warnings` <dbl>, Family <chr>,
#> #   Link <chr>, Dependent <chr>, Fixed <chr>

# Let's take a closer look at the different parts of the output 
# We won't repeat the parts too similar to those in Gaussian

# Results metrics
CV2 %>% select(1:9) %>% kable()
```

|  Balanced Accuracy|         F1|  Sensitivity|  Specificity|  Pos Pred Value|  Neg Pred Value|        AUC|   Lower CI|   Upper CI|
|------------------:|----------:|------------:|------------:|---------------:|---------------:|----------:|----------:|----------:|
|          0.7083333|  0.6363636|    0.5833333|    0.8333333|             0.7|            0.75|  0.7476852|  0.5621978|  0.9331726|

``` r
CV2 %>% select(10:14) %>% kable()
```

|      Kappa|        MCC|  Detection Rate|  Detection Prevalence|  Prevalence|
|----------:|----------:|---------------:|---------------------:|-----------:|
|  0.4285714|  0.4330127|       0.2333333|             0.3333333|         0.4|

``` r

# ROC curve info
CV2$ROC[[1]] %>% head() %>% kable()
```

|  Sensitivities|  Specificities|
|--------------:|--------------:|
|      1.0000000|      0.0000000|
|      1.0000000|      0.0833333|
|      0.9444444|      0.0833333|
|      0.9444444|      0.1666667|
|      0.9444444|      0.2500000|
|      0.8888889|      0.2500000|

Cross-validate multiple models
------------------------------

### Create model formulas

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
#> # A tibble: 2 x 17
#>    RMSE   MAE     r2m     r2c   AIC  AICc   BIC Predictions Results
#>   <dbl> <dbl>   <dbl>   <dbl> <dbl> <dbl> <dbl> <list>      <list> 
#> 1  16.7  14.0 0.272   0.272    195.  196.  198. <tibble [3… <tibbl…
#> 2  19.6  16.2 0.00427 0.00427  202.  203.  205. <tibble [3… <tibbl…
#> # … with 8 more variables: Coefficients <list>, Folds <int>, `Fold
#> #   Columns` <int>, `Convergence Warnings` <dbl>, Family <chr>,
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
#> # A tibble: 2 x 18
#>    RMSE   MAE     r2m   r2c   AIC  AICc   BIC Predictions Results
#>   <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <list>      <list> 
#> 1  8.65  6.94 0.290   0.810  175.  178.  180. <tibble [3… <tibbl…
#> 2 13.7  12.3  0.00467 0.488  195.  198.  200. <tibble [3… <tibbl…
#> # … with 9 more variables: Coefficients <list>, Folds <int>, `Fold
#> #   Columns` <int>, `Convergence Warnings` <dbl>, Family <chr>,
#> #   Link <chr>, Dependent <chr>, Fixed <chr>, Random <chr>
```

Repeated cross-validation
-------------------------

Note: currently only work with the github version of groupdata2!

Let's first create new folds. We will use the num\_fold\_cols argument to add 3 unique fold columns.

``` r
# devtools::install_github("ludvigolsen/groupdata2", ref="repeatedCV")

# Set seed for reproducibility
set.seed(7)

# Fold data 
data <- fold(data, k = 4,
             cat_col = 'diagnosis',
             id_col = 'participant',
             num_fold_cols = 3)

# Show first 15 rows of data
data %>% head(10) %>% kable()
```

| participant |  age|  diagnosis|  score|  session| .folds\_1 | .folds\_2 | .folds\_3 |
|:------------|----:|----------:|------:|--------:|:----------|:----------|:----------|
| 10          |   32|          0|     29|        1| 3         | 1         | 1         |
| 10          |   32|          0|     55|        2| 3         | 1         | 1         |
| 10          |   32|          0|     81|        3| 3         | 1         | 1         |
| 2           |   23|          0|     24|        1| 4         | 3         | 4         |
| 2           |   23|          0|     40|        2| 4         | 3         | 4         |
| 2           |   23|          0|     67|        3| 4         | 3         | 4         |
| 4           |   21|          0|     35|        1| 2         | 2         | 2         |
| 4           |   21|          0|     50|        2| 2         | 2         | 2         |
| 4           |   21|          0|     78|        3| 2         | 2         | 2         |
| 9           |   34|          0|     33|        1| 1         | 4         | 3         |

``` r
CV5 <- cross_validate(data, "diagnosis~score+(1|session)", 
                     fold_cols = c('.folds_1','.folds_2','.folds_3'), 
                     family='binomial', 
                     REML = FALSE)

# Show results
CV5
#> # A tibble: 1 x 26
#>   `Balanced Accur…    F1 Sensitivity Specificity `Pos Pred Value`
#>              <dbl> <dbl>       <dbl>       <dbl>            <dbl>
#> 1            0.852 0.822       0.833       0.870            0.812
#> # … with 21 more variables: `Neg Pred Value` <dbl>, AUC <dbl>, `Lower
#> #   CI` <dbl>, `Upper CI` <dbl>, Kappa <dbl>, MCC <dbl>, `Detection
#> #   Rate` <dbl>, `Detection Prevalence` <dbl>, Prevalence <dbl>,
#> #   Predictions <list>, ROC <list>, Coefficients <list>, Results <list>,
#> #   Folds <int>, `Fold Columns` <int>, `Convergence Warnings` <dbl>,
#> #   Family <chr>, Link <chr>, Dependent <chr>, Fixed <chr>, Random <chr>

# The binomial output now has a nested results tibble
# Let's see a subset of the columns
CV5$Results[[1]] %>% select(1:8) %>%  kable()
```

| Fold Column |  Balanced Accuracy|         F1|  Sensitivity|  Specificity|  Pos Pred Value|  Neg Pred Value|        AUC|
|:------------|------------------:|----------:|------------:|------------:|---------------:|---------------:|----------:|
| .folds\_1   |          0.8611111|  0.8333333|    0.8333333|    0.8888889|       0.8333333|       0.8888889|  0.8217593|
| .folds\_2   |          0.8333333|  0.8000000|    0.8333333|    0.8333333|       0.7692308|       0.8823529|  0.8333333|
| .folds\_3   |          0.8611111|  0.8333333|    0.8333333|    0.8888889|       0.8333333|       0.8888889|  0.8032407|

Plot results
------------

There are currently a small set of plots for quick visualization of the results. It is supposed to be easy to extract the needed information to create your own plots. If you lack access to any information or have other requests or ideas, feel free to open an issue.

### Gaussian

``` r
cv_plot(CV1, type = "RMSE") +
  theme_bw()
```

![](README-unnamed-chunk-12-1.png)

``` r
cv_plot(CV1, type = "r2") +
  theme_bw()
```

![](README-unnamed-chunk-12-2.png)

``` r
cv_plot(CV1, type = "IC") +
  theme_bw()
```

![](README-unnamed-chunk-12-3.png)

``` r
cv_plot(CV1, type = "coefficients") +
  theme_bw()
```

![](README-unnamed-chunk-12-4.png)

### Binomial

``` r
cv_plot(CV2, type = "ROC") +
  theme_bw()
```

![](README-unnamed-chunk-13-1.png)
