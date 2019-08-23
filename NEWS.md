# cvms 0.1.2.9000

* Adds Zenodo DOI for easier citation.

* In nested confusion matrices, the Reference column is renamed to Target, to use the same naming scheme as in the nested predictions.

* evaluate() is added. Evaluate your model's predictions with the same metrics as used in cross_validate().

* Adds 'multinomial' family to baseline() 

* Adds multiclass_probability_tibble() for generating a random probability tibble. 

# cvms 0.1.2

* Bug fix: p-values are correctly added to the nested coefficients tibble. Adds tests of this table as well.

* Adds extra unit tests to increase code coverage.

* When argument "model_verbose" is TRUE, the used model function is now messaged instead of printed.

* Adds badges to README, including travis-ci status, AppVeyor status, 
Codecov, min. required R version, CRAN version and monthly CRAN downloads. Note: Zenodo badge will be added post release.

# cvms 0.1.1

* Unit tests have been made compatible with R v. 3.5

# cvms 0.1.0

* Adds optional parallelization.

* Results now contain a count of singular fit messages. See ?lme4::isSingular for more information.

* Argument "positive" changes default value to 2. Now takes either 1 or 2 (previously 0 and 1). If your dependent variable has values 0 and 1, 1 is now the positive class by default.

* AUC calculation has changed. Now explicitly sets the direction in pROC::roc.

* Unit tests have been updated for the new random sampling generator in R 3.6.0. They will NOT run previous versions of R. 

* Adds baseline() for creating baseline evaluations.

* Adds reconstruct_formulas() for reconstructing formulas based on model definition columns in the results tibble.

* Adds combine_predictors() for generating model formulas from a set of fixed effects.

* Adds select_metrics() for quickly selecting the metrics and model definition columns.

* Breaking change: Metrics have been rearranged and a few metrics have been added.  

* Breaking change: Renamed argument folds_col to fold_cols to better fit the new repeated cross-validation option.  

* New: repeated cross-validation.  

* Created package :)  
