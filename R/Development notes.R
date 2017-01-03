
# cross_validate()

########################
# Notes for development:
#
# Make sure output is not scientific numbers (eg. 1.45e-3)
#
# Add folds to the output of gaussian
#
# 2. Create a sorting function that takes into account the rmse, AIC and whether there's any convergence errors
# Maybe a parameter can be to rank by rmse/AIC/r2m etc. as it can be hard to do weighting of the different 
# parameters that are on different scales, etc.
# Some might prefer to have the models in the way they've built it up - (so they keep adding variables)
#
#
# Question: Is it meaningful to have AIC from each fold and report a mean of that? 
## Or should we compare to AIC on the model fitted on the entire dataset instead?
#
# Warning messages:
#   1: In confusionMatrix.default(binomial_pred_obs_class$predicted_class,  :
#                                   Levels are not in the same order for reference and data. Refactoring data to match.
#
# Are we supposed to always get the same prevalence?
#
# What to do with:
#   fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients
#   Make specific warning for this and count it.
#
#
########################

