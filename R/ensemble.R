
# What if "models" can both be formulas and trained model objects?

# This should be object oriented!

# Should: Train k models and create a model object that uses all the trained models in an ensemble
# This object should have a predict method

ensemble <- function(data, models, folds_col = '.folds', family='gaussian',
                     link = NULL, control=NULL, REML=FALSE,
                     cutoff=0.5, positive=1, rm_nc = FALSE, model_verbose=FALSE){


  return( ensemble_list(data = data,
                        model_list = models,
                        folds_col = folds_col,
                        family = family,
                        link = link,
                        control=control,
                        REML = REML,
                        cutoff = cutoff,
                        positive = positive,
                        rm_nc = rm_nc,
                        model_verbose = model_verbose))



}
