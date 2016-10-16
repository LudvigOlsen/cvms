##############################################
#
#   cross_validate()
#   by Ludvig R. Olsen and Benjamin Zachariae 
#   Oct. 2016
#   Cognitive Science, Aarhus University
#
##############################################

# Main functions are:
# cross_validate()
# cross_validate_list()    

# Naming convention: underscore_seperated
# Commenting convention: one space after #
                 

# Libraries
library("pacman")
p_load(lmerTest, caret, tidyr, dplyr, plyr, hydroGOF, MuMIn, stats, pROC, broom, ggplot2)


# Helper functions

add_fold_ids_ = function(data, col){
  
  # Takes a dataframe and the column with the unique identifier (e.g. subject, ID, name, etc.)
  # Creates a fold_id column in the dataframe
  # .. So every unique value in col is assigned a number from a list 
  # ... ranging from 1 to the count of unique values
  # Returns the dataframe ordered by the fold_id column
  
  data$fold_id = as.numeric(interaction(data[,col], drop = T))
  
  return(data[order(data$fold_id),])
  
}


folder_ = function(data, k){
  
  # This creates the list of folds without having to type unique() every time. 
  
  return(createFolds(unique(data), k))
  
}


create_balanced_folds_ = function(data, cat_col, id_col, k=5){
  
  # Creates balanced folds based on a given column (cat_col), 
  # that needs to be somewhat equally represented in all folds
  
  # Split data by cat_col 
  subsets = split(data, data[,cat_col], drop=TRUE) 
  
  # Create column fold_id in all subsets
  data_list = llply(subsets, function(d){add_fold_ids_(d, id_col)})
  
  # Create lists of folds for all subsets
  fold_lists = llply(data_list, function(d){folder_(d$fold_id, k)})
  
  # Return a list with the 2 lists data_list and fold_list
  return(list(data_list=data_list, fold_lists=fold_lists))
}


fit_model_ = function(model, model_type, training_set, family, REML, model_verbose){
  
  # Checks the model_type and fits the model on the training_set
  
  if (model_type == 'lm'){
    
    if (model_verbose == TRUE){
      print('Used lm()')}
    
    # Fit the model using lm()
    # Return this model to model_temp
    return(lm(model,training_set))
    
  } else if (model_type == 'lmer'){
    
    if (model_verbose == TRUE){
      print('Used lme4::lmer()')}
    
    # Fit the model using lmer()
    # Return this model to model_temp
    return(lme4::lmer(model,training_set, REML=REML))
    
  } else if (model_type == 'glm'){
    
    if (model_verbose == TRUE){
      print('Used glm()')}
    
    # Fit the model using glm()
    # Return this model to model_temp
    return(glm(model,training_set, family = family))
    
  } else if (model_type == 'glmer'){
    
    if (model_verbose == TRUE){
      print('Used lme4::glmer()')}
    
    # Fit the model using glmer()
    # Return this model to model_temp
    return(lme4::glmer(model,training_set, family = family))
    
  } 
  
  
}


create_model_ = function(model, model_type, training_set, family, REML, fold, model_verbose){   
  
  # Tries to fit the given model with the given model_type
  # .. If it gives a warning
  # .... it checks if it's a convergence warning
  # ...... If it is
  # ........ it issues the warning using warning()
  # ........ and returns NULL to model_temp
  # ...... If it is not
  # ........ it issues the warning using warning()
  # ........ and returns the output of lm() / lmer()
  
  
  model_temp = tryCatch({
    
    # Fit model
    fit_model_(model, model_type, training_set, family, REML, model_verbose)
    
  }, warning = function(w){
    
    # A warning occured!
    
    # Check if the warning contains some words that would 
    # indicate that it is a convergence warning
    # --> This part could be improved upon, as it's currently only based on 
    # the warnings that we got with a limited dataset and set of models
    if (grepl('checkConv', as.character(w)) | 
        grepl('convergence', as.character(w))){ 
      
      # If it seemed to be a convergence warning:
      # .. message the user of the failed model and fold
      # .. issue the warning
      # .. and return NULL to model_temp
      
      message('-------------------------------------')
      message('cross_validate(): Convergence Warning:')
      message('In model:')
      message(model)
      message('In folder:')
      message(fold)
      
      warning(w)
      
      return(NULL)
      
    } else {
      
      # If it didn't seem to be a convergence warning
      # .. message the user of the failed model and fold
      # .. issue the warning
      # .. and return the fitted model
      
      message('-------------------------')
      message('cross_validate(): Warning:')
      message('In model:')
      message(model)
      message('In folder:')
      message(fold)
      
      warning(w)
      
      # Return the fitted model
      # model_verbose = FALSE; It printed the first time, so we don't need it again
      return(fit_model_(model, model_type, training_set, family, REML, model_verbose=FALSE))
      
    }
    
  })
  
  return(model_temp)
  
}


cv_gaussian_ = function(model, test_set, training_set, y_column, fold, random_effects, REML=REML, model_verbose){
  
  # Finds the RMSE, r2m, r2c, AIC, and BIC for the given model
  # Trains the model on training_set and tests it on test_set
  # Notices convergence warnings and returns NA if it's the case
  
  # When using cross_validate_list() we want the function to notice
  # convergence warnings, return NA, and go on with the next fold or model
  # First it finds the model_type (lmer() / lm()) based on whether the model
  # contains random effects
  # Then it uses create_model_() to fit the model 
  # .. In case of a convergence warning this will return NULL
  # Then it checks if model_temp is NULL
  # .. If it is
  # .... it sets the rmse, r2c, etc. of this fold to NA
  # .... it sets "converged" for this fold to "No"
  # .. If it is not
  # .... it finds the rmse by using predict()
  # .... it sets the rmse, r2c, etc. of this fold to the values
  # ..... that we found with lm() / lmer() and predict()
  # .... it sets "converged" for this fold to "Yes"
  
  if (TRUE %in%  random_effects){
    
    model_type = 'lmer'
    
  } else {
    
    model_type = 'lm'
    
  }
  
  # Create model_temp
  model_temp = create_model_(model, model_type, training_set, family, REML=FALSE, fold, model_verbose)
  
  # If model_temp returned NULL
  # .. set the output variables to NA
  # .. set converged to "no"
  # Else
  # .. set it to the right values
  # .. set converged to "yes"
  
  if (is.null(model_temp)){
    
    rmse = NA
    r2m = NA
    r2c = NA
    AIC = NA
    BIC = NA
    converged = "No"
    
  } else {
    
    # Predict the dependent variable in the test_set from model_temp
    predict_temp = predict(model_temp, test_set, allow.new.levels=TRUE)
    
    # Find the values rmse, r2m, r2c, AIC, and BIC
    rmse = rmse(predict_temp, test_set[,y_column])
    r2m = r.squaredGLMM(model_temp)[1]
    r2c = r.squaredGLMM(model_temp)[2]
    AIC = AIC(model_temp)
    BIC = BIC(model_temp)
    converged = "Yes"
    
  }
  
  model_temp_tidied = tidy(model_temp, effects = c("fixed"))
  
  # Return a list with 
  # .. a tidied summary of the model 
  # .. a dataframe with the found values
  return(list(
    model_temp_tidied, 
    data.frame('RMSE' = rmse, 'r2m' = r2m, 'r2c' = r2c, 'AIC' = AIC, 'BIC' = BIC, 'converged'= converged)
    ))
  
}


cv_binomial_ = function(model, test_set, training_set, y_column, fold, random_effects, family, model_verbose){
  # positive=x if the x level in y_column should be set as positive in confusion matrix (default=1)
  
  # Trains a given model on training_set and tests it on test_set
  # Returns list with 
  # .. a dataframe with predictions and the observed side by side
  # .. the variable "converged" (yes/no)
  # Notices convergence warnings and returns NA if it's the case
  
  # When using cross_validate_list() we want the function to notice
  # convergence warnings, return NA, and go on with the next fold or model
  # First it finds the model_type (lmer() / lm()) based on whether the model
  # contains random effects
  # Then it uses create_model_() to fit the model 
  # .. In case of a convergence warning this will return NULL
  # Then it checks if model_temp is NULL
  # .. If it is
  # .... it creates a NA prediction for each observation in the test_set
  # .... it creates a dataframe with the NA predictions and the observations
  # .... it sets "converged" for this fold to "No"
  # .. If it is not
  # .... it predicts the dependent variable in the test_set
  # .... using the fitted model
  # .... it creates a dataframe with the predictions and the observations
  # .... it sets "converged" for this fold to "Yes"
  
  
  if (TRUE %in%  random_effects){
    
    model_type = 'glmer'
    
  } else {
    
    model_type = 'glm'
    
  }
  
  
  model_temp = create_model_(model, model_type, training_set, family, REML=FALSE, fold, model_verbose)  
    
  
  
  # If model_temp returned NULL
  # .. create a list of NA predictions the length of y_column
  # .. create a dataframe with NA predictions and observations side by side
  # .. set converged to "no"
  # Else
  # .. predict the dependent variables in the test_set from the fitted model
  # .. create a dataframe with predictions and observations side by side
  # .. set converged to "yes"
  
  if (is.null(model_temp)){
    
    # Create a list of NA predictions the length of y_column
    predict_temp = list(rep(NA, length(test_set[,y_column])))
    
    # Create a dataframe with the NA predictions and the observations
    predictions_and_y_Temp = data.frame("prediction" = predict_temp[[1]], "y_column" = test_set[, y_column])
    
    # Set converged to no
    converged = "No"  
    
  } else {
    
    # Predict the dependent variable in the test_set from the fitted model
    predict_temp = predict(model_temp, test_set, type="response", allow.new.levels=TRUE) 
    
    # Create a dataframe with predictions and observations side by side
    predictions_and_y_Temp = data.frame("prediction" = predict_temp, y_column = test_set[, y_column])
    
    # Set converged to yes
    converged = "Yes"
    
  }
  
  # Return a list with 
  # .. the dataframe containing predictions and observations 
  # .. the converged variable
  return(list(predictions_and_observations=predictions_and_y_Temp, converged=converged)) 
  
  
}


print_boxplot_ = function(data, var_start, var_end=NULL, plot_theme=theme_bw()){
  
  # Data: gaussian_return or likewise
  # Vars (Variables to plot):
  # .. var_start: first in the var range (e.g. columns 1:3)
  # .. var_end: last in the var range (e.g. columns 1:3)
  # plot_theme: The theme for the plot
  
  # gather() data by the chosen range of vars
  # .. This leaves us with a column for the chosen measure variables 
  # .. and a column for the associated values
  # If only var_start is given, it means that we only want to plot var_start
  
  if (is.null(var_end)){
    data = data %>%
      gather(key, value, var_start)
    
  } else {
    
    data = data %>%
      gather(key, value, var_start:var_end)
  }
  
  # Create and print plot
  gg = ggplot(data, aes(key, value)) 
  print(gg + geom_boxplot() + labs(x = 'Measure', y = 'Value') + plot_theme)
  
  
}


# Main functions 


cross_validate = function(model, data, id_column, cat_column, 
                         nfolds=5, family='gaussian', REML=FALSE, 
                         cutoff=0.5, positive=1, do.plot=FALSE, 
                         which_plot = "all", plot_theme=theme_bw(), 
                         seed = NULL, model_verbose=TRUE){
  # model: y~a+b+(1|c)
  # data: Dataframe
  # id_column: Unique identifiers (e.g. subject, ID, or likewise)
  # cat_col: categorical column for balancing folds
  # nfolds: number of folds
  # family: gaussian or binomial
  # REML: Restricted maximum likelihood 
  # cutoff: For deciding prediction class from prediction (binomial)
  # positive: Level from dependent variable to predict (1/2 - alphabetically) (binomial)
  # do.plot: ROC curve plot (binomial)
  # which_plot: choice between available plots
  # .. 'all' plots
  # .. single plot (e.g. 'RMSE')
  # .. list of plots (e.g. c('RMSE', 'r2'))
  # plot_theme: which theme to use with ggplot2
  # seed: A number for setting seed. Makes sure the folds are the same for model comparison.
  # model_verbose: Printed feedback on the used model (lm() / lmer() / glm() / glmer()) (BOOL)
  
  
  # Set errors if input variables aren't what we expect / can handle
  # WORK ON THIS SECTION!
  stopifnot((!is.null(model)),
            is.data.frame(data), 
            (!is.null(id_column)),
            (!is.null(cat_column)),
            nfolds > 1,
            positive %in% c(1,2)
            )
  

  # Extract y_column from model
  y_column = unlist(strsplit(model, '\\s*~'))[1]

  
  # Check if there are random effects (yields a list e.g. (False, False, True))
  random_effects = grepl('\\(\\d', model, perl=TRUE)
  
  
  # Creating balanced folds
  
  # Set seed to create the same folds every time (for model comparison) (default is NULL)
  set.seed(seed)
  # Create balanced folds
  balanced_folds = create_balanced_folds_(data, cat_column, id_column, k=nfolds)
  # Extract the list of datasets created
  data_list = balanced_folds$data_list
  # Extract the list of lists of folds
  fold_lists = balanced_folds$fold_lists
  
  
  # Create empty dataframes
  if (family == "gaussian"){
    
    gaussian_return = data.frame()
    gaussian_fold_models = data.frame()
    
  } else if (family == 'binomial'){
    
    binomial_predictions_and_observations = data.frame()
    
  }
  
  converged = c()
  
  # Loop through the folds
  # .. Create a test_set and a training_set
  # .. Train the model on the training_set
  # .. Test the model on the test_set
  # .. Gather results from all folds
  
  for(fold in 1:nfolds){
    
    # Remove the test_set and the training_set if they exist
    if (exists ('test_set')){rm(test_set)}
    if (exists ('training_set')){rm(training_set)}
    
    # Initialise test_set and training_set dataframes
    test_set = data.frame()
    training_set = data.frame()
    
    # Loop through the list of lists of folds
    # For each split of the dataset 
    # .. (if folds were balanced by "diagnosis", 
    # .. and there are 2 diagnoses (i.e. factor levels), 
    # .. this means 2 splits / subsets)
    # .. create a test_set and a training_set
    # .... gather from all splits / subsets
    
    for (fl in 1:length(fold_lists)){
      
      # Get the rows from the chosen dataframe in data_list 
      # where the fold_id column matches with the current fold
      temp_test_set = dplyr::filter(data_list[[fl]], fold_id %in% unlist(fold_lists[[1]][fold]))
      
      # Get the rows from the chosen dataframe in data_list 
      # where the fold_id column does NOT match with the current fold
      temp_training_set = dplyr::filter(data_list[[fl]], !(fold_id %in% unlist(fold_lists[[1]][fold])))  
      
      # Gather test_set and training_set from this and previous iterations
      test_set = rbind(test_set,temp_test_set)
      training_set = rbind(training_set,temp_training_set)
      
    }
    
    # Train and test the model
    
    if (family=='gaussian'){
      
      # Call cv_gaussian_
      # Returns the RMSE, r2m, r2c, AIC, BIC, and whether the model converged
      temp_gaussian = cv_gaussian_(model, test_set, training_set, y_column,
                                   fold, random_effects, REML=REML, model_verbose)
      
      temp_gaussian_dataframe = temp_gaussian[[2]] 
      
      temp_gaussian_tidied_model = temp_gaussian[[1]]
      
      # Add the current fold to the dataframes
      temp_gaussian_dataframe$fold = as.factor(fold)
      temp_gaussian_tidied_model$fold = as.factor(fold)
      
      # Gather the tidied model summary with those from previously iterated folds
      gaussian_fold_models = rbind(gaussian_fold_models, temp_gaussian_tidied_model)
      
      # Gather the model comparison outputs with those from previously iterated folds
      gaussian_return = rbind(gaussian_return, temp_gaussian_dataframe)
      
      
      
      
    } else if (family == 'binomial'){
      
      # Call cv_binomial_
      # Returns a list with 1. a dataframe with predictions and observations and 2. whether the model converged or not (yes/no)
      temp_binomial = cv_binomial_(model, test_set, training_set, y_column,
                                   fold, random_effects, family, model_verbose)
      
      # Gather the dataframes from this and previous folds
      binomial_predictions_and_observations = rbind(binomial_predictions_and_observations, temp_binomial$predictions_and_observations) 
      
      # Add whether or not the model converged to the list converged
      converged = c(converged, temp_binomial$converged)
      
    }
    
  }
  
  # Returning
  
  if (family == 'gaussian'){
    
    # Count the convergence warnings
    
    conv_warns = as.integer(table(gaussian_return$converged)['No'])
    if (is.na(conv_warns)){
      conv_warns = 0
    }
    
    # Do plot(s) if requested by the user
    # User can request 1 plot, a list of plots, or all plots
    # Available plots:
    # .. RMSE
    # .. r2 
    # .... plots: r2m and r2c
    # .. IC 
    # .... plots: AIC and BIC
    # .. coefficients
    
    
    if (do.plot == TRUE){
      
      if (which_plot == 'all' || 'RMSE' %in% which_plot){
      
        print_boxplot_(gaussian_return, 1, plot_theme = theme_bw())
        
      }
      
      if (which_plot == 'all' || 'r2' %in% which_plot){
        
        print_boxplot_(gaussian_return, 2, 3, plot_theme = theme_bw())
        
      }
      
      if (which_plot == 'all' || 'IC' %in% which_plot){
        
        print_boxplot_(gaussian_return, 4, 5, plot_theme = theme_bw())
        
      }
      
      if (which_plot == 'all' || 'coefficients' %in% which_plot){
       
        gg = ggplot(gaussian_fold_models, aes(term, estimate)) 
        print(gg + 
              geom_boxplot() + 
              labs(x = 'Fixed Effects', y = 'Estimate') + 
              theme_bw())
      
        }
      
      
    }
    
    # Return a list with
    # .. the means of the found RMSE, r2m, r2c, AIC, and BIC
    # .. the count of convergence warnings
    
    return(c(
      'RMSE' = mean(na.omit(gaussian_return$RMSE)),
      'r2m' = mean(na.omit(gaussian_return$r2m)),
      'r2c' = mean(na.omit(gaussian_return$r2c)),
      'AIC' = mean(na.omit(gaussian_return$AIC)),
      'BIC' = mean(na.omit(gaussian_return$BIC)),
      "Convergence_Warnings" = as.integer(conv_warns)
    ))
    
    
  } else if (family == 'binomial'){
    
    # Find the levels in the categorical dependent variable
    cat_levels = c(as.character(levels(data[,y_column])[1]), as.character(levels(data[,y_column])[2]))
    
    # Create a new dataframe based on the dataframe with predictions and observations
    # Create a column with the predicted class based on the chosen cutoff
    binomial_pred_obs_class = binomial_predictions_and_observations %>%
      mutate(predicted_class = ifelse(.[[1]] < cutoff, cat_levels[1], cat_levels[2])) # e.g. "ASD", "TD"
    
    # Try to use a confusion matrix with the data
    # This will fail if one of the folds didn't converge
    conf_mat = tryCatch({
      confusionMatrix(binomial_pred_obs_class$predicted_class,  
                      binomial_pred_obs_class$y_column, 
                      positive=cat_levels[positive]) 
      
    }, error = function(e) {
      
      # If there are any NAs in the prediction, the model didn't converge
      # .. message the user and return NULL
      # else stop the execution of cross_validate()
      
      if (sum(is.na(binomial_pred_obs_class$prediction)) != 0){
        
        message('Confusion Matrix error as the model didn\'t converge.')
        
      } else {
        
        message('Confusion matrix error:')
        stop(e)
        
      }
      
      return(NULL)
      
    }
    )
    
    # Try to fit a ROC curve on the data
    # This will fail if one of the folds didn't converge
    
    roc_curve = tryCatch({
      roc(response =  binomial_pred_obs_class$y_column, predictor =  binomial_pred_obs_class$prediction)
      
    }, error = function(e) {
      
      # If there are any NAs in the prediction, the model didn't converge
      # .. message the user and return NULL
      # else stop the execution of cross_validate()
      
      if (sum(is.na(binomial_pred_obs_class$prediction)) != 0){
        
        message('Receiver Operator Characteristic (ROC) Curve error as the model didn\'t converge.')
        
      } else {
        
        message('Receiver Operator Characteristic (ROC) Curve error:')
        stop(e)
        
      }
      
      return(NULL)
      
    }
    )
    
    # If chosen by the user, plot the roc_curve
    if (do.plot == TRUE){
      plot(roc_curve)
    }
    
    # Count the convergence warnings
    
    conv_warns = as.integer(table(converged)['No'])
    
    if (is.na(conv_warns)){
      
      conv_warns = 0
      
    }
    
    # If both conf_mat and roc_curve are not NULL
    # .. Return 
    # .... AUC, CI, Kappa, Sensitivity, Specificity
    # .... Pos Pred Value, Neg Pred Value, Precision,
    # .... Recall, F1, Prevalence, Detection Rate,
    # .... Detection Prevalence, Balanced Accuracy,
    # .... number of folds, and the count of convergence warnings
    # Else
    # .. Return the same variables but with NA in all but 
    # .. folds and convergence warnings
    
    if (!(is.null(conf_mat)) && !(is.null(roc_curve))){
      return(c(c("AUC" = auc(roc_curve), "CI" = ci(roc_curve), 
                 (conf_mat$overall['Kappa'])), conf_mat$byClass, 
               "folds"=nfolds, "convergence_warnings" = conv_warns)) 
    } else {
      
      return(c(c("AUC" = NA, "CI1" = NA, "CI2" = NA, "CI3" = NA,
                 "Kappa" = NA, 'Sensitivity'=NA, 'Specificity'=NA, 'Pos Pred Value'=NA, 
                 "Neg Pred Value"=NA,"Precision"=NA,"Recall"=NA,"F1"=NA,"Prevalence"=NA,
                 "Detection Rate"=NA,"Detection Prevalence"=NA,"Balanced Accuracy"=NA), 
               "folds"=nfolds, "convergence_warnings" = conv_warns)) 
      
    }
    
  }
  
}


cross_validate_list = function(model_list, data, id_column, cat_column, 
                              nfolds=5, family='gaussian', REML=FALSE, 
                              cutoff=0.5, positive=1, do.plot=FALSE, seed = NULL, model_verbose=TRUE){
  

  # cross_validate() all the models using ldply()
  model_cvs_df = ldply(model_list,.fun = function(x) cross_validate(x, data, id_column, cat_column,
                                                   nfolds=nfolds, family=family, REML=REML,
                                                   cutoff=cutoff, positive=positive, do.plot=do.plot, seed = seed, model_verbose=model_verbose))
  
  
  # Now we want to take the model from the model_list and split it up into 
  # fixed effects and random effects
  # Some users might want to mix models with an without random effects,
  # and so we first try to seperate into fixed and random,
  # and if no random effects are found for any of the models,
  # we remove the column "random". 
  # Models without random effects will get NA in the random column.
  
 
  # First we create a dataframe with the list of models
  mixed_effects = data.frame("model" = as.character(model_list))
  
  suppressWarnings((
  # Then we use tidyr() to create a pipeline
    mixed_effects = mixed_effects %>%
    
    # First remove all whitespaces
    mutate(model = gsub("\\s", "", .$model)) %>% 
    
    # Seperate model into dependent variable and predictors
    separate(model, into = c("dependent", "predictors"), sep = "~") %>%
    
    # then we separate the model by "("  - because: diagnosis + (1|subject)
    # the first part is placed in a column called "fixed"
    # the other part in a column called "random"
    separate(predictors, into = c("fixed", "random"), sep = "\\(") %>%
    
    # then we clean up those strings a bit
    # from fixed we remove the last "+"
    # from random we remove the last ")"
    mutate(
           fixed = gsub("[+]$", "", fixed),
           random = substr(random,1,nchar(random)-1) 
           
    )))

  
  # If all models are without random effects, 
  # drop column random
  if (all(is.na(mixed_effects$random))){
    
    mixed_effects$random = NULL
    
  }
  
  # we put the two dataframes together and returns it
  return(cbind(model_cvs_df, mixed_effects))
  
}


########################
# Notes for development:
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
########################

