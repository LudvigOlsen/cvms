

cross_validate = function(model, data, id_column, cat_column=NULL,
                          nfolds=5, family='gaussian', REML=FALSE,
                          cutoff=0.5, positive=1, do.plot=FALSE,
                          which_plot = "all", plot_theme=theme_bw(),
                          seed = NULL, model_verbose=TRUE){
  # model: ("y~a+b+(1|c)")
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

        gg = ggplot2::ggplot(gaussian_fold_models, aes(term, estimate))
        print(gg +
                ggplot2::geom_boxplot() +
                ggplot2::labs(x = 'Fixed Effects', y = 'Estimate') +
                ggplot2::theme_bw())

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
      'AICc' = mean(na.omit(gaussian_return$AICc)),
      'BIC' = mean(na.omit(gaussian_return$BIC)),
      "Folds"=nfolds,
      "Convergence Warnings" = as.integer(conv_warns)
    ))


  } else if (family == 'binomial'){

    # Find the levels in the categorical dependent variable
    cat_levels = c(as.character(levels(data[[y_column]])[1]), as.character(levels(data[[y_column]])[2]))

    # Create a new dataframe based on the dataframe with predictions and observations
    # Create a column with the predicted class based on the chosen cutoff
    binomial_pred_obs_class = binomial_predictions_and_observations %>%
      dplyr::mutate(predicted_class = ifelse(.[[1]] < cutoff, cat_levels[1],
                                             cat_levels[2])) # e.g. "ASD", "TD"

    # Try to use a confusion matrix with the data
    # This will fail if one of the folds didn't converge
    conf_mat = tryCatch({
      caret::confusionMatrix(binomial_pred_obs_class$predicted_class,
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
      pROC::roc(response =  binomial_pred_obs_class$y_column, predictor =  binomial_pred_obs_class$prediction)

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

      return(c(c("AUC" = pROC::auc(roc_curve), "Lower CI" = pROC::ci(roc_curve)[1],
                 "Upper CI" = pROC::ci(roc_curve)[3],
                 (conf_mat$overall['Kappa'])), conf_mat$byClass[-5:-6],
               "Folds"=nfolds, "Convergence Warnings " = conv_warns))
    } else {

      return(c(c("AUC" = NA, "Lower CI" = NA, "Upper CI" = NA,
                 "Kappa" = NA, 'Sensitivity'=NA, 'Specificity'=NA,
                 'Pos Pred Value'=NA,"Neg Pred Value"=NA,"F1"=NA,
                 "Prevalence"=NA,"Detection Rate"=NA,
                 "Detection Prevalence"=NA,"Balanced Accuracy"=NA),
                 "Folds"=nfolds, "Convergence Warnings " = conv_warns))

    }

  }

}

