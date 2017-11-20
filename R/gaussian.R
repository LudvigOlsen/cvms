#library(hydroGOF)

#' @import hydroGOF
cv_gaussian_ = function(model, test_set, training_set, y_column,
                        fold, random_effects, REML, link, model_verbose){

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
  model_temp = create_model_(model = model,
                             model_type = model_type,
                             training_set = training_set,
                             family = 'gaussian',
                             REML = REML,
                             link = link,
                             fold = fold,
                             model_verbose = model_verbose)

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
    AICc = NA
    BIC = NA
    converged = "No"
    model_temp_tidied = data.frame('term'=NA, 'estimate'=NA, 'std.error'=NA, 'statistic'=NA)

  } else {

    # Predict the dependent variable in the test_set from model_temp
    predict_temp = stats::predict(model_temp, test_set, allow.new.levels=TRUE)

    # Find the values rmse, r2m, r2c, AIC, and BIC
    rmse <- tryCatch({
      hydroGOF::rmse(predict_temp, test_set[[y_column]])
    }, error = function(e){
      return(NA)
    })

    r2m <- tryCatch({
      MuMIn::r.squaredGLMM(model_temp)[1]
    }, error = function(e){
      return(NA)
    })

    r2c <- tryCatch({
      MuMIn::r.squaredGLMM(model_temp)[2]
    }, error = function(e){
      return(NA)
    })

    AIC <- tryCatch({
      stats::AIC(model_temp)
    }, error = function(e){
      return(NA)
    })

    AICc <- tryCatch({
      AICcmodavg::AICc(model_temp, return.K = REML)
    }, error = function(e){
      return(NA)
    })

    BIC <- tryCatch({
      stats::BIC(model_temp)
    }, error = function(e){
      return(NA)
    })

    converged = "Yes"
    model_temp_tidied = broom::tidy(model_temp, effects = c("fixed"))

  }



  # Return a list with
  # .. a tidied summary of the model
  # .. the model object
  # .. a dataframe with the found values
  return(list(
    'model_tidy' = cbind(model_temp_tidied, fold),
    'model' = model_temp,
    'result' = data.frame('RMSE' = rmse, 'r2m' = r2m, 'r2c' = r2c,
                          'AIC' = AIC, 'AICc' = AICc, 'BIC' = BIC,
                          'converged'= converged, fold)
  ))

}

