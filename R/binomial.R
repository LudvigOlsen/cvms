

cv_binomial_ = function(model, test_set, training_set, y_column, fold,
                        random_effects, family, link, control, REML, model_verbose){

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


  model_temp = create_model_(model = model,
                             model_type = model_type,
                             training_set = training_set,
                             family = 'binomial',
                             link = link,
                             control=control,
                             REML = REML,
                             fold = fold,
                             model_verbose = model_verbose)


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
    predict_temp = list(rep(NA, length(test_set[[y_column]])))

    # Create a dataframe with the NA predictions and the observations
    predictions_and_y_Temp = data.frame("prediction" = predict_temp[[1]], "y_column" = test_set[[y_column]])

    # Set converged to no
    converged = "No"

  } else {

    # Predict the dependent variable in the test_set from the fitted model
    predict_temp = stats::predict(model_temp, test_set, type="response", allow.new.levels=TRUE)

    # Create a dataframe with predictions and observations side by side
    predictions_and_y_Temp = data.frame("prediction" = predict_temp, y_column = test_set[[ y_column]])

    # Set converged to yes
    converged = "Yes"

  }

  # Return a list with
  # .. the dataframe containing predictions and observations
  # .. the converged variable
  return(list(predictions_and_observations=predictions_and_y_Temp, converged=converged, model=model_temp))


}
