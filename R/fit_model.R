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
