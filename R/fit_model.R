fit_model_ = function(model, model_type, training_set, family, link, REML, model_verbose){

  # Checks the model_type and fits the model on the training_set

  if (model_type == 'lm'){

    if (model_verbose == TRUE){
      if (is.null(link) || link == 'identity'){
        print('Used lm()')
      } else {
        print('Used glm()')
      }
    }

    # Fit the model using lm() or glm() depending on link function
    # Return this model to model_temp
    if (is.null(link) || link == 'identity'){
      return(lm(model,training_set))
    } else {
      return(glm(model, training_set,
                 family = gaussian(link=link)))
    }

  } else if (model_type == 'lmer'){

    if (model_verbose == TRUE){
      if (is.null(link) || link == 'identity'){
        print('Used lme4::lmer()')
      } else {
        print('Used lme4::glmer()')
      }
    }

    # Fit the model using lmer() or glmer() depending on link function
    if (is.null(link) || link == 'identity'){
      return(lme4::lmer(model,training_set, REML=REML))
    } else {
      return(lme4::glmer(model, training_set,
                         family = gaussian(link=link)))
    }

  } else if (model_type == 'glm'){

    if (model_verbose == TRUE){
      print('Used glm()')}

    # Fit the model using glm()
    # Return this model to model_temp
    if (!is.null(link)){
      return(glm(model, training_set, family = binomial(link=link)))
    } else {
      return(glm(model, training_set, family = family))
    }

  } else if (model_type == 'glmer'){

    if (model_verbose == TRUE){
      print('Used lme4::glmer()')}

    # Fit the model using glmer()
    # Return this model to model_temp
    if (!is.null(link)){
      return(lme4::glmer(model, training_set, family = binomial(link=link)))
    } else {
      return(lme4::glmer(model, training_set, family = family))
    }

  }

}
