# Metrics

calculate_RMSE <- function(predictions, targets, raise_errors = FALSE){
  tryCatch({
    hydroGOF::rmse(predictions, targets)
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}


calculate_r2m <- function(model, raise_errors = FALSE){
  tryCatch({
    suppressWarnings(MuMIn::r.squaredGLMM(model)[1])
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}


calculate_r2c <- function(model, raise_errors = FALSE){
  tryCatch({
    suppressWarnings(MuMIn::r.squaredGLMM(model)[2])
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}


calculate_AIC <- function(model, raise_errors = FALSE){
  tryCatch({
    stats::AIC(model)
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}


calculate_AICc <- function(model, REML, raise_errors = FALSE){
  tryCatch({
    AICcmodavg::AICc(model, return.K = REML)
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}


calculate_BIC <- function(model, raise_errors = FALSE){
  tryCatch({
    stats::BIC(model)
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}

