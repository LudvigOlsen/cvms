# Metrics

RMSE <- function(predictions, targets, raise_errors = FALSE){
  tryCatch({
    hydroGOF::rmse(predictions, targets)
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}


r2m <- function(model, raise_errors = FALSE){
  tryCatch({
    suppressWarnings(MuMIn::r.squaredGLMM(model)[1])
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}


r2c <- function(model, raise_errors = FALSE){
  tryCatch({
    suppressWarnings(MuMIn::r.squaredGLMM(model)[2])
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}


AIC <- function(model, raise_errors = FALSE){
  tryCatch({
    stats::AIC(model)
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}


AICc <- function(model, REML, raise_errors = FALSE){
  tryCatch({
    AICcmodavg::AICc(model, return.K = REML)
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}


BIC <- function(model, raise_errors = FALSE){
  tryCatch({
    stats::BIC(model)
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}

