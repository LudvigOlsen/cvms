# R CMD check NOTE handling
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

# Metrics

calculate_RMSE <- function(predictions, targets, raise_errors = FALSE){
  tryCatch({
    rmse(predictions, targets)
  }, error = function(e){
    if (raise_errors) stop(e)
    else warning(e)
    return(NA)
  })
}

calculate_MAE <- function(predictions, targets, raise_errors = FALSE){
  tryCatch({
    mae(predictions, targets)
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

mae <- function(predictions, targets, na.rm=TRUE){

  if (!(is.numeric(predictions) || is.integer(predictions))){
    stop("'predictions' must be numeric")
  }
  if (!(is.numeric(targets) || is.integer(targets))){
    stop("'predictions' must be numeric")
  }
  if (length(predictions) != length(targets)){
    stop("predictions and targets must have same length")
  }

  mean(abs(targets - predictions), na.rm = na.rm)

}

rmse <- function(predictions, targets, na.rm=TRUE){

  if (!(is.numeric(predictions) || is.integer(predictions))){
    stop("'predictions' must be numeric")
  }
  if (!(is.numeric(targets) || is.integer(targets))){
    stop("'predictions' must be numeric")
  }
  if (length(predictions) != length(targets)){
    stop("predictions and targets must have same length")
  }

  sqrt(mean((targets - predictions)^2, na.rm = na.rm))

}

