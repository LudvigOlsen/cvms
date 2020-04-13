
# Metrics

calculate_r2m <- function(model, raise_errors = FALSE) {
  model_metric_wrapper(model, metric_fn = function(model_) {
    tryCatch(
      {
        MuMIn::r.squaredGLMM(model_)[1]
      },
      warning = function(w) {
        if (grepl("now calculates a revised statistic",
          as.character(w),
          ignore.case = TRUE
        )) {
          return(MuMIn::r.squaredGLMM(model_)[1])
        } else {
          warning(w)
          return(MuMIn::r.squaredGLMM(model_)[1])
        }
      }
    )
  }, raise_errors = raise_errors)
}

calculate_r2c <- function(model, raise_errors = FALSE) {
  model_metric_wrapper(model, metric_fn = function(model_) {
    tryCatch(
      {
        MuMIn::r.squaredGLMM(model_)[2]
      },
      warning = function(w) {
        if (grepl("now calculates a revised statistic",
          as.character(w),
          ignore.case = TRUE
        )) {
          return(MuMIn::r.squaredGLMM(model_)[2])
        } else {
          warning(w)
          return(MuMIn::r.squaredGLMM(model_)[2])
        }
      }
    )
  }, raise_errors = raise_errors)
}

calculate_AIC <- function(model, raise_errors = FALSE) {
  model_metric_wrapper(model, metric_fn = function(model_) {
    stats::AIC(model_)
  }, raise_errors = raise_errors)
}

calculate_AICc <- function(model, REML, raise_errors = FALSE) {
  model_metric_wrapper(model, metric_fn = function(model_) {

    # When fitting a glm (e.g. with different link function)
    # we shouldn't pass REML to AICc
    tryCatch(
      {
        MuMIn::AICc(object = model_, REML = REML)
      },
      warning = function(w) {
        if (grepl("extra arguments discarded",
          as.character(w),
          ignore.case = TRUE
        )) {
          return(MuMIn::AICc(object = model_))
        } else {
          warning(w)
          return(MuMIn::AICc(object = model_, REML = REML))
        }
      }
    )
  }, raise_errors = raise_errors)
}

calculate_BIC <- function(model, raise_errors = FALSE) {
  model_metric_wrapper(model, metric_fn = function(model_) {
    stats::BIC(model_)
  }, raise_errors = raise_errors)
}

# E.g.:
# model_metric_wrapper(model, metric_fn = function(model){
#     MuMIn::r.squaredGLMM(model)[1]})
model_metric_wrapper <- function(model,
                                 metric_fn = function(model_) {
                                   NA
                                 },
                                 raise_errors = FALSE) {
  tryCatch(
    {
      metric_fn(model)
    },
    warning = function(w) {
      warning(w)
      return(NA)
    },
    error = function(e) {
      if (raise_errors) stop(e)
      if (grepl("no applicable method for", as.character(e), ignore.case = T)) {
        return(NA)
      }
      warning(e)
      return(NA)
    }
  )
}
