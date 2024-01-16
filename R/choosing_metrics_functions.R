


#   __________________ #< f29c509f0bd4fbc8cd8ea8271ecb30ce ># __________________
#   Specifying metrics list                                                 ####


##  .................. #< 382da835dd735cd1729cbe7d42f038d9 ># ..................
##  Specifying Gaussian metrics                                             ####


#' @title Select metrics for Gaussian evaluation
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Enable/disable metrics for Gaussian evaluation. Can be supplied to the
#'  \code{`metrics`} argument in many of the \code{cvms} functions.
#'
#'  Note: Some functions may have slightly different defaults than the ones supplied here.
#' @param all Enable/disable all arguments at once. (Logical)
#'
#'  Specifying other metrics will overwrite this, why you can
#'  use (\code{all = FALSE, rmse = TRUE}) to get only the \code{RMSE} metric.
#' @param rmse \code{RMSE}. (Default: TRUE)
#'
#'  Root Mean Square Error.
#' @param mae \code{MAE}. (Default: TRUE)
#'
#'  Mean Absolute Error.
#' @param nrmse_rng \code{NRMSE(RNG)}. (Default: FALSE)
#'
#'  Normalized Root Mean Square Error (by target range).
#' @param nrmse_iqr \code{NRMSE(IQR)}. (Default: TRUE)
#'
#'  Normalized Root Mean Square Error (by target interquartile range).
#' @param nrmse_std \code{NRMSE(STD)}. (Default: FALSE)
#'
#'  Normalized Root Mean Square Error (by target standard deviation).
#' @param nrmse_avg \code{NRMSE(AVG)}. (Default: FALSE)
#'
#'  Normalized Root Mean Square Error (by target mean).
#' @param rmsle \code{RMSLE}. (Default: TRUE)
#'
#'  Root Mean Square Log Error.
#' @param male \code{MALE}. (Default: FALSE)
#'
#'  Mean Absolute Log Error.
#' @param rae \code{RAE}. (Default: TRUE)
#'
#'  Relative Absolute Error.
#' @param rse \code{RSE}. (Default: FALSE)
#'
#'  Relative Squared Error.
#' @param rrse \code{RRSE}. (Default: TRUE)
#'
#'  Root Relative Squared Error.
#' @param mape \code{MAPE}. (Default: FALSE)
#'
#'  Mean Absolute Percentage Error.
#' @param mse \code{MSE}. (Default: FALSE)
#'
#'  Mean Square Error.
#' @param tae \code{TAE}. (Default: FALSE)
#'
#'  Total Absolute Error
#' @param tse \code{TSE}. (Default: FALSE)
#'
#'  Total Squared Error.
#' @param r2m \code{r2m}. (Default: FALSE)
#'
#'  Marginal R-squared.
#' @param r2c \code{r2c}. (Default: FALSE)
#'
#'  Conditional R-squared.
#' @param aic \code{AIC}. (Default: FALSE)
#'
#'  Akaike Information Criterion.
#' @param aicc \code{AICc}. (Default: FALSE)
#'
#'  Corrected Akaike Information Criterion.
#' @param bic \code{BIC}. (Default: FALSE)
#'
#'  Bayesian Information Criterion.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family evaluation functions
#' @examples
#' \donttest{
#' # Attach packages
#' library(cvms)
#'
#' # Enable only RMSE
#' gaussian_metrics(all = FALSE, rmse = TRUE)
#'
#' # Enable all but RMSE
#' gaussian_metrics(all = TRUE, rmse = FALSE)
#'
#' # Disable RMSE
#' gaussian_metrics(rmse = FALSE)
#' }
gaussian_metrics <- function(all = NULL,
                             rmse = NULL,
                             mae = NULL,
                             nrmse_rng = NULL,
                             nrmse_iqr = NULL,
                             nrmse_std = NULL,
                             nrmse_avg = NULL,
                             rae = NULL,
                             rse = NULL,
                             rrse = NULL,
                             rmsle = NULL,
                             male = NULL,
                             mape = NULL,
                             mse = NULL,
                             tae = NULL,
                             tse = NULL,
                             r2m = NULL,
                             r2c = NULL,
                             aic = NULL,
                             aicc = NULL,
                             bic = NULL) {
  # Check arguments ####
  aapply(
    checkmate::assert_flag,
    . ~ all + rmse + mae + nrmse_rng + nrmse_iqr + nrmse_std + nrmse_avg + rmsle + male +
      rae + rse + rrse + mape + mse + tae + tse + r2m + r2c + aic + aicc + bic,
    null.ok = TRUE
  )
  # End of argument checks ####

  list(
    "all" = all,
    "RMSE" = rmse,
    "MAE" = mae,
    "NRMSE(RNG)" = nrmse_rng,
    "NRMSE(IQR)" = nrmse_iqr,
    "NRMSE(STD)" = nrmse_std,
    "NRMSE(AVG)" = nrmse_avg,
    "RMSLE" = rmsle,
    "MALE" = male,
    "RAE" = rae,
    "RSE" = rse,
    "RRSE" = rrse,
    "MAPE" = mape,
    "MSE" = mse,
    "TAE" = tae,
    "TSE" = tse,
    "r2m" = r2m,
    "r2c" = r2c,
    "AIC" = aic,
    "AICc" = aicc,
    "BIC" = bic
  ) %>%
    plyr::compact()

}


##  .................. #< 68d2985c039d36fc7ee25b3faa3bc654 ># ..................
##  Specifying binomial metrics                                             ####


#' @title Select metrics for binomial evaluation
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Enable/disable metrics for binomial evaluation. Can be supplied to the
#'  \code{`metrics`} argument in many of the \code{cvms} functions.
#'
#'  Note: Some functions may have slightly different defaults than the ones supplied here.
#' @param all Enable/disable all arguments at once. (Logical)
#'
#'  Specifying other metrics will overwrite this, why you can
#'  use (\code{all = FALSE, accuracy = TRUE}) to get only the Accuracy metric.
#' @param balanced_accuracy \code{Balanced Accuracy} (Default: TRUE)
#' @param accuracy \code{Accuracy} (Default: FALSE)
#' @param f1 \code{F1} (Default: TRUE)
#' @param sensitivity \code{Sensitivity} (Default: TRUE)
#' @param specificity \code{Specificity} (Default: TRUE)
#' @param pos_pred_value \code{Pos Pred Value} (Default: TRUE)
#' @param neg_pred_value \code{Neg Pred Value} (Default: TRUE)
#' @param auc \code{AUC} (Default: TRUE)
#' @param lower_ci \code{Lower CI} (Default: TRUE)
#' @param upper_ci \code{Upper CI} (Default: TRUE)
#' @param kappa \code{Kappa} (Default: TRUE)
#' @param mcc \code{MCC} (Default: TRUE)
#' @param detection_rate \code{Detection Rate} (Default: TRUE)
#' @param detection_prevalence \code{Detection Prevalence} (Default: TRUE)
#' @param prevalence \code{Prevalence} (Default: TRUE)
#' @param false_neg_rate \code{False Neg Rate} (Default: FALSE)
#' @param false_pos_rate \code{False Pos Rate} (Default: FALSE)
#' @param false_discovery_rate \code{False Discovery Rate} (Default: FALSE)
#' @param false_omission_rate \code{False Omission Rate} (Default: FALSE)
#' @param threat_score \code{Threat Score} (Default: FALSE)
#' @param aic AIC. (Default: FALSE)
#' @param aicc AICc. (Default: FALSE)
#' @param bic BIC. (Default: FALSE)
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family evaluation functions
#' @examples
#' \donttest{
#' # Attach packages
#' library(cvms)
#'
#' # Enable only Balanced Accuracy
#' binomial_metrics(all = FALSE, balanced_accuracy = TRUE)
#'
#' # Enable all but Balanced Accuracy
#' binomial_metrics(all = TRUE, balanced_accuracy = FALSE)
#'
#' # Disable Balanced Accuracy
#' binomial_metrics(balanced_accuracy = FALSE)
#' }
binomial_metrics <- function(all = NULL,
                             balanced_accuracy = NULL,
                             accuracy = NULL,
                             f1 = NULL,
                             sensitivity = NULL,
                             specificity = NULL,
                             pos_pred_value = NULL,
                             neg_pred_value = NULL,
                             auc = NULL,
                             lower_ci = NULL,
                             upper_ci = NULL,
                             kappa = NULL,
                             mcc = NULL,
                             detection_rate = NULL,
                             detection_prevalence = NULL,
                             prevalence = NULL,
                             false_neg_rate = NULL,
                             false_pos_rate = NULL,
                             false_discovery_rate = NULL,
                             false_omission_rate = NULL,
                             threat_score = NULL,
                             aic = NULL,
                             aicc = NULL,
                             bic = NULL) {
  # Check arguments ####
  aapply(
    checkmate::assert_flag,
    . ~ all + balanced_accuracy + accuracy + f1 + sensitivity +
      specificity + pos_pred_value + neg_pred_value + auc +
      lower_ci + upper_ci + kappa + mcc + detection_rate +
      detection_prevalence + prevalence + false_neg_rate +
      false_pos_rate + false_discovery_rate + false_omission_rate +
      threat_score + aic + aicc + bic,
    null.ok = TRUE
  )
  # End of argument checks ####

  list(
    "all" = all,
    "Balanced Accuracy" = balanced_accuracy,
    "Accuracy" = accuracy,
    "F1" = f1,
    "Sensitivity" = sensitivity,
    "Specificity" = specificity,
    "Pos Pred Value" = pos_pred_value,
    "Neg Pred Value" = neg_pred_value,
    "AUC" = auc,
    "Lower CI" = lower_ci,
    "Upper CI" = upper_ci,
    "Kappa" = kappa,
    "MCC" = mcc,
    "Detection Rate" = detection_rate,
    "Detection Prevalence" = detection_prevalence,
    "Prevalence" = prevalence,
    "False Neg Rate" = false_neg_rate,
    "False Pos Rate" = false_pos_rate,
    "False Discovery Rate" = false_discovery_rate,
    "False Omission Rate" = false_omission_rate,
    "Threat Score" = threat_score,
    "AIC" = aic,
    "AICc" = aicc,
    "BIC" = bic
  ) %>%
    plyr::compact()

}


##  .................. #< 7891ac8d884014798acf6c83d9d63b6b ># ..................
##  Specifying multinomial metrics                                          ####


#' @title Select metrics for multinomial evaluation
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Enable/disable metrics for multinomial evaluation. Can be supplied to the
#'  \code{`metrics`} argument in many of the \code{cvms} functions.
#'
#'  Note: Some functions may have slightly different defaults than the ones supplied here.
#' @param all Enable/disable all arguments at once. (Logical)
#'
#'  Specifying other metrics will overwrite this, why you can
#'  use (\code{all = FALSE, accuracy = TRUE}) to get only the Accuracy metric.
#' @inheritParams binomial_metrics
#' @param overall_accuracy \code{Overall Accuracy} (Default: TRUE)
#' @param balanced_accuracy \code{Macro Balanced Accuracy} (Default: TRUE)
#' @param w_balanced_accuracy \code{Weighted Balanced Accuracy} (Default: FALSE)
#' @param w_accuracy \code{Weighted Accuracy} (Default: FALSE)
#' @param w_f1 \code{Weighted F1} (Default: FALSE)
#' @param w_sensitivity \code{Weighted Sensitivity} (Default: FALSE)
#' @param w_specificity \code{Weighted Specificity} (Default: FALSE)
#' @param w_pos_pred_value \code{Weighted Pos Pred Value} (Default: FALSE)
#' @param w_neg_pred_value \code{Weighted Neg Pred Value} (Default: FALSE)
#' @param auc \code{AUC} (Default: FALSE)
#' @param w_kappa \code{Weighted Kappa} (Default: FALSE)
#' @param mcc \code{MCC} (Default: TRUE)
#'
#'  Multiclass Matthews Correlation Coefficient.
#' @param w_detection_rate \code{Weighted Detection Rate} (Default: FALSE)
#' @param w_detection_prevalence \code{Weighted Detection Prevalence} (Default: FALSE)
#' @param w_prevalence \code{Weighted Prevalence} (Default: FALSE)
#' @param w_false_neg_rate \code{Weighted False Neg Rate} (Default: FALSE)
#' @param w_false_pos_rate \code{Weighted False Pos Rate} (Default: FALSE)
#' @param w_false_discovery_rate \code{Weighted False Discovery Rate} (Default: FALSE)
#' @param w_false_omission_rate \code{Weighted False Omission Rate} (Default: FALSE)
#' @param w_threat_score \code{Weighted Threat Score} (Default: FALSE)
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @family evaluation functions
#' @examples
#' \donttest{
#' # Attach packages
#' library(cvms)
#'
#' # Enable only Balanced Accuracy
#' multinomial_metrics(all = FALSE, balanced_accuracy = TRUE)
#'
#' # Enable all but Balanced Accuracy
#' multinomial_metrics(all = TRUE, balanced_accuracy = FALSE)
#'
#' # Disable Balanced Accuracy
#' multinomial_metrics(balanced_accuracy = FALSE)
#' }
multinomial_metrics <- function(all = NULL,
                                overall_accuracy = NULL,
                                balanced_accuracy = NULL,
                                w_balanced_accuracy = NULL,
                                accuracy = NULL,
                                w_accuracy = NULL,
                                f1 = NULL,
                                w_f1 = NULL,
                                sensitivity = NULL,
                                w_sensitivity = NULL,
                                specificity = NULL,
                                w_specificity = NULL,
                                pos_pred_value = NULL,
                                w_pos_pred_value = NULL,
                                neg_pred_value = NULL,
                                w_neg_pred_value = NULL,
                                auc = NULL,
                                kappa = NULL,
                                w_kappa = NULL,
                                mcc = NULL,
                                detection_rate = NULL,
                                w_detection_rate = NULL,
                                detection_prevalence = NULL,
                                w_detection_prevalence = NULL,
                                prevalence = NULL,
                                w_prevalence = NULL,
                                false_neg_rate = NULL,
                                w_false_neg_rate = NULL,
                                false_pos_rate = NULL,
                                w_false_pos_rate = NULL,
                                false_discovery_rate = NULL,
                                w_false_discovery_rate = NULL,
                                false_omission_rate = NULL,
                                w_false_omission_rate = NULL,
                                threat_score = NULL,
                                w_threat_score = NULL,
                                aic = NULL,
                                aicc = NULL,
                                bic = NULL) {
  # Check arguments ####
  aapply(
    checkmate::assert_flag,
    . ~ all + overall_accuracy + balanced_accuracy +
      w_balanced_accuracy + accuracy + w_accuracy + f1 + w_f1 +
      sensitivity + w_sensitivity + specificity + w_specificity +
      pos_pred_value + w_pos_pred_value + neg_pred_value +
      w_neg_pred_value + auc + kappa + w_kappa + mcc +
      detection_rate + w_detection_rate + detection_prevalence +
      w_detection_prevalence + prevalence + w_prevalence + false_neg_rate +
      w_false_neg_rate + false_pos_rate + w_false_pos_rate +
      false_discovery_rate + w_false_discovery_rate + false_omission_rate +
      w_false_omission_rate + threat_score + w_threat_score +
      aic + aicc + bic,
    null.ok = TRUE
  )
  # End of argument checks ####

  list(
    "all" = all,
    "Overall Accuracy" = overall_accuracy,
    "Balanced Accuracy" = balanced_accuracy,
    "Weighted Balanced Accuracy" = w_balanced_accuracy,
    "Accuracy" = accuracy,
    "Weighted Accuracy" = w_accuracy,
    "F1" = f1,
    "Weighted F1" = w_f1,
    "Sensitivity" = sensitivity,
    "Weighted Sensitivity" = w_sensitivity,
    "Specificity" = specificity,
    "Weighted Specificity" = w_specificity,
    "Pos Pred Value" = pos_pred_value,
    "Weighted Pos Pred Value" = w_pos_pred_value,
    "Neg Pred Value" = neg_pred_value,
    "Weighted Neg Pred Value" = w_neg_pred_value,
    "AUC" = auc,
    "Kappa" = kappa,
    "Weighted Kappa" = w_kappa,
    "MCC" = mcc,
    "Detection Rate" = detection_rate,
    "Weighted Detection Rate" = w_detection_rate,
    "Detection Prevalence" = detection_prevalence,
    "Weighted Detection Prevalence" = w_detection_prevalence,
    "Prevalence" = prevalence,
    "Weighted Prevalence" = w_prevalence,
    "False Neg Rate" = false_neg_rate,
    "Weighted False Neg Rate" = w_false_neg_rate,
    "False Pos Rate" = false_pos_rate,
    "Weighted False Pos Rate" = w_false_pos_rate,
    "False Discovery Rate" = false_discovery_rate,
    "Weighted False Discovery Rate" = w_false_discovery_rate,
    "False Omission Rate" = false_omission_rate,
    "Weighted False Omission Rate" = w_false_omission_rate,
    "Threat Score" = threat_score,
    "Weighted Threat Score" = w_threat_score,
    "AIC" = aic,
    "AICc" = aicc,
    "BIC" = bic
  ) %>%
    plyr::compact()

}
