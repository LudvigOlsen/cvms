#'
#' #' @importFrom dplyr %>%
#' validate_single = function(train_data,
#'                            model,
#'                            test_data = NULL,
#'                            partitions_col = '.partitions',
#'                            family = 'gaussian',
#'                            link = NULL,
#'                            control= NULL,
#'                            REML = FALSE,
#'                            cutoff = 0.5,
#'                            positive = 1,
#'                            err_nc = FALSE,
#'                            model_verbose = FALSE) {
#'   # Set errors if input variables aren't what we expect / can handle
#'   # WORK ON THIS SECTION!
#'   stopifnot(
#'     !is.null(model),
#'     is.character(model),
#'     is.data.frame(train_data),
#'     is.data.frame(test_data) || is.null(test_data),
#'     positive %in% c(1, 2),
#'     family %in% c("gaussian", "binomial")
#'   )
#'   if (is.null(test_data)) {
#'     stopifnot(is.factor(train_data[[partitions_col]]))
#'   }
#'
#'   # Extract y_column from model
#'   y_column = extract_y(model)
#'
#'   # Check if there are random effects (yields a list e.g. (False, False, True))
#'   random_effects = rand_effects(model)
#'
#'   # Create tibble with the fixed and random effects for the output
#'   mixed_effects <- extract_model_effects(model)
#'
#'   # If link is NULL we pass it
#'   # the default link function for the family
#'   link <- default_link(link, family)
#'
#'   # If train and test data is not already split,
#'   # get train and test set
#'   if (is.null(test_data)) {
#'     # Create test set
#'     test_data = train_data[train_data[[partitions_col]] == 2,]
#'     # Create training set
#'     train_data = train_data[train_data[[partitions_col]] == 1,]
#'   }
#'
#'   # Loop through the folds
#'   # .. Create a test_set and a training_set
#'   # .. Train the model on the training_set
#'   # .. Test the model on the test_set
#'
#'
#'   # Train and test the model
#'
#'   if (family == 'gaussian') {
#'     # Call cv_gaussian_
#'     # Returns list with
#'     # .. a results dataframe
#'     # .. whether the model converged (yes / no)
#'     gaussian_ = cv_gaussian_(
#'       model = model,
#'       test_set = test_data,
#'       training_set = train_data,
#'       y_column = y_column,
#'       fold = 1,
#'       random_effects = random_effects,
#'       REML = REML,
#'       link = link,
#'       control=control,
#'       model_verbose = model_verbose
#'     )
#'
#'   } else if (family == 'binomial') {
#'     # Call cv_binomial_
#'     # Returns a list with
#'     # .. a dataframe with predictions and observations
#'     # .. whether the model converged (yes / no)
#'     binomial_ = cv_binomial_(
#'       model = model,
#'       test_set = test_data,
#'       training_set = train_data,
#'       y_column = y_column,
#'       fold = 1,
#'       random_effects = random_effects,
#'       family = family,
#'       link = link,
#'       control=control,
#'       REML = REML,
#'       model_verbose = model_verbose
#'     )
#'
#'   }
#'
#'   # Returning
#'
#'   if (family == 'gaussian') {
#'     # Extract model dataframe
#'     model_tidy <- gaussian_$model_tidy
#'
#'     # Extract model object
#'     model_object <- gaussian_$model
#'
#'     # Extract result dataframe
#'     results <- gaussian_$result
#'
#'     # Count the convergence warnings
#'
#'     conv_warns = as.integer(table(results$converged)['No'])
#'     if (is.na(conv_warns)) {
#'       conv_warns = 0
#'     }
#'
#'     # Stop if any conv warnings?
#'     if (isTRUE(err_nc) && conv_warns != 0) {
#'       stop("Model did not converge.")
#'     }
#'
#'     # Return a list with
#'     # .. the found RMSE, r2m, r2c, AIC, AICc, and BIC
#'     # .. the count of convergence warnings
#'     # .. list of plot objects
#'
#'     # Make results into a tibble
#'     results <- tibble::as_tibble(results)
#'     rownames(results) <- NULL
#'
#'     # Make models into a tibble
#'     model_tidy <- tibble::as_tibble(model_tidy)
#'     model_tidy <- mutate(model_tidy,
#'                          p.value = if (exists('p.value', where = model_tidy))
#'                            p.value
#'                          else
#'                            NA) %>%
#'       tidyr::nest() %>%
#'       dplyr::rename(coefficients = data)
#'
#'     output_results <- tibble::tibble(
#'       'RMSE' = results$RMSE,
#'       'r2m' = results$r2m,
#'       'r2c' = results$r2c,
#'       'AIC' = results$AIC,
#'       'AICc' = results$AICc,
#'       'BIC' = results$BIC,
#'       "Convergence Warnings" = as.integer(conv_warns),
#'       "Family" = family,
#'       "Link" = link,
#'       "Coefficients" = model_tidy$coefficients
#'     )
#'
#'     # Add mixed effects tibble
#'     output_results <-
#'       dplyr::bind_cols(output_results, mixed_effects)
#'
#'     return(list(
#'       "Results" = output_results,
#'       'Model' = model_object
#'     ))
#'
#'
#'   } else if (family == 'binomial') {
#'     # Extract model dataframe
#'     pred_obs = binomial_$predictions_and_observations
#'
#'     # Extract model object
#'     model_object = binomial_$model
#'
#'     # Extract converged list
#'     conv_list = binomial_$converged
#'
#'     # Count the convergence warnings
#'
#'     # Count how many 'No's are in conv_list
#'     conv_warns = as.integer(table(conv_list)['No'])
#'
#'     # If conv_warns is NA, it means there were no 'No's in conv_warns
#'     if (is.na(conv_warns)) {
#'       conv_warns = 0
#'     }
#'
#'     # Stop if any conv warnings?
#'     if (isTRUE(err_nc) && conv_warns != 0) {
#'       stop("Model did not converge.")
#'     }
#'
#'     # Find the levels in the categorical dependent variable
#'     cat_levels = c(as.character(levels(factor(train_data[[y_column]]))[1]),
#'                    as.character(levels(factor(train_data[[y_column]]))[2]))
#'
#'     # Create a column with the predicted class based on the chosen cutoff
#'     pred_obs = pred_obs %>%
#'       dplyr::mutate(predicted_class = ifelse(.[[1]] < cutoff, cat_levels[1],
#'                                              cat_levels[2])) # e.g. "ASD", "TD"
#'
#'     # Try to use a confusion matrix with the data
#'     # This will fail if one of the folds didn't converge
#'     conf_mat = tryCatch({
#'       caret::confusionMatrix(factor(pred_obs$predicted_class, levels=c(0,1)),
#'                              factor(pred_obs$y_column, levels=c(0,1)),
#'                              positive = cat_levels[positive])
#'
#'     }, error = function(e) {
#'       # If there are any NAs in the prediction, the model didn't converge
#'       # .. message the user and return NULL
#'       # else stop the execution of validate()
#'
#'       if (sum(is.na(pred_obs$prediction)) != 0) {
#'         warning('Confusion Matrix error as the model didn\'t converge.')
#'
#'       } else {
#'         stop(paste0('Confusion matrix error: ', e))
#'
#'       }
#'
#'       return(NULL)
#'
#'     })
#'
#'     # Try to fit a ROC curve on the data
#'     # This will fail if the model didn't converge
#'
#'     roc_curve = tryCatch({
#'       pROC::roc(response =  pred_obs$y_column,
#'                 predictor =  pred_obs$prediction)
#'
#'     }, error = function(e) {
#'       # If there are any NAs in the prediction, the model didn't converge
#'       # .. message the user and return NULL
#'       # else stop the execution of cross_validate()
#'
#'       if (sum(is.na(pred_obs$prediction)) != 0) {
#'         warning(
#'           'Receiver Operator Characteristic (ROC) Curve error as the model didn\'t converge.'
#'         )
#'
#'       } else {
#'         stop(paste0('Receiver Operator Characteristic (ROC) Curve error: ', e))
#'
#'       }
#'
#'       return(NULL)
#'
#'     })
#'
#'     # If both conf_mat and roc_curve are not NULL
#'     # .. Return
#'     # .... AUC, CI, Kappa, Sensitivity, Specificity
#'     # .... Pos Pred Value, Neg Pred Value, Precision,
#'     # .... Recall, F1, Prevalence, Detection Rate,
#'     # .... Detection Prevalence, Balanced Accuracy,
#'     # .... the count of convergence warnings,
#'     # .... and the model object
#'     # Else
#'     # .. Return the same variables but with NA in all but
#'     # .. folds and convergence warnings
#'
#'     if (!(is.null(conf_mat)) && !(is.null(roc_curve))) {
#'       # Predictions and targets
#'       predictions_ <- pred_obs %>%
#'         dplyr::rename(target = y_column) %>%
#'         dplyr::select(prediction, predicted_class, target) %>%
#'         tibble::as_tibble() %>%
#'         tidyr::nest(1:3) %>%
#'         dplyr::rename(predictions = data)
#'
#'       # ROC sensitivities and specificities
#'       roc_ <- tibble::tibble(
#'         sensitivities = roc_curve$sensitivities,
#'         specificities = roc_curve$specificities
#'       ) %>%
#'         tidyr::nest(1:2) %>%
#'         dplyr::rename(roc = data)
#'
#'       output_results <- tibble::tibble(
#'         "AUC" = pROC::auc(roc_curve)[1],
#'         "Lower CI" = pROC::ci(roc_curve)[1],
#'         "Upper CI" = pROC::ci(roc_curve)[3],
#'         "Kappa" = unname(conf_mat$overall['Kappa']),
#'         "Sensitivity" = unname(conf_mat$byClass['Sensitivity']),
#'         'Specificity' = unname(conf_mat$byClass['Specificity']),
#'         'Pos Pred Value' = unname(conf_mat$byClass['Pos Pred Value']),
#'         'Neg Pred Value' = unname(conf_mat$byClass['Neg Pred Value']),
#'         'F1' = unname(conf_mat$byClass['F1']),
#'         'Prevalence' = unname(conf_mat$byClass['Prevalence']),
#'         'Detection Rate' = unname(conf_mat$byClass['Detection Rate']),
#'         'Detection Prevalence' = unname(conf_mat$byClass['Detection Prevalence']),
#'         'Balanced Accuracy' = unname(conf_mat$byClass['Balanced Accuracy']),
#'         "Convergence Warnings" = conv_warns,
#'         "Family" = family,
#'         "Link" = link,
#'         "Predictions" = predictions_$predictions,
#'         "ROC" = roc_$roc
#'       )
#'
#'       # Add mixed effects tibble
#'       output_results <-
#'         dplyr::bind_cols(output_results, mixed_effects)
#'
#'       return(list("Results" = output_results,
#'                   "Model" = model_object))
#'
#'     } else {
#'       output_results <- tibble::tibble(
#'         "AUC" = NA,
#'         "Lower CI" = NA,
#'         "Upper CI" = NA,
#'         "Kappa" = NA,
#'         'Sensitivity' = NA,
#'         'Specificity' = NA,
#'         'Pos Pred Value' = NA,
#'         "Neg Pred Value" = NA,
#'         "F1" = NA,
#'         "Prevalence" = NA,
#'         "Detection Rate" = NA,
#'         "Detection Prevalence" = NA,
#'         "Balanced Accuracy" = NA,
#'         "Convergence Warnings" = conv_warns,
#'         "Family" = family,
#'         "Link" = link,
#'         "Predictions" = NA,
#'         "ROC" = NA
#'       )
#'
#'       # Add mixed effects tibble
#'       output_results <-
#'         dplyr::bind_cols(output_results, mixed_effects)
#'
#'       return(list("Results" = output_results, "Model" = model_object))
#'     }
#'   }
#' }
