
#' @title Wrapper for plotting common plots using ggplot2
#' @description Creates various plots based on
#'  the output of cvms::\link{cross_validate}()
#' @param x Object returned by cvms::\link{cross_validate}() (tbl)
#' @param type Type of plot.
#'
#'  \subsection{Gaussian}{
#'
#'  'RMSE' - boxplot
#'
#'  'r2' - boxplot
#'
#'  'IC' - boxplot
#'
#'  'coefficients' - boxplot
#'  }
#'
#'  \subsection{Binomial}{
#'
#'  "ROC" - ROC curve
#'
#'  }
#' @author Ludvig Renbo Olsen, \email{r-pkgs@ludvigolsen.dk}
#' @export
#' @examples
#' # Attach packages
#' library(cvms)
#' library(groupdata2) # fold()
#'
#' # Load data (included in cvms)
#' data <- participant.scores
#'
#' # Fold data
#' data <- fold(data, k = 4,
#'              cat_col = 'diagnosis',
#'              id_col = 'participant')
#'
#' # Cross-validate a gaussian model
#' CVgauss <- cross_validate(data,
#'                           "score~diagnosis",
#'                           family='gaussian')
#'
#' # Plot results for gaussian model
#' cv_plot(CVgauss, type = 'RMSE')
#' cv_plot(CVgauss, type = 'r2')
#' cv_plot(CVgauss, type = 'IC')
#' cv_plot(CVgauss, type = 'coefficients')
#'
#' # Cross-validate a binomial model
#' CVbinom <- cross_validate(data,
#'                           "diagnosis~score",
#'                           family='binomial')
#'
#' # Plot results for binomial model
#' cv_plot(CVbinom, type = 'ROC')
cv_plot <- function(x, type){

  if (x$Family == 'gaussian'){

    if (type == 'RMSE'){

      x_ <- data.frame("RMSE" = x$Results[[1]]$RMSE)

      return(create_boxplot_(x_, 1))

    }

    if (type == 'r2'){

      x_ <- data.frame("r2m" = x$Results[[1]]$r2m,
                       "r2c" = x$Results[[1]]$r2c)

      return(create_boxplot_(x_, 1, 2))

    }

    if (type == 'IC'){

      x_ <- data.frame("AIC" = x$Results[[1]]$AIC,
                       "AICc" = x$Results[[1]]$AICc,
                       "BIC" = x$Results[[1]]$BIC)

      return(create_boxplot_(x_, 1, 3))

    }

    if (type == 'coefficients'){

      return(ggplot2::ggplot(x$Coefficients[[1]], ggplot2::aes(term, estimate)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(x = 'Fixed Effects', y = 'Estimate'))

    }

  } else if (x$Family == 'binomial') {

    if (type == 'ROC'){

      ROC_plot <- data.frame(y=x$ROC[[1]]$Sensitivities, x=x$ROC[[1]]$Specificities) %>%
        ggplot2::ggplot(ggplot2::aes(x, y)) +
        ggplot2::geom_line() +
        ggplot2::scale_x_reverse() +
        ggplot2::geom_abline(intercept=1, slope=1, linetype="dashed") +
        ggplot2::xlab("Specificity") +
        ggplot2::ylab("Sensitivity")

      return(ROC_plot)

    }
  }


}
