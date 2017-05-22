
#' @title Wrapper for plotting common plots using ggplot2
#' @description Creates various types of plots based
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
#' @export
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

      ROC_plot <- data.frame(y=x$ROC[[1]]$sensitivities, x=x$ROC[[1]]$specificities) %>%
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
