
#' Plot ROC Curve
#'
#' This function plots the ROC curve.
#'
#' @param Target A vector with values of 0/1 to be predicted.
#' @param Prediction A vector of prediction or covariate used to predict the target.
#' @param ... Other parameters for plot().
#' @export
#' @examples PlotROC(mtcars[, am], mtcars[, mpg])

PlotROC <- function(Target, Prediction, type = "l", pch = 16, ...){

  if(is.numeric(Prediction)){

    Results <- CalculateAUC(Target, Prediction)

  } else if(is.factor(Prediction) | is.character(Prediction)){

    Results <- CalculateFactorAUC(Target, Prediction)

  }

    plot(Results[, FP], Results[, TP],
         xlab = "False Positive Rate", ylab = "True Positive Rate",
         type = type, pch = pch, ... = ...)

}
