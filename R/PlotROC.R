
#' Plot ROC Curve
#'
#' This function plots the ROC curve.
#'
#' @param Target A vector with values of 0/1 to be predicted.
#' @param Prediction A vector of prediction or covariate used to predict the target.
#' @param ... Other parameters for plot().
#' @export
#' @examples PlotROC(mtcars$am, mtcars$mpg)

PlotROC <- function(Target, Prediction, ...){
  if(length(Target) != length(Prediction)){
    stop("Target and Prediction must be of the same length.")
  } else{
    if(is.numeric(Prediction) == T){
      Order <- order(Prediction, decreasing = T)
      Order <- Order[!Order %in% which(is.na(Prediction))]
      Prediction <- Prediction[Order]
      Target <- Target[Order]

      FP <- cumsum(!Target)/sum(!Target)
      TP <- cumsum(Target)/sum(Target)

      Repeated <- c(Prediction[-1] == Prediction[-length(Prediction)], FALSE)

      FP <- c(0, FP[!Repeated], 1)
      TP <- c(0, TP[!Repeated], 1)

    } else if(is.factor(Prediction) == T | is.character(Prediction) == T){

      DT1 <- data.table(Y = Target, X = Prediction)
      DT1 <- DT1[, list(Event = sum(Y),
                        `Non Event` = sum(!Y)),
                 by = X]
      DT1[, Probability := Event / (Event + `Non Event`)]
      DT1 <- DT1[order(Probability), ]

      FP <- c(0, cumsum(DT1[, `Non Event`])/ sum(DT1[, `Non Event`]), 1)
      TP <- c(0, cumsum(DT1[, Event])/ sum(DT1[, Event]), 1)

    } else{
      stop("The class of Prediction should be eiher numeric or factor.")
    }
    N <- length(FP)
    TpAvg <- (TP[-1] + TP[-N])/ 2
    FpDif <- FP[-1] - FP[-N]
    Area <- sum(TpAvg * FpDif)

    if(Area < 0.5){
      TP <- 1 - TP
      FP <- 1 - FP
    }

    plot(FP, TP, xlab = "False Positive Rate", ylab = "True Positive Rate",
         type = "l", pch = 16, ... = ...)
  }
}
