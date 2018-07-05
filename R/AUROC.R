
#' Area Under the ROC Curve
#'
#' This function computes the AUC, and returns the numeric value of it.
#'
#' @param Target A vector with values of 0/1 to be predicted.
#' @param Prediction A vector of prediction or covariate used to predict the target.
#' @export
#' @examples AUROC(mtcars$am, mtcars$mpg)

AUROC <- function(Target, Prediction){
  if(length(Target) != length(Prediction)){
    stop("Target and Prediction must be of the same length.")
  } else{
    if(is.numeric(Prediction) == T){
      Order <- order(Prediction)
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
    Results <- 0.5 + abs(Area - 0.5)
    return(Results)
  }
}
