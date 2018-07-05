
#' Logarithmic Loss
#'
#' This function computes the logarithmic loss for binary predictions.
#'
#' @param Target A vector of the binary dependent variable.
#' @param Prediction A vector of the predictions.
#' @export
#' @examples LogLoss(Data[, Y], Model$fitted.values)

LogLoss <- function(Target, Prediction){
  AdjPrediction <- pmin(pmax(Prediction, 1e-99), 1 - 1e-99)
  -sum(Target * log(AdjPrediction) +
         (1 - Target) * log(1 - AdjPrediction))/
    length(Target)
}
