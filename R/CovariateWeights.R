
#' Weights of Covariates
#'
#' This function takes the regression object and calculates the relative
#' importance of different covariates.
#' The relative importance is only valid when the covariates are standardized.
#'
#' @param Model A "lm" or "glm" object.
#' @param Rounding Desired rounding, default is up to 2 decimal places (i.e. 0.01%).
#' @param Exact If Exact = TRUE, the function will return weights with the sum of exactly
#' 100. Default is set to FALSE, to reduce time required for computation.
#' @param Intercept Does the regression object have an intercept term? Default is set to
#' TRUE.
#' @export
#' @examples CovariateWeights(RegressionModel, Rounding = 1, Exact = TRUE)

CovariateWeights <- function(Model, Rounding = 0.01, Exact = FALSE, Intercept = TRUE){

  if (Intercept == TRUE) {

    ModelCoefficients <- Model[["coefficients"]][-1]

  } else {

    ModelCoefficients <- Model[["coefficients"]]
  }

  Weights <- (ModelCoefficients / sum(ModelCoefficients))*100
  RoundWeights <- Rounding*round(Weights/ Rounding)

  if(Exact == T){

    Difference <- RoundWeights - Weights

    if(sum(RoundWeights) != 100){

      Direction <- sum(Difference)/ abs(sum(Difference))
      AdjustOrder <- order(Difference*Direction, decreasing = T)
      Magnitude <- abs(sum(Difference)/ Rounding)
      RoundWeights[AdjustOrder][1: Magnitude] <-
        RoundWeights[AdjustOrder][1: Magnitude] - (Rounding * Direction)

    }
  }
  return(RoundWeights)
}
