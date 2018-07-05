
#' Univariate Statistical Significance
#'
#' This function computes the Statistical Significance (p-value) equivalent to
#' that obtained from a linear regression.
#'
#' @param Target A vector of the dependent variable.
#' @param Prediction A vector of the predictions.
#' @export
#' @examples Significance(mtcars$mpg, mtcars$cyl)

Significance <- function(Target, Prediction){
  if(length(Target) != length(Prediction)){
    stop("Target and Prediction must be of the same length.")
  } else{
    NA.Index <- unique(c(which(is.na(Target)), which(is.na(Prediction))))
    Target <- Target[!1:length(Target) %in% NA.Index]
    Prediction <- Prediction[!1:length(Prediction) %in% NA.Index]

    if(is.numeric(Prediction) == T){
      RS <- cor(Target, Prediction)^2
      K <- 1
    } else if(is.factor(Prediction) == T | is.character(Prediction) == T){
      DT1 <- data.table(Y = Target, X = Prediction)
      DT1[, AvgTarget := mean(Y), by = X]
      RS <- cor(DT1[, Y], DT1[, AvgTarget])^2
      K <- length(unique(Prediction)) - 1
    }
    df1 <- K
    df2 <- length(Prediction) - K - 1
    FStat <- (RS/ (1-RS))/ (df1/ df2)
    Results <- 1 - pf(FStat, df1, df2)
    return(Results)
  }
}
