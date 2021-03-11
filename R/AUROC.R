
#' Area Under the ROC Curve
#'
#' Calculates the area under the ROC curve.
#'
#' @param Target A vector with values of 0/1 to be predicted.
#' @param Prediction A vector of prediction or covariate used to predict the target.
#' @export
#' @examples AUROC(mtcars[, "am"], mtcars[, "mpg"])

AUROC <- function(Target, Prediction){

  if(is.numeric(Prediction)){

    Results <- CalculateAUC(Target, Prediction)

  } else if(is.factor(Prediction) | is.character(Prediction) | is.logical(Prediction)){

    Results <- CalculateFactorAUC(Target, Prediction)

  }

  Results <- sum(Results[, Triangle])

  if(Results > 0.5){

    return(Results)

  } else {

    return(1 - Results)

  }

}
