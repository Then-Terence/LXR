
#' Cross Table of Numerical Covariates
#'
#' This function generates a data.table tabulating the different ranges of a continuous
#' covariate and the corresponding probabilities of the binary dependent
#' variable taking the value of 1.
#'
#' @param Target The name of binary target to be predicted.
#' @param Covariate The name of the covariate.
#' @param Data A data.table containing both the target and covariate.
#' @param NumberOfBins Number of bins the numerical value to be broken into.
#' @param CustomBins Allowing a custom set of values to be used for binning.
#' @param CustomIntervals The numerical values of the break points.
#' @param UseLogit If the value is TRUE, Log Odds will be generated. Otherwise,
#' a set of score derived from Log Odds, scaled from 0 to 100, will be generated.
#' @export
#' @examples mpgTable <- NumericalTable(Target = "am", Covariate = "mpg", Data = mtcars)


NumericalTable <- function(Target, Covariate, Data, NumberOfBins = 5,
                           CustomBins = F, CustomIntervals = NULL,
                           UseLogit = T){
  Results <- data.table(Data)
  ColNames <- names(Data)
  TargetPosition <- which(ColNames == Target)
  CovariatePosition <- which(ColNames == Covariate)
  Results <- Results[, c(TargetPosition, CovariatePosition), with = F]
  setnames(Results, Target, "Target")
  setnames(Results, Covariate, "Covariate")

  if(CustomBins == F){
    Breaks <- quantile(Results[, "Covariate"], probs = seq(0, 1, 1/ NumberOfBins),
                       na.rm = T)
  } else {
    Breaks <- CustomIntervals
  }

  Results[, Categories := cut(Covariate, breaks = Breaks, include.lowest = T)]

  Results <- Results[, list(Event = sum(Target),
                            `Non Event` = sum(Target == 0)),
                     by = Categories]
  Results[, Counts := Event + `Non Event`]
  Results[, Probability := round(Event/ Counts, 4)]
  Results[, Logit := log (Event/ `Non Event`)]

  if(UseLogit == F){
    Results[, Score := 100 - round(100 * (Logit - min(Logit)) /
                                     (max(Logit) - min(Logit)))]
    Results[, Logit := NULL]
  }

  LowerBounds <- gsub("^.*\\(", "", as.character(Results[, Categories]))
  LowerBounds <- gsub("^.*\\[", "", LowerBounds)
  LowerBounds <- gsub(",.*$", "", LowerBounds)
  LowerBounds <- as.numeric(LowerBounds)

  NAPosition <- which(is.na(LowerBounds))
  Order <- order(LowerBounds)
  Order <- c(NAPosition, Order[!Order %in% NAPosition])
  Results <- Results[Order, ]
  setnames(Results, "Categories", Covariate)
  return(Results)
}
