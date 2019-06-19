
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
#' @param UseCustomIntervals Allowing a custom set of values to be used for binning.
#' @param CustomIntervals The numerical values of the break points.
#' @param UseLogit If the value is TRUE, Log Odds will be generated. Otherwise,
#' a set of score derived from Log Odds, scaled from 0 to 100, will be generated.
#' @export
#' @examples NumericalTable(Target = "am", Covariate = "mpg", Data = mtcars)


NumericalTable <- function(Target, Covariate, Data, NumberOfBins = 5,
                           UseCustomIntervals = F,
                           CustomIntervals = NULL,
                           UseLogit = T){

  Results <- Data[, c(Target, Covariate), with = F]
  setnames(Results, names(Results), c("Target", "Covariate"))

  if(CustomBins == F){

    Breaks <- quantile(Results[, Covariate],
                       probs = seq(0, 1, 1/ NumberOfBins), na.rm = T)

  } else {

    Breaks <- CustomIntervals

  }

  Results[, (Covariate) := cut(Covariate, breaks = Breaks, include.lowest = T)]

  Results <- Results[, .(Event      = sum(Target),
                         `Non Event` = sum(!Target)),
                     by = eval(Covariate)]

  Results[, `:=`(Counts      = Event + `Non Event`,
                 Probability = round(Event / (Event + `Non Event`), 4),
                 Logit       = round(log(Event / `Non Event`), 4))]

  if(UseLogit == F){

    Results[, Score := 100 - round(100 * (Logit - min(Logit)) /
                                     (max(Logit) - min(Logit)))]
    Results[, Logit := NULL]

  }

  setkeyv(Results, Covariate)

  return(Results[])

}
