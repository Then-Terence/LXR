
#' Cross Table of Categorical Covariates
#'
#' This function generates a data.table tabulating the different levels of a categorical
#' independent variable and the corresponding probabilities of the binary dependent
#' variable taking the value of 1.
#'
#' @param Target The name of binary target to be predicted.
#' @param Covariate The name of the covariate.
#' @param Data A data.table containing both the target and covariate.
#' @param UseLogit If the value is TRUE, Log Odds will be generated. Otherwise,
#' a set of score derived from Log Odds, scaled from 0 to 100, will be generated.
#' @export
#' @examples gearTable <- CategoricalTable(Target = "am", Covariate = "gear", Data = mtcars)

CategoricalTable <- function(Target, Covariate, Data, UseLogit = T){
  Results <- data.table(Data)
  ColNames <- names(Data)
  TargetPosition <- which(ColNames == Target)
  CovariatePosition <- which(ColNames == Covariate)

  Results <- Results[, c(TargetPosition, CovariatePosition), with = F]
  setnames(Results, Target, "Target")
  setnames(Results, Covariate, "Covariate")

  Results <- Results[, list(Event = sum(Target),
                            `Non Event` = sum(Target == 0)),
                     by = Covariate]
  Results[, Counts := Event + `Non Event`]
  Results[, Probability := round(Event/ Counts, 4)]
  Results[, Logit := log(Event/ `Non Event`)]

  if(UseLogit == F){
    Results[, Score := 100 - round(100 * (Logit - min(Logit)) /
                                     (max(Logit) - min(Logit)))]
    Results[, Logit := NULL]
  }

  NAPosition <- which(is.na(Results[, 1]))
  Order <- order(Results[, 1])
  Order <- c(NAPosition, Order[!Order %in% NAPosition])
  Results <- Results[Order, ]
  setnames(Results, "Covariate", Covariate)
  return(Results)
}
