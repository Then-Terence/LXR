
#' Cross Table of Categorical Covariates
#'
#' This function generates a data.table tabulating the different levels of a categorical
#' independent variable and the corresponding probabilities of the binary dependent
#' variable taking the value of 1.
#'
#' @param Target The name of binary target to be predicted.
#' @param Covariate The name of the covariate.
#' @param DT A data.table containing both the target and covariate.
#' @param UseLogit If the value is TRUE, Log Odds will be generated. Otherwise,
#' a set of score derived from Log Odds, scaled from 0 to 100, will be generated.
#' @export
#' @examples CategoricalTable(Target = "am", Covariate = "gear", DT = mtcars)

CategoricalTable <- function(Target, Covariate, DT, UseLogit = T){

  Results <- data.table(DT)
  Results <- Results[, c(Target, Covariate), with = F]
  setnames(Results, Target, "Target")

  Results <- Results[, .(Event       = sum(Target),
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
