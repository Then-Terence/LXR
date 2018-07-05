
#' Generate Logit
#'
#' This function generates a new column containing the conditional logit
#' corresponding to a covariate.
#'
#' @param Data A data.frame or data.table for which a new column will be
#' generated in.
#' @param CrossTable A cross table with the different levels of the covariate.
#' @param Covariate The name of the covariate.
#' @param UseLogit If the value is TRUE, the join will be based on a column
#' named "Logit". Otherwise, it will look for a column named "Score".
#' @export
#' @examples
#' Discretize a continuous covariate
#' mtcars[, mpgCat := cut(mpg, breaks = c(10, 17, 21, 35), include.lowest = T)]
#'
#' Generate the conditional logit
#' mtcars <- GenerateLogit(mtcars, mpgTable, "mpgCat")

GenerateLogit <- function(Data, CrossTable, Covariate, UseLogit = T){
  DT1 <- copy(Data)
  DT2 <- copy(CrossTable)
  DT1[, Foo := 1:nrow(DT1)]
  setnames(DT2, names(DT2)[1], Covariate)

  if(UseLogit == T){
    NewName <- paste0(Covariate, "Logit")
    setnames(DT2, "Logit", NewName)
  } else {
    NewName <- paste0(Covariate, "Score")
    setnames(DT2, "Score", NewName)
  }

  DTJoined <- merge(DT1, DT2[, c(Covariate, NewName), with = F],
                    by = Covariate)
  DTJoined <- DTJoined[order(Foo), ]
  DTJoined[, Foo := NULL]
  return(DTJoined)
}
