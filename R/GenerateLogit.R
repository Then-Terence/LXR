
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
#' Generate a cross table
#' mpgTable <- CategoricalTable("am", "mpgCat", mtcars)
#'
#' Generate the conditional logit
#' GenerateLogit(mtcars, mpgTable, "mpgCat")

GenerateLogit <- function(DT, CrossTable, Covariate, Suffix = "Logit",
                          SelectCol = NULL, NewName = NULL){

  if(is.null(SelectCol)){

    SelectCol <- Suffix

  }

  if(is.null(NewName)){

    NewName <- paste0(Covariate, Suffix)

  }

  DT[, Foo := .I]

  CT <- CrossTable[, c(names(CrossTable)[1], SelectCol), with = F]
  setnames(CT, names(CT), c(Covariate, "Bar"))

  setkeyv(DT, Covariate)
  setkeyv(CT, Covariate)

  DT[CT, (NewName) := i.Bar]
  rm(CT)

  setkey(DT, Foo)
  DT[, Foo := NULL]

  ArrangeColumn(Covariate, NewName, DT)

}
