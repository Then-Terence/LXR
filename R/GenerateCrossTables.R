
#' Generate Cross Tables
#'
#' This function generates cross tables for the columns of covariates in a
#' data.table or data.frame in relation to a binary dependent variable. It
#' returns a list containing the cross tables
#'
#' @param Target The name of the binary dependent variable.
#' @param Covariate The name of the covariates for which the cross tables
#' will be generated on.
#' @param Data A data.frame or data.table which contains the covariates and the
#' binary dependent variable.
#' @param Groups If the covariate is numeric, how many roughly equal-sized
#' groups should they be divided into.
#' This can be a vector of values. The highest value will first be attempted.
#' If roughly equal-sized groups cannot be created, the second highest value
#' will be attempted. If roughly equal-sized groups cannot be created from
#' the lowest value, the covariate will be treated as categorical with each
#' distinct value being a category.
#' @param UseLogit Argument for NumericalTable()/ CategoricalTable(). If the
#' value is TRUE, Log Odds will be generated. Otherwise, a set of score derived
#' from Log Odds, scaled from 0 to 100, will be generated.
#' @export
#' @examples
#' TableList <- GenerateCrossTables(Target = "am",
#' Covariate = c("mpg", "cyl", "hp"), Groups = c(3, 4))

GenerateCrossTables <- function(Target, Covariate, Data, Groups, UseLogit = T){

  Data <- data.table(Data)

  Results <- list()

  for(i in 1:length(Covariate)){

    CovariateVector <- Data[[Covariate[i]]]

    if(is.numeric(CovariateVector)){

      UniqueGroups <- c()

      for(j in 1:length(Groups)){

        UniqueGroups[j] <-
          length(
            unique(
              quantile(CovariateVector, probs = seq(0, 1, 1/Groups[j])))) - 1

      }

      if(sum(Groups == UniqueGroups) != 0){

        PossibleGroup <- max(Groups[Groups == UniqueGroups])

        Results[[i]] <- NumericalTable(Target, Covariate[i], Data, PossibleGroup,
                                       UseLogit = UseLogit)

      } else {

        Results[[i]] <- CategoricalTable(Target, Covariate[i], Data,
                                         UseLogit = UseLogit)

      }

    } else{

      Results[[i]] <- CategoricalTable(Target, Covariate[i], Data,
                                       UseLogit = UseLogit)

    }
  }

  names(Results) <- paste0(Covariate, "Table")
  return(Results)

}
