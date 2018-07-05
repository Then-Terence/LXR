
#' Summary of Covariates
#'
#' This function allows you to explore the discriminatory power of covariates
#' against a binary target, and summarizes the number of NA and 0 in each of
#' the covariates.
#'
#' If the Type is "Binary", it returns a data.table containing the names of
#' covariates, number of NA, number of 0, and Area Under the Curve (AUC).
#'
#' If the Type is "Continuous", it returns a data.table containing the names of
#' covariates, number of NA, number of 0, Adjusted R-squared, and p-value.
#'
#' @param Target A string of the name of target to be predicted.
#' @param Data A data.frame or data.table containing both the target and
#' covariates.
#' @param Skip A vector of strings containing the names of columns to be
#' skipped, such as ID.
#' @param Type The type of dependent variables, either "Binary" or
#' "Continuous". Default is "Binary".
#' @export
#' @examples VariableSummary(Target = "am", Data = mtcars, Skip = c("vs", "carb"), Type = "Binary")

CovariateSummary <- function(Target, Data, Skip = NULL, Type = "Binary"){

  DF <- data.frame(Data)

  ColNames <- names(DF)
  SkipPosition <- which(ColNames %in% Skip)
  TargetPosition <- which(ColNames == Target)
  CovarPosition <- c(1:length(ColNames))
  CovarPosition <- CovarPosition[! CovarPosition %in% c(SkipPosition,
                                                        TargetPosition)]
  if(Type == "Binary"){
    AUC <- c()
    `Count of NA` <- c()
    `Count of 0` <- c()
    Type <- c()

    for(i in 1:length(CovarPosition)){
      Type[i] <- class(DF[, CovarPosition[i]])
      AUC[i] <- round(AUROC(DF[, TargetPosition],
                            DF[, CovarPosition[i]]), 4)
      `Count of NA`[i] <- sum(is.na(DF[, CovarPosition[i]]))
      `Count of 0` [i] <- sum(DF[, CovarPosition[i]] == 0, na.rm = T)
    }

    Covariate <- names(DF)[CovarPosition]

    Results <- data.table(cbind(Covariate, Type, `Count of NA`, `Count of 0`,
                                AUC))
  } else if(Type == "Continuous"){
    Type <- c()
    `Count of NA` <- c()
    `Count of 0` <- c()
    Formula <- c()
    `Adj. R squared` <- c()
    `p-value` <- c()

    for(i in 1:length(CovarPosition)){
      Type[i] <- class(DF[, CovarPosition[i]])
      `Count of NA`[i] <- sum(is.na(DF[, CovarPosition[i]]))
      `Count of 0` [i] <- sum(Data[, CovarPosition[i]] == 0, na.rm = T)
      `Adj. R squared`[i] <- round(ARS(DF[, TargetPosition],
                                       DF[, CovarPosition[i]]), 4)
      `p-value`[i] <- round(Significance(DF[, TargetPosition],
                                         DF[, CovarPosition[i]]), 4)
    }

    Covariate <- names(DF)[CovarPosition]

    Results <- data.table(cbind(Covariate, Type, `Count of NA`, `Count of 0`,
                                `Adj. R squared`, `p-value`))
  } else{
    stop("Type has to be specified as either Binary or Continuous.")
  }
  return(Results)
}
