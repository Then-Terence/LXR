
#' Summary of Covariates
#'
#' This function allows you to explore the discriminatory power of covariates
#' against a binary target, and summarizes the number of NA and 0 in each of
#' the covariates.
#'
#' It returns a data.table containing the names of
#' covariates, number of NA, number of 0, and Area Under the Curve (AUC).
#'
#' @param Target A string of the name of target to be predicted.
#' @param Data A data.frame or data.table containing both the target and
#' covariates.
#' @param Skip A vector of strings containing the names of columns to be
#' skipped.
#' @export
#' @examples CovariateSummary(Target = "am", Data = mtcars, Skip = c("vs", "carb"))

CovariateSummary <- function(Target, Data, Skip = NULL){

  Data <- data.table(Data)

  CovarNames <- names(Data)
  CovarNames <- CovarNames[!CovarNames %in% c(Target, Skip)]

  AUC <- numeric()
  `Count of NA` <- numeric()
  `Count of 0` <- numeric()
  Type <- character()

  for(i in 1:length(CovarNames)){
    Type[i] <- class(Data[[CovarNames[i]]])
    AUC[i] <- round(AUROC(Data[[Target]],
                          Data[[CovarNames[i]]]), 4)
    `Count of NA`[i] <- sum(is.na(Data[[CovarNames[i]]]))
    `Count of 0` [i] <- sum(Data[[CovarNames[i]]] == 0, na.rm = T)
  }

  Results <- data.table(Covariate     = CovarNames,
                        Type          = Type,
                        `Count of NA` = `Count of NA`,
                        `Count of 0`  = `Count of 0`,
                        AUC           = AUC)

  return(Results)
}
