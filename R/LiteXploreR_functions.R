
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

#' Cross Table of Categorical Covariates
#'
#' This function generates a data.table tabulating the different levels of a categorical
#' independent variable and the corresponding probabilities of the binary dependent
#' variable taking the value of 1.
#'
#' @param Target The name of binary target to be predicted.
#' @param Covariate The name of the covariate.
#' @param Data A data.table containing both the target and covariate.
#' @export
#' @examples gearTable <- CategoricalTable(Target = "am", Covariate = "gear", Data = mtcars)

CategoricalTable <- function(Target, Covariate, Data){
  DT1 <- data.table(Data)
  ColNames <- names(Data)
  TargetPosition <- which(ColNames == Target)
  CovariatePosition <- which(ColNames == Covariate)

  DT1 <- DT1[, c(TargetPosition, CovariatePosition), with = F]
  setnames(DT1, Target, "Target")
  setnames(DT1, Covariate, "Covariate")

  DT1 <- DT1[, list(Event = sum(Target),
                    `Non Event` = sum(Target == 0)),
             by = Covariate]
  DT1[, Counts := Event + `Non Event`]
  DT1[, Probability := round(Event/ Counts, 4)]
  DT1[, Logit := log(Event/ `Non Event`)]

  NA.Position <- which(is.na(DT1[, 1]))
  Order <- order(DT1[, 1])
  Order <- c(NA.Position, Order[!Order %in% NA.Position])
  DT1 <- DT1[Order, ]
  setnames(DT1, "Covariate", Covariate)
  return(DT1)
}

#' Cross Table of Numerical Covariates
#'
#' This function generates a data.table tabulating the different ranges of a continuous
#' covariate and the corresponding probabilities of the binary dependent
#' variable taking the value of 1.
#'
#' @param Target The name of binary target to be predicted.
#' @param Covariate The name of the covariate.
#' @param Data A data.table containing both the target and covariate.
#' @export
#' @examples mpgTable <- NumericalTable(Target = "am", Covariate = "mpg", Data = mtcars)


NumericalTable <- function(Target, Covariate, Data, NumberOfBins = 5,
                           CustomBins = F, CustomIntervals = NULL){
  DT1 <- data.table(Data)
  ColNames <- names(Data)
  TargetPosition <- which(ColNames == Target)
  CovariatePosition <- which(ColNames == Covariate)
  DT1 <- DT1[, c(TargetPosition, CovariatePosition), with = F]
  setnames(DT1, Target, "Target")
  setnames(DT1, Covariate, "Covariate")

  if(CustomBins == F){
    Breaks <- quantile(DT1[, "Covariate"], probs = seq(0, 1, 1/ NumberOfBins),
                       na.rm = T)
  } else {
    Breaks <- CustomIntervals
  }

  DT1[, Categories := cut(Covariate, breaks = Breaks, include.lowest = T)]

  DT1 <- DT1[, list(Event = sum(Target),
                    `Non Event` = sum(Target == 0)),
             by = Categories]
  DT1[, Counts := Event + `Non Event`]
  DT1[, Probability := round(Event/ Counts, 4)]
  DT1[, Logit := log (Event/ `Non Event`)]

  LowerBounds <- gsub("^.*\\(", "", as.character(DT1[, Categories]))
  LowerBounds <- gsub("^.*\\[", "", LowerBounds)
  LowerBounds <- gsub(",.*$", "", LowerBounds)
  LowerBounds <- as.numeric(LowerBounds)

  NAPosition <- which(is.na(LowerBounds))
  Order <- order(LowerBounds)
  Order <- c(NAPosition, Order[!Order %in% NAPosition])
  DT1 <- DT1[Order, ]
  setnames(DT1, "Categories", Covariate)
  return(DT1)
}

#' Generate Logit
#'
#' This function generates a new column containing the conditional logit
#' corresponding to a covariate.
#'
#' @param Data A data.frame or data.table for which a new column will be
#' generated in.
#' @param CrossTable A cross table with the different levels of the covariate.
#' @param Covariate The name of the covariate.
#' @export
#' @examples
#' Discretize a continuous covariate
#' mtcars[, mpgCat := cut(mpg, breaks = c(10, 17, 21, 35), include.lowest = T)]
#'
#' Generate the conditional logit
#' mtcars <- GenerateLogit(mtcars, mpgTable, "mpgCat")

GenerateLogit <- function(Data, CrossTable, Covariate){
  DT1 <- copy(CrossTable)
  setnames(DT1, names(DT1)[1], Covariate)
  NewName <- paste0(Covariate, "Logit")
  setnames(DT1, "Logit", NewName)
  DTJoined <- merge(Data, DT1[, c(Covariate, NewName), with = F],
                    by = Covariate)
  return(DTJoined)
}

#' Weights of Covariates
#'
#' This function takes the regression object and calculates the relative
#' importance of different covariates.
#' The relative importance is only valid when the covariates are standardized.
#'
#' @param Model A "lm" or "glm" object.
#' @param Rounding Desired rounding, default is up to 2 decimal places.
#' @param Exact If Exact = TRUE, the function will return weights with the sum of exactly
#' 100. Default is set to FALSE, to reduce time required for computation.
#' @param Intercept Does the regression object have an intercept term? Default is set to
#' TRUE.
#' @export
#' @examples CovariateWeights(RegressionModel, Rounding = 1, Exact = TRUE)

CovariateWeights <- function(Model, Rounding = 0.01, Exact = FALSE, Intercept = TRUE){
  if (Intercept == TRUE) {
    ModelCoefficients <- Model$coefficients[-1]
  } else {
    ModelCoefficients <- Model$coefficients
  }
  Weights <- (ModelCoefficients / sum(ModelCoefficients))*100
  RoundWeights <- Rounding*round(Weights/ Rounding)
  if(Exact == T){
    Difference <- RoundWeights - Weights
    if(sum(RoundWeights) != 100){
      Direction <- sum(Difference)/ abs(sum(Difference))
      AdjustOrder <- order(Difference*Direction, decreasing = T)
      Magnitude <- abs(sum(Difference)/ Rounding)
      RoundWeights[AdjustOrder][1: Magnitude] <-
        RoundWeights[AdjustOrder][1: Magnitude] - (Rounding * Direction)
    }
  }
  return(RoundWeights)
}

#' Area Under the ROC Curve
#'
#' This function computes the AUC, and returns the numeric value of it.
#'
#' @param Target A vector with values of 0/1 to be predicted.
#' @param Prediction A vector of prediction or covariate used to predict the target.
#' @export
#' @examples AUROC(mtcars$am, mtcars$mpg)

AUROC <- function(Target, Prediction){
  if(length(Target) != length(Prediction)){
    stop("Target and Prediction must be of the same length.")
  } else{
  if(is.numeric(Prediction) == T){
    Order <- order(Prediction)
    Order <- Order[!Order %in% which(is.na(Prediction))]
    Prediction <- Prediction[Order]
    Target <- Target[Order]

    FP <- cumsum(!Target)/sum(!Target)
    TP <- cumsum(Target)/sum(Target)

    Repeated <- c(Prediction[-1] == Prediction[-length(Prediction)], FALSE)

    FP <- c(0, FP[!Repeated], 1)
    TP <- c(0, TP[!Repeated], 1)

    } else if(is.factor(Prediction) == T | is.character(Prediction) == T){

    DT1 <- data.table(Y = Target, X = Prediction)
    DT1 <- DT1[, list(Event = sum(Y),
                      `Non Event` = sum(!Y)),
               by = X]
    DT1[, Probability := Event / (Event + `Non Event`)]
    DT1 <- DT1[order(Probability), ]

    FP <- c(0, cumsum(DT1[, `Non Event`])/ sum(DT1[, `Non Event`]), 1)
    TP <- c(0, cumsum(DT1[, Event])/ sum(DT1[, Event]), 1)

    } else{
      stop("The class of Prediction should be eiher numeric or factor.")
  }
    N <- length(FP)
    TpAvg <- (TP[-1] + TP[-N])/ 2
    FpDif <- FP[-1] - FP[-N]

    Area <- sum(TpAvg * FpDif)
    Results <- 0.5 + abs(Area - 0.5)
    return(Results)
  }
}

#' Plot ROC Curve
#'
#' This function plots the ROC curve.
#'
#' @param Target A vector with values of 0/1 to be predicted.
#' @param Prediction A vector of prediction or covariate used to predict the target.
#' @param ... Other parameters for plot().
#' @export
#' @examples PlotROC(mtcars$am, mtcars$mpg)

PlotROC <- function(Target, Prediction, ...){
  if(length(Target) != length(Prediction)){
    stop("Target and Prediction must be of the same length.")
  } else{
  if(is.numeric(Prediction) == T){
    Order <- order(Prediction, decreasing = T)
    Order <- Order[!Order %in% which(is.na(Prediction))]
    Prediction <- Prediction[Order]
    Target <- Target[Order]

    FP <- cumsum(!Target)/sum(!Target)
    TP <- cumsum(Target)/sum(Target)

    Repeated <- c(Prediction[-1] == Prediction[-length(Prediction)], FALSE)

    FP <- c(0, FP[!Repeated], 1)
    TP <- c(0, TP[!Repeated], 1)

  } else if(is.factor(Prediction) == T | is.character(Prediction) == T){

    DT1 <- data.table(Y = Target, X = Prediction)
    DT1 <- DT1[, list(Event = sum(Y),
                      `Non Event` = sum(!Y)),
               by = X]
    DT1[, Probability := Event / (Event + `Non Event`)]
    DT1 <- DT1[order(Probability), ]

    FP <- c(0, cumsum(DT1[, `Non Event`])/ sum(DT1[, `Non Event`]), 1)
    TP <- c(0, cumsum(DT1[, Event])/ sum(DT1[, Event]), 1)

  } else{
    stop("The class of Prediction should be eiher numeric or factor.")
  }
    N <- length(FP)
    TpAvg <- (TP[-1] + TP[-N])/ 2
    FpDif <- FP[-1] - FP[-N]
    Area <- sum(TpAvg * FpDif)

    if(Area < 0.5){
      TP <- 1 - TP
      FP <- 1 - FP
    }

  plot(FP, TP, xlab = "False Positive Rate", ylab = "True Positive Rate",
       type = "l", pch = 16, ... = ...)
  }
}

#' Univariate Adjusted R Squared
#'
#' This function computes the Adjusted R Squared equivalent to that obtained
#' from a linear regression.
#'
#' @param Target A vector of the dependent variable.
#' @param Prediction A vector of the predictions.
#' @export
#' @examples ARS(mtcars$mpg, mtcars$cyl)

ARS <- function(Target, Prediction){
  if(length(Target) != length(Prediction)){
    stop("Target and Prediction must be of the same length.")
  } else{
  NA.Index <- unique(c(which(is.na(Target)), which(is.na(Prediction))))
  Target <- Target[!1:length(Target) %in% NA.Index]
  Prediction <- Prediction[!1:length(Prediction) %in% NA.Index]

  if(is.numeric(Prediction) == T){
    RS <- cor(Target, Prediction)^2
    K <- 1
  } else if(is.factor(Prediction) == T | is.character(Prediction) == T){
    DT1 <- data.table(Y = Target, X = Prediction)
    DT1[, AvgTarget := mean(Y), by = X]
    RS <- cor(DT1[, Y], DT1[, AvgTarget])^2
    K <- length(unique(Prediction)) - 1
  }
  ARS <- 1 - (1- RS) * (length(Prediction) - 1) / (length(Prediction) - K - 1)
  return(ARS)
  }
}

#' Univariate Statistical Significance
#'
#' This function computes the Statistical Significance (p-value) equivalent to
#' that obtained from a linear regression.
#'
#' @param Target A vector of the dependent variable.
#' @param Prediction A vector of the predictions.
#' @export
#' @examples Significance(mtcars$mpg, mtcars$cyl)

Significance <- function(Target, Prediction){
  if(length(Target) != length(Prediction)){
    stop("Target and Prediction must be of the same length.")
  } else{
  NA.Index <- unique(c(which(is.na(Target)), which(is.na(Prediction))))
  Target <- Target[!1:length(Target) %in% NA.Index]
  Prediction <- Prediction[!1:length(Prediction) %in% NA.Index]

  if(is.numeric(Prediction) == T){
    RS <- cor(Target, Prediction)^2
    K <- 1
  } else if(is.factor(Prediction) == T | is.character(Prediction) == T){
    DT1 <- data.table(Y = Target, X = Prediction)
    DT1[, AvgTarget := mean(Y), by = X]
    RS <- cor(DT1[, Y], DT1[, AvgTarget])^2
    K <- length(unique(Prediction)) - 1
  }
  df1 <- K
  df2 <- length(Prediction) - K - 1
  FStat <- (RS/ (1-RS))/ (df1/ df2)
  Significance <- 1 - pf(FStat, df1, df2)
  return(Significance)
  }
}

#' Logarithmic Loss
#'
#' This function computes the logarithmic loss for binary predictions.
#'
#' @param Target A vector of the binary dependent variable.
#' @param Prediction A vector of the predictions.
#' @export
#' @examples LogLoss(Data[, Y], Model$fitted.values)

LogLoss <- function(Target, Prediction){
  -sum(Target * log(Prediction + 1e-99) +
         (1 - Target) * log(1 - Prediction - 1e-99))/
    length(Target)
}
