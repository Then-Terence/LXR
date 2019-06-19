
#' Cut
#'
#' This function discretizes a column of numerical values and returns the
#' values in a new column.
#'
#' It is similar to base::cut() but it also arranges the new column to be right
#' next to the old column being discretized.
#'
#' @param Data The data.frame or data.table for cut() to be used in.
#' @param NewColumn The name for the new column.
#' @param x Argument for cut(), a numeric vector which is to be converted to a
#' factor by cutting.
#' @param breaks Argument for cut(), either a numeric vector of two or more
#' unique cut points or a single number (greater than or equal to 2) giving the
#' number of intervals into which x is to be cut.
#' @param include.lowest Argument for cut(), logical, indicating if an ‘x[i]’
#' equal to the lowest (or highest, for right = FALSE) ‘breaks’ value should be
#' included.
#' @param ... Other argument for cut().
#' @export
#' @examples
#' Discretize a continuous covariate
#' mtcars[, mpgCat := cut(mpg, breaks = c(10, 17, 21, 35), include.lowest = T)]
#'
#' is comparable to
#'
#' Cut(Data = mtcars, NewColumn =  "mpgCat", x = mpg,
#' breaks = c(10, 17, 21, 35))

Cut <- function(Data, NewColumn, x, breaks, include.lowest = T, ...){

  Data <- data.table(Data)

  ArgX <- substitute(x)
  ArgBreaks <- substitute(breaks)

  Data[, (NewColumn) := cut(x              = eval(ArgX),
                            breaks         = eval(ArgBreaks),
                            include.lowest = include.lowest,
                            ...            = ...)]

  ArrangeColumn(as.character(ArgX), NewColumn, DT)

}
