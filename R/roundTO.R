#' Round 2 Function
#'
#' Rounding function renamed 'roundTO' to prevent homonyms
#'
#' @param x Numeric value to be rounded.
#' @param y Numeric value to round \code{x} to nearest multiple of.
#'
#' @returns Numeric value.

roundTO <- function(x,y){
  #roundTO rounds number to nearest multiple of arbitrary precision.
  round(x/y)*y
}
