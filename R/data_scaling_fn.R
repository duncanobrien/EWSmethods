#' Scaling Function for EWSNet
#'
#' Scales a numeric vector to be in the range 1-2.
#'
#' @param x A numeric vector to be scaled.
#'
#' @returns A scaled numeric vector where the minimum value is 1 and the maximum is 2.
#'
#' @keywords internal
#' @noRd

data_scaling <- function(x){

  x_min <- min(x,na.rm = T)
  s <- (x-x_min)/(max(x,na.rm = T)-x_min)

  return(s+1)

}
