
#' Coefficient of Variation
#'
#' @param x A numeric vector.
#' @param na.rm Boolean. If TRUE, missing values are removed.
#'
#' @importFrom stats sd
#' @noRd
CV <- function(x, na.rm){
  ave<-mean(x, na.rm=na.rm)
  dev<-sd(x, na.rm=na.rm)
  return((dev/ave))
}

#' Linear Interpolation
#'
#' @param days A numeric vector.
#' @param obs A numeric vector.
#'
#' @importFrom stats approx
#' @noRd
interp<-function(days, obs){
  int.dat<-as.data.frame(approx(days, obs, n = length(obs), method = "linear"))
  names(int.dat)<-c("time", "counts")
  return(int.dat)
}

#' Rolling Mean
#'
#' @param x A numeric vector.
#' @noRd
rolling_mean <- function(x){
  k = length(x);
  result = rep(0, k);
  for(i in 1 : k){
    result[i] <- mean(x[1:i], na.rm=T);
  }
  return(result);
}

#' Rolling Standard Deviation
#'
#' @param x A numeric vector.
#'
#' @importFrom stats sd
#' @noRd

rolling_sd <- function(x){
  k = length(x);
  result = rep(0, k);
  for(i in 1 : k){
    result[i] <- sd(x[1:i], na.rm=T);
  }
  return(result);
}
