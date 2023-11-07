#' Multivariate Variance Index function
#'
#' Calculate a multivariate variance following Brock, W. A., and S. R. Carpenter. 2006. Variance as a leading indicator of regime shift in ecosystem services. Ecology and Society 11(2): 9.
#'
#' @param data A numeric matrix of species abundances, names across columns, time across rows. The first column is a time vector, the remainder are species values.
#' @param winsize Numeric. Defines the window size of the rolling window as a percentage of the time series length.
#'
#' @returns A matrix where the first column is last time index of the window and the second column is the estimated index value.
#'
#' @examples
#' #Load the multivariate simulated
#' #dataset `simTransComms`
#'
#' data(simTransComms)
#'
#' #Estimate the MVI for the second community
#'
#' egMVI <- mvi(data = simTransComms$community2[,2:7],
#' winsize = 10)
#'
#' @export
#' @source Brock, W.A. & Carpenter, S.R. (2006) Variance as a leading indicator of regime shift in ecosystem services. Ecology and Society 11(2): 9.


mvi <- function(data,winsize = 50){

  data <- as.data.frame(data)

  if(NCOL(data) <= 2){
    stop("Data only contains two columns. mvi require 2+ timeseries")
  }
  if(!all(apply(data[,-1],2,is.numeric))){
    stop("Not all timeseries are numeric")
  }

  window <- round(dim(data)[1] * winsize/100)

  out <- matrix(ncol = 2,nrow=dim(data)[1]-window+1)
  for(i in 1:(dim(data)[1]-window+1)){
    #tmp <- cov(data[i:(i+1),])
    tmp <- cov(data[i:(i+window-1),-1])
    out[i,2] <- sqrt(eigen(tmp,symmetric = T)$values[1])
    out[i,1] <- data[(i+window-1),1]

  }
  colnames(out) <- c("time","mvi")
  return(out)
}
