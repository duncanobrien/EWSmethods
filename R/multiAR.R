#' Multivariate Jacobian Index Estimated From Multivariate Autocorrelation Matrix
#'
#' Estimate the dominant Jacobian eigenvalue of a multivariate time series using autocorrelated stochastic differential equations
#'
#' @param data Numeric matrix with time in first column and species abundance in the second
#' @param winsize Numeric. Defines the window size of the rolling window as a percentage of the time series length.
#' @param scale Boolean. Should data be scaled prior to estimating the Jacobian.
#' @param dt Numeric An appropriate time step
#'
#' @returns A dataframe where the first column is last time index of the window and the second column is the estimated index value. A value <1.0 indicates stability, a value >1.0 indicates instability.
#'
#' @examples
#' #Load the multivariate simulated
#' #dataset `simTransComms`
#'
#' data(simTransComms)
#'
#'#Subset the second community prior to the transition
#'
#' pre_simTransComms <- subset(simTransComms$community2,time < inflection_pt)
#'
#' #Estimate the univariate stability index for the first species in
#' #the second community
#'
#' egarJ <- multiAR(data = pre_simTransComms[,2:7],
#' winsize = 25, dt = 1)
#'
#' @export
#' @source Williamson and Lenton (2015). Detection of bifurcations in noisy coupled systems from multiple time series. Chaos, 25, 036407

multiAR <- function(data, scale = TRUE, winsize = 50, dt = 1){

  if(NCOL(data) <= 2){
    stop("Data only contains two columns. multiAR require 2+ timeseries")
  }

  data <- as.data.frame(data)

  window <- round(dim(data)[1] * winsize/100)

  out <- lapply(1:(dim(data)[1]-window+1), function(i){

    sub_data <- data[i:(i+window-1),]
    if(isTRUE(scale)){
    sub_data <- sapply(sub_data[,-1],FUN = function(x){return(c(scale(x)))} )
    }
    mAr_mod <- mAr::mAr.est(as.matrix(sub_data[,-1]), 1)
    yy <- eigen(mAr_mod$AHat)
    jac_eig <- (1/dt)*(log(abs(yy$values)))
    return(cbind("time" = data[(i+window-1),1],
                 "multiAR" = max(jac_eig)))
    })
  out <- as.data.frame(do.call("rbind", out))

  return(out)
  }

