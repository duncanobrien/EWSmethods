#' Univariate Jacobian Index Estimated From an Univariate Autocorrelation Matrix
#'
#' Estimate the dominant Jacobian eigenvalue of a univariate time series using autocorrelated stochastic differential equations
#'
#' @param data Numeric matrix with time in first column and species abundance in the second
#' @param winsize Numeric. Defines the window size of the rolling window as a percentage of the time series length.
#' @param scale Boolean. Should data be scaled prior to estimating the Jacobian.
#' @param p Numeric. Defines the model order. Defaults to `1`.
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
#' #Subset the second community prior to the transition
#'
#' pre_simTransComms <- subset(simTransComms$community2,time < inflection_pt)
#'
#' #Estimate the univariate stability index for the first species in
#' #the second community
#'
#' egarJ <- uniAR(data = pre_simTransComms[,2:3],
#' winsize = 25, dt = 1)
#'
#' @export

uniAR <- function(data, scale = TRUE, winsize = 50, p = 1, dt = 1){

  if(NCOL(data) != 2){
    stop("Data requires only a time column and a timeseries")
  }
  if(!all(is.numeric(data[,-1]))){
    stop("Not all time series are numeric")
  }
  data <- as.data.frame(data)

  window <- round(dim(data)[1] * winsize/100)

  out <- lapply(1:(dim(data)[1]-window+1), function(i){

    sub_data <- data[i:(i+window-1),]
    if(isTRUE(scale)){
      sub_data[,2] <- c(scale(sub_data[,2]))
    }
    Ar_mod <- stats::ar.ols(as.matrix(sub_data[,-1]), aic = FALSE, order.max = p, dmean = FALSE,intercept = FALSE)$ar

    jac <- rbind(as.numeric(Ar_mod),
                 cbind(diag(p - 1), rep(0, p - 1)))

    yy <- eigen(jac)
    jac_eig <- (1/dt)*(log(abs(yy$values)))
    return(cbind("time" = data[(i+window-1),1],
                 "uniAR" = max(jac_eig)))
  })
  out <- as.data.frame(do.call("rbind", out))

  return(out)
}
