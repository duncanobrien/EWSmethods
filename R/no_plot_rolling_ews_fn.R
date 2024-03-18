#' Rolling Window Early Warning Signals

#' An adaptation of the \code{generic_ews} function in the R package \code{earlywarnings}.
#'
#' @param timeseries A dataframe where first column is time (equally spaced) and second column is abundance.
#' @param winsize Numeric. Defines the window size of the rolling window as a percentage of the timeseries.
#' @param detrending A string. Should detrending be performed on the time series.
#' @param bandwidth Numeric. Bandwidth used for the Gaussian kernel when gaussian filtering is applied. It is expressed as percentage of the time series length.
#' @param span  Numeric. Parameter that controls the degree of smoothing (numeric between 0 and 100, Default 25).
#' @param degree Numeric. The degree of polynomial when loess detrending is applied.
#' @param logtransform Boolean. If TRUE, data is log transformed as log(x+1).
#' @param interpolate Boolean. If TRUE, interpolates missing values found within the abundance time series.
#' @param AR_n Boolean. If TRUE, the best fitted AR(n) model is fitted to the data.
#' @param powerspectrum Boolean. If TRUE the power spectrum within each rolling window.
#'
#' @importFrom stats bw.nrd0
#' @importFrom stats ksmooth
#' @importFrom stats resid
#' @importFrom stats lm
#' @importFrom stats fitted
#' @importFrom stats loess
#' @importFrom stats predict
#' @importFrom stats acf
#'
#' @keywords internal
#' @noRd


no.plot.ews<-function(timeseries, winsize = 50, detrending = c("no", "gaussian",
                                                                "loess", "linear", "first-diff"),
                      bandwidth = NULL, span = NULL,
                       degree = NULL, logtransform = FALSE, interpolate = FALSE,
                       AR_n = FALSE, powerspectrum = FALSE){

  data <- data.matrix(timeseries)

  if(length(dim(timeseries)) != 2){
    stop("not right format of data input")
  }

winsize_true <- round(dim(data)[1] * winsize/100)

RES <- list()
for(i in 1:(dim(data)[1]-winsize_true+1)){
  if(length(which(diff(data[i:(i+winsize_true-1),2])!=0))>0){

  nARR  <- ar.ols(data[i:(i+winsize_true-1),2], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]

  nSK  <- abs(moments::skewness(data[i:(i+winsize_true-1),2], na.rm = TRUE))

  nKURT <- moments::kurtosis(data[i:(i+winsize_true-1),2], na.rm = TRUE)

  nSD <- sd(data[i:(i+winsize_true-1),2], na.rm = TRUE)

  nACF <- acf(data[i:(i+winsize_true-1),2], lag.max = 1, type = c("correlation"),
              plot = FALSE)$acf[2]

  # spectfft <- spec.ar(data[i:(i+winsize_true-1),2], n.freq =  dim(data)[1] - winsize_true + 1,
  #                     plot = FALSE,
  #                     order = 1)
  spectfft <- spec.ar(data[i:(i+winsize_true-1),2], n.freq = winsize_true, plot = FALSE, order = 1)

  nDENSITYRATIO <- spectfft$spec[1]/spectfft$spec[winsize_true]

  nRETURNRATE = 1/nARR

  }else{
    nARR <- NA
    nSK <- NA
    nKURT <- NA
    nSD <- NA
    nACF <- NA
    nDENSITYRATIO<- NA
    nRETURNRATE <- NA
  }

  RES[[i]] <- data.frame("timeindex" = data[i+winsize_true-1,1],
                         "ar1" = nARR,
                         "skew" = nSK,
                         "kurt" = nKURT,
                         "SD" = nSD,
                         "acf" = nACF,
                         "dr" = nDENSITYRATIO,
                         "rr" = nRETURNRATE)
}
output<-do.call("rbind", RES)
output$cv <- sapply(1:(dim(data)[1]-winsize_true+1),function(i){
  sd(data[i:(i+winsize_true-1),2], na.rm = TRUE)/mean(data[i:(i+winsize_true-1),2])
})

out.cor <- data.frame("ar1" = tryCatch({cor.test(as.numeric(output$time), output$ar1, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){ warning("Correlation coefficents not returned as too few observations"); return(NA)}),
                      "skew" = tryCatch({cor.test(as.numeric(output$time), output$skew, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                      "kurt" = tryCatch({cor.test(as.numeric(output$time), output$kurt, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                      "SD" = tryCatch({cor.test(as.numeric(output$time), output$SD, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                      "cv" = tryCatch({cor.test(as.numeric(output$time), output$cv, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                      "acf" = tryCatch({cor.test(as.numeric(output$time), output$acf, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                      "dr" = tryCatch({cor.test(as.numeric(output$time), output$dr, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                      "rr" = tryCatch({cor.test(as.numeric(output$time), output$rr, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}))

return(list("raw" = output, "cor" = out.cor))
}

