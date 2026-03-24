#' Rolling Window Early Warning Signals

#' An adaptation of the \code{generic_ews} function in the R package \code{earlywarnings}.
#'
#' @param timeseries A dataframe where first column is time (equally spaced) and second column is abundance.
#' @param winsize Numeric. Defines the window size of the rolling window as a percentage of the timeseries.
#'
#' @importFrom stats acf
#' @importFrom stats spec.ar
#'
#' @keywords internal
#' @noRd


no.plot.ews<-function(timeseries, winsize = 50){

  data <- data.matrix(timeseries)

  if(length(dim(timeseries)) != 2){
    stop("not right format of data input")
  }

winsize_true <- round(dim(data)[1] * winsize/100)

RES <- list()
for(i in 1:(dim(data)[1]-winsize_true+1)){
  if(length(which(diff(data[i:(i+winsize_true-1),2])!=0))>0){

  nARR  <- stats::ar(data[i:(i+winsize_true-1),2], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE, method = "yule-walker")$ar[1]

  nSK  <- abs(moments::skewness(data[i:(i+winsize_true-1),2], na.rm = TRUE))

  nKURT <- moments::kurtosis(data[i:(i+winsize_true-1),2], na.rm = TRUE)

  nSD <- sd(data[i:(i+winsize_true-1),2], na.rm = TRUE)

  nACF <- acf(data[i:(i+winsize_true-1),2], lag.max = 1, type = c("correlation"),
              plot = FALSE)$acf[2]

  # spectfft <- spec.ar(data[i:(i+winsize_true-1),2], n.freq =  dim(data)[1] - winsize_true + 1,
  #                     plot = FALSE,
  #                     order = 1)
  spectfft <- spec.ar(data[i:(i+winsize_true-1),2], n.freq = winsize_true, plot = FALSE, order = 1, method = "yule-walker")

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

