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
                       AR_n = FALSE, powerspectrum = FALSE)
# {
#   timeseries <- data.matrix(timeseries)
#   if (dim(timeseries)[2] == 1) {
#     Y = timeseries
#     timeindex = 1:dim(timeseries)[1]
#   }
#   else if (dim(timeseries)[2] == 2) {
#     Y <- timeseries[, 2]
#     timeindex <- timeseries[, 1]
#   }
#   else {
#     warning("not right format of timeseries input")
#   }
#   if (interpolate) {
#     YY <- approx(timeindex, Y, n = length(Y), method = "linear")
#     Y <- YY$y
#   }
#   else {
#     Y <- Y
#   }
#   if (logtransform) {
#     Y <- log(Y + 1)
#   }
#   detrending <- match.arg(detrending)
#   if (detrending == "gaussian") {
#     if (is.null(bandwidth)) {
#       bw <- round(bw.nrd0(timeindex))
#     }
#     else {
#       bw <- round(length(Y) * bandwidth/100)
#     }
#     smYY <- ksmooth(timeindex, Y, kernel = "normal", bandwidth = bw,
#                     range.x = range(timeindex), x.points = timeindex)
#     nsmY <- Y - smYY$y
#     smY <- smYY$y
#   }
#   else if (detrending == "linear") {
#     nsmY <- resid(lm(Y ~ timeindex))
#     smY <- fitted(lm(Y ~ timeindex))
#   }
#   else if (detrending == "loess") {
#     if (is.null(span)) {
#       span <- 25/100
#     }
#     else {
#       span <- span/100
#     }
#     if (is.null(degree)) {
#       degree <- 2
#     }
#     else {
#       degree <- degree
#     }
#     smYY <- loess(Y ~ timeindex, span = span, degree = degree,
#                   normalize = FALSE, family = "gaussian")
#     smY <- predict(smYY, data.frame(x = timeindex), se = FALSE)
#     nsmY <- Y - smY
#   }
#   else if (detrending == "first-diff") {
#     nsmY <- diff(Y)
#     timeindexdiff <- timeindex[1:(length(timeindex) - 1)]
#   }
#   else if (detrending == "no") {
#     smY <- Y
#     nsmY <- Y
#   }
  # mw <- round(length(Y) * winsize/100)
  # omw <- length(nsmY) - mw + 1
  # low <- 6
  # high <- omw
  # nMR <- matrix(data = NA, nrow = mw, ncol = omw)
  # x1 <- 1:mw
  # for (i in 1:omw) {
  #   Ytw <- nsmY[i:(i + mw - 1)]
  #   nMR[, i] <- Ytw
  # }
  # nARR <- numeric()
  # nSD <- numeric()
  # nSK <- numeric()
  # nKURT <- numeric()
  # nACF <- numeric()
  # nDENSITYRATIO <- numeric()
  # nSPECT <- matrix(0, nrow = omw, ncol = ncol(nMR))
  # nCV <- numeric()
  # smARall <- numeric()
  # smARmaxeig <- numeric()
  # detB <- numeric()
  # ARn <- numeric()
  # nSD <- apply(nMR, 2, sd, na.rm = TRUE)
  # for (i in 1:ncol(nMR)) {
  #   if(length(which(diff(nMR[, i])!=0))>0){
  #     nYR <- ar.ols(nMR[, i], aic = FALSE, order.max = 1, dmean = FALSE,
  #                   intercept = FALSE)
  #     nARR[i] <- nYR$ar
  #     nSK[i] <- abs(moments::skewness(nMR[, i], na.rm = TRUE))
  #     nKURT[i] <- moments::kurtosis(nMR[, i], na.rm = TRUE)
  #     nCV[i] <- nSD[i]/mean(nMR[, i])
  #     ACF <- acf(nMR[, i], lag.max = 1, type = c("correlation"),
  #                plot = FALSE)
  #     nACF[i] <- ACF$acf[2]
  #     spectfft <- spec.ar(nMR[, i], n.freq = omw, plot = FALSE,
  #                         order = 1)
  #     nSPECT[, i] <- spectfft$spec
  #     nDENSITYRATIO[i] <- spectfft$spec[low]/spectfft$spec[high]
  #     if (AR_n) {
  #       ARall <- ar.ols(nMR[, i], aic = TRUE, order.max = 6,
  #                       demean = F, intercept = F)
  #       smARall[i] <- ARall$ar[1]
  #       ARn[i] <- ARall$order
  #       roots <- Mod(polyroot(c(rev(-ARall$ar), 1)))
  #       smARmaxeig[i] <- max(roots)
  #       detB[i] <- (prod(roots))^(2/ARn[i])
  #     }
  #   }else{ nYR<-NA
  #   nARR[i] <- NA
  #   nSK[i] <- NA
  #   nKURT[i] <- NA
  #   nCV[i] <- NA
  #   nACF[i] <- NA
  #   nSPECT[, i] <- NA
  #   nDENSITYRATIO[i] <- NA
  #   if (AR_n) {
  #     ARall <- ar.ols(nMR[, i], aic = TRUE, order.max = 6,
  #                     demean = F, intercept = F)
  #     smARall[i] <- ARall$ar[1]
  #     ARn[i] <- ARall$order
  #     roots <- Mod(polyroot(c(rev(-ARall$ar), 1)))
  #     smARmaxeig[i] <- max(roots)
  #     detB[i] <- (prod(roots))^(2/ARn[i])
  #   }
  #   }
  # }
  # nRETURNRATE = 1/nARR
  # timevec <- seq(1, length(nARR))
  #
  # KtAR <- try(cor.test(timevec, nARR, alternative = c("two.sided"),
  #                      method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate)
  # KtACF <- try(cor.test(timevec, nACF, alternative = c("two.sided"),
  #                       method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate)
  # KtSD <- try(cor.test(timevec, nSD, alternative = c("two.sided"),
  #                      method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate)
  # KtSK <- try(cor.test(timevec, nSK, alternative = c("two.sided"),
  #                      method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate)
  # KtKU <- try(cor.test(timevec, nKURT, alternative = c("two.sided"),
  #                      method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate)
  # KtDENSITYRATIO <- try(cor.test(timevec, nDENSITYRATIO, alternative = c("two.sided"),
  #                                method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate)
  # KtRETURNRATE <- try(cor.test(timevec, nRETURNRATE, alternative = c("two.sided"),
  #                              method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate)
  # KtCV <- try(cor.test(timevec, nCV, alternative = c("two.sided"),
  #                      method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate)
  #
  # if(inherits(KtAR, "try-error")){ #error occurs if too few observations therefore is applicable to all measures
  #   warning("Correlation coefficents not returned as too few observations")
  #   KtAR <- NA
  #   KtSD <- NA
  #   KtSK <- NA
  #   KtKU <- NA
  #   KtCV <- NA
  #   KtDENSITYRATIO <- NA
  #   KtRETURNRATE <- NA
  #   KtACF <- NA
  # }
#   out.cor <- data.frame("ar1" = KtAR, "SD" = KtSD,"skew" = KtSK, "kurt" = KtKU,
#                         "cv" = KtCV, "rr" = KtRETURNRATE,  "dr" = KtDENSITYRATIO, "acf" = KtACF)
#
#     out.raw <- data.frame(timeindex[mw:length(nsmY)], nARR, nSD,
#                           nSK, nKURT, nCV, nRETURNRATE, nDENSITYRATIO, nACF)
#     colnames(out.raw) <- c("timeindex", "ar1", "SD", "skew", "kurt",
#                            "cv", "rr", "dr", "acf")
#     out <- list("cor" = out.cor,"raw" = out.raw)
#
#   return(out)
# }



{
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

  spectfft <- spec.ar(data[i:(i+winsize_true-1),2], n.freq =  dim(data)[1] - winsize_true + 1,
                      plot = FALSE,
                      order = 1)

  nDENSITYRATIO <- spectfft$spec[6]/spectfft$spec[winsize_true]

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

