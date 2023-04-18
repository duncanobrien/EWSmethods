#' Multivariate Early Warning Signal Assessment
#'
#' A single function for performing early warning signal (EWS) assessment on multivariate systems where multiple time series have been measured. Both methods of EWS assessment can be performed (rolling or expanding windows) with the assessments returned as a dataframe. The two methods of dimension reduction used to perform these assessments are Principal Component Analysis and Maximum/Minimum Autocorrelation Factors.
#'
#' @param data A dataframe where the first column is an equally spaced time vector and all other columns are individual time series. These could be different species, populations or measurements.
#' @param metrics String vector of early warning signal metrics to be assessed.  Options include: \code{"meanSD"}, \code{"maxSD"}, \code{"meanAR"}, \code{"maxAR"}, \code{"eigenMAF"}, \code{"mafAR"}, \code{"mafSD"}, \code{"pcaAR"}, \code{"pcaSD"}, \code{"eigenCOV"}, \code{"maxCOV"} and \code{"mutINFO"}.
#' @param method Single string of either \code{"expanding"} or \code{"rolling"}. \code{"expanding"} calls composite, expanding window EWS assessment. \code{"rolling"} calls typical, rolling window EWS assessment.
#' @param winsize Numeric value. If \code{method = "rolling"}, defines the window size of the rolling window as a percentage of the time series' length.
#' @param burn_in Numeric value. If \code{method = "expanding"}, defines the number of data points to 'train' signals prior to EWS assessment.
#' @param threshold Numeric value of either \code{1} or \code{2}. Threshold*sigma is the value which, if the EWS strength exceeds it, constitutes a "signal".
#' @param tail.direction String of either \code{"one.tailed"} or \code{"two.tailed"}. \code{"one.tailed"} only indicates a warning if positive threshold sigma exceeded. \code{"two.tailed"} indicates a warning if positive OR negative threshold*sigma exceeded.
#'
#' @returns A list containing up to two objects: EWS outputs through time (\code{EWS}), and an identifier string (\code{method}).
#' \item{EWS$raw}{Dataframe of EWS measurements through time. If \code{method = "expanding"}, then each metric has been rbound into a single dataframe and extra columns are provided indicating whether the threshold*sigma value has been exceeded (i.e. \code{"threshold.crossed"}). If \code{method = "rolling"}, then each metric's evolution over time is returned in individual columns.}
#' \item{EWS$dimred.ts}{Dataframe containing the dimension reduction time series}
#' \item{EWS$cor}{Dataframe of Kendall Tau correlations. Only returned if \code{method = "rolling"}.}
#'
#' @examples
#' #Generate a random five species, non-transitioning
#' #ecosystem with 50 years of monitoring data.
#'
#' spp_data <- matrix(nrow = 50, ncol = 5)
#' spp_data <- sapply(1:dim(spp_data)[2], function(x){
#'  spp_data[,x] <- rnorm(50,mean=20,sd=5)})
#'  multi_spp_data <- as.data.frame(cbind("time" =
#'  seq(1:50), spp_data))
#'
#' #Rolling window early warning signal assessment of
#' #the ecosystem.
#'
#' roll_ews <- multiEWS(
#'  data = multi_spp_data,
#'  method = "rolling",
#'  winsize = 50)
#'
#' #Expanding window early warning signal assessment of
#' #the ecosystem.
#'
#' exp_ews <- multiEWS(
#'  data = multi_spp_data,
#'  method = "expanding",
#'  burn_in = 10)
#'
#' @export
multiEWS <- function(data, metrics = c("meanAR","maxAR","meanSD","maxSD","eigenMAF","mafAR","mafSD","pcaAR","pcaSD","eigenCOV","maxCOV","mutINFO"),
                     method = c("expanding","rolling"),winsize = 50,
                     burn_in = 5, threshold = 2,
                     tail.direction = "one.tailed"){

  method <- match.arg(method,choices = c("rolling","expanding"))
  metrics <-match.arg(metrics, choices =  c("meanAR","maxAR","meanSD","maxSD","eigenMAF","mafAR","mafSD","pcaAR","pcaSD","eigenCOV","maxCOV","mutINFO"), several.ok=T)

  if(any(is.na(data))){
    stop('Data contains missing values. Interpolation of missing values is recommended')
  }

  if(method == "expanding"){

    bind.res <- wMAF(data = data,metrics=metrics,threshold = threshold, burn_in = burn_in,
                     tail.direction = tail.direction,
                     method = "expanding")

    bind.res$raw <- subset(bind.res$raw,bind.res$raw$metric.code %in% metrics)
    bind.res$raw$str<-(bind.res$raw$metric.score-bind.res$raw$rolling.mean)/bind.res$raw$rolling.sd
    bind.res$raw<-as.data.frame(bind.res$raw)

    out <- list("EWS" = bind.res, "method" = method,"threshold" = threshold, "tail.direction" = tail.direction)
    class(out) <- c("EWSmethods","expEWS","multiEWS")

  }

  if(method == "rolling"){

    bind.res <- wMAF(data = data,metrics=metrics,winsize = winsize,method = "rolling")

    bind.res$raw <- bind.res$raw[,c("time",metrics)]
    bind.res$cor <- as.data.frame(bind.res$cor[,c(metrics)])
    if(length(bind.res$cor) == 1){
      names(bind.res$cor) <- metrics
    }

    out <- list("EWS" = bind.res, "method" = method)
    class(out) <- c("EWSmethods","rollEWS","multiEWS")

  }
  return(out)
}
