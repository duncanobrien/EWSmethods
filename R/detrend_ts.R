#' Detrend Time Series
#'
#' Removes directional signals from time series using loess, linear regression or gaussian detrending.
#'
#' @param data The dataframe to be detrended. The first column must be a vector of dates with all other columns the individual time series.
#' @param method The method of detrending. Options include \code{"linear"} (residuals of a linear regression), \code{loess} (smoothing by local polynomial regression), \code{gaussian} (smoothing by a gaussian kernel), or \code{first.difference}.
#' @param bandwidth If \code{method = "gaussian"}, dictates the bandwidth of the gaussian kernel. If \code{NULL}, this is estimated from the data.
#' @param span If \code{method = "loess"}, controls the degree of smoothing. If \code{NULL}, is set to 0.25.
#' @param degree If \code{method = "loess"}, specifies the degree polynomials allowed. Options are normally \code{1} or \code{2}.
#'
#' @returns Dataframe of deseasoned time series.
#'
#' @examples
#' #Generate five random monthly time series
#' #of 5 years length.
#'
#' spp_data <- matrix(nrow = 5*12, ncol = 5)
#' spp_data <- sapply(1:dim(spp_data)[2], function(x){
#' spp_data[,x] <- rnorm(5*12,mean=20,sd=5)})
#' multi_spp_data <- cbind("time" =
#'  seq(as.Date('2000/01/01'), as.Date('2004/12/01'), by="month"),
#'    as.data.frame(spp_data))
#'
#' detrend_dat <- detrend_ts(data = multi_spp_data,
#' method = "gaussian",
#' bandwidth = 2)
#'
#' @export


detrend_ts <- function(data, method = "linear", bandwidth = NULL, span = NULL, degree = 2){

  if(length(dim(data)) == 1){
    stop("not right format of data input")
  }

  meth <- match.arg(method, choices = c("linear","loess","gaussian","first.difference"))

  detrend_dat <- data
  timevec <- as.numeric(data[,1])

  if(meth == "linear"){

    detrend_dat[,-1] <- sapply(detrend_dat[,-1],FUN = function(x){
    resid(lm(x ~ timevec))
          })

      }else if(meth == "loess"){

        detrend_dat[,-1] <- sapply(detrend_dat[,-1],FUN = function(x){

          loess_ts <- stats::loess(x ~ timevec, span = span, degree = degree,
                normalize = FALSE, family = "gaussian")
          loess_ts_tmp <- predict(loess_ts, newdata = data.frame(x = timevec), se = FALSE)
          x - loess_ts_tmp
        })

    }else if(meth == "gaussian"){

      detrend_dat[,-1] <- sapply(detrend_dat[,-1],FUN = function(x){
        if (is.null(bandwidth)) {
          bw <- round(stats::bw.nrd0(timevec))
        }else{
          bw <- round(length(x) * bandwidth/100)
        }
        gauss_ts <- stats::ksmooth( timevec, x, kernel = "normal", bandwidth = bw, range.x = range(timevec),
                            x.points = timevec)
        x - gauss_ts$y
      })

    }else if(meth == "first.difference") {

      detrend_dat[,-1] <- sapply(detrend_dat[,-1],FUN = function(x){
        c(NA,diff(x))
      })

    }

  return(detrend_dat)

}
