#' Significance Testing of Rolling Window Early Warning Signals
#'
#' A function for identifying whether a warning has been generated from rolling early warning signal data using permutation tests. If a parallel connection is setup via \code{parallel} or \code{future} prior to usage of \code{perm_rollEWS()}, then the function is parallelised.
#'
#' @param data A dataframe where the first column is an equally spaced time vector and the remainder column are the time series to be assessed. If a two column dataframe is provided, and \code{variate = "uni"}, \code{uniEWS()} is called, whereas if number of columns exceeds two & \code{variate = "multi"}, then \code{multiEWS()} is called.
#' @param metrics String vector of early warning signal metrics to be assessed. For \code{variate = "uni"} these include: \code{"ar1"}, \code{"cv"}, \code{"SD"}, \code{"acf"}, \code{"rr"}, \code{"dr"}, \code{"skew"} and \code{"kurt"}. For \code{variate = "multi"}, pptions include: \code{"meanSD"}, \code{"maxSD"}, \code{"meanAR"}, \code{"maxAR"}, \code{"eigenMAF"}, \code{"mafAR"}, \code{"mafSD"}, \code{"pcaAR"}, \code{"pcaSD"}, \code{"eigenCOV"}, \code{"maxCOV"} and \code{"mutINFO"}.
#' @param winsize Numeric value. Defines the window size of the rolling window as a percentage of the time series length.
#' @param variate String. Is a \code{"uni"}variate or \code{"multi"}variate assessment to be made.
#' @param perm.meth String dictating the pseudo-randomisation technique to be used. Options include: "arima" (sampled from predictions of an ARIMA model), "red.noise" (red noise process using data mean, variance and autocorrelation coef) or "sample" (sampled from observed data without replacement).
#' @param iter Numeric value. The number of permutations.
#'
#' @returns A list containing up to two objects: EWS outputs through time (\code{EWS}), and an identifier string (\code{method}).
#' \item{EWS$raw}{Dataframe of EWS measurements through time. Each metric's evolution over time is returned in individual columns.}
#' \item{EWS$cor}{Dataframe of Kendall Tau correlations and permuted p-values.}
#' \item{EWS$dimred.ts}{Dataframe containing the dimension reduction time series. Only returned if \code{variate = "multi"}.}
#'
#' @examples
#'
#' data(simTransComms)
#'
#' #Permute p value for a multivariate
#' #community using a red.noise process
#'
#'
#' \dontrun{
#' #if parallelisation desired,
#' #this can be achieved using the
#' #below code
#' cl <- parallel::makeCluster(3)
#'
#' doParallel::registerDoParallel(cl)
#' }
#'
#  #(data trimmed for speed)
#' perm_multi <- perm_rollEWS(
#' data = simTransComms$community1[1:10,2:7],
#'  winsize = 75,
#'  variate = "multi",
#'  metrics = c("meanAR", "maxAR", "meanSD"),
#'  perm.meth = "red.noise",
#'  iter = 25)
#'
#' \dontrun{
#' parallel::stopCluster(cl)
#' }
#'
#'
#' #Permute p value for a univariate
#' #time series using resampling
#'
#' #(data trimmed for speed)
#' perm_uni <- perm_rollEWS(
#' data = simTransComms$community1[1:10,2:3],
#'  winsize = 75,
#'  variate = "uni",
#'  metrics = c("ar1", "SD", "skew"),
#'  perm.meth = "sample",
#'  iter = 25)
#'
#' @export
#' @importFrom foreach %dopar%

perm_rollEWS <- function(data, metrics, winsize = 50, variate = c("uni","multi"), perm.meth = "arima", iter = 500){

  data <- as.data.frame(data)

  perm.meth <- match.arg(perm.meth,choices=c("arima", "sample","red.noise"), several.ok = FALSE)
  variate <- match.arg(variate,choices=c("uni","multi"), several.ok = FALSE)

  if(variate == "uni"){
    metrics <- match.arg(metrics, choices = c("cv", "acf", "ar1", "dr", "rr", "skew","kurt","mean.size", "sd.size", "size.95","SD","trait"), several.ok=T)

    if(perm.meth == "arima"){
      fit <- forecast::auto.arima(stats::as.ts(data[,2]),allowdrift =F,seasonal = T)
      perm.df <- data

      for(perm in seq_len(iter)){ # for each permutation randomly sample from confidence interval at each time point
        new_col_name <- paste0("perm_", perm) # new column and colname for each permutation
        perm.df[,new_col_name] <- stats::arima.sim(n = length(data[,1]), as.list(stats::coef(fit)), sd = sqrt(fit$sigma2))
      }

    }

    if(perm.meth == "red.noise"){

      d1.ar1 <- tryCatch({stats::arima(stats::as.ts(data[,2]), order = c(1, 0, 0),optim.control = list(maxit = 1000),method="ML")$coef[1]
      },error = function(err){return(0)})

      perm.df <- data

      for (perm in seq_len(iter)) {
        new_col_name <- paste0("perm_", perm)
        perm.df[,new_col_name] <- red.noise.ts(data[,2],lag=d1.ar1,length=length(data[,1])) #permuted autocorrelated surrogates
      }
    }

    if(perm.meth == "sample"){
      ## Create permutation order
      perm.df <- data

      for (perm in seq_len(iter)){ # for each permutation randomly sample from ts with replacement at each time point
        new_col_name <- paste0("perm_", perm) # new column and colname for each permutation
        perm.df[,new_col_name] <- sample(x = data[,2],replace = F,size = length(data[,1]))
        while( any(rle(perm.df[,new_col_name])$lengths >= floor(length(perm.df[,new_col_name])*(winsize/100)))){ #if ts contains many zeroes or runs to prevent svd, prevent the sampling with replacement
          #while(mean( perm.df[,new_col_name] == min( perm.df[,new_col_name])) > 0.47 ){

          perm.df[,new_col_name] <- sample(x = data[,2],replace = F,size = length(data[,1]))
        }
      }

    }

    true.ews <- uniEWS(perm.df[,c(1,2)],metrics =metrics,method = "rolling",winsize = winsize)


      perm.ews <- foreach::foreach(perm = 3:dim(perm.df)[2], .combine = rbind,
                                   #.options.snow = opts,
                                   .export = "uniEWS") %dopar%
        {
          uniEWS(perm.df[,c(1,perm)],metrics = metrics,method = "rolling",
                 winsize = winsize)$EWS$cor
        }

  }else if(variate == "multi"){

    if(NCOL(data) <= 2){
      stop("Data only contains two columns. Multivariate EWS require 2+ timeseries")
    }

    metrics <- match.arg(metrics, choices =  c("meanAR","maxAR","meanSD","maxSD","eigenMAF","mafAR","mafSD","pcaAR","pcaSD","eigenCOV","maxCOV","mutINFO"), several.ok=T)

    if(perm.meth == "arima"){

      perm.ls <- lapply(1:iter,FUN = function(perm){ #create list of permuted time series

        sapply(2:dim(data)[2], function(ts){
          fit <- forecast::auto.arima(stats::as.ts(data[,ts]),allowdrift =F,seasonal = T)
          stats::arima.sim(n = length(data[,1]), as.list(stats::coef(fit)), sd = sqrt(fit$sigma2))
        })
      })
    }

    if(perm.meth == "red.noise"){

      perm.ls <- lapply(1:iter,FUN = function(perm){

        sapply(2:dim(data)[2], function(ts){
          d1.ar1 <- tryCatch({stats::arima(stats::as.ts(data[,ts]), order = c(1, 0, 0),optim.control = list(maxit = 1000),method="ML")$coef[1]
          },error = function(err){return(0)})
          red.noise.ts(ts = data[,ts],lag=d1.ar1,length=length(data[,1]))
        })
      })

    }

    if(perm.meth == "sample"){

      perm.ls <- lapply(X = 1:iter,FUN = function(X){
        sapply(2:dim(data)[2], function(ts){
          s_out <- sample(x = data[,ts],replace = F,size = length(data[,1]))
          while(mean(s_out == min(s_out)) > 0.47 | any(rle(s_out)$lengths >= floor(length(s_out)*winsize/100))){ #if ts contains many zeroes or runs to prevent svd, prevent the sampling with replacement
            s_out <- sample(x = data[,ts],replace = F,size = length(data[,1]))
          }
          return(s_out)
        })
      })

    }

    true.ews <- multiEWS(data,metrics = metrics,method = "rolling",
                                     winsize = winsize)

      perm.ews <- foreach::foreach(perm = 1:iter, .combine = rbind,
                                   #.options.snow = opts,
                                   .export = "multiEWS") %dopar%
        {
          multiEWS(cbind(data[,1],perm.ls[[perm]]),metrics = metrics,method = "rolling",
                   winsize = winsize)$EWS$cor
        }

  }

  out.ews <- sapply(seq_len(length(perm.ews)), FUN = function(x){
    ifelse(!all(is.na(perm.ews[,x])),stats::ecdf(perm.ews[,x])(true.ews$EWS$cor[x]),NA)
  })

  true.ews$EWS$cor["perm_pvalue",] <- 1-out.ews

  return(true.ews)

}

#' Generate A Red Noise Process
#'
#' Using a reference timeseries, simulates a rednoise process of the same length and autocorrelation as the reference.
#'
#' @param ts A reference time series vector
#' @param lag Numeric The desired autocorrelation lag
#' @param length Numeric. Length of process
#'
#' @returns A simulated red noise process vector
#' @keywords internal
#' @noRd


red.noise.ts <- function(ts,lag,length){
  x  <- rep(NA, length)
  ts <- stats::rnorm(length,mean = mean(ts,na.rm=T),sd=sd(ts,na.rm=T))
  x[1] <- ts[1]
  for(i in 2:length){
    x[i] <- lag*x[i-1] + ((1-lag^2)^0.5)*ts[i]
  }
  return(x) #create rednoise process from a time series' mean, variance and autocorrelation coef
}
