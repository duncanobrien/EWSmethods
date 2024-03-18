#' Multivariate Early Warning Signals
#'
#' @param data A n x m dataframe with the first column being time indices and the remainder of the columns being species abundances.
#' @param metrics String vector of early warning signal metrics to be assessed.  Options include: \code{"meanSD"}, \code{"maxSD"}, \code{"meanAR"}, \code{"maxAR"}, \code{"eigenMAF"}, \code{"mafAR"}, \code{"mafSD"}, \code{"pcaAR"}, \code{"pcaSD"}, \code{"eigenCOV"}, \code{"maxCOV"} and \code{"mutINFO"}.
#' @param method A single string stating either \code{"expanding"} or \code{"rolling"}.\code{"expanding"} calls the composite, expanding window EWS assessment. \code{"rolling"} calls the typical, rolling window EWS assessment.
#' @param winsize Numeric. If "method" = \code{"rolling"}, defines the window size of the rolling window as a percentage of the time series length.
#' @param burn_in Numeric. If "method" = \code{"expanding"}, defines the number of data points to 'train' signals prior to EWS assessment.
#' @param threshold Numeric. If "method" = \code{"expanding"}, defines the threshold*sigma warning threshold.
#' @param tail.direction A string. If "method" = \code{"expanding"}, should both positive and negative thresholds be considered.

#' @return A list containing \code{"raw"} (the early warning signals through time) and \code{"dimred.ts"} (the dimension reduction time series)
#'
#' @importFrom stats ar.ols
#' @importFrom stats cor.test
#' @importFrom stats cov
#' @importFrom stats na.omit
#' @importFrom stats prcomp
#' @importFrom dplyr %>%
#' @importFrom dplyr .data
#'
#' @keywords internal
#' @noRd


wMAF <- function(data, metrics = c("meanAR","maxAR","meanSD","maxSD","eigenMAF","mafAR","mafSD","pcaAR","pcaSD","eigenCOV","maxCOV","mutINFO"),
                 method = c("rolling","expanding"),winsize , burn_in = 5, tail.direction = "one.tailed",threshold =2){

  meth <- match.arg(method,choices = c("rolling","expanding"))
  metrics <-match.arg(metrics, choices =  c("meanAR","maxAR","meanSD","maxSD","eigenMAF","mafAR","mafSD","pcaAR","pcaSD","eigenCOV","maxCOV","mutINFO"), several.ok=T)

  if(length(class(data)) > 1 & isTRUE(is.data.frame(data))){
  data <- as.data.frame(data)
  } #allows tibbles to be used

  RES<-list()

  if(meth == "rolling"){
    winsize_true <- round(dim(data)[1] * winsize/100)
  for(i in 1:(dim(data)[1]-winsize_true+1)){

    tmp.maf <- maf(x=data[i:(i+winsize_true-1),-1])
    tmp.pca <- summary(prcomp(scale(data[i:(i+winsize_true-1),-1]))) #summary required to extract explained variance

    mean.ar <- mean(sapply(2:dim(data)[2],function(x){ar.ols(data[i:(i+winsize_true-1),x], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]}))
    max.ar <- max(sapply(2:dim(data)[2],function(x){ar.ols(data[i:(i+winsize_true-1),x], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]}))
    mean.sd <- mean(sapply(2:dim(data)[2],function(x){sd(data[i:(i+winsize_true-1),x])}))
    max.sd <- max(sapply(2:dim(data)[2],function(x){sd(data[i:(i+winsize_true-1),x])}))
    eigen <- min(tmp.maf$eigen.values/sum(tmp.maf$eigen.values))
    ar <- ar.ols(tmp.maf$mafs[,1], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]
    sd <- sd(tmp.maf$mafs[,1])
    pca.ar <- ar.ols(tmp.pca$x[,1], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]
    pca.sd <- sd(tmp.pca$x[,1])
    eigen.cov <- max(eigen(cov(scale(data[i:(i+winsize_true-1),-1])))$values)
    max.cov <- max(cov(scale(data[i:(i+winsize_true-1),-1]))[lower.tri(cov(data[i:(i+winsize_true-1),-1]),diag = F)])
    #expl.sd <- tmp.pca$importance[2,1]
    mi <- MI(data[i:(i+winsize_true-1),-1])

    RES[[i]] <- data.frame("time" = data[i+winsize_true-1,1],
                           "meanAR" = mean.ar,
                           "maxAR" = max.ar,
                           "meanSD" = mean.sd,
                           "maxSD" = max.sd,
                           "eigenMAF" = -1*eigen, #as expected to decrease
                           "mafAR" = ar,
                           "mafSD" = sd,
                           "pcaAR" = pca.ar,
                           "pcaSD" = pca.sd,
                           #"explSD" = expl.sd,
                           "eigenCOV" = eigen.cov,
                           "maxCOV" = max.cov,
                           "mutINFO" = mi)
  }
    output<-do.call("rbind", RES)

    out.cor <-data.frame("meanAR" = tryCatch({cor.test(as.numeric(output$time), output$meanAR, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){ warning("Correlation coefficents not returned as too few observations"); return(NA)}),
                          "maxAR" = tryCatch({cor.test(as.numeric(output$time), output$maxAR, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                          "meanSD" = tryCatch({cor.test(as.numeric(output$time), output$meanSD, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                          "maxSD" = tryCatch({cor.test(as.numeric(output$time), output$maxSD, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                          "eigenMAF" = tryCatch({cor.test(as.numeric(output$time), output$eigenMAF, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                          "mafAR" = tryCatch({cor.test(as.numeric(output$time), output$mafAR, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                          "mafSD" = tryCatch({cor.test(as.numeric(output$time), output$mafSD, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                          "pcaAR" = tryCatch({cor.test(as.numeric(output$time), output$pcaAR, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                          "pcaSD" = tryCatch({cor.test(as.numeric(output$time), output$pcaSD, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                          #"explSD" = tryCatch({cor.test(as.numeric(output$time), output$explSD, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                          "eigenCOV" = tryCatch({cor.test(as.numeric(output$time), output$eigenCOV, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                          "maxCOV" = tryCatch({cor.test(as.numeric(output$time), output$maxCOV, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}),
                          "mutINFO" = tryCatch({cor.test(as.numeric(output$time), output$mutINFO, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate}, error = function(e){return(NA)}))

  }

  if(meth == "expanding"){

    roll.eigen <-NULL
    roll.ar<-NULL
    roll.sd<-NULL
    roll.pca.ar <- NULL
    roll.pca.sd <- NULL
    roll.cov <- NULL
    roll.expl.sd <- NULL
    roll.mean.ar <- NULL
    roll.max.ar <- NULL
    roll.mean.sd <- NULL
    roll.max.sd <- NULL
    roll.cov.eigen <- NULL
    roll.mi <- NULL

    for(i in (burn_in):dim(data)[1]){

      tmp.maf <- maf(x=data[1:i,-1])
      tmp.pca <- summary(prcomp(x=scale(data[1:i,-1])))

      roll.mean.ar[[i]]<-mean(sapply(2:dim(data)[2],function(x){ar.ols(data[1:i,x], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]}))
      mean.ar<-(mean(sapply(2:dim(data)[2],function(x){ar.ols(data[1:i,x], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]}))-mean(unlist(roll.mean.ar), na.rm=TRUE))/sd(unlist(roll.mean.ar), na.rm = TRUE)

      roll.max.ar[[i]]<-max(sapply(2:dim(data)[2],function(x){ar.ols(data[1:i,x], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]}))
      max.ar<-(max(sapply(2:dim(data)[2],function(x){ar.ols(data[1:i,x], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]}))-mean(unlist(roll.max.ar), na.rm=TRUE))/sd(unlist(roll.max.ar), na.rm = TRUE)

      roll.mean.sd[[i]]<-mean(sapply(2:dim(data)[2],function(x){sd(data[1:i,x])}))
      mean.sd<-(mean(sapply(2:dim(data)[2],function(x){sd(data[1:i,x])}))-mean(unlist(roll.mean.sd), na.rm=TRUE))/sd(unlist(roll.mean.sd), na.rm = TRUE)

      roll.max.sd[[i]]<-max(sapply(2:dim(data)[2],function(x){sd(data[1:i,x])}))
      max.sd<-(max(sapply(2:dim(data)[2],function(x){sd(data[1:i,x])}))-mean(unlist(roll.max.sd), na.rm=TRUE))/sd(unlist(roll.max.sd), na.rm = TRUE)

      roll.eigen[[i]]<-min(tmp.maf$eigen.values/sum(tmp.maf$eigen.values))
      eigen<-(min(tmp.maf$eigen.values/sum(tmp.maf$eigen.values))-mean(unlist(roll.eigen), na.rm=TRUE))/sd(unlist(roll.eigen), na.rm = TRUE)

      roll.ar[[i]]<-ar.ols(tmp.maf$mafs[,1], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]
      ar<-(ar.ols(tmp.maf$mafs[,1], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]-mean(unlist(roll.ar), na.rm=TRUE))/sd(unlist(roll.ar), na.rm = TRUE)

      roll.sd[[i]]<-sd(tmp.maf$mafs[,1])
      sd<-(sd(tmp.maf$mafs[,1])-mean(unlist(roll.sd), na.rm=TRUE))/sd(unlist(roll.sd), na.rm = TRUE)

      roll.pca.ar[[i]] <-  ar.ols(tmp.pca$x[,1], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]
      pca.ar<-(ar.ols(tmp.pca$x[,1], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]-mean(unlist(roll.pca.ar), na.rm=TRUE))/sd(unlist(roll.pca.ar), na.rm = TRUE)

      roll.pca.sd[[i]]<-sd(tmp.pca$x[,1])
      pca.sd<-(sd(tmp.pca$x[,1])-mean(unlist(roll.pca.sd), na.rm=TRUE))/sd(unlist(roll.pca.sd), na.rm = TRUE)

      roll.cov[[i]]<-max(cov(scale(data[1:i,-1]))[lower.tri(cov(data[1:i,-1]),diag = F)])
      max.cov<-(max(cov(scale(data[1:i,-1]))[lower.tri(cov(data[1:i,-1]),diag = F)])-mean(unlist(roll.cov), na.rm=TRUE))/sd(unlist(roll.cov), na.rm = TRUE)

      roll.cov.eigen[[i]]<-max(eigen(cov(scale(data[1:i,-1])))$values)
      eigen.cov<-(max(eigen(cov(scale(data[1:i,-1])))$values)-mean(unlist(roll.cov.eigen), na.rm=TRUE))/sd(unlist(roll.cov.eigen), na.rm = TRUE)

      #roll.expl.sd[[i]]<- tmp.pca$importance[2,1]
      #expl.sd<-(tmp.pca$importance[2,1]-mean(unlist(roll.expl.sd), na.rm=TRUE))/sd(unlist(roll.expl.sd), na.rm = TRUE)

      roll.mi[[i]]<- MI(data[1:i,-1])
      mi<-(MI(data[1:i,-1])-mean(unlist(roll.mi), na.rm=TRUE))/sd(unlist(roll.mi), na.rm = TRUE)

      RES[[i]] <- data.frame(time =data[i,1],
                             "meanAR" = mean.ar,
                             "maxAR" = max.ar,
                             "meanSD" = mean.sd,
                             "maxSD" = max.sd,
                             "eigenMAF" = -1*eigen, #as expected to decrease
                             "mafAR" = ar,
                             "mafSD" = sd,
                             "pcaAR" = pca.ar,
                             "pcaSD" = pca.sd,
                             #"explSD" = expl.sd,
                             "eigenCOV" = eigen.cov,
                             "maxCOV" = max.cov,
                             "mutINFO" = mi)

    }
    results<-do.call("rbind", RES)

    output<-data.frame(results) %>%
      tidyr::pivot_longer(-c("time"), names_to = "metric.code",values_to = "metric.score") %>%
      dplyr::group_by(.data$metric.code) %>% dplyr::arrange(.data$time,.by_group = TRUE) %>%
      dplyr::mutate(rolling.mean = rolling_mean(.data$metric.score),
             rolling.sd = rolling_sd(.data$metric.score))
    output$threshold.crossed<-NA

    if(tail.direction == "two.tailed"){
      output$threshold.crossed[which(output$metric.score>(output$rolling.mean+(threshold*output$rolling.sd))|output$metric.score<(output$rolling.mean-(threshold*output$rolling.sd)))]<-1
      output$threshold.crossed[is.na(output$threshold.crossed)] <- 0
    }else{
      output$threshold.crossed[which(output$metric.score>(output$rolling.mean+(threshold*output$rolling.sd)))]<-1
      output$threshold.crossed[is.na(output$threshold.crossed)] <- 0
    }

  }

  dimred.ts <- data.frame("time" = data[,1],"maf1" = maf(x=data[,-1])$mafs[,1],"pc1" = prcomp(scale(data[,-1]))$x[,1])
  if(meth == "rolling"){
    return(list("raw" = output, "cor" = out.cor,"dimred.ts" = dimred.ts))
  }else{
  return(list("raw" = output,"dimred.ts" = dimred.ts))
  }
}


#' Maximum/Minimum Autocorrelation Factors

#' @param x dataframe A n x m dataframe species (columns) abundances through time (rows)
#' @return out A list containing the input data, the dimension reduction time series, rotations, autocorrelation and eigenvalues
#' @keywords internal
#' @noRd

maf <- function(x){
  if (isTRUE(is.data.frame(x)) || (inherits(x,"matrix") && dim(x)[2]>1)){
    p = dim(x)[2]
    n = dim(x)[1]
    x = scale(x)
    svd = svd(cov(x))
    #svd = svd(cov(x)*(n-1)) #If you want to sum of the timesteps to be 1

    a = svd$u%*%diag(svd$d^(-0.5))%*%t(svd$u)

    y = x%*%a
    cov.zd = cov(apply(y,2,diff))*(n-2)

    svd.zd = svd(cov.zd/(n-1))
    u = svd.zd$u[,p:1]
    aa = a%*%u
    aa = apply(aa,2,function(x) {x/sqrt(sum(x^2))})

    maf = x%*%aa
    neg.time.cor = diag(cov(maf,matrix(rep(1:n,p),n,p)))<0
    mafs = t(apply(maf,1,function(x) {(-neg.time.cor+!neg.time.cor)*x}))
    aa = t(apply(aa, 1,function(x) {(-neg.time.cor+!neg.time.cor)*x}))

    d = svd.zd$d[p:1]
  } else {
    warning("x is not a matrix or a data.frame, no MAF transform performed.")
    mafs = as.matrix(x)
    aa = as.matrix(1); a.i = as.matrix(1); d = as.matrix(1)
  }
  out = list(x=x, mafs=maf, rotation=aa, autocor=1 - d/2, eigen.values = d )
  class(out) = c("Maf")
  out
}

#' Calculation Mutual Information
#'
#' @param data A numeric matrix/dataframe.
#' @param bins Numeric. The number of bins to discretise over.
#' @param method String. The entropy estimator. Choices are \code{"emp"}, \code{"mm"},\code{"shrink"} or \code{"sg"}
#'
#' @importFrom infotheo discretize
#' @importFrom infotheo multiinformation

#' @noRd
#'

MI <- function(data, bins = 5, method = "emp"){

  if(is.vector(data)){
    stop("data must be a numeric matrix/dataframe")
  }
  dat <- infotheo::discretize(data,nbins = bins)

  out <- infotheo::multiinformation(dat,method = method)

  #out <- NlinTS::mi_disc(dat,normalize = T)

  #out <-sum(sapply(colnames(dat),FUN = function(x) infotheo::entropy(dat[,x],method = method))) - infotheo::entropy(dat, method = method)


  return(out)
}

# MI <- function(data, max.lag = 12, bins = 10, method = "emp"){
#
#   if(is.vector(data)){
#     stop("data must be a numeric matrix/dataframe")
#   }
#
#   init.mi <- infotheo::multiinformation(infotheo::discretize(data,nbins = bins),method = method)
#
#   prep_data <- do.call("rbind", lapply(1:dim(data)[2],function(i){
#
#     sub.dat <- data.frame("lag" = seq(0,max.lag),"mi" = c(tseriesChaos::mutual(data[,i],partitions = bins,lag.max = max.lag,plot = F)))
#
#     #fm <- lm(log1p(mi) ~ lag,data = sub.dat)
#
#     #return(
#       #coefficients(
#       # nls(mi ~ SSasymp(lag, Asym, R0, lrc), data = sub.dat,
#       #     control=nls.control(maxiter = 1000, tol = 1e-08,warnOnly = T)
#
#          # nls(mi ~ 1+a^lag, data = sub.dat,start = list(a =1))
#           #coef(fm)[2]*log(0.5)
#
#           #)
#       #)
#   out <- infotheo::multiinformation(dat,method = method)
#   return(out)
# }
#
#     return(sub.dat)
#   }) ) |>
#     as.data.frame()
#   prep_data$data_source <- rep(colnames(data),each = length(0:max.lag))
#
#   #fit <- coefficients(nls(mi ~ stats::SSasymp(lag, yf, y0, log_alpha), data = prep_data))[1]
#
#   #fit <- lm(log1p(mi) ~ lag,data = prep_data[sort(prep_data$lag),])
#
#   fm0 <- lm(log1p(mi) ~ lag, prep_data[sort(prep_data$lag),])
#   st <- list(a = exp(coef(fm0)[[1]]), b = -coef(fm0)[[2]])
#
#   fit <- coefficients(nls(mi ~ a * exp(-b * lag ), data = prep_data[sort(prep_data$lag),],
#                           start = st, control = nls.control(maxiter=1000)))[1]
#
#    # fit <- coefficients(
#    #   nls(mi ~ SSasymp(lag, Asym, R0, lrc), data = prep_data[sort(prep_data$lag),])
#    #   )[3]
#
#   return(fit*log(0.5))
#   #return(coef(fit)[2]*log(0.5))
# }
