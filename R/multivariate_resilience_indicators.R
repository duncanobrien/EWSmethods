#' Multivariate Early Warning Signals
#' @param data A n x m dataframe with the first column time indices and remainder of columns species abundances.
#' @param method c("expanding","rolling")."expanding" calls composite, expanding window EWS assessment. "rolling" calls typical, rolling window EWS assessment.
#' @param winsize Numeric. If "method" = "rolling",defines the window size of the rolling window.
#' @param burn_in Numeric. If "method" = "expanding, defines the number of data points to 'train' signals prior to EWS assessment.
#' @param threshold Numeric. If "method" = "expanding, defines the threshold*sigma warning threshold.
#' @param tail.direction A string. If "method" = "expanding, should both positive and negative thresholds be considered.

#' @returns A list containing "raw" (the early warning signals through time) and "dimred.ts" (the dimension reduction time series)
#' @export
#'
wMAF <- function(data,method = c("rolling","expanding"),winsize , burn_in = 5, tail.direction = "one.tailed",threshold =2){

  meth <- match.arg(method,choices = c("rolling","expanding"))

  RES<-list()

  if(meth == "rolling"){

  for(i in 1:(dim(data)[1]-winsize+1)){

    tmp.maf <- maf(x=data[i:(i+winsize-1),-1])
    tmp.pca <- summary(prcomp(scale(data[i:(i+winsize-1),-1]))) #summary required to extract explained variance

    mean.ar <- mean(sapply(2:dim(data)[2],function(x){ar.ols(data[i:(i+winsize-1),x], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]}))
    max.ar <- max(sapply(2:dim(data)[2],function(x){ar.ols(data[i:(i+winsize-1),x], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]}))
    mean.sd <- mean(sapply(2:dim(data)[2],function(x){sd(data[i:(i+winsize-1),x])}))
    max.sd <- max(sapply(2:dim(data)[2],function(x){sd(data[i:(i+winsize-1),x])}))
    eigen <- min(tmp.maf$eigen.values/sum(tmp.maf$eigen.values))
    ar <- ar.ols(tmp.maf$mafs[,1], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]
    sd <- sd(tmp.maf$mafs[,1])
    pca.ar <- ar.ols(tmp.pca$x[,1], aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]
    pca.sd <- sd(tmp.pca$x[,1])
    max.cov <- max(cov(scale(data[i:(i+winsize-1),-1]))[lower.tri(cov(data[i:(i+winsize-1),-1]),diag = F)])
    expl.sd <- tmp.pca$importance[2,1]

    RES[[i]] <- data.frame(time = data[i+winsize-1,1],
                           "meanAR" = mean.ar,
                           "maxAR" = max.ar,
                           "meanSD" = mean.sd,
                           "maxSD" = max.sd,
                           "mafEIGENval" = eigen,
                           "mafAR" = ar,
                           "mafSD" = sd,
                           "pcaAR" = pca.ar,
                           "pcaSD" = pca.sd,
                           "explSD" = expl.sd,
                           "maxCOV" = max.cov)
  }
    output<-do.call("rbind", RES)

    out.cor <- data.frame("meanAR" = cor.test(as.numeric(output$time), output$meanAR, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate,
                          "maxAR" = cor.test(as.numeric(output$time), output$maxAR, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate,
                          "meanSD" = cor.test(as.numeric(output$time), output$meanSD, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate,
                          "maxSD" = cor.test(as.numeric(output$time), output$maxSD, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate,
                          "mafEIGENval" = cor.test(as.numeric(output$time), output$mafEIGENval, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate,
                          "mafAR" = cor.test(as.numeric(output$time), output$mafAR, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate,
                          "mafSD" = cor.test(as.numeric(output$time), output$mafSD, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate,
                          "pcaAR" = cor.test(as.numeric(output$time), output$pcaAR, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate,
                          "pcaSD" = cor.test(as.numeric(output$time), output$pcaSD, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate,
                          "explSD" = cor.test(as.numeric(output$time), output$explSD, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate,
                          "maxCOV" = cor.test(as.numeric(output$time), output$maxCOV, alternative = c("two.sided"), method = c("kendall"), conf.level = 0.95,na.action = na.omit)$estimate)

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

      roll.expl.sd[[i]]<- tmp.pca$importance[2,1]
      expl.sd<-(tmp.pca$importance[2,1]-mean(unlist(roll.expl.sd), na.rm=TRUE))/sd(unlist(roll.expl.sd), na.rm = TRUE)

      RES[[i]] <- data.frame(time =data[i,1],
                             "meanAR" = mean.ar,
                             "maxAR" = max.ar,
                             "meanSD" = mean.sd,
                             "maxSD" = max.sd,
                             "mafEIGENval" = eigen,
                             "mafAR" = ar,
                             "mafSD" = sd,
                             "pcaAR" = pca.ar,
                             "pcaSD" = pca.sd,
                             "explSD" = expl.sd,
                             "maxCOV" = max.cov)

    }
    results<-do.call("rbind", RES)

    output<-data.frame(results) %>%
      tidyr::pivot_longer(-time, names_to = "metric.code",values_to = "metric.score") %>%
      dplyr::group_by(metric.code) %>% dplyr::arrange(time,.by_group = TRUE) %>%
      dplyr::mutate(rolling.mean = rolling_mean(metric.score),
             rolling.sd = rolling_sd(metric.score))
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

maf <- function(x){
  if (class(x)=="data.frame" || (class(x)=="matrix" && dim(x)[2]>1)){
    p = dim(x)[2]
    n = dim(x)[1]
    x = scale(x)
    svd = svd(cov(x)) #If you want to sum of the timesteps/n to be 1
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
    print("x is not a matrix or a data.frame, no MAF transform performed.")
    mafs = as.matrix(x)
    aa = as.matrix(1); a.i = as.matrix(1); d = as.matrix(1)
  }
  out = list(x=x, mafs=maf, rotation=aa, autocor=1 - d/2, eigen.values = d )
  class(out) = c("Maf")
  out
}
