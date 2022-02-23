#' Expanding Window Early Warning Signals
#'
#' Modified code from Clements, McCarthy, and Blanchard (2019) Nature
#' Coms - "Early warning signals of rocovery in complex systems". Additional
#' EWS indicators, burn_in and interpolation capability added
#'
#' @param dat A n x m dataframe with the first column  time indices and second column abundances.
#' @param indicators A string vector of the early warning signal indicators of interest. Options include: \code{"ar1"}, \code{"cv"}, \code{"SD"}, \code{"acf"},\code{"rr"},\code{"dr"},\code{"skew"},\code{"kurt"},\code{"mean.size"},\code{"sd.size"},\code{"sd.95"},\code{"trait"}.
#' @param weights A two column dataframe of weights for each indicator. First column is the indicators and second the weights.
#' @param trait Boolean. If true, \code{"trait"} has been provided as an indicator.
#' @param threshold Numeric. The threshold*sigma cutoff past which a warning is identified.
#' @param burn_in Numeric. Number of data points used to train signals before early warning signal assessment is performed.
#' @param tail.direction A string match to \code{"one.tailed"} or \code{"two.tailed"}.
#' @param plotIt Boolean. If true, base R plots are returned
#' @param interpolate Boolean. Should missing values in dat be interpolated.
#'
#' @return output A matrix of early warning signal indicators through time.

#' @export
W_composite_ews<-function(dat, indicators, weights, trait = NULL, threshold = 2, burn_in = 5, tail.direction = "one.tailed", plotIt, interpolate = F, ...){

  ############################################################
  ##print some warnings
  if(length(dat[,1])<3){stop("Warning: Time series too short")}

  if(is.data.frame(weights)){
    if(length(match(weights[,1], indicators))!=length(indicators)){stop("weights and indicators do not match")}
    ############################################################

    #dat<-data.frame("time"=dat[,1], "counts"=dat[,2], "mean.size"=dat[,3], "sd.size"=dat[,4])
    dat<-data.frame("time"=dat[,1], "counts"=dat[,2])
    if(!is.null(trait)){
      dat$trait <- trait}

    #if(length(which(diff(dat$time)>1))>0){
    if(any(is.na(dat$counts))==T){
      print("Warning: some missing data. Count data should be interpolated")
    }
    #if(length(which(diff(dat$time)>1))>0 & interpolate == T){
    if(any(is.na(dat$counts))==T & interpolate == T){
      print("Warning: some missing data. Count data interpolated where required")
      dat.long<-interp(dat$time, dat$counts)
      if(!is.null(trait)){
        dat.long$trait <- interp(dat$time, dat$trait)$counts
      }
      #dat.long$mean.size<-interp(dat$time, dat$mean.size)$counts
      #dat.long$sd.size<-interp(dat$time, dat$sd.size)$counts
      #dat.long$size.95<-interp(dat$time, dat$size.95)$counts
     dat<-dat.long
    }else{
      #####dat$counts<-interp(dat$time, dat$counts)$counts EDITED (detrending?)
      dat <- na.omit(dat)
      #dat$counts<-interp(dat$time, dat$counts)$counts
      dat$counts<-dat$counts

      if(!is.null(trait)){
        dat$trait <- interp(dat$time, dat$trait)$counts
      }
      #dat$mean.size<-interp(dat$time, dat$mean.size)$counts
      #dat$sd.size<-interp(dat$time, dat$sd.size)$counts
      #dat$size.95<-interp(dat$time, dat$size.95)$counts
   } ##########EDITs MADE HERE!!!!!!^^

    ##if there is no variation in the data (i.e. counts are all the same) then cut the data to where variation starts
    #dat<-dat[min(which(diff(dat$counts)!=0)):(length(dat$counts)-min(which(diff(dat$counts)!=0))),]

    if(length(dat[,1])>2){

      ##blank objects to save results
      RES<-list()
      roll.cv <-NULL
      roll.acf<-NULL
      roll.ar<-NULL
      roll.ar.rr<-NULL
      roll.return.rate<-NULL
      roll.density.ratio<-NULL
      roll.skew <- NULL
      roll.kurt <- NULL
      roll.body<-NULL
      roll.body.sd<-NULL
      roll.body.95<-NULL
      roll.SD <- NULL
      roll.trait <- NULL

      inds<-match.arg(indicators, choices=c("cv", "acf", "ar1", "dr", "rr", "skew","kurt","mean.size", "sd.size", "size.95","SD","trait"), several.ok=T)
      tail <- match.arg(tail.direction, choices=c("one.tailed","two.tailed"), several.ok=F)


      ##looped to calculate the rolling change
      for(i in (burn_in):length(dat[,1])){
        #i=7
        ##subset the population of interest up until dat i
        dat.t<-subset(dat, time<=unique(sort(dat$time))[i])

        ##calculate the CV at time t in the focal pop, relative to mean CV through time of that pop
        if(length(which(inds=="cv"))==1){
          roll.cv[[i]]<-CV(dat.t$counts,TRUE)
          cv<-(CV(dat.t$counts,TRUE)-mean(unlist(roll.cv), na.rm=TRUE))/sd(unlist(roll.cv), na.rm = TRUE)
        }

        ## for autocorrelation
        if(length(which(inds=="acf"))==1){
          roll.acf[[i]]<-acf(dat.t$counts, lag.max = 1, type = c("correlation"), plot = FALSE)$acf[2]
          acf<-(acf(dat.t$counts, lag.max = 1, type = c("correlation"), plot = FALSE)$acf[2]-mean(unlist(roll.acf), na.rm=T))/sd(unlist(roll.acf), TRUE)
        }

        ##for SD
        if(length(which(inds=="SD"))==1){
          roll.SD[[i]]<-sd(dat.t$counts,TRUE)
          SD.t<-(sd(dat.t$counts,TRUE)-mean(unlist(roll.SD), na.rm=TRUE))/sd(unlist(roll.SD), na.rm = TRUE)
        }

        ## for ar1
        if(length(which(inds=="ar1"))==1){
          if(length(which(diff(dat.t$counts)!=0))>0){
            roll.ar[[i]]<-ar.ols(dat.t$counts, aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]
            ar1<-(ar.ols(dat.t$counts, aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]-mean(unlist(roll.ar), na.rm=T))/sd(unlist(roll.ar), TRUE)
          }else{ar1<-NA}
        }

        ##for density ratio
        if(length(which(inds=="dr"))==1){
          if(length(which(diff(dat.t$counts)>0))>0){
            spectfft <- spec.ar(dat.t$counts, n.freq = length(dat.t$counts), plot = FALSE, order = 1)
            roll.density.ratio[[i]] <- spectfft$spec[1]/spectfft$spec[length(dat.t$counts)]
            dr<-(roll.density.ratio[[i]]-mean(unlist(roll.density.ratio), na.rm=T))/sd(unlist(roll.density.ratio), TRUE)
          }else{dr<-NA}
        }

        ##for return rate
        if(length(which(inds=="rr"))==1){
          if(length(which(diff(dat.t$counts)!=0))>0){
            roll.ar.rr[[i]]<-ar.ols(dat.t$counts, aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]
            ar.t.rr<-(ar.ols(dat.t$counts, aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]-mean(unlist(roll.ar.rr), na.rm=T))/sd(unlist(roll.ar.rr), na.rm = TRUE)
            #ar.t.rr<-ar.ols(dat.t$counts, aic = FALSE, order.max = 1, dmean = FALSE, intercept = FALSE)$ar[1]
          }else{ar.t.rr <-NA}
          roll.return.rate[[i]]<-1/ar.t.rr
          rr<-((1/ar.t.rr)-(mean(unlist(roll.return.rate), na.rm=T)))/sd(unlist(roll.return.rate), TRUE)
        }

        ## for skewness
        if(length(which(inds=="skew"))==1){
          roll.skew[[i]]<-moments::skewness(dat.t$counts,na.rm = TRUE)
          skewness<-(moments::skewness(dat.t$counts,na.rm = TRUE)-mean(unlist(roll.skew), na.rm=T))/sd(unlist(roll.skew), TRUE)
        }

        ## for kurtosis
        if(length(which(inds=="kurt"))==1){
          roll.kurt[[i]]<-moments::kurtosis(dat.t$counts,na.rm = TRUE)
          kurtosis<-(moments::kurtosis(dat.t$counts,na.rm = TRUE)-mean(unlist(roll.kurt), na.rm=T))/sd(unlist(roll.kurt), TRUE)
        }

        ##mean body size
        if(length(which(inds=="mean.size"))==1){
          roll.body[[i]]<-mean(dat.t$mean.size, na.rm=T)
          size.t<- (mean(dat.t$mean.size, na.rm=T)-mean(unlist(roll.body), na.rm=T))/sd(unlist(roll.body), TRUE)
        }

        ##for sd body size
        if(length(which(inds=="sd.size"))==1){
          roll.body.sd[[i]]<-mean(dat.t$sd.size, na.rm=T)
          size.sd.t<-(mean(dat.t$sd.size, na.rm=T)-mean(unlist(roll.body.sd), na.rm=T))/sd(unlist(roll.body.sd), TRUE)
        }

        ##mean 95% body size
        if(length(which(inds=="size.95"))==1){
          roll.body.95[[i]]<-mean(dat.t$size.95, na.rm=T)
          size.95.t<- (mean(dat.t$size.95, na.rm=T)-mean(unlist(roll.body.95), na.rm=T))/sd(unlist(roll.body.95), TRUE)
        }

        ##mean trait value
        if(length(which(inds=="trait"))==1){
          roll.trait[[i]]<-mean(dat.t$trait, na.rm=T)
          trait.t<- (mean(dat.t$trait, na.rm=T)-mean(unlist(roll.trait), na.rm=T))/sd(unlist(roll.trait), TRUE)
        }

        ##save results
        RES[[i]]<-data.frame("time"=dat.t[i,1]
                             ,"cv"=if(length(which(inds=="cv"))==1){"cv"=cv}else{NA}
                             ,"acf"=if(length(which(inds=="acf"))==1){"acf"=acf}else{NA}
                             ,"ar1"=if(length(which(inds=="ar1"))==1){"ar1"=ar1}else{NA}
                             ,"dr"=if(length(which(inds=="dr"))==1){"dr"=dr}else{NA}
                             ,"rr"=if(length(which(inds=="rr"))==1){"rr"=rr}else{NA}
                             ,"skew"=if(length(which(inds=="skew"))==1){"skew"=skewness}else{NA}
                             ,"kurt"=if(length(which(inds=="kurt"))==1){"kurt"=kurtosis}else{NA}
                             ,"mean.size"=if(length(which(inds=="mean.size"))==1){"mean.size"= size.t}else{NA}
                             ,"sd.size"=if(length(which(inds=="sd.size"))==1){"sd.size"= size.sd.t}else{NA}
                             ,"size.95"=if(length(which(inds=="size.95"))==1){"size.95"= size.95.t}else{NA}
                             ,"SD"=if(length(which(inds=="SD"))==1){"SD"= SD.t}else{NA}
                             ,"trait"=if(length(which(inds=="trait"))==1){"trait"= trait.t}else{NA})

      }
      }
    results<-do.call("rbind", RES)

    results$rr<- -1*results$rr #convert rr, mean.size and trait to correct orientation
    results$mean.size<- -1*results$mean.size
    results$trait <- -1*results$trait

    ## multiply the calculated statistics by the weighting given in the function
    results[,match(weights[,1], names(results))]<-data.frame(mapply(`*`,results[,match(weights[,1], names(results))],weights[,2]))


    output<-data.frame(results[,1],
                       "metric.score"=rowSums(results[,2:length(results)],
                                              na.rm=T),
                       "metric.code"=paste(inds, collapse=" + "))

    output$rolling.mean<-rolling_mean(output$metric.score)
    output$rolling.sd<-rolling_sd(output$metric.score)
    output$threshold.crossed<-NA

    if(tail == "two.tailed"){
    output$threshold.crossed[which(output$metric.score>(output$rolling.mean+(threshold*output$rolling.sd))|output$metric.score<(output$rolling.mean-(threshold*output$rolling.sd)))]<-1
    output$threshold.crossed[is.na(output$threshold.crossed)] <- 0
    }else{
      output$threshold.crossed[which(output$metric.score>(output$rolling.mean+(threshold*output$rolling.sd)))]<-1
      output$threshold.crossed[is.na(output$threshold.crossed)] <- 0
    }

    names(output)[1]<-c("time")
    output$count.used <- dat.t$counts[dat.t$time %in% output$time] ### added variable to identify interp differences droppng values

    if(plotIt==T){
      dev.new()
      par(mar = (c(1, 2, 0, 1) + 0.2), oma = c(4, 2, 3, 1))
      plot(output$time, output$rolling.mean, type="l", lwd=1, xlab="Time", ylab="metric score",
           ylim=c(min(output[,c(2,4,5)], na.rm=T), max(output[,c(2,4,5)], na.rm=T)), lty="solid")
      lines(output$time, output$metric.score, type="l", lwd=2, col="skyblue")
      lines(output$time, output$rolling.mean+(output$rolling.sd*threshold), type="l", lwd=1, lty="dashed")
      lines(output$time, output$rolling.mean-(output$rolling.sd*threshold), type="l", lwd=1, lty="dashed")
      legend("topright", legend=c("Rolling mean", "Metric score","Confidence interval"),
            col=c("black","skyblue", "black"), lty=c(1,1,2), cex=0.8)
         }

    return(output)
  }
  }
