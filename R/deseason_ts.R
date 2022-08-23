#' Deseason Seasonal Time Series
#'
#' Removes seasonal signals from time series using either averaging or time series decomposition methods. Three decomposition methods are available: traditional decompostion, loess decomposition and X11 decompostion.
#'
#' @param data The dataframe to be transformed, The first column must be a vector of dates with all other columns the individual time series.
#' @param increment The time-step increment in either \code{month}, \code{year}, \code{week}, or \code{day}. Provides the basis for deaseasoning.
#' @param method String of either \code{"average"}, \code{"decompose"}, \code{"stl"} or \code{"x11"} indicating the method of deseasoning. \code{"average"} subtracts the average representative month/week/day-of-the-year from each time point whereas \code{"decompose"}, \code{"stl"} and \code{"x11"} subtracts the seasonal component estimated by time series decomposition, loess decomposition and the X11 method respectively.
#' @param order String indicating the date format of the date columns. Options are \code{"dmy"}, \code{"ymd"} or \code{"mdy"}.
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
#' #Deseason using time series
#' #decomposition.
#'
#' decomp_dat <- deseason_ts(data = multi_spp_data,
#' increment = "month",
#' method = "decompose",
#' order = "ymd")
#'
#' #Deseason using loess
#'
#' decomp_dat <- deseason_ts(data = multi_spp_data,
#' increment = "month",
#' method = "stl",
#' order = "ymd")
#'
#' @importFrom forecast seasadj
#' @importFrom seasonal seas
#' @export

deseason_ts <- function(data, increment=c("month","year","week","day"),
                        method=c("average","decompose","stl","x11"),order=NULL) {

  increment<-increment[1]
  method<-method[1]

  # remove any columns that contain only zeros
  data1<-data
  if(any(apply(data[,-1],2,mean,na.rm=T)==0)){
    data1<-data[,-(which(apply(data[,-1],2,mean,na.rm=T)==0)+1)]
    cat(paste(length(which(apply(data[,-1],2,mean,na.rm=T)==0)),"columns containing all zeros omitted\n"))
  }
  data1 <- as.data.frame(data1)
  #====================================================================================
  # 1) FORMAT DATES AND AGGREGATE TAXA BY INCREMENT:
  #====================================================================================

  dates<-as.character(data1[,1])
  d<-strsplit(dates[1],"")[[1]]
  sep<-unique(d[is.na(suppressWarnings(as.numeric(d)))])
  if(length(sep)!=1) stop("Unrecognized date format. Make sure dates are in first column
  and use consistent non-numeric seperator between year, month, and day.")
  d<-data.frame(apply(t(data.frame(strsplit(dates,sep))),2,as.numeric))

  if(is.null(order)){
    if(length(which(apply(d,2,max)<13))>1) stop("Cannot distinguish between days and months in dates.
	Provide 'order' argument in function call.")
    names(d)[which(apply(d,2,max)>12)]<-"d"
    names(d)[which(apply(d,2,max)<13)]<-"m"
    names(d)[which(apply(d,2,max)>100)]<-"y"
  } else {names(d)<-strsplit(order,"")[[1]]}

  data2<-data.frame(date=as.Date(paste(d$y,d$m,d$d,sep="-")),data1[,-1])
  data2<-stats::aggregate(data2[,-1],by=list(data2[,1]),mean,na.rm=T)
  names(data2)[1]<-"date"
  date<-seq.Date(min(data2[,1]),max(data2[,1]),1)
  data2<-merge(data.frame(date=date),data2,all=T)
  taxa <- data2[,-1]

  if(increment=="month") t<-"%Y-%m"
  if(increment=="year") t<-"%Y"
  if(increment=="week") t<-"%Y-%U"
  if(increment=="day") t<-"%Y-%j"

  tstep=as.character(date,t)

  byinc <- stats::aggregate(data2,by=list(tstep=tstep),mean,na.rm=T)
  byinc.taxa<-byinc[,-c(1:2)]

  if(any(is.na(byinc[,-c(1:2)]))){
    byinc[,-c(1:2)] <- sapply(byinc[,-c(1:2)], function(x){
      zoo::na.approx(x,na.rm=F,maxgap = 2)
    })
    warning(paste("Missing dates have resulting in missing values which have been interpolated"),
            call.=F,immediate.=F)
  }
  if(increment=="day") incremently<-"daily" else incremently<-paste(increment,"ly",sep="")
  cat(paste("data successfully aggregated into",incremently,"time steps\n"))


  #====================================================================================
  # 2) Deseason:
  #====================================================================================


  if(method==FALSE) zscores<-byinc

  if (method=="average"){

    # remove year part of tstep to get general increment values
    overall.inc<-substr(byinc$tstep,6,nchar(byinc$tstep))
    deseason.zeros<-length(unique(overall.inc))/length(overall.inc)
    if(deseason.zeros>.25) warning(paste(
      "Using the deseasoning z-score method has generated a large
	      number of zeros due to many unique sampling",increment,"values."),
      call.=F,immediate.=F)

    # create a data frame of overall increment means 'oim'
    oim <- data.frame(stats::aggregate(byinc[,-c(1:2)], by=list(increment=overall.inc), mean))

    # create a data frame of overall increment standard deviations 'oisd'
    oisd <- data.frame(stats::aggregate(byinc[,-c(1:2)], by=list(increment=overall.inc), sd))

    oisd[is.na(oisd)]<-1  # sds will be NA if only one occurence of overall value
    oisd[oisd==0]<-1  # sds will be 0 if multiple occurences of overall value are equal

    zscores <- NULL

    for(i in 1:nrow(byinc)) {
      inc <- overall.inc[i]
      #(byinc[i,-c(1:2)]-oim[which(oim$increment==inc),-1])/oisd[which(oisd$increment==inc),-1]->zscore
      (byinc[i,-c(1:2)]-oim[which(oim$increment==inc),-1])->zscore

      zscores<-rbind(zscores,zscore)}

    zscores <- data.frame(byinc[,1:2],zscores)

    #cat("data successfully z-scored with deseasoning\n")
  }

  if (method=="decompose"){
    byinc[,-c(1:2)] <- sapply(byinc[,-c(1:2)],FUN = function(x){
      x - c(stats::decompose(stats::ts(x,frequency = 12),type="additive")$seasonal)
    })
    zscores<-byinc
  }

  if (method=="stl"){
    byinc[,-c(1:2)] <- sapply(byinc[,-c(1:2)],FUN = function(x){
      x - c(stats::stl(stats::ts(x,frequency = 12),s.window=13, robust=F)$time.series[,1])
    })
    zscores<-byinc
  }

  if(method=="x11"){
    byinc[,-c(1:2)] <- sapply(byinc[,-c(1:2)],FUN = function(x){
      tmp.ts <- stats::ts(x,start = c(as.numeric(substr(byinc$date[1],1,4)), as.numeric(substr(byinc$date[1],6,7))),frequency = 12)
      if(any(tmp.ts == 0)){
        tmp.ts <- tmp.ts+1
        out <- tryCatch(forecast::seasadj(seasonal::seas(tmp.ts,estimate.maxiter = 9999,x11="")), error=function(e){
          return(rep(NA,length = length(tmp.ts)))
        })
        if(!all(is.na(out))){
          out <- out-1
        }
      }else{
        out <- tryCatch(forecast::seasadj(seasonal::seas(tmp.ts,estimate.maxiter = 9999,x11="")), error=function(e){
          return(rep(NA,length = length(tmp.ts)))
        })
      }
      out
    })
    zscores<-byinc

  }

  #zscores<-zscores[,-1]

  return(zscores[,-1])
}
