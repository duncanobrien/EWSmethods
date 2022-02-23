
#coefficient of variation
CV <- function(x, na.rm){
  ave<-mean(x, na.rm=na.rm)
  dev<-sd(x, na.rm=na.rm)
  return((dev/ave))
}

#interpolate function edited so not constrained to year
interp<-function(days, obs){
  int.dat<-as.data.frame(approx(days, obs, n = length(obs), method = "linear"))
  names(int.dat)<-c("time", "counts")
  return(int.dat)
}

##function for rolling mean
rolling_mean <- function(x){
  k = length(x);
  result = rep(0, k);
  for(i in 1 : k){
    result[i] <- mean(x[1:i], na.rm=T);
  }
  return(result);
}

##function for rolling sd
rolling_sd <- function(x){
  k = length(x);
  result = rep(0, k);
  for(i in 1 : k){
    result[i] <- sd(x[1:i], na.rm=T);
  }
  return(result);
}
