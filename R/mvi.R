#' Multivariate Variance Index function
#'
#' Calculate a multivariate variance following Brock, W. A., and S. R. Carpenter. 2006. Variance as a leading indicator of regime shift in ecosystem services. Ecology and Society 11(2): 9.
#'
#' @param df A numeric matrix of species abundances, names across columns, time across rows
#' @param winsize Numeric. Defines the window size of the rolling window as a percentage of the time series length.
#'
#' @returns A matrix where the first column is last time index of the window and the second column is the estimated index value.
#'
#' @export

mvi <- function(df,winsize){

  window <- round(dim(df)[1] * winsize/100)

  out <- matrix(ncol = 2,nrow=dim(df)[1]-window+1)
  for(i in 1:(dim(df)[1]-window+1)){
    #tmp <- cov(df[i:(i+1),])
    tmp <- cov(df[i:(i+window-1),])
    out[i,2] <- sqrt(eigen(tmp,symmetric = T)$values[1])
    out[i,1] <- (i+window-1)

  }
  colnames(out) <- c("maxt","mvi")
  return(out)
}
