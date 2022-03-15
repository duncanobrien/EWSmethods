#' Calculate Fisher Information
#'
#' Uses a multivariate array of time series to estimate Fisher information following the approach of Karunanithi et al. (2010).
#'
#' @param timedat A numeric vector of equal spacing representing the time points
#' @param data A numeric matrix of individual time series across the columns. These could be different species, populations or measurements.
#' @param sost A 1 x n matrix where n is a length equal to the number of time series in \code{data}. Each value is the 'size of state' tolerable for that time series and typically is represented by the standard deviation of the time series during a reference period.
#' @param hwin Numeric value. The number of data points within the window.
#' @param winspace Numeric value. The number of data points to roll the window over in each iteration. Must be less than \code{hwin}.
#' @param TL Numeric value. The 'tightening level' or percentage of points shared between states that allows the algorithm to classify data points as the same state.
#'
#' @returns A list containing three objects:
#' \item{FI}{A numeric vector of Fisher information estimates}
#' \item{midt_win}{A numeric vector of the time index at the centre of the window for that associated value in \code{FI}.}
#' \item{t_win}{A n x m numeric matrix where the length of n is the winspace and length of m is the number of window shifts made. Values are consequently the timepoint indices that contribute to that window.}

#' @export
#'
GFisher <- function(timedat,data,sost,hwin,winspace = 1,TL = 90){
  # calculates the FI from time series data.
  # Important parameters include:
  # FI: fisher information
  # midt_win: mid point of each time window
  # t_win: time points within each window
  # t: time data
  # data: Input data (matrix) size (#time steps, #variables)
  # sost:size of states
  # hwin: window size(number of points in each calculation)
  # winspace: number of time steps we move the window (<hwin)
  # TL: tightening Level

  # pre-define variables
  midt_win <- matrix()
  FI <- matrix()
  t_win <- matrix(NA,nrow = hwin, ncol = 1)#?
  window <- 0

  for (i in seq(from = 1, to = nrow(data), by = winspace)){
    #start big loop to go through all the data
    lmin <- pmin(i,nrow(data)) #pmin or min?
    lmax <- pmin(i+hwin-1,nrow(data))
    NP <- nrow(data[lmin:lmax,])
    if (NP == hwin){
      window <- window+1
      array.data <- NFisherpdf(data,lmin,lmax,sost,TL/100) # requires Nfisherpdf fn
      pdf <- array.data$pdf # extract pdf from NFisherpdf return
      neighbour <- array.data$neighbour # extract neighbour from NFisherpdf return

      q <- sqrt(pdf) #convert to amplitude of the pdf

      #modification
      counter <- 0
      Q <- matrix(0,nrow =nrow(q), ncol = 1) # matrix based upon pdf

      neighbourQ <- matrix(0,nrow =nrow(q), ncol = ncol(neighbour))
      for (j in 1:nrow(q)){
        if (q[j,1] != 0){
          counter <- counter+1
          Q[counter,] <- q[j,]
          neighbourQ[counter,] <- neighbour[j,]
        }
      }
      # Re-arranges the states by assessing proximity.
      # Calculates the Euclidean distance (zz) of the points in the state,
      # finds the smallest distance and orders the q vector by the Euclidean distance.
      # Ensures that the states are indeed ordered by distance if size(neighbourQ,1)>2
      if (nrow(neighbourQ)>2){
        minimumneighbourQ <- 0
        tempneighbourQ <- 0
        tempQ <- 0
        z <- matrix(0, nrow = nrow(neighbourQ)-1, ncol = length(neighbourQ))
        for (ii in 1:(nrow(neighbourQ)-1)) {
          for (jj in 1:nrow(neighbourQ)) {
            if (jj>ii){
              z[ii,jj] <- sqrt((sum(neighbourQ[ii])-sum(neighbourQ[jj]))^2)
            }
            else{
              z[ii,jj]=5000000000
            }
          }

          minimumneighbourQ <- min(z[ii,])
          for (kk in 2:nrow(neighbourQ)){
            if (kk>ii){
              if (z[ii,kk]== minimumneighbourQ){
                tempneighbourQ <- neighbourQ[ii+1,]
                tempQ <- Q[ii+1,]
                neighbourQ[ii+1,] <- neighbourQ[kk,]
                Q[ii+1,] <- Q[kk]
                neighbourQ[kk,] <- tempneighbourQ
                Q[kk,] <- tempQ
              }
            }
          }
        }
      }

      QQ <- c(0,Q,0) # adding points (beginning and end) for edge gradients
      dq <- diff(QQ)/1 # calculating dqs (difference between vector elements)

      # Calculates Fisher info for each window and places it in the middle of the window
      if ((i+(hwin-winspace))<=nrow(data)){
        t_win <- cbind(t_win,timedat[lmin:lmax]) # time points within each window
        FI[window] <- 4*sum(dq^2*1) # One fisher for each window
        midt_win[window] <- ceiling(t_win[(hwin/2),window+1]) # mid point of window is element
        # indexed halfway through window. Used for plotting. +1 required to remove first NA col
        # Ceiling accounts for odd hwin
      }
    }
  }
  listFI <- list("FI" = FI,"midt_win" = midt_win,"t_win" = t_win[,-1]) # t_win[,-1] drops NA column
  return(listFI)
}

