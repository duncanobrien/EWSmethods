#' Fisher Information Binning
#'
#' The purpose of this program is to bin the points into states by calculating the difference (dist) of each variable vector per timestep and then counting the variable vector if dist<= size of the state AND the percentage of variables in the vector that meets the criteria is over the tightening level.
#'
#' @param data A numeric matrix of individual time series across the columns. These could be different species, populations or measurements.
#' @param lmin Numeric value. Index of lower limit of window.
#' @param lmax Numeric value. Index of upper limit of window.
#' @param sizeofstates A 1 x n matrix where n is a length equal to the number of time series in \code{data}. Each value is the 'size of state' tolerable for that time series and typically is represented by the standard deviation of the time series during a reference period.
#' @param TL Numeric value. The 'tightening level' or percentage of points shared between states that allows the algorithm to classify data points as the same state.
#'
#' @returns A list \code{NewList} containing to two objects:
#' \item{pdf}{A uniform probability density function.}
#' \item{neighbour}{The time point indices that contribute to the calculation.}
#' @keywords internal


NFisherpdf <- function(data,lmin,lmax,sizeofstates,TL){

  # The purpose of this program is to bin the points into states by
  # calculating the difference (dist) of each variable vector per timestep and then counting
  # the variable vector if dist<= size of the state AND the percentage of variables in the
  # vector that meets the criteria is over the tightening level (TL). Further, the number of
  # vectors that fit within a state are counted (rnum)and then used to calculate the probability
  # density function (pdf = rnum/sum(rnum)).
  fp_tol <- 1e-12 #correcting for floating point error (specifies significant digits)

  # Initialize the vector which will hold the counter for points in a state
  rnum <- matrix(0,lmax-lmin+1,1)

  # Initialize array of points that will be counted in line with lmin and lmax.
  # This dummy variable is used to ensure that the rows are not double counted.
  pout <- matrix(0,lmax-lmin+1,1)

  neighbour <- matrix(0, nrow = (lmax-lmin+1), ncol = ncol(data))

  # find recurrence points in window and record the percentage of their instances,
  # ignoring points that have already been counted
  for (j in seq(from = lmin, to = lmax, by = 1)){ # looping over the window in time steps
    count <- 0
    if (pout[abs(j-lmin)+1,1]==0){ # no double counting variable vector
      pouttemp <- matrix(0,dim(pout))
      for (k in lmin:lmax){ # over window
        Mcount <- 0
        for (m in 1:ncol(data)){ # over num of variables

          dist <- abs(data[j,m]-data[k,m]) # for each variable 1 by 1

          #Dealing with floating point error. Rounds dist vector to fp_tol precision
          r_dist <- roundTO(dist,fp_tol)

          if (r_dist<=(sizeofstates[1,m])){
            Mcount <- Mcount+1
          }
        }



        #Ensures that the number of variables that have been counted meet
        #the criteria >= TL*total number of variables and this variable
        #vector has not been previously counted
        if (Mcount>= TL*ncol(data) & pout[abs(lmin-k)+1,1] == 0){
          count<-count+1 # populating observations in state
          pouttemp[abs(lmin-k)+1,1] <- 1
        }
      }
      rnum[abs(lmin-j)+1,1] <- count # number of points in state

      neighbour[(abs(lmin-j)+1),] <- unlist(data[j,])
      # unlist required to prevent neighbour becoming list
    }
    pout <- pout|pouttemp # update no double counting using logical OR
  }

  ##Calculating pdf
  if (colSums(rnum) == 0){ # pdf represents the percentage of variables that meet criteria
    pdfa <- rnum} else{
      pdfa <- rnum/colSums(rnum)}

  size1 <- dim(neighbour)
  newList <- list("pdf" = pdfa, "neighbour" = neighbour)
  return(newList)
}


