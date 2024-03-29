#' Multivariate S-map Jacobian index function
#'
#' Calculate a stability metric from the multivariate s-map estimated Jacobian
#'
#' @param data Numeric matrix with time in first column and species abundances in other columns
#' @param winsize Numeric. Defines the window size of the rolling window as a percentage of the time series length.
#' @param theta_seq Numeric vector of thetas (nonlinear tuning parameters) to estimate the Jacobian over. If `NULL`, a default sequence covering `0:8` is provided.
#' @param scale Boolean. Should data be scaled within each window prior to estimating the Jacobian.
#'
#' @returns A dataframe where the first column is last time index of the window and the second column is the estimated index value. A value <1.0 indicates stability, a value >1.0 indicates instability.
#'
#' @examples
#' #Load the multivariate simulated
#' #dataset `simTransComms`
#'
#' data(simTransComms)
#'
#' #Subset the third community prior to the transition
#'
#' pre_simTransComms <- subset(simTransComms$community3,time < inflection_pt)
#'
#' #Estimate the stability index for the third community
#' #(trimmed for speed)
#'
#' egJI <- multiJI(data = pre_simTransComms[1:10,2:5],
#' winsize = 75)
#'
#' @export
#' @source Ushio, M., Hsieh, Ch., Masuda, R. et al. (2018) Fluctuating interaction network and time-varying stability of a natural fish community. Nature 554, 360–363.

multiJI <- function(data, winsize = 50,theta_seq =  NULL,scale = TRUE){

  data <- as.data.frame(data)

  if(NCOL(data) <= 2){
    stop("Data only contains two columns. multiJI require 2+ timeseries")
  }
  if(!all(apply(data[,-1],2,is.numeric))){
    stop("Not all timeseries are numeric")
  }

  window <- round(dim(data)[1] * winsize/100)

  out <- lapply(1:(dim(data)[1]-window+1), function(i){

    jac <- multi_smap_jacobian(data = data[i:(i+window-1),], theta_seq = theta_seq, scale = scale)

    jac_out <- Reduce("+",jac$smapJ)/length(jac$smapJ) #elementwise mean of time varying Jacobians

    #j_dom_eig <- max(abs(Re(eigen(jac$smapJ[[length(jac$smapJ)]])$values))) #extract last Jacobian only
    j_dom_eig <- max(abs(Re(eigen(jac_out)$values))) #average across timevarying Jacobians

    return(data.frame("time" = data[i+window-1,1],
                      "smap_J" = j_dom_eig))

    })
  out <- do.call("rbind", out)
  return(out)
}
