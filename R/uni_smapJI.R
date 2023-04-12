#' Univariate S-map Jacobian index function
#'
#' Calculate a stability metric from the s-map estimated Jacobian of a univariate time series
#'
#' @param data Numeric matrix with time in first column and species abundance in the second
#' @param winsize Numeric. Defines the window size of the rolling window as a percentage of the time series length.
#' @param theta_seq Numeric vector of thetas (nonlinear tuning parameters) to estimate the Jacobian over. If `NULL`, a default sequence is provided.
#' @param E Numeric. The embedding dimension. Is suggested to be positive.
#' @param tau Numeric. The time-delay offset to use for time delay embedding. Suggested to be positive here, but if not provided, is set to 10\% the length of the time series.
#' @param scale Boolean. Should data be scaled prior to estimating the Jacobian.
#'
#' @returns A dataframe where the first column is last time index of the window and the second column is the estimated index value. A value <1.0 indicates stability, a value >1.0 indicates instability.
#'
#' @examples
#' #Load the multivariate simulated
#' #dataset `simTransComms`
#'
#' data(simTransComms)
#'
#'#Subset the "1_38_1" community prior to the transition
#'
#' pre_simTransComms <- subset(simTransComms$`1_38_1`,time < inflection_pt)
#'
#' #Estimate the univariate stability index for the first species in
#' #the "1_38_1" community
#'
#' egJI <- uniJI(data = pre_simTransComms[,2:3],
#' winsize = 25, E = 3)
#'
#' @export
#' @source Grziwotz, F., Chang, C.-W., Dakos, V., van Nes, E.H., Schwarzländer, M., Kamps, O., et al. (2023). Anticipating the occurrence and type of critical transitions. Science Advances, 9.

uniJI <- function(data, winsize = 50,theta_seq =  NULL, E = 1, tau = NULL, scale = TRUE){

  window <- round(dim(data)[1] * winsize/100)

  out <- lapply(1:(dim(data)[1]-window+1), function(i){

    jac <- uni_smap_jacobian(data = data[i:(i+window-1),], theta_seq = theta_seq, E = E, tau = tau, scale = scale)

    return(data.frame("time" = data[i+window-1,1],
                      "smap_J" =  jac$eigenJ))

  })
  out <- do.call("rbind", out)
  return(out)
}
