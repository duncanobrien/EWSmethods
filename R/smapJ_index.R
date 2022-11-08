#' S-map Jacobian index function
#'
#' Calculate a stability metric from the s-map estimated Jacobian
#'
#' @param data Numeric matrix with time in first column and species abundances in other columns
#' @param winsize Numeric. Defines the window size of the rolling window as a percentage of the time series length.
#' @param theta_seq Numeric vector of thetas (nonlinear tuning parameters) to estimate the Jacobian over. If `NULL`, a default sequence is provided.
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
#' #Estimate the stability index for the "1_38_1" community
#'
#' egJI <- smapJI(data = pre_simTransComms[,2:7],
#' winsize = 25)
#'
#' @export
#' @source Ushio, M., Hsieh, Ch., Masuda, R. et al. (2018) Fluctuating interaction network and time-varying stability of a natural fish community. Nature 554, 360–363.

smapJI <- function(data, winsize = 50,theta_seq =  NULL){

  window <- round(dim(data)[1] * winsize/100)

  out <- lapply(1:(dim(data)[1]-window+1), function(i){

    jac <- smap_jacobian(data = data[i:(i+window-1),], theta_seq = theta_seq)

    j_dom_eig <- max(abs(Re(eigen(jac$smapJ[[length(jac$smapJ)]])$values)))

    return(data.frame("time" = data[i+window-1,1],
                      "smap_J" = j_dom_eig))

    })
  out <- do.call("rbind", out)
  return(out)
}
