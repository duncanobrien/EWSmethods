#' S-map Inferred Jacobian
#'
#' Performs the S-map on a univariate time series to infer the Jacobian matrix at different points in time across thetas.

#' @param data Numeric matrix with time in first column and species abundance in the second
#' @param theta_seq Numeric vector of thetas (nonlinear tuning parameters) to estimate the Jacobian over. If `NULL`, a default sequence is provided.
#' @param E Numeric. The embedding dimension. Is suggested to be positive.
#' @param tau Numeric. The time-delay offset to use for time delay embedding. Suggested to be positive here, but if not provided, is set to 10\% the length of the time series.
#' @param scale Boolean. Should data be scaled prior to estimating the Jacobian.
#'
#' @returns A list containing three objects:
#' \item{smap_J}{Jacobian matrices across taus. It is recommended to average across these matrices.}
#' \item{eigenJ}{Absolute maximum eigenvalue.}
#' \item{reJ}{Real component of dominant eigenvalue}
#' \item{imJ}{Imaginary component of dominant eigenvalue.}
#'
#' @examples
#' #Load the multivariate simulated
#' #dataset `simTransComms`
#'
#' data("simTransComms")
#'
#' #Subset the second community prior to the transition
#'
#' pre_simTransComms <- subset(simTransComms$community2,time < inflection_pt)
#' winsize <- round(dim(pre_simTransComms)[1] * 50/100)
#'
#' #Estimate the Jacobian for the first 50 timepoints of the
#' #second species using s-map
#' est_jac <- uni_smap_jacobian(pre_simTransComms[1:50,2:3])
#'
#' @export
#' @source Grziwotz, F., Chang, C.-W., Dakos, V., van Nes, E.H., Schwarzländer, M., Kamps, O., et al. (2023). Anticipating the occurrence and type of critical transitions. Science Advances, 9.

uni_smap_jacobian <- function(data, theta_seq =  NULL, E = 1,tau = NULL, scale = TRUE){

  if(E <= 0){
    stop("E must be positive")
  }

  if(is.null(theta_seq)){
    theta_seq <- c(0, 1e-04, 3e-04, 0.001, 0.003, 0.01, 0.03, 0.1, 0.3, 0.5,
      0.75, 1, 1.5, 2, 3, 4, 6, 8)
  }
  if(is.null(tau)){
    tau <- 1*floor(dim(data)[1]*0.1)
  }
  if(tau == 0 ){
    tau <- 1
  }

  if(length(unique(data[,2])) == 1 |  (sum(as.numeric(base::table(data[,2]) == 1)) == 1 & length(unique(data[,2])) == 2)){ #if the entire ts is equal, or a only a single additional value is present, smap crashes. Therefore we preempt this and set to 0 before occurs
    smap_results <- rep(list(NA),10)
    lambda <- rep(list(NA),10)
    warning("Entire time series is uniform. NAs produced")
  }else{
    if(isTRUE(scale)){
      data[,2] <- c(scale(data[,2]))
    }
    # calculate best theta

    best_smap <- tryCatch(rEDM::PredictNonlinear(dataFrame = data, lib = c(1, NROW(data)), pred = c(1, NROW(data)),
                           columns = names(data)[2], target = names(data)[2],
                           E=E, tau=-tau, theta=theta_seq,  knn = 0,
                           verbose=FALSE, showPlot = FALSE),
                          error = function(err){warning("Long period of unchanging values. uniJI not calculated for node")})

    # if(inherits(smap_int,"character")){
    #   smap_results <- rep(list(NA),10)
    #   lambda <- rep(list(NA),10)
    if(inherits(best_smap,"character")){
        smap_results <- rep(list(NA),10)
        lambda <- rep(list(NA),10)

    }else{
    #smap_int <- rEDM::s_map(c(data[,2]), E=E, tau=tau, theta=theta_seq, silent=TRUE)
    best <- order(-best_smap$rho)[1]
    theta <- theta_seq[best]

  smap_results <- uni_smap_jacobian_est(ts=data, theta = theta, E = E, tau = tau)

  # lambda <- lapply(smap_results,function(j){
  #   eig <- eigen(j)$values
  #   eig[order(abs(eig))[E]]}) #extract time varying Jacobian with lowest stability
  smap_avg <- Reduce("+",smap_results)/length(smap_results) #average Jacobian across time varying Jacobians
  eig <- eigen(smap_avg)$values
  lambda <- eig[order(abs(eig))[E]]
    }
    }

  return(list("smapJ" = smap_results,"eigenJ" =  mean(abs(unlist(lambda))),"reJ" = mean(Re(unlist(lambda))),"imJ" =  mean(Im(unlist(lambda)))))
}

#' S-map Inferred Jacobian
#'
#' Performs the S-map on a univariate time series to infer the Jacobian matrix at different points in time across thetas.

#' @param data Numeric matrix with time in first column and species abundance in the second
#' @param theta_seq Numeric vector of thetas (nonlinear tuning parameters) to estimate the Jacobian over. If `NULL`, a default sequence is provided.
#' @param E Numeric. The embedding dimension. Is suggested to be positive.
#' @param tau Numeric. The time-delay offset to use for time delay embedding. Suggested to be positive here, but if not provided, is set to 10% the length of the time series.
#' @param scale Boolean. Should data be scaled within each window prior to estimating the Jacobian.
#'
#' @returns A list containing three objects:
#' \item{smap_J}{Jacobian matrices across taus. It is recommended to average across these matrices.}
#' \item{eigenJ}{Absolute maximum eigenvalue.}
#' \item{reJ}{Real component of dominant eigenvalue}
#' \item{imJ}{Imaginary component of dominant eigenvalue.}
#' @keywords internal
#' @noRd
uni_smap_jacobian_est <- function(ts, E, tau, theta){

  # calculate eigenvalues for best theta
  #smap_best <- rEDM::s_map(ts, E=E, tau=tau, theta=theta, silent=TRUE, save_smap_coefficients=TRUE)

  smap_best <- rEDM::SMap(dataFrame = ts, E=E, tau=tau, theta=theta,
             lib = c(1, NROW(ts)), pred = c(1, NROW(ts)),
             knn = 0, columns = names(ts)[2], target = names(ts)[2],
             verbose=FALSE)

  #smap_coef <- smap_best$smap_coefficients[[1]]
  smap_coef <- smap_best$coefficients

  jac <- lapply(2:NROW(smap_coef),function(k){
    if(!is.na(smap_coef[k,2])){
      M <- rbind(as.numeric(smap_coef[k, 3:(2+E)]), cbind(diag(E - 1), rep(0, E - 1)))
    }else{
      M <- NA
    }
    return(M)
  })

  return(jac)

}
