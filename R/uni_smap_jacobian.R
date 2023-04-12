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
#' \itemize{\item{smap_J}{Jacobian matrices across taus. It is recommended to average across these matrices.}
#' \item{eigenJ}{Absolute maximum eigenvalue.}
#' \item{reJ}{Real component of dominant eigenvalue}
#' \item{imJ}{Imaginary component of dominant eigenvalue.}
#' }
#'
#' @examples
#' #Load the multivariate simulated
#' #dataset `simTransComms`
#'
#' data("simTransComms")
#'
#' #Subset the "1_38_1" community prior to the transition
#'
#' pre_simTransComms <- subset(simTransComms$`1_255_1`,time < inflection_pt)
#' winsize <- round(dim(pre_simTransComms)[1] * 50/100)
#'
#' #Estimate the Jacobian for the second species using s-map
#' est_jac <- uni_smap_jacobian(pre_simTransComms[,2:3])
#'
#' @export
#' @source Grziwotz, F., Chang, C.-W., Dakos, V., van Nes, E.H., Schwarzländer, M., Kamps, O., et al. (2023). Anticipating the occurrence and type of critical transitions. Science Advances, 9.

uni_smap_jacobian <- function(data, theta_seq =  NULL, E = 1,tau = NULL, scale = T){

  if(E <= 0){
    stop("E must be positive")
  }

  if(is.null(theta_seq)){
    theta_seq <- seq(0,2.5,by=0.5)
  }
  if(is.null(tau)){
    tau <- floor(dim(data)[1]*0.1)
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
    smap_int <- tryCatch(rEDM::s_map(c(data[,2]), E=E, tau=tau, theta=theta_seq, silent=TRUE),
                         error = function(err){warning("Long period of unchanging values. uniJI not calculated for node")})

    #invisible(capture.output(smap_int <- rEDM::s_map(c(data[,2]), E=E, tau=tau, theta=theta_seq, silent=TRUE), type = "output"))

    #smap_int <- rEDM::s_map(c(data[,2]), E=E, tau=tau, theta=theta_seq, silent=TRUE)

    if(inherits(smap_int,"character")){
      smap_results <- rep(list(NA),10)
      lambda <- rep(list(NA),10)

    }else{
    #smap_int <- rEDM::s_map(c(data[,2]), E=E, tau=tau, theta=theta_seq, silent=TRUE)
    best <- order(-unlist(smap_int$rho))[1]
  theta <- smap_int[best,]$theta

  smap_results <- uni_smap_jacobian_est(ts=c(data[,2]), theta = 0, E = E, tau = tau)

  lambda <- lapply(smap_results,function(j){
    eig <- eigen(j)$values
    eig[order(abs(eig))[E]]})
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
  smap_best <- rEDM::s_map(ts, E=E, tau=tau, theta=theta, silent=TRUE, save_smap_coefficients=TRUE)

  smap_coef <- smap_best$smap_coefficients[[1]]

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
