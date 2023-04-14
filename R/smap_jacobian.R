#' S-map Inferred Jacobian
#'
#' Performs the S-map on a multivariate time series to infer the Jacobian matrix at different points in time across thetas.

#' @param data Numeric matrix with time in first column and species abundances in other columns
#' @param theta_seq Numeric vector of thetas (nonlinear tuning parameters) to estimate the Jacobian over. If `NULL`, a default sequence is provided.
#' @param scale Boolean. Should data be scaled prior to estimating the Jacobian.
#'
#' @returns A list containing three objects:
#' \item{smap_J}{Jacobian matrices for each point in time. It is recommended to just use the last estimate.}
#' \item{rmse}{Average root mean squared error for each species.}
#' \item{smap_intercept.r}{Intercepts of the regression fit.}
#'
#' @examples
#' #Load the multivariate simulated
#' #dataset `simTransComms`
#'
#' data("simTransComms")
#'
#' #Subset the third community prior to the transition
#'
#' pre_simTransComms <- subset(simTransComms$community3,time < inflection_pt)
#' winsize <- round(dim(pre_simTransComms)[1] * 50/100)
#'
#' #Estimate the Jacobian using s-map (typically only
#' #the final estimate is informative)
#' est_jac <- multi_smap_jacobian(pre_simTransComms[,2:7])
#'
#' @export
#' @source Medeiros, L.P., Allesina, S., Dakos, V., Sugihara, G. & Saavedra, S. (2022) Ranking species based on sensitivity to perturbations under non-equilibrium community dynamics. Ecology Letters, 00, 1– 14.

multi_smap_jacobian <- function(data, theta_seq =  NULL, scale = TRUE){

  if(is.null(theta_seq)){
  theta_seq <- c(0, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5,
               1, 2, 3, 4, 5, 6, 7, 8)
  }

  #sapply(data[,-1],FUN = function(j){length(unique(j)) == 1 |  (sum(as.numeric(base::table(j) == 1)) == 1 & length(unique(j)) == 2)})
  ts_check <- sapply(data,FUN = function(j){length(unique(j)) == 1 |  (sum(as.numeric(base::table(j) == 1)) == 1 & length(unique(j)) == 2)})
  #ts_check <- c(FALSE,TRUE,TRUE,FALSE,FALSE)
  if(any(ts_check)){ #drop colums that are constant or nearly constant. If not done so, rEDM crashes
    warning(paste0(paste(colnames(data)[ts_check],collapse="/")," is/are uniform and have been excluded"))
    data <- data[,!ts_check]
  }

  if(isTRUE(scale)){
    #data[,-1] <- sapply(data[,-1],FUN = function(x){ifelse(length(unique(x)) == 1,return(x),return(c(scale(x))))} )
    #due to rEDM crashing if NAs are present (as occurs when scaling a vector of all zeroes),
    #only scale if vector not only zeroes
    data[,-1] <- sapply(data[,-1],FUN = function(x){return(c(scale(x)))} )

  }

  rmse_seq <- sapply(theta_seq, function(x){mean(smap_jacobian_est(theta = x, ts = data)[[2]])})
  theta <- theta_seq[which.min(rmse_seq)]
  smap_results <- smap_jacobian_est(data, theta = theta)

  return(smap_results)
}


#' S-map Inferred Jacobian
#'
#' Performs the S-map on a multivariate time series to infer the Jacobian matrix at different points in time

#' @param ts Numeric matrix with time in first column and species abundances in other columns
#' @param theta Numeric nonlinear tuning parameter
#'
#' @returns A list containing three objects:
#' \item{smap_J}{Jacobian matrices for each point in time.}
#' \item{rmse}{Average root mean squared error for each species.}
#' \item{smap_intercept.r}{Intercepts of the regression fit.}
#' @keywords internal
#' @noRd

smap_jacobian_est <- function(ts, theta){
  # number of species
  n_sp <- ncol(ts) - 1
  # data points to use
  lib <- nrow(ts)
  # species to use
  cols <- paste("x", 1:n_sp, sep = "")
  names(ts) <- c("time",cols)
  # perform s-map for each target species (effect of all species on target species)
  smap_J <- lapply(rep(NA, lib), matrix, nrow = n_sp, ncol = n_sp)
  smap_intercept <- matrix(rep(NA, n_sp * lib), nrow = lib, ncol = n_sp)
  rmse <- rep(NA, n_sp)
  for (i in 1:length(cols)) {
    # target species
    target <- cols[i]
    # performing s-map using the function block_lnlp from the rEDM package
    smap_output <- rEDM::block_lnlp(block = ts, method = "s-map",
                              columns = cols,  target_column = target, theta = theta,
                              stats_only = FALSE, first_column_time = TRUE,
                              save_smap_coefficients = TRUE, silent = TRUE)
    # RMSE for this target variable
    rmse[i] <- smap_output$stats$rmse[[1]]
    # s-map coefficients (fitted local regression coefficients through time)
    smap_coeffs <- smap_output$smap_coefficients[[1]]
    # s-map intercept values
    smap_intercept[ , i] <- smap_output$smap_coefficients[[1]]$C0[-1]
    # fill time-varying jacobians
    for (j in 1:lib) {
      smap_J[[j]][i, ] <- as.numeric(smap_coeffs[j+1, 3:(n_sp+2)])
    }
  }
  return(list("smapJ" = smap_J, "rmse" = rmse, "smap_intercept" = smap_intercept))
}
