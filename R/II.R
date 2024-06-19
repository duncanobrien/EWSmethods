#' Information Imbalance
#'
#' Estimates the information imbalance of two hypothesised linked system measurements for a given scalar (`alpha`).
#'
#' @param X Numeric matrix of hypothesised driving variable measurements. If univariate, call `embed_ts(X)` prior to calling `II()`.
#' @param Y Numeric matrix of hypothesised response variable measurements. If univariate, call `embed_ts(Y)` prior to calling `II()`.
#' @param tau Numeric. Time lag of information transfer between X and Y.
#' @param alpha Numeric. Scaling parameter for X. If information imbalance is minimised at an `alpha` > 0, this may be indicative of Granger causality.
#' @param k Numeric. Number of nearest neighbours when estimating ranks.
#' @param method String. Distance measure to be used - defaults to `euclidean` but see `?dist` for options.
#'
#' @returns Information imbalance
#'
#' @examples
#' #Load the multivariate simulated
#' #dataset `simTransComms`
#'
#' data(simTransComms)
#'
#' #Embed the spp_2 and spp_5 of the third community
#'
#' embedX <- embed_ts(X = simTransComms$community3[,c("time","spp_2")],
#' E = 5, tau = 1)
#'
#' embedY <- embed_ts(X = simTransComms$community3[,c("time","spp_5")],
#' E = 5, tau = 1)
#'
#' #Estimate the forward information imbalance
#' #between spp_2 and spp_5
#'
#' egII_for <- II(X = embedX[,-1], Y = embedY[,-1],
#' tau = 1, alpha = 1, k = 5)
#'
#' #Estimate the reverse information imbalance
#' #between spp_2 and spp_5
#'
#' egII_rev <- II(X = embedY[,-1], Y = embedX[,-1],
#' tau = 1, alpha = 1, k = 5)
#'
#' @export
#' @source Del Tatto, V., Bueti, D. & Laio, A. (2024) Robust inference of causality in high-dimensional dynamical processes from the Information Imbalance of distance ranks. PNAS 121 (19) e2317256121.

II <- function(X,
               Y,
               tau = 1,
               alpha = 1,
               k = 1,
               method = "euclidean") {
  N <- dim(X)[1]

  if (is.null(N)) {
    stop("X should be multivariate. Did you forget to embed?")
  }

  if (N != dim(Y)[1]) {
    stop("Dimensions of X and Y should be identical")
  }

  # rank_matrix_A <- rnk_matrix(data = cbind(alpha * X[1:(N-tau),], Y[1:(N-tau),]), method = method)
  #
  # # Find indices of nearest neighbors in space A ((alpha*X0, Y0))
  # nns_A <- nns_index_array(rank_matrix_A, k = k)
  #
  # rank_matrix_Y <- rnk_matrix(data = Y[(1+tau):N,], method = method)

  if(tau<0){
    rank_matrix_A <- rnk_matrix(data = cbind(alpha * X[(abs(tau)+1):N,], Y[(abs(tau)+1):N,]), method = method)
    rank_matrix_Y <- rnk_matrix(data = Y[1:(N-abs(tau)),], method = method)

  }else{
    rank_matrix_A <- rnk_matrix(data = cbind(alpha * X[1:(N-tau),], Y[1:(N-tau),]), method = method)
    rank_matrix_Y <- rnk_matrix(data = Y[(1+tau):N,], method = method)
  }

  # Find indices of nearest neighbors in space A ((alpha*X0, Y0))
  nns_A <- nns_index_array(rank_matrix_A, k = k)

  conditional_ranks_B <- sapply(
    1:(N-abs(tau)),
    FUN = function(row)
      mean(rank_matrix_Y[row, ][nns_A[row, ]])
  )

  # Compute the Information Imbalance:
  info_imbalance <- 2 / N * mean(conditional_ranks_B)

  return(info_imbalance)
}
