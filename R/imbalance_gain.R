#' Information Gain
#'
#' Estimates the information imbalance of two hypothesised linked system measurements using distance ranks.
#'
#' @param info_imbalance Dataframe outputted by `tuneII()`.
#
#' @returns A dataframe of the optimal alpha and the estimated information gain.
#'
#' @examples
#' #Load the multivariate simulated
#' #dataset `simTransComms`
#'
#' data(simTransComms)
#'
#' #Embed the spp_4 and spp_3 of the third community
#'
#' embedX <- embed_ts(X = simTransComms$community3[,c("time","spp_4")],
#' E = 5, tau = 1)
#'
#' embedY <- embed_ts(X = simTransComms$community3[,c("time","spp_3")],
#' E = 5, tau = 1)
#'
#' alphas <- seq(from = 0, to = 1, by = 0.1)
#'
#' #Estimate the forward information imbalance
#' #between spp_4 and spp_3
#'
#' egII_for <- tuneII(target = embedX[,-1], columns = embedY[,-1],
#' tau = 1, alphas = alphas, k = 5)
#'
#' #Estimate the reverse information imbalance
#' #between spp_4 and spp_3
#'
#' egII_rev <- tuneII(target = embedY[,-1], columns = embedX[,-1],
#' tau = 1, alphas = alphas, k = 5)
#'
#' #Calculate the information gain
#' igain_for <- imbalance_gain(egII_for)
#' igain_rev <- imbalance_gain(egII_rev)
#'
#' @export
#' @source Del Tatto, V., Bueti, D. & Laio, A. (2024) Robust inference of causality in high-dimensional dynamical processes from the Information Imbalance of distance ranks. PNAS 121 (19) e2317256121.


imbalance_gain <- function(info_imbalance) {
  imbalance_gain <- (info_imbalance[1, 2] - min(info_imbalance[, 2])) / info_imbalance[1, 2]

  out <- subset(info_imbalance, info_imbalance == min(info_imbalance)) |>
    transform(imbalance_gain = imbalance_gain) |>
    _[c("alpha", "imbalance_gain")]

  return(out)

}
