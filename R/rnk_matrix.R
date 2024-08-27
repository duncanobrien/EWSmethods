#' Calculate Rank Matrix
#'
#' Estimates the ranks of matrix elements using a distance matrix.
#'
#' @param data Numeric matrix of timeseries.
#' @param method String indicating the distance measure to estimate.
#'
#' @returns Matrix of ranks.
#'
#' @keywords internal
#' @noRd

rnk_matrix <- function(data,method = "euclidean"){

  pairwise_dist <- as.matrix(stats::dist(data, method = method), labels = FALSE)
  diag(pairwise_dist) <- NA

  if("data.table" %in% rownames(utils::installed.packages())){
    out <- t(apply(
      pairwise_dist,
      MARGIN = 1,
      FUN = function(row)
        data.table::frank(
          base::xtfrm(row),
          na.last = TRUE,
          ties.method = "average"
        )
    ))
  }else{
  out <- t(apply(
    pairwise_dist,
    MARGIN = 1,
    FUN = function(row)
      base::rank(
        base::xtfrm(row),
        na.last = TRUE,
        ties.method = "average"
      )
  ))
  }

  diag(out) <- Inf

  return(out)

}
