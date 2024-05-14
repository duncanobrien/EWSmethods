#' Find Indices of Rank Matrix
#'
#' @param rank.matrix Numeric matrix of ranks.
#' @param k Numeric number of neighbours.
#'
#' @returns Matrix of rank indices.
#'
#' @keywords internal
#' @noRd
#'

nns_index_array <- function(rank.matrix, k = 1) {
  N <- dim(rank.matrix)[1]

  NNs <- matrix(t(apply(
    rank.matrix,
    MARGIN = 1,
    FUN = function(row)
      base::sort(row, index.return = TRUE)$ix[1:k]
  )), nrow = N, ncol = k)
  return(NNs)
}

