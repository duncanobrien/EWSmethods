#' Construct an Embedded Timeseries
#'
#' Embeds timeseries given an embedding dimension (`E`) and a time lag (`tau`).
#'
#' @param X A numeric matrix of species abundances, names across columns, time across rows. The first column is a time vector, the remainder are species values.
#' @param E Numeric. Embedding dimension.
#' @param tau Numeric. Time lag.
#' @param sample_times Numeric vector. Defines the time indices to subset prior to embedding.
#'
#' @returns A matrix where the first column is last time index of the window and the second column is the estimated index value.
#'
#' @examples
#' #Load the multivariate simulated
#' #dataset `simTransComms`
#'
#' data(simTransComms)
#'
#' #Embed one timeseries by 5 time points
#'
#' eg_embed <- embed_ts(X = simTransComms$community2[,2:3],
#' E = 5, tau = 1)
#'
#' @export

embed_ts <- function(X,
                    E,
                    tau = 1,
                    sample_times = NULL) {
  if (is.null(sample_times)) {
    N <- dim(X)[1]
    start_time <- E * tau

    X_time_delay <- matrix(0, nrow = N - start_time, ncol = E)
    X_time_delay[, 1] <- X[(start_time + 1):N, 2]

    for (i in 2:E) {
      #X_time_delay[, i + 1] = X[(start_time + 1 - (i * tau)):(N - i * tau)]
      X_time_delay[, i] = X[(start_time + 2 - (i * tau)):(N + 1 - i * tau), 2]
    }
    X_time_delay <- cbind(X[(start_time + 1):N, 1], X_time_delay)

  } else {
    start_time <- E * tau

    sample_times <- sample_times[sample_times > start_time] #only select sample times inside the embedding dimension

    Nstops <- length(sample_times)
    X_time_delay <- matrix(0, nrow = Nstops, ncol = E)

    for (i in 1:Nstops) {
      embedding_times <- seq(sample_times[i], (sample_times[i] + 1 - tau * E), by = -tau)
      X_time_delay[i, ] <- X[embedding_times, 2]
    }
    X_time_delay <- cbind(X[sample_times, 1], X_time_delay)
  }

  colnames(X_time_delay) <- c("time", paste0("E_", E:1))

  return(as.data.frame(X_time_delay))
}
