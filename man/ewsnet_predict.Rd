% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ewsnet_predict.R
\name{ewsnet_predict}
\alias{ewsnet_predict}
\title{EWSNET Predict}
\usage{
ewsnet_predict(x, noise_type = "W", ensemble = 25, envname)
}
\arguments{
\item{x}{A numeric vector of values to be tested.}

\item{noise_type}{A string stating the form of noise to use. Options are "W" or "C".}

\item{ensemble}{A numeric value stating the number of models to average over. Options range from 1 to 25.}

\item{envname}{A string naming the Python environment prepared by \code{ewsnet_int()}.}
}
\value{
A dataframe of EWSNET predictions. Values represent the estimated probability that the quoted event will occur.
}
\description{
Communicates with EWSNet (https://ewsnet.github.io), a deep learning framework for modeling and anticipating regime shifts in dynamical systems, and returns the model's prediction for the inputted univariate time series.
}
\examples{
#A dummy dataset of a hedgerow bird population
#monitored over 50 years.

abundance_data <- data.frame(time = seq(1:50),
 abundance = rnorm(50,mean = 20))

#Activate python environment (only necessary
#on first opening of R session).

\dontrun{ewsnet_init(envname = "EWSNET_env")}

#Generate EWSNET predictions.

\dontrun{pred <- ewsnet_predict(
 abundance_data$abundance,
 noise_type = "W",
 ensemble = 15,
 envname = "EWSNET_env")}

}