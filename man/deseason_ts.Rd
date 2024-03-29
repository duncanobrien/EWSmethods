% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deseason_ts.R
\name{deseason_ts}
\alias{deseason_ts}
\title{Deseason Seasonal Time Series}
\usage{
deseason_ts(
  data,
  increment = c("month", "year", "week", "day"),
  method = c("average", "decompose", "stl"),
  order = NULL
)
}
\arguments{
\item{data}{The dataframe to be transformed, The first column must be a vector of dates with all other columns the individual time series.}

\item{increment}{The time-step increment in either \code{month}, \code{year}, \code{week}, or \code{day}. Provides the basis for deaseasoning.}

\item{method}{String of either \code{"average"}, \code{"decompose"}, \code{"stl"} or \code{"x11"} indicating the method of deseasoning. \code{"average"} subtracts the average representative month/week/day-of-the-year from each time point whereas \code{"decompose"}, \code{"stl"} and \code{"x11"} subtracts the seasonal component estimated by time series decomposition, loess decomposition and the X11 method respectively.}

\item{order}{String indicating the date format of the date columns. Options are \code{"dmy"}, \code{"ymd"} or \code{"mdy"}.}
}
\value{
Dataframe of deseasoned time series.
}
\description{
Removes seasonal signals from time series using either averaging or time series decomposition methods. Three decomposition methods are available: traditional decompostion, loess decomposition and X11 decompostion.
}
\examples{
#Generate five random monthly time series
#of 5 years length.

spp_data <- matrix(nrow = 5*12, ncol = 5)
spp_data <- sapply(1:dim(spp_data)[2], function(x){
spp_data[,x] <- rnorm(5*12,mean=20,sd=5)})
multi_spp_data <- cbind("time" =
 seq(as.Date('2000/01/01'), as.Date('2004/12/01'), by="month"),
   as.data.frame(spp_data))

#Deseason using time series
#decomposition.

decomp_dat <- deseason_ts(data = multi_spp_data,
increment = "month",
method = "decompose",
order = "ymd")

#Deseason using loess

decomp_dat <- deseason_ts(data = multi_spp_data,
increment = "month",
method = "stl",
order = "ymd")

}
