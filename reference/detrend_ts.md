# Detrend Time Series

Removes directional signals from time series using loess, linear
regression or gaussian detrending.

## Usage

``` r
detrend_ts(data, method = "linear", bandwidth = NULL, span = 0.25, degree = 2)
```

## Arguments

- data:

  The dataframe to be detrended. The first column must be a vector of
  dates with all other columns the individual time series.

- method:

  The method of detrending. Options include `"linear"` (residuals of a
  linear regression), `loess` (smoothing by local polynomial
  regression), `gaussian` (smoothing by a gaussian kernel), or
  `first.difference`.

- bandwidth:

  If `method = "gaussian"`, dictates the bandwidth of the gaussian
  kernel. If `NULL`, this is estimated from the data.

- span:

  If `method = "loess"`, controls the degree of smoothing as a
  proportion of points to be used (if `span = 1`, all points are used)

- degree:

  If `method = "loess"`, specifies the degree polynomials allowed.
  Options are normally `1` or `2`.

## Value

Dataframe of deseasoned time series.

## Examples

``` r
#Generate five random monthly time series
#of 5 years length.

spp_data <- matrix(nrow = 5*12, ncol = 5)
spp_data <- sapply(1:dim(spp_data)[2], function(x){
spp_data[,x] <- rnorm(5*12,mean=20,sd=5)})
multi_spp_data <- cbind("time" =
 seq(as.Date('2000/01/01'), as.Date('2004/12/01'), by="month"),
   as.data.frame(spp_data))

detrend_dat <- detrend_ts(data = multi_spp_data,
method = "gaussian",
bandwidth = 2)
```
