# Multivariate Jacobian Index Estimated From a Multivariate Autocorrelation Matrix

Estimate the dominant Jacobian eigenvalue of a multivariate time series
using autocorrelated stochastic differential equations

## Usage

``` r
multiAR(data, scale = TRUE, winsize = 50, p = 1, dt = 1)
```

## Source

Williamson and Lenton (2015). Detection of bifurcations in noisy coupled
systems from multiple time series. Chaos, 25, 036407

## Arguments

- data:

  Numeric matrix with time in first column and species abundance in the
  remainder.

- scale:

  Boolean. Should data be scaled prior to estimating the Jacobian.

- winsize:

  Numeric. Defines the window size of the rolling window as a percentage
  of the time series length.

- p:

  Numeric. Defines the model order. Defaults to \`1\`.

- dt:

  Numeric An appropriate time step

## Value

A dataframe where the first column is last time index of the window and
the second column is the estimated index value. A value \<1.0 indicates
stability, a value \>1.0 indicates instability.

## Examples

``` r
#Load the multivariate simulated
#dataset `simTransComms`

data(simTransComms)

#Subset the second community prior to the transition

pre_simTransComms <- subset(simTransComms$community2,time < inflection_pt)

#Estimate the univariate stability index for the first species in
#the second community

egarJ <- multiAR(data = pre_simTransComms[,2:7],
winsize = 25, dt = 1)
```
