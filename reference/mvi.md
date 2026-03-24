# Multivariate Variance Index function

Calculate a multivariate variance following Brock, W. A., and S. R.
Carpenter. 2006. Variance as a leading indicator of regime shift in
ecosystem services. Ecology and Society 11(2): 9.

## Usage

``` r
mvi(data, winsize = 50)
```

## Source

Brock, W.A. & Carpenter, S.R. (2006) Variance as a leading indicator of
regime shift in ecosystem services. Ecology and Society 11(2): 9.

## Arguments

- data:

  A numeric matrix of species abundances, names across columns, time
  across rows. The first column is a time vector, the remainder are
  species values.

- winsize:

  Numeric. Defines the window size of the rolling window as a percentage
  of the time series length.

## Value

A matrix where the first column is last time index of the window and the
second column is the estimated index value.

## Examples

``` r
#Load the multivariate simulated
#dataset `simTransComms`

data(simTransComms)

#Estimate the MVI for the second community

egMVI <- mvi(data = simTransComms$community2[,2:7],
winsize = 10)
```
