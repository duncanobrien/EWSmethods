# Multivariate S-map Jacobian index function

Calculate a stability metric from the multivariate s-map estimated
Jacobian

## Usage

``` r
multiJI(data, winsize = 50, theta_seq = NULL, scale = TRUE)
```

## Source

Ushio, M., Hsieh, Ch., Masuda, R. et al. (2018) Fluctuating interaction
network and time-varying stability of a natural fish community. Nature
554, 360–363.

## Arguments

- data:

  Numeric matrix with time in first column and species abundances in
  other columns

- winsize:

  Numeric. Defines the window size of the rolling window as a percentage
  of the time series length.

- theta_seq:

  Numeric vector of thetas (nonlinear tuning parameters) to estimate the
  Jacobian over. If \`NULL\`, a default sequence covering \`0:8\` is
  provided.

- scale:

  Boolean. Should data be scaled within each window prior to estimating
  the Jacobian.

## Value

A dataframe where the first column is last time index of the window and
the second column is the estimated index value. A value \<1.0 indicates
stability, a value \>1.0 indicates instability.

## Examples

``` r
#Load the multivariate simulated
#dataset `simTransComms`

data(simTransComms)

#Subset the third community prior to the transition

pre_simTransComms <- subset(simTransComms$community3,time < inflection_pt)

#Estimate the stability index for the third community
#(trimmed for speed)

egJI <- multiJI(data = pre_simTransComms[1:10,2:5],
winsize = 75)
```
