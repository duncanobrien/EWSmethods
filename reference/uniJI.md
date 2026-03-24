# Univariate S-map Jacobian index function

Calculate a stability metric from the s-map estimated Jacobian of a
univariate time series

## Usage

``` r
uniJI(data, winsize = 50, theta_seq = NULL, E = 1, tau = NULL, scale = TRUE)
```

## Source

Grziwotz, F., Chang, C.-W., Dakos, V., van Nes, E.H., Schwarzländer, M.,
Kamps, O., et al. (2023). Anticipating the occurrence and type of
critical transitions. Science Advances, 9.

## Arguments

- data:

  Numeric matrix with time in first column and species abundance in the
  second

- winsize:

  Numeric. Defines the window size of the rolling window as a percentage
  of the time series length.

- theta_seq:

  Numeric vector of thetas (nonlinear tuning parameters) to estimate the
  Jacobian over. If \`NULL\`, a default sequence is provided.

- E:

  Numeric. The embedding dimension. Is suggested to be positive.

- tau:

  Numeric. The time-delay offset to use for time delay embedding.
  Suggested to be positive here, but if not provided, is set to 10% the
  length of the time series.

- scale:

  Boolean. Should data be scaled prior to estimating the Jacobian.

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

egJI <- uniJI(data = pre_simTransComms[1:25,2:3],
winsize = 75, E = 3)
```
