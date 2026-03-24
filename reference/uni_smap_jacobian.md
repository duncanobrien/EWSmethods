# Univariate S-map Inferred Jacobian

Performs the S-map on a univariate time series to infer the Jacobian
matrix at different points in time across thetas.

## Usage

``` r
uni_smap_jacobian(data, theta_seq = NULL, E = 1, tau = NULL, scale = TRUE)
```

## Source

Grziwotz, F., Chang, C.-W., Dakos, V., van Nes, E.H., Schwarzländer, M.,
Kamps, O., et al. (2023). Anticipating the occurrence and type of
critical transitions. Science Advances, 9.

## Arguments

- data:

  Numeric matrix with time in first column and species abundance in the
  second

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

A list containing three objects:

- smap_J:

  Jacobian matrices across taus. It is recommended to average across
  these matrices.

- eigenJ:

  Absolute maximum eigenvalue.

- reJ:

  Real component of dominant eigenvalue

- imJ:

  Imaginary component of dominant eigenvalue.

## Examples

``` r
#Load the multivariate simulated
#dataset `simTransComms`

data("simTransComms")

#Subset the second community prior to the transition

pre_simTransComms <- subset(simTransComms$community2,time < inflection_pt)
winsize <- round(dim(pre_simTransComms)[1] * 50/100)

#Estimate the Jacobian for the first 50 timepoints of the
#second species using s-map
est_jac <- uni_smap_jacobian(pre_simTransComms[1:50,2:3])
```
