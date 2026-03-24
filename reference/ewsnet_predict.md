# EWSNet Predict

Communicates with EWSNet (https://ewsnet.github.io), a deep learning
framework for modelling and anticipating regime shifts in dynamical
systems, and returns the model's prediction for the inputted univariate
time series.

## Usage

``` r
ewsnet_predict(
  x,
  scaling = TRUE,
  ensemble = 25,
  envname,
  weights_path = default_weights_path()
)
```

## Arguments

- x:

  A numeric vector of values to be tested.

- scaling:

  Boolean. If `TRUE`, the time series will be scaled between 1 and 2 and
  scaled EWSNet model weights will be used. This is the recommended
  setting.

- ensemble:

  A numeric value stating the number of models to average over. Options
  range from 1 to 25.

- envname:

  A string naming the Python environment prepared by
  [`ewsnet_init()`](https://duncanobrien.github.io/EWSmethods/reference/EWSNET_init.md).

- weights_path:

  A string naming the path to model weights installed by
  [`ewsnet_reset()`](https://duncanobrien.github.io/EWSmethods/reference/ewsnet_reset.md).

## Value

A dataframe of EWSNet predictions. Values represent the estimated
probability that the quoted event will occur.

## Examples

``` r
#A dummy dataset of a hedgerow bird population
#monitored over 50 years.

abundance_data <- data.frame(time = seq(1:50),
 abundance = rnorm(50,mean = 20))

#Activate python environment (only necessary
#on first opening of R session).

if (FALSE) { # \dontrun{
ewsnet_init(envname = "EWSNET_env")
} # }

#Generate EWSNet predictions.

if (FALSE) { # \dontrun{
pred <- ewsnet_predict(
 abundance_data$abundance,
 scaling = TRUE,
 ensemble = 15,
 envname = "EWSNET_env")
 } # }
```
