# EWSNet Finetune

Communicates with EWSNet (https://ewsnet.github.io), a deep learning
framework for modelling and anticipating regime shifts in dynamical
systems, and finetunes the model to match the inputted training data.
This overwrites the Pretrained weights bundled with `EWSmethods`. See
`reset_ewsnet()` on how to reset these trained weights.

## Usage

``` r
ewsnet_finetune(
  x,
  y,
  scaling = TRUE,
  envname,
  weights_path = default_weights_path()
)
```

## Arguments

- x:

  A numeric matrix to finetune EWSNet on. Each column represents a
  separate timeseries and each row is a timestep.

- y:

  A numeric vector consisting of target labels for each training time
  series. Labels include: 0 (no transition), 1 (smooth transition) or 2
  (critical transition).

- scaling:

  Boolean. If `TRUE`, the time series will be scaled between 1 and 2 and
  scaled EWSNet model weights will be used. This is the recommended
  setting.

- envname:

  A string naming the Python environment prepared by
  [`ewsnet_init()`](https://duncanobrien.github.io/EWSmethods/reference/EWSNET_init.md).

- weights_path:

  A string naming the path to model weights installed by
  [`ewsnet_reset()`](https://duncanobrien.github.io/EWSmethods/reference/ewsnet_reset.md).

## Value

No return value, called for side effects.

## Examples

``` r
#Activate python environment (only necessary
#on first opening of R session).

if (FALSE) { # \dontrun{
ewsnet_init(envname = "EWSNET_env")
} # }

#A dummy dataset of a hedgerow bird population
#monitored over 50 years that needs to be tuned.

abundance_data <- data.frame(time = seq(1:50),
 abundance = rnorm(50,mean = 20))

#Generate training data (this is random data as
#an example).

x <- matrix(nrow = 50, ncol = 10)
x <- sapply(1:dim(x)[2], function(i){
 x[,i] <- rnorm(50,mean=20,sd=10)})

#Label each time series.

y <- sample(0:2,10,replace = TRUE)

#Finetune EWSNet.

if (FALSE) { # \dontrun{
ewsnet_finetune(
 x = x,
 y = y,
 scaling = TRUE,
 envname = "EWSNET_env")
 } # }

#Generate new EWSNet predictions.

if (FALSE) { # \dontrun{
pred <- ewsnet_predict(
 abundance_data$abundance,
 scaling = TRUE,
 ensemble = 15,
 envname = "EWSNET_env")
 } # }
```
