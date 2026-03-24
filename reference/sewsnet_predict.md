# S-EWSNet Predict

Communicates with S-EWSNet (https://doi.org/10.1098/rsos.231767), a deep
learning framework for modelling and anticipating regime shifts in
dynamical spatial systems, and returns the model's prediction for the
inputted spatial time series.

## Usage

``` r
sewsnet_predict(
  x,
  id = NULL,
  envname,
  delta = 0.1,
  inp_size = 25,
  model_path = default_sewsnet_path()
)
```

## Source

Deb, S., Ekansh, M., Paras, G. et al. (2024) Optimal sampling of spatial
patterns improves deep learning-based early warning signals of critical
transitions. Royal Society Open Science. 11, 231767.

## Arguments

- x:

  A list of square integer matrices representing presence/absence
  pixels. Pixels could be vegetation presence. Ensure entires are
  integers not numeric.

- id:

  Vector identifying each entry in x. Could be year, plot identity etc.

- envname:

  A string naming the Python environment prepared by
  [`ewsnet_init()`](https://duncanobrien.github.io/EWSmethods/reference/EWSNET_init.md).

- delta:

  Numeric. Difference in densities.

- inp_size:

  Numeric. Size of clipped Fourier transformed square matrix.

- model_path:

  A string naming the path to the S-EWSnet model installed by
  [`sewsnet_reset()`](https://duncanobrien.github.io/EWSmethods/reference/sewsnet_reset.md).

## Value

A dataframe of S-EWSNet predictions. Values represent the estimated
probability that the quoted event will occur.

## Examples

``` r
#A dummy dataset of a patchy savanna
#monitored over 10 sites/years.

vegetation_data <- vector("list", length = 50)
vegetation_data <- lapply(vegetation_data,function(x){
matrix(rbinom(128^2,1,0.6),nrow = 128,ncol=128)
})

#Activate python environment (only necessary
#on first opening of R session).

if (FALSE) { # \dontrun{
ewsnet_init(envname = "EWSNET_env")
} # }

#Generate EWSNet predictions.

if (FALSE) { # \dontrun{
pred <- sewsnet_predict(
 vegetation_data,
 delta = 0.1,
 inp_size = 25,
 envname = "EWSNET_env")
 } # }
```
