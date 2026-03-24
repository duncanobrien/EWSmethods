# Significance Testing of Rolling Window Early Warning Signals

A function for identifying whether a warning has been generated from
rolling early warning signal data using permutation tests. If a parallel
connection is setup via `parallel` or `future` prior to usage of
`perm_rollEWS()`, then the function is parallelised.

## Usage

``` r
perm_rollEWS(
  data,
  metrics,
  winsize = 50,
  variate = c("uni", "multi"),
  perm.meth = "arima",
  iter = 500
)
```

## Arguments

- data:

  A dataframe where the first column is an equally spaced time vector
  and the remainder column are the time series to be assessed. If a two
  column dataframe is provided, and `variate = "uni"`,
  [`uniEWS()`](https://duncanobrien.github.io/EWSmethods/reference/uniEWS.md)
  is called, whereas if number of columns exceeds two &
  `variate = "multi"`, then
  [`multiEWS()`](https://duncanobrien.github.io/EWSmethods/reference/multiEWS.md)
  is called.

- metrics:

  String vector of early warning signal metrics to be assessed. For
  `variate = "uni"` these include: `"ar1"`, `"cv"`, `"SD"`, `"acf"`,
  `"rr"`, `"dr"`, `"skew"` and `"kurt"`. For `variate = "multi"`,
  pptions include: `"meanSD"`, `"maxSD"`, `"meanAR"`, `"maxAR"`,
  `"eigenMAF"`, `"mafAR"`, `"mafSD"`, `"pcaAR"`, `"pcaSD"`,
  `"eigenCOV"`, `"maxCOV"` and `"mutINFO"`.

- winsize:

  Numeric value. Defines the window size of the rolling window as a
  percentage of the time series length.

- variate:

  String. Is a `"uni"`variate or `"multi"`variate assessment to be made.

- perm.meth:

  String dictating the pseudo-randomisation technique to be used.
  Options include: "arima" (sampled from predictions of an ARIMA model),
  "red.noise" (red noise process using data mean, variance and
  autocorrelation coef) or "sample" (sampled from observed data without
  replacement).

- iter:

  Numeric value. The number of permutations.

## Value

A list containing up to two objects: EWS outputs through time (`EWS`),
and an identifier string (`method`).

- EWS\$raw:

  Dataframe of EWS measurements through time. Each metric's evolution
  over time is returned in individual columns.

- EWS\$cor:

  Dataframe of Kendall Tau correlations and permuted p-values.

- EWS\$dimred.ts:

  Dataframe containing the dimension reduction time series. Only
  returned if `variate = "multi"`.

## Examples

``` r
data(simTransComms)

#Permute p value for a univariate
#time series using resampling

#(data trimmed for speed)
perm_uni <- perm_rollEWS(
data = simTransComms$community1[1:10,2:3],
 winsize = 75,
 variate = "uni",
 metrics = c("ar1", "SD", "skew"),
 perm.meth = "sample",
 iter = 25)

 #Permute p value for a multivariate
#community using a red.noise process

# \donttest{
#if parallelisation desired,
#this can be achieved using the
#below code
cl <- parallel::makeCluster(2)
# }

# \donttest{
doParallel::registerDoParallel(cl)
# }

perm_multi <- perm_rollEWS(
data = simTransComms$community1[1:10,2:7],
 winsize = 75,
 variate = "multi",
 metrics = c("meanAR", "maxAR", "meanSD"),
 perm.meth = "red.noise",
 iter = 25)
#> Warning: the standard deviation is zero

# \donttest{
parallel::stopCluster(cl)
# }
```
