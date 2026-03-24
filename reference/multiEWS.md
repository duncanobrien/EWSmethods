# Multivariate Early Warning Signal Assessment

A single function for performing early warning signal (EWS) assessment
on multivariate systems where multiple time series have been measured.
Both methods of EWS assessment can be performed (rolling or expanding
windows) with the assessments returned as a dataframe. The two methods
of dimension reduction used to perform these assessments are Principal
Component Analysis and Maximum/Minimum Autocorrelation Factors.

## Usage

``` r
multiEWS(
  data,
  metrics = c("meanAR", "maxAR", "meanSD", "maxSD", "eigenMAF", "mafAR", "mafSD",
    "pcaAR", "pcaSD", "eigenCOV", "maxCOV", "mutINFO"),
  method = c("expanding", "rolling"),
  winsize = 50,
  burn_in = 5,
  threshold = 2,
  tail.direction = "one.tailed"
)
```

## Arguments

- data:

  A dataframe where the first column is an equally spaced time vector
  and all other columns are individual time series. These could be
  different species, populations or measurements.

- metrics:

  String vector of early warning signal metrics to be assessed. Options
  include: `"meanSD"`, `"maxSD"`, `"meanAR"`, `"maxAR"`, `"eigenMAF"`,
  `"mafAR"`, `"mafSD"`, `"pcaAR"`, `"pcaSD"`, `"eigenCOV"`, `"maxCOV"`
  and `"mutINFO"`.

- method:

  Single string of either `"expanding"` or `"rolling"`. `"expanding"`
  calls composite, expanding window EWS assessment. `"rolling"` calls
  typical, rolling window EWS assessment.

- winsize:

  Numeric value. If `method = "rolling"`, defines the window size of the
  rolling window as a percentage of the time series' length.

- burn_in:

  Numeric value. If `method = "expanding"`, defines the number of data
  points to 'train' signals prior to EWS assessment.

- threshold:

  Numeric value of either `1` or `2`. Threshold\*sigma is the value
  which, if the EWS strength exceeds it, constitutes a "signal".

- tail.direction:

  String of either `"one.tailed"` or `"two.tailed"`. `"one.tailed"` only
  indicates a warning if positive threshold sigma exceeded.
  `"two.tailed"` indicates a warning if positive OR negative
  threshold\*sigma exceeded.

## Value

A list containing up to two objects: EWS outputs through time (`EWS`),
and an identifier string (`method`).

- EWS\$raw:

  Dataframe of EWS measurements through time. If `method = "expanding"`,
  then each metric has been rbound into a single dataframe and extra
  columns are provided indicating whether the threshold\*sigma value has
  been exceeded (i.e. `"threshold.crossed"`). If `method = "rolling"`,
  then each metric's evolution over time is returned in individual
  columns.

- EWS\$dimred.ts:

  Dataframe containing the dimension reduction time series

- EWS\$cor:

  Dataframe of Kendall Tau correlations. Only returned if
  `method = "rolling"`.

## Examples

``` r
#Generate a random five species, non-transitioning
#ecosystem with 50 years of monitoring data.

spp_data <- matrix(nrow = 50, ncol = 5)
spp_data <- sapply(1:dim(spp_data)[2], function(x){
 spp_data[,x] <- rnorm(50,mean=20,sd=5)})
 multi_spp_data <- as.data.frame(cbind("time" =
 seq(1:50), spp_data))

#Rolling window early warning signal assessment of
#the ecosystem.

roll_ews <- multiEWS(
 data = multi_spp_data,
 method = "rolling",
 winsize = 50)
#> Warning: the standard deviation is zero

#Expanding window early warning signal assessment of
#the ecosystem.

exp_ews <- multiEWS(
 data = multi_spp_data,
 method = "expanding",
 burn_in = 10)
```
