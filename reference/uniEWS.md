# Univariate Early Warning Signal Assessment

A function for performing early warning signal (EWS) assessment on
univariate time series. Both rolling and expanding window methods of EWS
assessment can be performed with the assessments returned as a
dataframe.

## Usage

``` r
uniEWS(
  data,
  metrics,
  method = c("expanding", "rolling"),
  winsize = 50,
  burn_in = 5,
  threshold = 2,
  tail.direction = "one.tailed",
  trait = NULL
)
```

## Arguments

- data:

  A dataframe where the first column is an equally spaced time vector
  and the second column is the time series to be assessed.

- metrics:

  String vector of early warning signal metrics to be assessed. Options
  include: `"ar1"`, `"cv"`, `"SD"`, `"acf"`, `"rr"`, `"dr"`, `"skew"`,
  `"kurt"`, and `"trait"` (only available if `method = "expanding"`).

- method:

  Single string of either `"expanding"` or `"rolling"`. `"expanding"`
  calls composite, expanding window EWS assessment. `"rolling"` calls
  typical, rolling window EWS assessment.

- winsize:

  Numeric value. If `method = "rolling"`, defines the window size of the
  rolling window as a percentage of the time series length.

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

- trait:

  A vector of numeric trait values if desired. Can be `NULL`

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

- EWS\$cor:

  Dataframe of Kendall Tau correlations. Only returned if
  `method = "rolling"`.

## Examples

``` r
#A dummy dataset of a hedgerow bird population over
#25 years where both the number of individuals and
#the average bill length has been measured.

abundance_data <- data.frame(time = seq(1:25),
 abundance = rnorm(25,mean = 20),
 trait = rnorm(25,mean=1,sd=0.5))

#The early warning signal metrics to compute.

ews_metrics <- c("SD","ar1","skew")

#Rolling window early warning signal assessment of
#the bird abundance.

roll_ews <- uniEWS(
 data = abundance_data[,1:2],
 metrics =  ews_metrics,
 method = "rolling",
 winsize = 50)

#Expanding window early warning signal assessment of
#the bird abundance (with plotting).

exp_ews <- uniEWS(
 data = abundance_data[,1:2],
 metrics = ews_metrics,
 method = "expanding",
 burn_in = 10)

#Expanding window early warning signal assessment of
#the bird abundance incorporating the trait
#information.

ews_metrics_trait <- c("SD","ar1","trait")

trait_exp_ews <- uniEWS(
 data = abundance_data[,1:2],
 metrics = ews_metrics_trait,
 method = "expanding",
 burn_in = 10,
 trait = abundance_data$trait)
```
