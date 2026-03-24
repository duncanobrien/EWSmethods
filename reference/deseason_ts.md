# Deseason Seasonal Time Series

Removes seasonal signals from time series using either averaging or time
series decomposition methods. Three decomposition methods are available:
traditional decompostion, loess decomposition and X11 decompostion.

## Usage

``` r
deseason_ts(
  data,
  increment = c("month", "year", "week", "day"),
  method = c("average", "decompose", "stl"),
  order = NULL
)
```

## Arguments

- data:

  The dataframe to be transformed, The first column must be a vector of
  dates with all other columns the individual time series.

- increment:

  The time-step increment in either `month`, `year`, `week`, or `day`.
  Provides the basis for deaseasoning.

- method:

  String of either `"average"`, `"decompose"`, `"stl"` or `"x11"`
  indicating the method of deseasoning. `"average"` subtracts the
  average representative month/week/day-of-the-year from each time point
  whereas `"decompose"`, `"stl"` and `"x11"` subtracts the seasonal
  component estimated by time series decomposition, loess decomposition
  and the X11 method respectively.

- order:

  String indicating the date format of the date columns. Options are
  `"dmy"`, `"ymd"` or `"mdy"`.

## Value

Dataframe of deseasoned time series.

## Examples

``` r
#Generate five random monthly time series
#of 5 years length.

spp_data <- matrix(nrow = 5*12, ncol = 5)
spp_data <- sapply(1:dim(spp_data)[2], function(x){
spp_data[,x] <- rnorm(5*12,mean=20,sd=5)})
multi_spp_data <- cbind("time" =
 seq(as.Date('2000/01/01'), as.Date('2004/12/01'), by="month"),
   as.data.frame(spp_data))

#Deseason using time series
#decomposition.

decomp_dat <- deseason_ts(data = multi_spp_data,
increment = "month",
method = "decompose",
order = "ymd")
#> data successfully aggregated into monthly time steps

#Deseason using loess

decomp_dat <- deseason_ts(data = multi_spp_data,
increment = "month",
method = "stl",
order = "ymd")
#> data successfully aggregated into monthly time steps
```
