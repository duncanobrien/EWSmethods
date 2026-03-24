# Three Recovering Cod Populations

A dataset containing three simulated cod populations. Community 1 does
not recovery whereas Community 100 and 200 do.

## Usage

``` r
CODrecovery
```

## Format

A list of three dataframes with 191 rows and 6 variables each:

- community_id:

  the identity of the simulated community

- time:

  time index

- biomass:

  population total biomass

- mean.size:

  average length of cod individuals

- sd.size:

  variation in length of cod individuals

- inflection_pt:

  the time index where transition occurs

## Source

Clements C., McCarthy M., Blanchard J. (2019) Early warning signals of
recovery in complex systems. Nature Communications, 10:1681.

## Examples

``` r
data("CODrecovery", package = "EWSmethods")

cod_data <- CODrecovery$scenario2
```
