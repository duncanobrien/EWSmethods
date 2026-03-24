# Three Simulated Transitioning Communities.

A dataset containing three simulated five species communities stressed
through a critical transition.

## Usage

``` r
simTransComms
```

## Format

A list of three dataframes with 301 rows and 7 variables each:

- community_id:

  the identity of the simulated community

- time:

  time index

- spp_1:

  density of species 1

- spp_2:

  density of species 1

- spp_3:

  density of species 1

- spp_4:

  density of species 1

- spp_5:

  density of species 1

- inflection_pt:

  the time index where transition occurs

## Examples

``` r
data("simTransComms", package = "EWSmethods")

community_data <- simTransComms$community1
```
