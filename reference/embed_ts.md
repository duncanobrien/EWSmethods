# Construct an Embedded Timeseries

Embeds timeseries given an embedding dimension (\`E\`) and a time lag
(\`tau\`).

## Usage

``` r
embed_ts(X, E, tau = 1, sample_times = NULL)
```

## Arguments

- X:

  A numeric matrix of species abundances, names across columns, time
  across rows. The first column is a time vector, the remainder are
  species values.

- E:

  Numeric. Embedding dimension.

- tau:

  Numeric. Time lag.

- sample_times:

  Numeric vector. Defines the time indices to subset prior to embedding.

## Value

A matrix where the first column is last time index of the window and the
second column is the estimated index value.

## Examples

``` r
#Load the multivariate simulated
#dataset `simTransComms`

data(simTransComms)

#Embed one timeseries by 5 time points

eg_embed <- embed_ts(X = simTransComms$community2[,2:3],
E = 5, tau = 1)
```
