# Information Imbalance

Estimates the information imbalance of two hypothesised linked system
measurements for a given scalar (\`alpha\`).

## Usage

``` r
II(X, Y, tau = 1, alpha = 1, k = 1, method = "euclidean")
```

## Source

Del Tatto, V., Bueti, D. & Laio, A. (2024) Robust inference of causality
in high-dimensional dynamical processes from the Information Imbalance
of distance ranks. PNAS 121 (19) e2317256121.

## Arguments

- X:

  Numeric matrix of hypothesised driving variable measurements. If
  univariate, call \`embed_ts(X)\` prior to calling \`II()\`.

- Y:

  Numeric matrix of hypothesised response variable measurements. If
  univariate, call \`embed_ts(Y)\` prior to calling \`II()\`.

- tau:

  Numeric. Time lag of information transfer between X and Y.

- alpha:

  Numeric. Scaling parameter for X. If information imbalance is
  minimised at an \`alpha\` \> 0, this may be indicative of Granger
  causality.

- k:

  Numeric. Number of nearest neighbours when estimating ranks.

- method:

  String. Distance measure to be used - defaults to \`euclidean\` but
  see \`?dist\` for options.

## Value

Information imbalance

## Examples

``` r
#Load the multivariate simulated
#dataset `simTransComms`

data(simTransComms)

#Embed the spp_2 and spp_5 of the third community

embedX <- embed_ts(X = simTransComms$community3[,c("time","spp_2")],
E = 5, tau = 1)

embedY <- embed_ts(X = simTransComms$community3[,c("time","spp_5")],
E = 5, tau = 1)

#Estimate the forward information imbalance
#between spp_2 and spp_5

egII_for <- II(X = embedX[,-1], Y = embedY[,-1],
tau = 1, alpha = 1, k = 5)

#Estimate the reverse information imbalance
#between spp_2 and spp_5

egII_rev <- II(X = embedY[,-1], Y = embedX[,-1],
tau = 1, alpha = 1, k = 5)
```
