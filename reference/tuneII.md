# Information Imbalance Across Alphas

Estimates the information imbalance of two hypothesised linked system
measurements using distance ranks.

## Usage

``` r
tuneII(columns, target, tau, alphas, k = 1, method = "euclidean")
```

## Source

Del Tatto, V., Bueti, D. & Laio, A. (2024) Robust inference of causality
in high-dimensional dynamical processes from the Information Imbalance
of distance ranks. PNAS 121 (19) e2317256121.

## Arguments

- columns:

  Numeric matrix of hypothesised driving variable measurements. If
  univariate, call \`embed_ts(X)\` prior to calling \`II()\`.

- target:

  Numeric matrix of hypothesised response variable measurements. If
  univariate, call \`embed_ts(Y)\` prior to calling \`II()\`.

- tau:

  Numeric. Time lag of information transfer between X and Y.

- alphas:

  Numeric vector. Range of X scaling parameters bewtween \`0\` & \`1\`
  inclusive. If information imbalance is minimised at an \`alpha\` \> 0,
  this may be indicative of Granger causality.

- k:

  Numeric. Number of nearest neighbours when estimating ranks.

- method:

  String. Distance measure to be used - defaults to \`euclidean\` but
  see \`?dist\` for options.

## Value

A dataframe of alphas and the estimate information imbalance

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

alphas <- seq(from = 0, to = 1, by = 0.1)

# \donttest{
#if parallelisation desired,
#this can be achieved using the
#below code
cl <- parallel::makeCluster(2)
# }

# \donttest{
doParallel::registerDoParallel(cl)
# }

#Estimate the forward information imbalance
#between spp_2 and spp_5

egII_for <- tuneII(columns = embedX[,-1], target = embedY[,-1],
tau = 1, alphas = alphas, k = 5)

#Estimate the reverse information imbalance
#between spp_2 and spp_5

egII_rev <- tuneII(columns = embedX[,-1], target = embedY[,-1],
tau = 1, alphas = alphas, k = 5)

# \donttest{
parallel::stopCluster(cl)
# }
```
