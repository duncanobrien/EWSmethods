# Package index

## Early warning signal assessment

Functions for calculating univariate and multivariate early warning
signals.

- [`uniEWS()`](https://duncanobrien.github.io/EWSmethods/reference/uniEWS.md)
  : Univariate Early Warning Signal Assessment
- [`multiEWS()`](https://duncanobrien.github.io/EWSmethods/reference/multiEWS.md)
  : Multivariate Early Warning Signal Assessment
- [`perm_rollEWS()`](https://duncanobrien.github.io/EWSmethods/reference/perm_rollEWS.md)
  : Significance Testing of Rolling Window Early Warning Signals
- [`plot(`*`<EWSmethods>`*`)`](https://duncanobrien.github.io/EWSmethods/reference/plot.EWSmethods.md)
  : Plot an EWSmethods object

## EWSNet

Functions for interfacing with EWSNet.

- [`ewsnet_init()`](https://duncanobrien.github.io/EWSmethods/reference/EWSNET_init.md)
  : EWSNet Initialisation
- [`ewsnet_finetune()`](https://duncanobrien.github.io/EWSmethods/reference/ewsnet_finetune.md)
  : EWSNet Finetune
- [`ewsnet_predict()`](https://duncanobrien.github.io/EWSmethods/reference/ewsnet_predict.md)
  : EWSNet Predict
- [`ewsnet_reset()`](https://duncanobrien.github.io/EWSmethods/reference/ewsnet_reset.md)
  : Reset EWSNet Model Weights
- [`conda_clean()`](https://duncanobrien.github.io/EWSmethods/reference/conda_clean.md)
  : Python Removal
- [`default_weights_path()`](https://duncanobrien.github.io/EWSmethods/reference/default_weights_path.md)
  : Path to Model Weights

## S-EWSNet

Functions for interfacing with S-EWSNet.

- [`sewsnet_predict()`](https://duncanobrien.github.io/EWSmethods/reference/sewsnet_predict.md)
  : S-EWSNet Predict
- [`sewsnet_reset()`](https://duncanobrien.github.io/EWSmethods/reference/sewsnet_reset.md)
  : Reset S-EWSNet Model
- [`default_sewsnet_path()`](https://duncanobrien.github.io/EWSmethods/reference/default_sewsnet_path.md)
  : Path to S-EWSNet Model

## Information imbalance

Functions for estimating Granger causality via information imbalance.

- [`II()`](https://duncanobrien.github.io/EWSmethods/reference/II.md) :
  Information Imbalance
- [`tuneII()`](https://duncanobrien.github.io/EWSmethods/reference/tuneII.md)
  : Information Imbalance Across Alphas
- [`imbalance_gain()`](https://duncanobrien.github.io/EWSmethods/reference/imbalance_gain.md)
  : Information Gain

## Other stability metrics

Additional functions estimating alternative measures of system
stability/resilience.

- [`FI()`](https://duncanobrien.github.io/EWSmethods/reference/FI.md) :
  Calculate Fisher Information
- [`uniJI()`](https://duncanobrien.github.io/EWSmethods/reference/uniJI.md)
  : Univariate S-map Jacobian index function
- [`uni_smap_jacobian()`](https://duncanobrien.github.io/EWSmethods/reference/uni_smap_jacobian.md)
  : Univariate S-map Inferred Jacobian
- [`multiJI()`](https://duncanobrien.github.io/EWSmethods/reference/multiJI.md)
  : Multivariate S-map Jacobian index function
- [`multi_smap_jacobian()`](https://duncanobrien.github.io/EWSmethods/reference/multi_smap_jacobian.md)
  : Multivariate S-map Inferred Jacobian
- [`mvi()`](https://duncanobrien.github.io/EWSmethods/reference/mvi.md)
  : Multivariate Variance Index function
- [`uniAR()`](https://duncanobrien.github.io/EWSmethods/reference/uniAR.md)
  : Univariate Jacobian Index Estimated From an Univariate
  Autocorrelation Matrix
- [`multiAR()`](https://duncanobrien.github.io/EWSmethods/reference/multiAR.md)
  : Multivariate Jacobian Index Estimated From a Multivariate
  Autocorrelation Matrix

## Data preparation

Functions useful for preprocessing data prior to analysis.

- [`detrend_ts()`](https://duncanobrien.github.io/EWSmethods/reference/detrend_ts.md)
  : Detrend Time Series
- [`deseason_ts()`](https://duncanobrien.github.io/EWSmethods/reference/deseason_ts.md)
  : Deseason Seasonal Time Series
- [`embed_ts()`](https://duncanobrien.github.io/EWSmethods/reference/embed_ts.md)
  : Construct an Embedded Timeseries

## Datasets

The exemplary datasets used in `EWSmethods` tutorials and testing.

- [`simTransComms`](https://duncanobrien.github.io/EWSmethods/reference/simTransComms.md)
  : Three Simulated Transitioning Communities.
- [`CODrecovery`](https://duncanobrien.github.io/EWSmethods/reference/CODrecovery.md)
  : Three Recovering Cod Populations
