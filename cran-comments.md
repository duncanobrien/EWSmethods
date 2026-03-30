## Submission
In this version I have:

* Hotfix - replaced stats::ar.ols with stats::ar to compensate for R-devel changes. Affected functions: uniAR, W_composite_ews, no.plot.ews.
* Minor patch - added functions sewsnet_reset and sewsnet_predict, fixed depreciated arguments of ggplots::margins, fixed broken URL called by ewsnet_reset

## R CMD check results

0 errors \| 0 warnings \| 0 notes
