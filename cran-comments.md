## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new release.

* Examples with CPU (user + system) or elapsed time > 5s:
                     user system elapsed
multiJI             23.89   0.17   25.55
perm_rollEWS        18.94   0.21   21.03
multi_smap_jacobian 11.74   0.01   12.76
uniJI                7.11   0.01    7.19

These functions cannot be sped up further due to bottlenecks occurring in the dependency. Data has been shrunk to minimise example run time.
