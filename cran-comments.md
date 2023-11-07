## Policy violation and dependency fix

* Removed policy violating function `bypass_reticulate_autoinit()` (and changed associated tests) which altered users Renviron.

* Fixed broken `uni_smap_jacobian_est`, `uni_smap_jacobian`, `smap_jacobian_est` and `multi_smap_jacobian`  functions due to breaking dependency (rEDM) changes.

## R CMD check results

0 errors |02 warnings | 1 notes

* New submission

* Package was archived on CRAN



