# EWSmethods (development version)

# EWSmethods 1.3.2
Added support for spatial EWSNet (https://doi.org/10.1098/rsos.231767) and sped up `II` via suggested dependency to `data.table`.

# EWSmethods 1.3.1
Bug fix for tensorflow installation on Windows - downgraded to tensorflow 2.13.0

# EWSmethods 1.3.0
Remove `tidyr` and `dplyr` dependencies but now require >= R 4.4.0. Temporary fix for EWSNet as requires full rewrite for tensorflow 12.6.1. Addition of the Granger causality method - Information Imbalance.

# EWSmethods 1.2.5
Patch to `ewsnet_reset` to ensure it gracefully fails when users' internet connection fails.

# EWSmethods 1.2.4
Hot fix to `multiJI` and `uniJI` following discussions with `rEDM` maintainers. These functions now average over time-varying Jacobians in a window rather than simply returning the final time-varying Jacobian.

# EWSmethods 1.2.1
Due to API changes in the dependency `rEDM`, all Jacobian index functions were non-longer functional. Hot fix to convert functions to new `rEDM` API.

# EWSmethods 1.2.0
Introduce `multiAR` as an alternative to `multiJI` for estimating Jacobians. Additional bug fixes and warning messages for resilience metrics. Removed unnecessary and problematic `bypass_reticulate_autoinit()`.

# EWSmethods 1.1.2
First CRAN release following CRAN maintainer comments. `ewsnet_*()` functions no longer download files without asking permission.

# EWSmethods 1.1.1
Response to CRAN maintainer comments regarding spelling errors and slow example run times.

# EWSmethods 1.1.0

# EWSmethods 1.0.0
First release 

* Added a `NEWS.md` file to track changes to the package.
