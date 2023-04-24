## Resubmission
This is a resubmission. In this version I have:

* Added <doi:...> references to DESCRIPTION

* Ensured all arguments are `TRUE`/`FALSE` rather than `T`/`F`

* Added \value to any .Rd values that functions that do not return a value

* Converted \dontrun{} to \donttest{} where possible. `ewsnet_init()`, `ewsnet_predict()`, and `ewsnet_finetune()` retain \dontrun{} due to `reticulate` (a dependency) developers' request (https://github.com/rstudio/reticulate/issues/1145)

* Replaced any `print()` with `message()`/`error()`

* Added arguments to `ewsnet_init()`, `ewsnet_reset()`, `ewsnet_finetune()` and `conda_clean()` so they do not write by default to meet CRAN policy. Arguments `conda_path = ` and `weights_path =` allow user to define location, and `auto = FALSE` (the default) asks permission (and requires user input) to run.

* Added `on.exit(setwd(...), add = TRUE)` to functions dependent on `reticulate`. `reticulate` changes working directory when calling python and cannot be bypassed

## R CMD check results

0 errors | 0 warnings | 1 notes

* This is a new release.
