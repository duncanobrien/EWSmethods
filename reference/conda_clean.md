# Python Removal

Removes
[`ewsnet_init()`](https://duncanobrien.github.io/EWSmethods/reference/EWSNET_init.md)
downloaded Anaconda binaries and environments.

## Usage

``` r
conda_clean(envname, conda_path = reticulate::miniconda_path(), auto = FALSE)
```

## Arguments

- envname:

  A string naming the desired Python environment to remove.

- conda_path:

  The location of Python install. By default, this follows
  `minicondata_path` defined by the `reticulate` package.

- auto:

  Boolean. If `FALSE`, asks permission to uninstall Python, packages and
  specified environment. If `TRUE`, no user confirmation is required for
  activation.

## Value

Does not return an object as is cleaning Python and its environments.

## Examples

``` r
#Prior to running `conda_clean()`, you must restart
#your R session to detach any activated environments
# \donttest{
conda_clean("EWSNET_env", auto = TRUE)
#> Warning: Anaconda doesn't exist
# }
```
