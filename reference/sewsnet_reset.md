# Reset S-EWSNet Model

Restores S-EWSNet model weights to the defaults published at
https://github.com/SMITA1996/S-EWSNet/. This is vital on first use of
S-EWSNet as no model files are provided with \`EWSmethods\`.

## Usage

``` r
sewsnet_reset(
  model_path = default_sewsnet_path(),
  remove_model = FALSE,
  auto = FALSE
)
```

## Arguments

- model_path:

  A string naming the path to install model files. Can be changed, but
  by default, attempts to add files to the same location as the Python
  scripts bundled with EWSmethods.

- remove_model:

  Boolean. Should all model files be removed (`TRUE`) or should model
  files be re/downloaded (`FALSE`).

- auto:

  Boolean. If `FALSE`, asks permission to download model files from
  Github. If `TRUE`, no user confirmation is required for re/download.

## Value

No return value, called for side effects.

## Examples

``` r
# \donttest{
# to remove all downloaded weights
sewsnet_reset(remove_model = TRUE, auto = TRUE,
model_path = tempfile())
#> Model files removed
# }
```
