# Reset EWSNet Model Weights

Restores EWSNet model weights to the pretrained defaults published at
https://ewsnet.github.io. This is vital on first use of EWSNet as no
model weights are provided with \`EWSmethods\`. The use of this function
may also be necessary after finetuning to reset the model.

## Usage

``` r
ewsnet_reset(
  weights_path = default_weights_path(),
  remove_weights = FALSE,
  auto = FALSE
)
```

## Arguments

- weights_path:

  A string naming the path to install model weights. Can be changed, but
  by default, attempts to add weights to the same location as the Python
  scripts bundled with EWSmethods.

- remove_weights:

  Boolean. Should all weights be removed (`TRUE`) or should weights be
  re/downloaded (`FALSE`).

- auto:

  Boolean. If `FALSE`, asks permission to download model weights from
  Google Drive. If `TRUE`, no user confirmation is required for
  re/download.

## Value

No return value, called for side effects.

## Examples

``` r
# \donttest{
# on first use of EWSNet via `EWSmethods`
ewsnet_reset(remove_weights = FALSE, auto = TRUE,
weights_path = tempfile())
#> Model weights downloaded
# }

# if this fails due to timeout, you may need to
# increase the timeout length using something
# like below:
# \donttest{
options(timeout = max(300, getOption("timeout")))
# }

# \donttest{
# to remove all downloaded weights
ewsnet_reset(remove_weights = TRUE, auto = TRUE,
weights_path = tempfile())
#> Model weights removed
# }
```
