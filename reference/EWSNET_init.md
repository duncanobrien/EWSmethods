# EWSNet Initialisation

Prepares your R session for communicating with Python. This function
first searches your computer for an appropriate Python environment and
activates it, importing the vital Python packages required. If no
appropriate Python install or environment is found, after asking
permission, miniconda is downloaded and an environment created.

## Usage

``` r
ewsnet_init(
  envname,
  conda_path = reticulate::miniconda_path(),
  pip_ignore_installed = FALSE,
  auto = FALSE
)
```

## Arguments

- envname:

  A string naming the desired Python environment to create/activate. If
  no Python or environment found, the functions prompts to install
  miniconda and the required python packages.

- conda_path:

  The location to install Python. By default, this follows
  `minicondata_path` defined by the `reticulate` package.

- pip_ignore_installed:

  Boolean. If `FALSE`, any packages already installed are loaded and not
  re-downloaded. However, if `TRUE`, packages are downloaded
  irregardless, overwriting any version already present (is analagous to
  updating if required).

- auto:

  Boolean. If `FALSE`, asks permission to install Python and/or
  packages. If `TRUE`, no user confirmation is required for activation.

## Value

Does not return an object as is simply preparing your R session.

## Examples

``` r
if (FALSE) { # \dontrun{
ewsnet_init(envname = "EWSNET_env", auto = FALSE)
} # }
#Common errors at this stage result from 'reticulate's
#behaviour. For example, conflicts between 'ewsnet_init'
#and RETICULATE_PYTHON may occur if run inside a
#RStudio R project. To fix this, navigate to
#Preferences -> Python, untick 'Automatically
#activate project-local Python environments'
#and restart R.

#if this fails due to timeout, you may need to
#increase the timeout length using something
#like below:
# \donttest{
options(timeout = max(300, getOption("timeout")))
# }

if (FALSE) { # \dontrun{
reticulate::py_config()
} # }
#If successful, 'EWSNET_env forced by use_python
#function' will be printed.
```
