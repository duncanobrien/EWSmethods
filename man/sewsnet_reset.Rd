% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reset_sewsnet.R
\name{sewsnet_reset}
\alias{sewsnet_reset}
\title{Reset S-EWSNet Model}
\usage{
sewsnet_reset(
  model_path = default_sewsnet_path(),
  remove_model = FALSE,
  auto = FALSE
)
}
\arguments{
\item{model_path}{A string naming the path to install model files. Can be changed, but by default, attempts to add files to the same location as the Python scripts bundled with EWSmethods.}

\item{remove_model}{Boolean. Should all model files be removed (\code{TRUE}) or should model files be re/downloaded (\code{FALSE}).}

\item{auto}{Boolean. If \code{FALSE}, asks permission to download model files from Github. If \code{TRUE}, no user confirmation is required for re/download.}
}
\value{
No return value, called for side effects.
}
\description{
Restores S-EWSNet model weights to the defaults published at https://github.com/SMITA1996/S-EWSNet/. This is vital on first use of S-EWSNet as no model files are provided with `EWSmethods`.
}
\examples{

\donttest{
# to remove all downloaded weights
sewsnet_reset(remove_model = TRUE, auto = TRUE,
model_path = tempfile())
}

}
