% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reset_ewsnet.R
\name{ewsnet_reset}
\alias{ewsnet_reset}
\title{Reset EWSNet Model Weights}
\usage{
ewsnet_reset(
  weights_path = default_weights_path(),
  remove_weights = FALSE,
  auto = FALSE
)
}
\arguments{
\item{weights_path}{A string naming the path to install model weights. Can be changed, but by default, attempts to add weights to the same location as the Python scripts bundled with EWSmethods.}

\item{remove_weights}{Boolean. Should all weights be removed (\code{TRUE}) or should weights be re/downloaded (\code{FALSE}).}

\item{auto}{Boolean. If \code{FALSE}, asks permission to download model weights from Google Drive. If \code{TRUE}, no user confirmation is required for re/download.}
}
\value{
No return value, called for side effects.
}
\description{
Restores EWSNet model weights to the pretrained defaults published at https://ewsnet.github.io. This is vital on first use of EWSNet as no model weights are provided with `EWSmethods`. The use of this function may also be necessary after finetuning to reset the model.
}
\examples{
\donttest{
# on first use of EWSNet via `EWSmethods`
ewsnet_reset(remove_weights = FALSE, auto = TRUE,
weights_path = tempfile())
}

# if this fails due to timeout, you may need to
# increase the timeout length using something
# like below:
\donttest{
options(timeout = max(300, getOption("timeout")))
}

\donttest{
# to remove all downloaded weights
ewsnet_reset(remove_weights = TRUE, auto = TRUE,
weights_path = tempfile())
}

}
