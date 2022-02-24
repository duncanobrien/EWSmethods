#' EWSNET Predict
#'
#' @param x A numeric vector of values to be tested
#'
#' @return A data.frame of EWSNET predictions

ewsNETw_25 <- function(x){

  #reticulate::source_python("src/inference/ewsNET_w_script_25.py")
  reticulate::source_python(system.file("python", "ewsNET_w_script_25.py", package = "EWSmethods"))
  out <- py$ewsnetW_25$predict(x)

  return(out)
}
