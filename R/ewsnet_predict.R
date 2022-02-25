#' EWSNET Predict
#'
#' @param x A numeric vector of values to be tested.
#' @param noise_type A string stating the form of noise to use. Options are "W" or "C".
#' @param ensemble A numeric value stating the number of models to average over. Options range from 1 to 25.
#'
#' @return A data.frame of EWSNET predictions.
#' @export
#'
#ewsNETw_25 <- function(x, noise_type = "W", ensemble = "25"){
ewsNETpredict <- function(x, noise_type = "W", ensemble = 25){

  if(length(reticulate::conda_list())<2){
    warning("Call 'EWSNET_init()' before attempting to use ewsNETpredict()")
  }else{

  noise_type <- match.arg(noise_type, choices = c("W","C"))
  ensemble <- match.arg(paste(ensemble), choices = paste(1:25))

  noise_string = paste(c("noise_type = '", paste(noise_type),"'"),collapse = "")
  ensemble_string = paste(c("ensemble = ", paste(ensemble)),collapse = "")

  #reticulate::source_python("src/inference/ewsNET_w_script_25.py")
  #reticulate::source_python(system.file("inst/python/src/inference/ewsNET_w_script_25.py", package = "EWSmethods"))

  reticulate::py_run_string(noise_string)
  reticulate::py_run_string(ensemble_string)

  reticulate::source_python(system.file("inst/python/src/inference/ewsNET_generic.py", package = "EWSmethods"))


  #pred <- ewsnetW_25$predict(x)
  pred <- ewsnet_obj$predict(x)

  out <- data.frame("pred" = pred[[1]],
                    "no_trans_prob" = pred[[2]]$`No Transition`,
                    "smooth_trans_prob" = pred[[2]]$`Smooth Transition`,
                    "critical_trans_prob" = pred[[2]]$`Critical Transition`)
  return(out)
  }

}
