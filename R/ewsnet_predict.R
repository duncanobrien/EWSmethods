#' EWSNet Predict
#'
#' Communicates with EWSNet (https://ewsnet.github.io), a deep learning framework for modelling and anticipating regime shifts in dynamical systems, and returns the model's prediction for the inputted univariate time series.
#'
#' @param x A numeric vector of values to be tested.
#' @param noise_type A string stating the form of noise to use. Options are "W" (white noise) or "C" (coloured noise).
#' @param ensemble A numeric value stating the number of models to average over. Options range from 1 to 25.
#' @param envname A string naming the Python environment prepared by \code{ewsnet_init()}.
#' @returns A dataframe of EWSNet predictions. Values represent the estimated probability that the quoted event will occur.
#'
#' @examples
#' #A dummy dataset of a hedgerow bird population
#' #monitored over 50 years.
#'
#' abundance_data <- data.frame(time = seq(1:50),
#'  abundance = rnorm(50,mean = 20))
#'
#' #Activate python environment (only necessary
#' #on first opening of R session).
#'
#' \dontrun{ewsnet_init(envname = "EWSNET_env")}
#'
#' #Generate EWSNet predictions.
#'
#' \dontrun{pred <- ewsnet_predict(
#'  abundance_data$abundance,
#'  noise_type = "W",
#'  ensemble = 15,
#'  envname = "EWSNET_env")}
#'
#' @export
#'
#ewsNETw_25 <- function(x, noise_type = "W", ensemble = "25"){
ewsnet_predict <- function(x, noise_type = "W", ensemble = 25,envname){

  if(!envname %in% (reticulate::conda_list()$name)){
    warning("Call 'ewsnet_init()' before attempting to use ewsnet_predict(), or check your spelling of envname")
  }else{

    if(!is.numeric(x) ){
      stop('Time series is not numeric')
    }

  wd <- getwd() #get working directory so it can be reset when Python alters the directory
  EWSNet <- NULL # global variable to be populated by Python

  noise_type <- match.arg(noise_type, choices = c("W","C"))
  ensemble <- match.arg(paste(ensemble), choices = paste(1:25))

  noise_string = paste(c("Dataset-",paste(noise_type)),collapse = "")

  directory_string = paste(c("directory_string = '", system.file(package = "EWSmethods"),"'"),collapse = "")
  #wd_string = paste(c("wd_string = '", wd,"'"),collapse = "")
  reticulate::py_run_string(directory_string)
  #reticulate::py_run_string(wd_string)
  reticulate::py_run_string("import os")
  reticulate::py_run_string("os.chdir(directory_string)")
  #reticulate::py_run_string("print(os.getcwd())")

  #reticulate::source_python(system.file("inst/python/src/inference/ewsNET_generic.py", package = "EWSmethods"))
  #reticulate::source_python(system.file("python/src/inference/stupid_attempt.py", package = "EWSmethods"))

  reticulate::source_python(system.file("python/src/inference/ewsNET_generic.py", package = "EWSmethods"))
  #pred <- ewsnetW_25$predict(x)

  ewsnet_obj <- EWSNet(ensemble = as.integer(ensemble), weight_dir = paste(c(directory_string,"python/weights/Pretrained",noise_string),collapse = "/"), prefix = "", suffix = ".h5")
  pred <- ewsnet_obj$predict(x)

  out <- data.frame("pred" = pred[[1]],
                    "no_trans_prob" = pred[[2]]$`No Transition`,
                    "smooth_trans_prob" = pred[[2]]$`Smooth Transition`,
                    "critical_trans_prob" = pred[[2]]$`Critical Transition`)

  setwd(wd) # reset working directory
  #reticulate::py_run_string("os.chdir(wd_string)")

  # out <- data.frame("pred" = names(which.max(unlist(pred[[2]]))),
  #                   "no_trans_prob" = pred[[2]]$`No Transition`,
  #                   "smooth_trans_prob" = pred[[2]]$`Smooth Transition`,
  #                   "critical_trans_prob" = pred[[2]]$`Critical Transition`)
  return(out)
  }

}
