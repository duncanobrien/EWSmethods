#' EWSNet Predict
#'
#' Communicates with EWSNet (https://ewsnet.github.io), a deep learning framework for modelling and anticipating regime shifts in dynamical systems, and returns the model's prediction for the inputted univariate time series.
#'
#' @param x A numeric vector of values to be tested.
#' @param scaling Boolean.  If \code{TRUE}, the time series will be scaled between 1 and 2 and scaled EWSNet model weights will be used. This is the recommended setting.
#' @param ensemble A numeric value stating the number of models to average over. Options range from 1 to 25.
#' @param envname A string naming the Python environment prepared by \code{ewsnet_init()}.
#' @param weights_path A string naming the path to model weights installed by \code{ewsnet_reset()}.
#'
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
#' \dontrun{
#' ewsnet_init(envname = "EWSNET_env")
#' }
#'
#' #Generate EWSNet predictions.
#'
#' \dontrun{
#' pred <- ewsnet_predict(
#'  abundance_data$abundance,
#'  scaling = TRUE,
#'  ensemble = 15,
#'  envname = "EWSNET_env")
#'  }
#'
#' @export
#'
ewsnet_predict <- function(x, scaling = TRUE, ensemble = 25, envname, weights_path = default_weights_path()){

  if(!envname %in% (reticulate::conda_list()$name)){
    warning("Call 'ewsnet_init()' before attempting to use ewsnet_predict(), or check your spelling of envname")
  }else{

    if(!is.vector(x)){
      stop('Time series is not a vector')
    }

    if(!is.numeric(x) ){
      stop('Time series is not numeric')
    }

  wd <- getwd() #get working directory so it can be reset when Python/reticulate alters the directory
  on.exit(setwd(wd),add = TRUE)

  EWSNet <- NULL # global variable to be populated by Python

  ensemble <- match.arg(paste(ensemble), choices = paste(1:25))

  if(isTRUE(scaling)){
    scaling_string <- paste("Scaled")
  }else if(isFALSE(scaling)){
    scaling_string <- paste("Unscaled")
  }

  if(!dir.exists(file.path(paste(weights_path,"Pretrained",sep="/"))) & weights_path != ""){
    stop('No model weights found. Call ewsnet_reset(remove_weights = FALSE) to download weights')
  }

  directory_string <- paste(c("directory_string = '", system.file(package = "EWSmethods"),"'"),collapse = "")
  reticulate::py_run_string(directory_string)
  reticulate::py_run_string("import os")
  reticulate::py_run_string("os.chdir(directory_string)")

  reticulate::source_python(system.file("python/src/inference/ewsNET_generic.py", package = "EWSmethods"))

 #ewsnet_obj <- EWSNet(ensemble = as.integer(25), weight_dir = paste(c(system.file(package = "EWSmethods"),"python/weights/Pretrained",scaling_string),collapse = "/"), prefix = "", suffix = ".h5")
  ewsnet_obj <- EWSNet(ensemble = as.integer(25), weight_dir = paste(c(weights_path,"Pretrained",scaling_string),collapse = "/"), prefix = "", suffix = ".h5")

  if(isTRUE(scaling)){
    pred <- ewsnet_obj$predict(data_scaling(x),ensemble_subset = as.integer(ensemble))
  }else if(isFALSE(scaling)){
    pred <- ewsnet_obj$predict(x,ensemble_subset = as.integer(ensemble))
  }

  out <- data.frame("pred" = pred[[1]],
                    "no_trans_prob" = pred[[2]]$`No Transition`,
                    "smooth_trans_prob" = pred[[2]]$`Smooth Transition`,
                    "critical_trans_prob" = pred[[2]]$`Critical Transition`)

  setwd(wd) # reset working directory

  return(out)
  }

}
