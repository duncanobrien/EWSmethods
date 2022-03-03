#' EWSNet Finetune
#'
#' Communicates with EWSNet (https://ewsnet.github.io), a deep learning framework for modelling and anticipating regime shifts in dynamical systems, and finetunes the model to match the inputted training data.
#'
#' @param x A numeric matrix of to finetune EWSNet on. Each column represents a separate timeseries and each row is a timestep.
#' @param y A numeric vector consisting of target labels for each training time series. Labels include: 0 (no transition), 1 (smooth transition) or 2 (critical transition).
#' @param noise_type A string stating the form of noise to use. Options are "W" or "C".
#' @param ensemble A numeric value stating the number of models to average over. Options range from 1 to 25.
#' @param envname A string naming the Python environment prepared by \code{ewsnet_int()}.
#' @returns A dataframe of EWSNET predictions. Values represent the estimated probability that the quoted event will occur.
#'
#' @examples
#' #Activate python environment (only necessary
#' #on first opening of R session).
#'
#' \dontrun{ewsnet_init(envname = "EWSNET_env")}
#'
#' #A dummy dataset of a hedgerow bird population
#' #monitored over 50 years that needs to be tuned.
#'
#' abundance_data <- data.frame(time = seq(1:50),
#'  abundance = rnorm(50,mean = 20))
#'
#' #Generate training data (this is random data as
#' #an example).
#'
#' x <- matrix(nrow = 50, ncol = 10)
#' x <- sapply(1:dim(x)[2], function(i){
#'  x[,i] <- rnorm(50,mean=20,sd=10)})
#'
#' #Label each time series.
#' y <- sample(0:2,10,replace = TRUE)
#'
#' #Finetune EWSNet.
#'
#' \dontrun{ewsnet_finetune(
#'  x = x,
#'  y = y,
#'  noise_type = "W",
#'  ensemble = 25,
#'  envname = "EWSNET_env")}
#'
#' #Generate new EWSNet predictions.
#'
#' \dontrun{pred <- ewsnet_predict(
#'  abundance_data$abundance,
#'  noise_type = "W",
#'  ensemble = 15,
#'  envname = "EWSNET_env")}
#'
#' @export
#'
ewsnet_finetune <- function(x, y, noise_type = "W", ensemble = 25,envname){

  if(!envname %in% (reticulate::conda_list()$name)){
    warning("Call 'ewsnet_init()' before attempting to use ewsnet_finetune(), or check your spelling of envname")
  }else{

    noise_type <- match.arg(noise_type, choices = c("W","C"))
    ensemble <- match.arg(paste(ensemble), choices = paste(1:25))

    noise_string = paste(c("Dataset-",paste(noise_type)),collapse = "")

    directory_string = paste(c("directory_string = '", system.file(package = "EWSmethods"),"'"),collapse = "")

    reticulate::py_run_string(directory_string)
    reticulate::py_run_string("import os")
    reticulate::py_run_string("os.chdir(directory_string)")
    #reticulate::py_run_string("print(os.getcwd())")

    #reticulate::source_python(system.file("inst/python/src/inference/ewsNET_generic.py", package = "EWSmethods"))
    #reticulate::source_python(system.file("python/src/inference/stupid_attempt.py", package = "EWSmethods"))

    reticulate::source_python(system.file("python/src/inference/ewsNET_generic.py", package = "EWSmethods"))

    ewsnet_obj <- EWSNet(ensemble = as.integer(ensemble), weight_dir = paste(c(directory_string,"python/weights/Pretrained",noise_string),collapse = "/"), prefix = "", suffix = ".h5")
    pred <- ewsnet_obj$finetune(x,y)

    message("Finetuning successful. Now run ews_predict() using the test data")
  }

}
