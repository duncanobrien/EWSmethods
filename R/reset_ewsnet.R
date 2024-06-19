#' Reset EWSNet Model Weights
#'
#' Restores EWSNet model weights to the pretrained defaults published at https://ewsnet.github.io. This is vital on first use of EWSNet as no model weights are provided with `EWSmethods`. The use of this function may also be necessary after finetuning to reset the model.
#'
#' @param weights_path A string naming the path to install model weights. Can be changed, but by default, attempts to add weights to the same location as the Python scripts bundled with EWSmethods.
#' @param remove_weights Boolean. Should all weights be removed (\code{TRUE}) or should weights be re/downloaded (\code{FALSE}).
#' @param auto Boolean. If \code{FALSE}, asks permission to download model weights from Google Drive. If \code{TRUE}, no user confirmation is required for re/download.
#'
#' @returns No return value, called for side effects.

#' @examples
#' \donttest{
#' # on first use of EWSNet via `EWSmethods`
#' ewsnet_reset(remove_weights = FALSE, auto = TRUE,
#' weights_path = tempfile())
#' }
#'
#' # if this fails due to timeout, you may need to
#' # increase the timeout length using something
#' # like below:
#' \donttest{
#' options(timeout = max(300, getOption("timeout")))
#' }
#'
#'\donttest{
#' # to remove all downloaded weights
#' ewsnet_reset(remove_weights = TRUE, auto = TRUE,
#' weights_path = tempfile())
#' }
#'
#' @export

ewsnet_reset <- function(weights_path = default_weights_path(), remove_weights = FALSE, auto = FALSE){

  #path <- system.file("python/weights", package = "EWSmethods")

  if(!dir.exists(file.path(weights_path)) & weights_path != ""){
    dir.create(file.path(weights_path))
    }

  if(isTRUE(remove_weights)){
    unlink(paste(c(weights_path,"Pretrained"),collapse = "/"),recursive = T)
    message("Model weights removed")

    }else{

      if(!curl::has_internet()) {
        message("No internet connection to download weights.")
        return(invisible(NULL))
      }

      if(isTRUE(auto)){

        zip <- paste(c(weights_path,"temp.zip"),collapse = "/")
        warn_dwn <- tryCatch(utils::download.file("https://drive.usercontent.google.com/download?id=19OuqzrY1LQxZusByf4ACPj-yiex4LY2e&export=download&confirm=EoIm",
                                                  destfile  = zip, mode = "wb"),
                      error = function(e) conditionMessage(e),
                      warning = function(w) conditionMessage(w)) #VariableLenModel EWSNet

         if(inherits(warn_dwn,"character")){
          message(warn_dwn)
          return(invisible(NULL))
           }

        warn_unzip <-tryCatch(utils::unzip(zip,exdir = weights_path),
                              error = function(e) conditionMessage(e),
                              warning = function(w) conditionMessage(w))
        unlink(zip,recursive = T)

        if(all(warn_unzip == "error 1 in extracting from zip file") | (length(fs::dir_ls(path = weights_path, regexp = "*\\.h5$",recurse = T)) != 50)){
          message("Corrupted download. Please check your internet connection and retry.")
          return(invisible(NULL))
        }

        message("Model weights downloaded")

      }else if(isFALSE(auto)){
      user_input1 <- readline("EWSNet weights are are a ~ 220 MB download. Would you like to continue? (y/n) ")

      if(user_input1 != 'y'){
        message('Aborting')

      }else{
        message(paste("Attention: weights will be downloaded"))

        zip <- paste(c(weights_path,"temp.zip"),collapse = "/")

        warn_dwn <- tryCatch(utils::download.file("https://drive.usercontent.google.com/download?id=19OuqzrY1LQxZusByf4ACPj-yiex4LY2e&export=download&confirm=EoIm",
                                                 destfile  = zip, mode = "wb"),
                             error = function(e) conditionMessage(e),
                             warning = function(w) conditionMessage(w)) #VariableLenModel EWSNet

        if(inherits(warn_dwn,"character")){
          message(warn_dwn)
          return(invisible(NULL))
        }

        warn_unzip <-tryCatch(utils::unzip(zip,exdir = weights_path),
                              error = function(e) conditionMessage(e),
                              warning = function(w) conditionMessage(w))
        unlink(zip,recursive = T)

        if(all(warn_unzip == "error 1 in extracting from zip file") | (length(fs::dir_ls(path = weights_path, regexp = "*\\.h5$",recurse = T)) != 50)){
          message("Corrupted download. Please check your internet connection and retry.")
          return(invisible(NULL))
        }

      message("Model weights downloaded")
      }
      }
    }

}

#' Path to Model Weights
#'
#' The default path for EWSNet model weights to use. Is the location of the EWSmethods package. If you'd like to instead set your own path, \code{ewsnet_reset()} contains the argument \code{weights_path} to do so.
#'
#' @returns No return value, called for reference.
#'
#' @export

default_weights_path <- function(){
  target_folder <- paste(c(system.file("python", package = "EWSmethods"),"weights"),collapse = "/")
  file.path(target_folder)
}
