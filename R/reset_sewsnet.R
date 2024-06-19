#' Reset S-EWSNet Model
#'
#' Restores S-EWSNet model weights to the defaults published at https://github.com/SMITA1996/S-EWSNet/. This is vital on first use of S-EWSNet as no model files are provided with `EWSmethods`.
#'
#' @param model_path A string naming the path to install model files. Can be changed, but by default, attempts to add files to the same location as the Python scripts bundled with EWSmethods.
#' @param remove_model Boolean. Should all model files be removed (\code{TRUE}) or should model files be re/downloaded (\code{FALSE}).
#' @param auto Boolean. If \code{FALSE}, asks permission to download model files from Github. If \code{TRUE}, no user confirmation is required for re/download.
#'
#' @returns No return value, called for side effects.

#' @examples
#'
#'\donttest{
#' # to remove all downloaded weights
#' sewsnet_reset(remove_model = TRUE, auto = TRUE,
#' model_path = tempfile())
#' }
#'
#' @export

sewsnet_reset <- function(model_path = default_sewsnet_path(), remove_model = FALSE, auto = FALSE){

  if(!dir.exists(file.path(model_path)) & model_path != ""){
    dir.create(file.path(model_path))
  }

  if(isTRUE(remove_model)){
    unlink(paste(model_path),recursive = T)
    message("Model files removed")

  }else{

    if(!curl::has_internet()) {
      message("No internet connection to download model files")
      return(invisible(NULL))
    }

    if(isTRUE(auto)){

      zip <- paste(c(model_path,"temp.zip"),collapse = "/")
      warn_dwn <- tryCatch(utils::download.file("https://github.com/SMITA1996/S-EWSNet/blob/main/S-EWSNet.zip?raw=TRUE",
                                                destfile  = zip, mode = "wb"),
                           error = function(e) conditionMessage(e),
                           warning = function(w) conditionMessage(w))

      if(inherits(warn_dwn,"character")){
        message(warn_dwn)
        return(invisible(NULL))
      }

      warn_unzip <-tryCatch(utils::unzip(zip,exdir = model_path),
                            error = function(e) conditionMessage(e),
                            warning = function(w) conditionMessage(w))
      unlink(zip,recursive = T)

      if(all(warn_unzip == "error 1 in extracting from zip file") | (length(fs::dir_ls(path = model_path,recurse = T)) != 6)){
        message("Corrupted download. Please check your internet connection and retry.")
        return(invisible(NULL))
      }

      message("Model files downloaded")

    }else if(isFALSE(auto)){
      user_input1 <- readline("S-EWSNet files are are a ~ 4 MB download. Would you like to continue? (y/n) ")

      if(user_input1 != 'y'){
        message('Aborting')

      }else{
        message(paste("Attention: model will be downloaded"))

        zip <- paste(c(model_path,"temp.zip"),collapse = "/")

        warn_dwn <- tryCatch(utils::download.file("https://github.com/SMITA1996/S-EWSNet/blob/main/S-EWSNet.zip?raw=TRUE",
                                                  destfile  = zip, mode = "wb"),
                             error = function(e) conditionMessage(e),
                             warning = function(w) conditionMessage(w)) #VariableLenModel EWSNet

        if(inherits(warn_dwn,"character")){
          message(warn_dwn)
          return(invisible(NULL))
        }

        warn_unzip <-tryCatch(utils::unzip(zip,exdir = model_path),
                              error = function(e) conditionMessage(e),
                              warning = function(w) conditionMessage(w))
        unlink(zip,recursive = T)

        if(all(warn_unzip == "error 1 in extracting from zip file") | (length(fs::dir_ls(path = model_path,recurse = T)) != 6)){
          message("Corrupted download. Please check your internet connection and retry.")
          return(invisible(NULL))
        }

        message("Model weights downloaded")
      }
    }
  }

}

#' Path to S-EWSNet Model
#'
#' The default path to the S-EWSNet model. Is the location of the EWSmethods package.
#'
#' @returns No return value, called for reference.
#'
#' @export

default_sewsnet_path <- function() {
  #target_folder <- paste(c(system.file("python", package = "EWSmethods"), "src/spatial"), collapse = "/")
  target_folder <- paste(c(system.file(package = "EWSmethods"), "bin/spatial"), collapse = "/")
  file.path(target_folder)
}
