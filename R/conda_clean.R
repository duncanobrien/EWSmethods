#' Python Removal
#'
#' Removes \code{ewsnet_init()} downloaded Anaconda binaries and environments.
#'
#' @param envname A string naming the desired Python environment to remove.
#' @param conda_path The location of Python install. By default, this follows \code{minicondata_path} defined by the \code{reticulate} package.
#' @param auto Boolean. If \code{FALSE}, asks permission to uninstall Python, packages and specified environment. If \code{TRUE}, no user confirmation is required for activation.
#'
#' @returns Does not return an object as is cleaning Python and its environments.
#'
#' @export
#'
#' @examples
#' #Prior to running `conda_clean()`, you must restart
#' #your R session to detach any activated environments
#' \donttest{
#' conda_clean("EWSNET_env", auto = TRUE)
#' }
#'

conda_clean <- function(envname, conda_path = reticulate::miniconda_path(), auto = FALSE){

  if(identical(.Platform$OS.type, "windows")){
    conda_binary <- paste0(conda_path,"/condabin/conda.bat")
  }else{
    conda_binary <- paste0(conda_path,"/bin/conda")
  }

  conda_exists <- try(reticulate::conda_list(conda = conda_binary), silent = TRUE)
  if(inherits(conda_exists, "try-error")){
    warning("Anaconda doesn't exist")
    }else{

      env_active <- try(reticulate::py_config()$pythonhome, silent = TRUE)

      if(grepl(paste0(envname,"$"),env_active)){
        stop(paste0(envname," is already activated. Restart your R session and retry"))
      }

      if(isTRUE(auto)){
        reticulate::conda_remove(envname = envname,packages = NULL, conda=conda_binary)
        reticulate::miniconda_uninstall(path=conda_path) # start with a blank slate
        message("Anaconda and environment successfully removed")
      }

      if(isFALSE(auto)){
        user_input1 <- readline("Anaconda found. Would you like to uninstall Anaconda and environment? (y/n) ")

        if(user_input1 != 'y'){
          message('Aborting')
          }else{
            reticulate::conda_remove(envname = envname,packages = NULL, conda=conda_binary)
            reticulate::miniconda_uninstall(path=conda_path) # start with a blank slate
            message("Anaconda and environment successfully removed")
          }
      }
    }
}
