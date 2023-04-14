#' EWSNet Initialisation
#'
#' Prepares your R session for communicating with Python. This function first searches your computer for an appropriate Python environment and activates it, importing the vital Python packages required. If no appropriate Python install or environment is found, after asking permission, miniconda is downloaded and an environment created.
#'
#' @param envname A string naming the desired Python environment to create/activate. If no Python or environment found, the functions prompts to install miniconda and the required python packages.
#' @param pip_ignore_installed Boolean. If \code{FALSE}, any packages already installed are loaded and not re-downloaded. However, if \code{TRUE}, packages are downloaded irregardless, overwriting any version already present (is analagous to updating if required).
#' @param conda_refresh Boolean. If \code{TRUE}, miniconda is uninstalled and then gives the user the opportunity to perform a fresh reinstall.
#' @param auto Boolean. If \code{TRUE}, no user confirmation is required for activation.
#'
#' @returns Does not return an object as is simply preparing your R session.
#'
#' @examples
#' \dontrun{
#' bypass_reticulate_autoinit()
#' }
#' #this function prevents 'reticulate' automatically
#' #reloading the python environment from the
#' #previous session. Only needs to be run once
#' #on first install of 'EWSmethods'.
#'
#' \dontrun{
#' ewsnet_init(envname = "EWSNET_env")
#' }
#' #Common errors at this stage result from 'reticulate's
#' #behaviour. For example, conflicts between 'ewsnet_init'
#' #and RETICULATE_PYTHON may occur if run inside a
#' #RStudio R project. To fix this, navigate to
#' #Preferences -> Python, untick 'Automatically
#' #activate project-local Python environments'
#' #and restart R.
#'
#' \dontrun{
#' reticulate::py_config()
#' }
#' #If successful, 'EWSNET_env forced by use_python
#' #function' will be printed.
#'
#
#' @export

ewsnet_init <- function(envname, pip_ignore_installed = FALSE, conda_refresh = FALSE, auto = FALSE){

  if(isTRUE(conda_refresh)){
    reticulate::miniconda_uninstall() # start with a blank slate
    #reticulate::conda_remove(envname, packages = NULL, conda = "auto")
  }

  detect.sys <- paste(Sys.info()["sysname"],Sys.info()["machine"],sep = "_") #identify whether
  wd <- getwd() #get working directory so it can be reset when Python alters the directory

  conda <- try(reticulate::conda_list()) #extract list of conda Python environments
  if(inherits(conda, "try-error")){

    if(isTRUE(auto)){
      message("Attention: may take up to 10 minutes to complete")

      reticulate::install_miniconda(force = T)

      reticulate::conda_create(
        envname = paste0(envname),
        forge = FALSE,environment = NULL,
        conda = "auto")

      if(detect.sys == "Darwin_arm64"){ # specfic install for MACOSX M1 machines
        reticulate::conda_install(envname = paste0(envname),
                                  packages = c("tensorflow-macos","scikit-learn","pandas","matplotlib",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed)
      }else{
        reticulate::conda_install(envname = paste0(envname),
                                  packages = c("tensorflow","scikit-learn","pandas","matplotlib",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed)
      }
      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = "auto", required = T)
      #keras::install_keras(envname = paste0(envname))
      # try.attempt <- suppressMessages(try(reticulate::use_condaenv(condaenv = paste0(envname),
      #                          conda = "auto", required = T),silent = T))
      #
      # if(class(try.attempt) == "try-error"){
      #   message("miniconda has successfully been installed but reticulate has initialised\nanother environment. You need to restart the R session and re-run 'ewsnet_init()'\nto complete python integration")
      # }

      message(paste(envname,"successfully installed and activated. Necessary python packages installed"),
              collapse = " ")

    }else if(isFALSE(auto)){
    user_input1 <- readline("Anaconda not found. Would you like to download Anaconda? (y/n) ")

    if(user_input1 != 'y'){
      message('Aborting')

    }else{
      message("Attention: may take up to 10 minutes to complete")

      reticulate::install_miniconda(force = F)

      reticulate::conda_create(
        envname = paste0(envname),
        forge = FALSE,environment = NULL,
        conda = "auto")

      if(detect.sys == "Darwin_arm64"){ # specfic install for MACOSX M1 machines
      reticulate::conda_install(envname = paste0(envname),
                                packages = c("tensorflow-macos","scikit-learn","pandas","matplotlib",
                                             "sphinxcontrib-matlabdomain","seaborn"),
                                pip = T, pip_ignore_installed = pip_ignore_installed)
      }else{
        reticulate::conda_install(envname = paste0(envname),
                                  packages = c("tensorflow","scikit-learn","pandas","matplotlib",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed)
        }
      reticulate::use_condaenv(condaenv = paste0(envname),
                        conda = "auto", required = T)
      #keras::install_keras(envname = paste0(envname))
      # try.attempt <- suppressMessages(try(reticulate::use_condaenv(condaenv = paste0(envname),
      #                          conda = "auto", required = T),silent = T))
      #
      # if(class(try.attempt) == "try-error"){
      #   message("miniconda has successfully been installed but reticulate has initialised\nanother environment. You need to restart the R session and re-run 'ewsnet_init()'\nto complete python integration")
      # }

      message(paste(envname,"successfully installed and activated. Necessary python packages installed"),
              collapse = " ")
}
    }

  }else if(envname %in% conda$name){ #if desired environment found, activate and install necessary packages

    if(isTRUE(auto)){
      message("Attention: may take up to 10 minutes to complete")

      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = "auto", required = T)

      if(detect.sys == "Darwin_arm64"){
        reticulate::conda_install(envname = paste0(envname),
                                  packages = c("tensorflow-macos","scikit-learn","pandas","matplotlib",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed)
      }else{
        reticulate::conda_install(envname = paste0(envname),
                                  packages = c("tensorflow","scikit-learn","pandas","matplotlib",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed)
      }

      message(paste(envname,"successfully found and activated. Necessary python packages installed"),
              collapse = " ")
    }else if(isFALSE(auto)){

    string <- c("conda env",envname, "found. Would you like to activate it and install necessary\npackages? (y/n) ")
    user_input2 <- readline(paste(string,collapse =" "))

    if(user_input2 != 'y'){
      message('Aborting')
    }else{
      message("Attention: may take up to 10 minutes to complete")

      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = "auto", required = T)

      if(detect.sys == "Darwin_arm64"){
        reticulate::conda_install(envname = paste0(envname),
                                  packages = c("tensorflow-macos","scikit-learn","pandas","matplotlib",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed)
      }else{
        reticulate::conda_install(envname = paste0(envname),
                                  packages = c("tensorflow","scikit-learn","pandas","matplotlib",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed)
      }

      message(paste(envname,"successfully found and activated. Necessary python packages installed"),
              collapse = " ")
    }

    }

  }else{ #if desired environment not found, ask permission to create new environment

    if(isTRUE(auto)){

      reticulate::conda_create(
        envname = paste0(envname),
        forge = FALSE,environment = NULL,
        conda = "auto")

      if(detect.sys == "Darwin_arm64"){
        reticulate::conda_install(envname = paste0(envname),
                                  packages = c("tensorflow-macos","scikit-learn","pandas","matplotlib",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed)
      }else{
        reticulate::conda_install(envname = paste0(envname),
                                  packages = c("tensorflow","scikit-learn","pandas","matplotlib",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed)
      }
      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = "auto", required = T)

      message(paste(envname,"successfully installed and activated. Necessary python packages installed"),
              collapse = " ")
    }


    if(isFALSE(auto)){
    string <- c("conda env",envname, "not found. Would you like to install a new env? (y/n) ")
    user_input2 <- readline(paste(string,collapse =" "))
    if(user_input2 != 'y'){
      message('Aborting')
    }else{

      reticulate::conda_create(
        envname = paste0(envname),
        forge = FALSE,environment = NULL,
        conda = "auto")

      if(detect.sys == "Darwin_arm64"){
        reticulate::conda_install(envname = paste0(envname),
                                  packages = c("tensorflow-macos","scikit-learn","pandas","matplotlib",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed)
      }else{
        reticulate::conda_install(envname = paste0(envname),
                                  packages = c("tensorflow","scikit-learn","pandas","matplotlib",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed)
      }
      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = "auto", required = T)

      message(paste(envname,"successfully installed and activated. Necessary python packages installed"),
              collapse = " ")
    }
    }
  }
  setwd(wd) # reset working directory

}
