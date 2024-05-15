#' EWSNet Initialisation
#'
#' Prepares your R session for communicating with Python. This function first searches your computer for an appropriate Python environment and activates it, importing the vital Python packages required. If no appropriate Python install or environment is found, after asking permission, miniconda is downloaded and an environment created.
#'
#' @param envname A string naming the desired Python environment to create/activate. If no Python or environment found, the functions prompts to install miniconda and the required python packages.
#' @param conda_path The location to install Python. By default, this follows \code{minicondata_path} defined by the \code{reticulate} package.
#' @param pip_ignore_installed Boolean. If \code{FALSE}, any packages already installed are loaded and not re-downloaded. However, if \code{TRUE}, packages are downloaded irregardless, overwriting any version already present (is analagous to updating if required).
#' @param auto Boolean. If \code{FALSE}, asks permission to install Python and/or packages. If \code{TRUE}, no user confirmation is required for activation.
#'
#' @returns Does not return an object as is simply preparing your R session.
#'
#' @examples
#' \dontrun{
#' ewsnet_init(envname = "EWSNET_env", auto = FALSE)
#' }
#' #Common errors at this stage result from 'reticulate's
#' #behaviour. For example, conflicts between 'ewsnet_init'
#' #and RETICULATE_PYTHON may occur if run inside a
#' #RStudio R project. To fix this, navigate to
#' #Preferences -> Python, untick 'Automatically
#' #activate project-local Python environments'
#' #and restart R.
#'
#' #if this fails due to timeout, you may need to
#' #increase the timeout length using something
#' #like below:
#' \donttest{
#' options(timeout = max(300, getOption("timeout")))
#' }
#'
#' \dontrun{
#' reticulate::py_config()
#' }
#' #If successful, 'EWSNET_env forced by use_python
#' #function' will be printed.
#'
#
#' @export

ewsnet_init <- function(envname, conda_path = reticulate::miniconda_path(), pip_ignore_installed = FALSE, auto = FALSE){

  detect.sys <- paste(Sys.info()["sysname"],Sys.info()["machine"],sep = "_") #identify specific OS
  wd <- getwd() #get working directory so it can be reset when Python alters the directory
  on.exit(setwd(wd), add = TRUE)

  if(identical(.Platform$OS.type, "windows")){ #windows has different file strucutre to Linux/MAC
    conda_binary <- paste0(conda_path,"/condabin/conda.bat")
    }else{
    conda_binary <- paste0(conda_path,"/bin/conda")
    }

  conda <- try(reticulate::conda_list(conda = conda_binary)) #extract list of conda Python environments
  if(inherits(conda, "try-error")){

    if(isTRUE(auto)){
      message("Attention: may take up to 10 minutes to complete")

      reticulate::install_miniconda(path = conda_path,force = F)

      reticulate::conda_create(
        conda = conda_binary,
        envname = paste0(envname),
        forge = FALSE,environment = NULL)

      if(detect.sys == "Darwin_arm64"){ # specfic install for MACOSX M1 machines (will need to change once tensorflow updates to 2.16)
        reticulate::conda_install(envname = paste0(envname),
                                  conda = conda_binary,
                                  packages = c("tensorflow-macos==2.13.0","scikit-learn",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed,
                                  pip_options = "--timeout=1000")
      }else{
        reticulate::conda_install(envname = paste0(envname),
                                  conda = conda_binary,
                                  packages = c("tensorflow==2.13.0","scikit-learn",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed,
                                  pip_options = "--timeout=1000")
      }
      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = conda_binary,
                               required = T)
      message(paste(envname,"successfully installed and activated. Necessary python packages installed"),
              collapse = " ")

    }else if(isFALSE(auto)){
    user_input1 <- readline("Anaconda not found. Would you like to download Anaconda? (y/n) ")

    if(user_input1 != 'y'){
      message('Aborting')

    }else{
      message("Attention: may take up to 10 minutes to complete")

      reticulate::install_miniconda(path = conda_path, force = F)

      reticulate::conda_create(
        conda = conda_binary,
        envname = paste0(envname),
        forge = FALSE,environment = NULL)

      if(detect.sys == "Darwin_arm64"){ # specfic install for MACOSX arm machines
      reticulate::conda_install(envname = paste0(envname),
                                conda = conda_binary,
                                packages = c("tensorflow-macos==2.13.0","scikit-learn",
                                             "sphinxcontrib-matlabdomain","seaborn"),
                                pip = T, pip_ignore_installed = pip_ignore_installed,
                                pip_options = "--timeout=1000")
      }else{
        reticulate::conda_install(envname = paste0(envname),
                                  conda = conda_binary,
                                  packages = c("tensorflow==2.13.0","scikit-learn",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed,
                                  pip_options = "--timeout=1000")
        }
      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = conda_binary,
                               required = T)
      message(paste(envname,"successfully installed and activated. Necessary python packages installed"),
              collapse = " ")
}
    }

  }else if(envname %in% conda$name){ #if desired environment found, activate and install necessary packages

    if(isTRUE(auto)){
      message("Attention: may take up to 10 minutes to complete")

      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = conda_binary,
                              required = T)

      if(detect.sys == "Darwin_arm64"){
        reticulate::conda_install(envname = paste0(envname),
                                  conda = conda_binary,
                                  packages = c("tensorflow-macos==2.13.0","scikit-learn",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed,
                                  pip_options = "--timeout=1000")
      }else{
        reticulate::conda_install(envname = paste0(envname),
                                  conda = conda_binary,
                                  packages = c("tensorflow==2.13.0","scikit-learn",
                                               "sphinxcontrib-matlabdomain", "seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed,
                                  pip_options = "--timeout=1000")
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
                               conda = conda_binary,
                                required = T)

      if(detect.sys == "Darwin_arm64"){
        reticulate::conda_install(envname = paste0(envname),
                                  conda = conda_binary,
                                  packages = c("tensorflow-macos==2.13.0","scikit-learn",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed,
                                  pip_options = "--timeout=1000")
      }else{
        reticulate::conda_install(envname = paste0(envname),
                                  conda = conda_binary,
                                  packages = c("tensorflow==2.13.0","scikit-learn",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed,
                                  pip_options = "--timeout=1000")
      }

      message(paste(envname,"successfully found and activated. Necessary python packages installed"),
              collapse = " ")
    }

    }

  }else{ #if desired environment not found, ask permission to create new environment

    if(isTRUE(auto)){

      reticulate::conda_create(
        envname = paste0(envname),
        conda = conda_binary,
        forge = FALSE,environment = NULL)

      if(detect.sys == "Darwin_arm64"){
        reticulate::conda_install(envname = paste0(envname),
                                  conda = conda_binary,
                                  packages = c("tensorflow-macos==2.13.0","scikit-learn",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed,
                                  pip_options = "--timeout=1000")
      }else{
        reticulate::conda_install(envname = paste0(envname),
                                  conda = conda_binary,
                                  packages = c("tensorflow==2.13.0","scikit-learn",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed,
                                  pip_options = "--timeout=1000")
      }
      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = conda_binary,
                                required = T)

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
        conda = conda_binary,
        forge = FALSE,environment = NULL)

      if(detect.sys == "Darwin_arm64"){
        reticulate::conda_install(envname = paste0(envname),
                                  conda = conda_binary,
                                  packages = c("tensorflow-macos==2.13.0","scikit-learn",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed,
                                  pip_options = "--timeout=1000")
      }else{
        reticulate::conda_install(envname = paste0(envname),
                                  conda = conda_path,
                                  packages = c("tensorflow==2.13.0","scikit-learn",
                                               "sphinxcontrib-matlabdomain","seaborn"),
                                  pip = T, pip_ignore_installed = pip_ignore_installed,
                                  pip_options = "--timeout=1000")
      }
      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = conda_binary,
                               required = T)

      message(paste(envname,"successfully installed and activated. Necessary python packages installed"),
              collapse = " ")
    }
    }
  }
  setwd(wd) # reset working directory

}
