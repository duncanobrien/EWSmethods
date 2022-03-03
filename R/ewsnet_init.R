#' EWSNet Initialisation
#'
#' Prepares your R session for communicating with Python. This function first searches your computer for an appropriate Python environment and activates it, importing the vital Python packages required. If no appropriate Python install or environment is found, after asking permission, Anaconda is downloaded and an environment created.
#'
#' @param envname A string naming the desired Python environment to create/activate. If no python or environment found, the functions prompts to install Anaconda and the required python packages.
#' @param pip_ignore_installed Boolean. If \code{FALSE}, any packages already installed are loaded and not re-downloaded. However, if \code{TRUE}, packages are downloaded irregardless, overwriting any version already present (is analagous to updating if required).
#'
#' @returns Does not return an object as is simply preparing your R session.
#'
#' @examples
#' \dontrun{ewsnet_init(envname = "EWSNET_env")}
#'
#' \dontrun{reticulate::py_config()}
#' #If successful, 'EWSNET_env forced by use_python
#' #function' will be printed.
#'
#' @export

ewsnet_init <- function(envname, pip_ignore_installed = FALSE){

  conda <- try(reticulate::conda_list()) #extract list of conda Python environments
  if(class(conda) == "try-error"){

    user_input1 <- readline("Anaconda not found. Would you like to download Anaconda? (y/n) ")

    if(user_input1 != 'y'){

      message('Aborting')

    }else{
      message("Attention: may take up to 10 minutes to complete")

      reticulate::install_miniconda(force = T)

      reticulate::conda_create(
        envname = paste0(envname),
        forge = FALSE,environment = NULL,
        conda = "auto")

      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = "auto", required = T)

      reticulate::conda_install(envname = paste0(envname),
                                packages = c("tensorflow","scikit-learn","pandas","matplotlib",
                                             "sphinxcontrib-matlabdomain","seaborn"),
                                pip = T, pip_ignore_installed = pip_ignore_installed)

      message(paste(envname,"successfully installed and activated. Necessary python packages installed"),
              collapse = " ")

    }
  }else if(envname %in% conda$name){ #if desired environment found, activate and install necessary packages

    string <- c("conda env",envname, "found. Would you like to activate it and install necessary\npackages? (y/n) ")
    user_input2 <- readline(paste(string,collapse =" "))

    if(user_input2 != 'y'){
      message('Aborting')
    }else{
      message("Attention: may take up to 10 minutes to complete")

      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = "auto", required = T)

      reticulate::conda_install(envname = paste0(envname),
                                packages = c("tensorflow","scikit-learn","pandas","matplotlib",
                                             "sphinxcontrib-matlabdomain","seaborn"),
                                pip = T, pip_ignore_installed	= pip_ignore_installed)

      message(paste(envname,"successfully found and activated. Necessary python packages installed"),
              collapse = " ")
    }

  }else{ #if desired environment not found, ask permission to create new environment
    string <- c("conda env",envname, "not found. Would you like to install a new env? (y/n) ")
    user_input2 <- readline(paste(string,collapse =" "))
    if(user_input2 != 'y'){ message('Aborting')
    }else{

      reticulate::conda_create(
        envname = paste0(envname),
        forge = FALSE,environment = NULL,
        conda = "auto")

      reticulate::use_condaenv(condaenv = paste0(envname),
                               conda = "auto", required = T)

      reticulate::conda_install(envname = paste0(envname),
                                packages = c("tensorflow","scikit-learn","pandas","matplotlib",
                                             "sphinxcontrib-matlabdomain","seaborn"),
                                pip = T, pip_ignore_installed	= pip_ignore_installed)

      message(paste(envname,"successfully installed and activated. Necessary python packages installed"),
              collapse = " ")
    }
  }
}
