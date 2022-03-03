#' Prevent Reticulate Autoconfiguring Python environments
#'
#'
bypass_reticulate_autoinit <- function(){

  rEnvPath = file.path("~", ".Renviron")
  envLines = c()  ## init blank lines
  if (file.exists(rEnvPath)) {
    envLines = readLines(rEnvPath)# get rProfile
  }
  ## add new line to bottom of file
  newLine = 'RETICULATE_AUTOCONFIGURE = "FALSE"'
  if(newLine %in% envLines){
    message("RETICULATE_AUTOCONFIGURE already set to FALSE")
    Sys.setenv(RETICULATE_AUTOCONFIGURE = FALSE)
  }else{
  envLines = c(envLines, newLine)
  writeLines(envLines, rEnvPath)
  ## also set line for current session
  Sys.setenv(RETICULATE_AUTOCONFIGURE = FALSE)
message("RETICULATE_AUTOCONFIGURE set to FALSE")
}
}
