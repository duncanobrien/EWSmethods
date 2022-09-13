#' Reset EWSNet Model Weights
#'
#' Restores EWSNet model weights to the pretrained defaults published at https://ewsnet.github.io. This is vital on first use of EWSNet as no model weights are provided with `EWSmethods`. The use of this function may also be necessary after finetuning to reset the model.
#'
#'
#' @param remove_weights Boolean. Should all weights be removed (\code{TRUE}) or should weights be re/downloaded (\code{FALSE}).
#' @param auto Boolean. If \code{TRUE}, no user confirmation is required for re/download.
#'
#' @examples
#' \dontrun{
#' ewsnet_reset(remove_weights = FALSE, auto = FALSE) # on first use of EWSNet via `EWSmethods`
#' }
#'
#'\dontrun{
#' ewsnet_reset(remove_weights = TRUE) # to remove all downloaded weights
#'}
#'
#' @export

# ewsnet_reset <- function(new_weight_location){
#
#  target_folder <- system.file("python/weights", package = "EWSmethods")
#  file.copy(new_weight_location, target_folder, recursive = T)
#
#  message("Model weights reset")
# }


ewsnet_reset <- function(remove_weights = FALSE, auto = FALSE){

  target_folder <- system.file("python/weights", package = "EWSmethods")

  if(isTRUE(remove_weights)){
    unlink(paste(c(target_folder,"Pretrained"),collapse = "/"),recursive = T)
    message("Model weights removed")

    }else{

      if(isTRUE(auto)){

        zip <- paste(c(target_folder,"temp.zip"),collapse = "/")

        download.file("https://drive.google.com/u/0/uc?export=download&confirm=EoIm&id=19OuqzrY1LQxZusByf4ACPj-yiex4LY2e",
                      destfile  = zip, overwrite = TRUE) #VariableLenModel EWSNet

        unzip(zip,exdir = target_folder)
        unlink(zip)
        message("Model weights reset")

      }else if(isFALSE(auto)){
      user_input1 <- readline("EWSNet weights are are a ~ 220 MB download. Would you like to continue? (y/n) ")

      if(user_input1 != 'y'){
        message('Aborting')

      }else{
        message("Attention: weights will be downloaded")

      zip <- paste(c(target_folder,"temp.zip"),collapse = "/")
      # download.file("https://drive.google.com/u/0/uc?export=download&confirm=EoIm&id=1-aY2MepouLQdMSNkYD6jgSedwFXB8BUP",
      #               destfile  = zip, overwrite = TRUE) #original EWSNet

      utils::download.file("https://drive.google.com/u/0/uc?export=download&confirm=EoIm&id=19OuqzrY1LQxZusByf4ACPj-yiex4LY2e",
                destfile  = zip, overwrite = TRUE) #VariableLenModel EWSNet

      utils::unzip(zip,exdir = target_folder)
      unlink(zip)
      message("Model weights downloaded")
      }
      }
    }

}

