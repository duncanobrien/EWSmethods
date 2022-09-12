#' Reset EWSNet Model Weights
#'
#' Restores EWSNet model weights to the pretrained defaults published at https://ewsnet.github.io. This may be necessary after finetuning to reset the model. Follow this link (https://github.com/sahilsid/ewsnet) to find the original weights and extract the zip file in to a suitable location to use in \code{new_weight_location}.
#'
#'
#' @param new_weight_location A path string to the location where the unzipped file has been saved. Include "Pretrained" as the final path folder.
#'
#' @examples
#' \dontrun{
#' ewsnet_reset(new_weight_location = "C:/Users/your_name/Downloads/Pretrained") # Windows example
#' }
#'
#'\dontrun{
#' ewsnet_reset(new_weight_location = "/Users/your_name/Downloads/Pretrained") # Mac example
#'}
#'
#' @export

ewsnet_reset <- function(new_weight_location){

 target_folder <- system.file("python/weights", package = "EWSmethods")
 file.copy(new_weight_location, target_folder, recursive = T)

 message("Model weights reset")
}


# reset_ewsnet <- function(){
#
#   target_folder <- system.file("python/weights", package = "EWSmethods")
#
#   zip <- paste(c(target_folder,"temp.zip"),collapse = "/")
#
#   # download.file("https://drive.google.com/u/0/uc?export=download&confirm=EoIm&id=1-aY2MepouLQdMSNkYD6jgSedwFXB8BUP",
#   #               destfile  = zip, overwrite = TRUE) #original EWSNet
#   download.file("https://drive.google.com/u/0/uc?export=download&confirm=EoIm&id=19OuqzrY1LQxZusByf4ACPj-yiex4LY2e",
#                 destfile  = zip, overwrite = TRUE) #VariableLenModel EWSNet
#
#   unzip(zip,exdir = target_folder)
#   file.remove(zip)
#
#   message("Model weights reset")
# }

