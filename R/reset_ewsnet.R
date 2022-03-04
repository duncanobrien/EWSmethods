#' Reset EWSNet Model Weights
#'
#' Restores EWSNet model weights to the pretrained defaults published at https://ewsnet.github.io. This may be necessary after finetuning to reset the model. Follow this link (https://github.com/sahilsid/ewsnet) to find the original weights and extract the zip file in to a suitable location to use in \code{new_weight_location}.
#'
#'
#' @param new_weight_location A path string to the location where the unzipped file has been saved. Include "Pretrained" as the final path folder.
#'
#' @examples
#' \dontrun{reset_ewsnet(new_weight_location = "C:/Users/your_name/Downloads/Pretrained")} # Windows example
#' \dontrun{reset_ewsnet(new_weight_location = "/Users/your_name/Downloads/Pretrained")} # Mac example
#'
#' @export

reset_ewsnet <- function(new_weight_location){

 target_folder <- system.file("python/weights", package = "EWSmethods")
 file.copy(new_weight_location, target_folder, recursive = T)
 message("Model weights replaced")
}
