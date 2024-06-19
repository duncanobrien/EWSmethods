#' S-EWSNet Predict
#'
#' Communicates with S-EWSNet (https://ewsnet.github.io), a deep learning framework for modelling and anticipating regime shifts in dynamical spatial systems, and returns the model's prediction for the inputted spatial time series.
#'
#' @param x A list of square integer matrices representing presence/absence pixels. Pixels could be vegetation presence. Ensure entires are integers not numeric.
#' @param id Vector identifying each entry in x. Could be year, plot identity etc.
#' @param envname A string naming the Python environment prepared by \code{ewsnet_init()}.
#' @param delta Numeric. Difference in densities.
#' @param inp_size Numeric. Size of clipped Fourier transformed square matrix.
#' @param model_path A string naming the path to the S-EWSnet model installed by \code{sewsnet_reset()}.
#'
#' @returns A dataframe of S-EWSNet predictions. Values represent the estimated probability that the quoted event will occur.
#'
#' @examples
#' #A dummy dataset of a patchy savanna
#' #monitored over 10 sites/years.
#'
#' vegetation_data <- vector("list", length = 50)
#' vegetation_data <- lapply(vegetation_data,function(x){
#' matrix(rbinom(128^2,1,0.6),nrow = 128,ncol=128)
#' })
#'
#' #Activate python environment (only necessary
#' #on first opening of R session).
#'
#' \dontrun{
#' ewsnet_init(envname = "EWSNET_env")
#' }
#'
#' #Generate EWSNet predictions.
#'
#' \dontrun{
#' pred <- sewsnet_predict(
#'  vegetation_data,
#'  delta = 0.1,
#'  inp_size = 25,
#'  envname = "EWSNET_env")
#'  }
#'
#' @export
#' @source Deb, S., Ekansh, M., Paras, G. et al. (2024) Optimal sampling of spatial patterns improves deep learning-based early warning signals of critical transitions. Royal Society Open Science. 11, 231767.
#'
sewsnet_predict <- function(x, id = NULL, envname, delta = 0.1, inp_size = 25, model_path = default_sewsnet_path()) {
  if (!envname %in% (reticulate::conda_list()$name)) {
    warning(
      "Call 'ewsnet_init()' before attempting to use ewsnet_predict(), or check your spelling of envname"
    )
  } else{
    if (!is.list(x)) {
      stop('Time series is not a list of dataframes/matrices')
    }

    if (!all(sapply(x,function(ls)is.integer(unlist(ls))))) {
      stop('Pixel matrices are not integer valued. If your dataframes are numeric, you can convert to integer using:
              for(i in seq_along(x)){
                   mode(x[[i]]) <- "integer"
              }')
    }

    if(!dir.exists(file.path(paste(model_path,"S-EWSNet",sep="/"))) & model_path != ""){
      stop('No S-EWSNet model files found. Call sewsnet_reset(remove_model = FALSE) to download model files')
    }

    reticulate::py_run_string("from tensorflow.keras.models import load_model")
    directory_string <- paste(c("directory_string = '", paste(model_path,"S-EWSNet",sep="/"),"'"),collapse = "")
    reticulate::py_run_string(directory_string)
    reticulate::py_run_string("sewsnet = load_model(directory_string)")

    densities <- do.call("c", lapply(x, function(x) {
      mean(as.matrix(x))
    }))

    samples <- 5 #parameter must be set given modellers' decisions
    bins <- floor(densities / delta)
    bins <- as.numeric(factor(bins,labels = 1:length(unique(bins))))
    n_entries <- table(bins)
    buckets <- vector("list", length = length(unique(bins)))
    buckets <- lapply(unique(bins), function(x) {
      vector("list", length = n_entries[names(n_entries) == x])
    })
    names(buckets) <- unique(bins)

    for(i in 1:length(x)){
      curr_bin <- bins[i]
      pos_in_bin <- which(unlist(lapply(buckets[[which(names(buckets) %in% curr_bin)]], function(k) {
        is.null(k)
      })))[1]
      buckets[[paste(curr_bin)]][[pos_in_bin]] <- x[[i]]
    }

    data <- lapply(1:length(x), function(i) {
      iters <- which(names(buckets) %in% (bins[i] + 1):length(x))

      sample_img <- lapply(which(names(buckets) %in% (bins[i] + 1):length(x)),
                           function(k) {
                             new_img = base::sample(buckets[[k]], 1)[[1]]
                             new_img = stats::fft(as.matrix(new_img))[1:inp_size, 1:inp_size]
                             return(list(abs(new_img), Arg(new_img)))
                           })

      img <- stats::fft(as.matrix(x[[i]]))[1:inp_size, 1:inp_size]
      dat <- c(list(list(Re(img), Im(img))), sample_img)
      if(length(dat)>=samples){
        dat <- dat[1:samples]
      }
      dat <- rev(dat)

      out <- array(dim = c(1, length(dat), inp_size, inp_size, 2))

      for (z in seq_along(dat)) {
        out[1, z, , , 1] <- dat[[z]][[1]]
        out[1, z, , , 2] <- dat[[z]][[2]]
      }
      return(out)
    })

    preds <- do.call("rbind",lapply(data,function(step){

      if(dim(step)[2]<samples){
        data.frame("pred" = "No transition",
                   "no_trans_prob" = 1,
                   "smooth_trans_prob" = 0,
                   "critical_trans_prob" = 0)
      }else{

      pred <- reticulate::py$sewsnet$predict(step)

      data.frame("pred" = ifelse(max(pred) == pred[1],"Smooth",
                                 ifelse(max(pred) == pred[2],"Critical","No transition")),
                 "no_trans_prob" = pred[3],
                 "smooth_trans_prob" = pred[1],
                 "critical_trans_prob" = pred[2])
      }

    }))

    if(!is.null(id)){
      return(cbind("id" = id[length(id):1],preds,"density" = densities))
      #return(cbind("id" = id,preds,"density" = densities))
    }else{
    return(cbind(preds,"density" = densities))
    }
  }
}
