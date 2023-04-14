#' Univariate Early Warning Signal Assessment
#'
#' A function for performing early warning signal (EWS) assessment on univariate time series. Both rolling and expanding window methods of EWS assessment can be performed with the assessments returned as a dataframe.
#'
#' @param data A dataframe where the first column is an equally spaced time vector and the second column is the time series to be assessed.
#' @param metrics String vector of early warning signal metrics to be assessed.  Options include: \code{"ar1"}, \code{"cv"}, \code{"SD"}, \code{"acf"}, \code{"rr"}, \code{"dr"}, \code{"skew"}, \code{"kurt"}, and \code{"trait"}.
#' @param method Single string of either \code{"expanding"} or \code{"rolling"}. \code{"expanding"} calls composite, expanding window EWS assessment. \code{"rolling"} calls typical, rolling window EWS assessment.
#' @param winsize Numeric value. If \code{method = "rolling"}, defines the window size of the rolling window as a percentage of the time series length.
#' @param burn_in Numeric value. If \code{method = "expanding"}, defines the number of data points to 'train' signals prior to EWS assessment.
#' @param threshold Numeric value of either \code{1} or \code{2}. Threshold*sigma is the value which, if the EWS strength exceeds it, constitutes a "signal".
#' @param tail.direction String of either \code{"one.tailed"} or \code{"two.tailed"}. \code{"one.tailed"} only indicates a warning if positive threshold sigma exceeded. \code{"two.tailed"} indicates a warning if positive OR negative threshold*sigma exceeded.
#' @param trait A vector of numeric trait values if desired. Can be \code{NULL}
#'
#' @returns A list containing up to two objects: EWS outputs through time (\code{EWS}), and a plot object (\code{plot}) if \code{ggplotIt = TRUE}.
#' \item{EWS$raw}{Dataframe of EWS measurements through time. If \code{method = "expanding"}, then each metric has been rbound into a single dataframe and extra columns are provided indicating whether the threshold*sigma value has been exceeded (i.e. \code{"threshold.crossed"}). If \code{method = "expanding"}, then each metric's evolution over time is returned in individual columns.}
#' \item{EWS$cor}{Dataframe of Kendall Tau correlations. Only returned if \code{method = "rolling"}.}
#'
#' @examples
#' #A dummy dataset of a hedgerow bird population over
#' #50 years where both the number of individuals and
#' #the average bill length has been measured.
#'
#' abundance_data <- data.frame(time = seq(1:50),
#'  abundance = rnorm(50,mean = 20),
#'  trait = rnorm(50,mean=1,sd=0.5))
#'
#' #The early warning signal metrics to compute.
#'
#' ews_metrics <- c("SD","ar1","skew")
#'
#' #Rolling window early warning signal assessment of
#' #the bird abundance.
#'
#' roll_ews <- uniEWS(
#'  data = abundance_data[,1:2],
#'  metrics =  ews_metrics,
#'  method = "rolling",
#'  winsize = 50)
#'
#' #Expanding window early warning signal assessment of
#' #the bird abundance (with plotting).
#'
#' exp_ews <- uniEWS(
#'  data = abundance_data,
#'  metrics = ews_metrics,
#'  method = "expanding",
#'  burn_in = 10)
#'
#' #Expanding window early warning signal assessment of
#' #the bird abundance incorporating the trait
#' #information.
#'
#' ews_metrics_trait <- c("SD","ar1","trait")
#'
#' trait_exp_ews <- uniEWS(
#'  data = abundance_data,
#'  metrics = ews_metrics_trait,
#'  method = "expanding",
#'  burn_in = 10,
#'  trait = abundance_data$trait)
#'
#' @export

uniEWS <- function(data,metrics,method = c("expanding","rolling"),
                   winsize = 50, burn_in = 5, threshold = 2,
                   tail.direction = "one.tailed", trait = NULL){

  method <- match.arg(method,choices = c("expanding","rolling"))
  tail.direction <- match.arg(tail.direction,choices = c("one.tailed","two.tailed"))
  metrics <- match.arg(metrics,choices = c("cv", "acf", "ar1", "dr", "rr", "skew","kurt","SD","trait"), several.ok=T)

  if(length(class(data)) > 1 & isTRUE(is.data.frame(data))){
    data <- as.data.frame(data)
  } #allows tibbles to be used


  if(any(is.na(data))){
    stop('Data contains missing values. Interpolation of missing values is recommended')
  }

  if(method == "expanding"){
    to.test.l<-list()
    for(jj in 1:length(metrics)){
      #jj=1
      to.test.l[[jj]]<-split(gtools::combinations(n = length(metrics), r = jj, v = metrics, repeats.allowed = FALSE), seq(nrow(gtools::combinations(n = length(metrics), r = jj, v = metrics, repeats.allowed = FALSE))))
    }
    to.test<-unlist(to.test.l, recursive=FALSE)

    ##object to store results
    res<-NULL

    ##loop through all the metrics
    ##object to store results
    for(i in 1:length(to.test)){
      #i=10
      ##set the weighing to 1 - just required to make the code run, doesnt do anythnig
      W<-data.frame(inds=sort(unlist(to.test[i])), "wei"=1)
      ##run the EWS from clements & ozgul nat comms and save out:
      res[[i]]<-W_composite_ews(dat=data, indicators=sort(unlist(to.test[i])), weights=W, trait = trait, threshold=threshold, burn_in = burn_in, tail.direction = tail.direction,interpolate = F)
    }

    bind.res <- do.call("rbind",res)
    bind.res$str<-(bind.res$metric.score-bind.res$rolling.mean)/bind.res$rolling.sd

    if(!is.null(trait)){
      bind.res<-as.data.frame(bind.res) %>%
        dplyr::left_join(data.frame("time" = data[,1],"trait" = trait),by = "time")
    }
    out <- list("EWS" = bind.res, "method" = method,"threshold" = threshold, "tail.direction" = tail.direction)
    class(out) <- c("EWSmethods","expEWS","uniEWS")
  }

  if(method == "rolling"){

    bind.res <- no.plot.ews(timeseries = data, winsize = winsize,interpolate = F)

    bind.res$raw <- bind.res$raw[,c("timeindex",metrics)]
    bind.res$raw<-as.data.frame(bind.res$raw) %>%
      dplyr::left_join(data.frame("timeindex" = data[,1],"count.used" = data[,2]),by = "timeindex") %>%
      dplyr::rename("time" = "timeindex")
    bind.res$cor <- bind.res$cor[,metrics]
    out <- list("EWS" = bind.res, "method" = method)
    class(out) <- c("EWSmethods","rollEWS","uniEWS")
  }
  return(out)
}
