#' Univariate Early Warning Signal Assessment
#'
#' A function for performing early warning signal (EWS) assessment on univariate time series. Both traditional methods of EWS assessment can be performed (rolling vs expanding window) with the assessments returned as a dataframe with and/or without a standardised ggplot-based figure.
#'
#' @param data A dataframe where the first column is an equally spaced time vector and the second column is the time series to be assessed.
#' @param metrics String vector of early warning signal metrics to be assessed.  Options include: \code{"ar1"}, \code{"cv"}, \code{"SD"}, \code{"acf"}, \code{"rr"}, \code{"dr"}, \code{"skew"}, \code{"kurt"}, \code{"mean.size"}, \code{"sd.size"}, \code{"sd.95"} and \code{"trait"}.
#' @param method Single string of either \code{"expanding"} or \code{"rolling"}. \code{"expanding"} calls composite, expanding window EWS assessment. \code{"rolling"} calls typical, rolling window EWS assessment.
#' @param interpolate Boolean. If \code{TRUE}, linearly interpolates missing values found within the abundance time series.
#' @param ggplotIt Boolean. If \code{TRUE}, returns a ggplot plot of EWS strength trends AND input abundance.
#' @param y_lab String label. If \code{ggplotIt = TRUE}, labels the abundance y axis.
#' @param winsize Numeric value. If \code{method = "rolling"}, defines the window size of the rolling window as a percentage of the time series length.
#' @param threshold Numeric value of either \code{1} or \code{2}. Threshold*sigma is the value which, if the EWS strength exceeds it, constitutes a "signal".
#' @param tail.direction String of either \code{"one.tailed"} or \code{"two.tailed"}. \code{"one.tailed"} only indicates a warning if positive threshold sigma exceeded. \code{"two.tailed"} indicates a warning if positive OR negative threshold*sigma exceeded.
#' @param burn_in Numeric value. The number of data points to 'train' signals prior to EWS assessment.
#' @param trait A vector of numeric trait values if desired. Can be \code{NULL}
#' @param trait_lab String label. If \code{ggplotIt = TRUE}, & trait populated, & \code{"trait"} supplied in metrics, labels the right side y axis which represents trait values through time.
#' @param trait_scale Numeric value. Scales trait y axis relative to abundance y axis.
#'
#' @returns A list containing up to two objects: EWS outputs through time (\code{EWS}), and a plot object (\code{plot}) if \code{ggplotIt = TRUE}.
#' \item{EWS$raw}{Dataframe of EWS measurements through time. If \code{method = "expanding"}, then each metric has been rbound into a single dataframe and extra columns are provided indicating whether the threshold*sigma value has been exceeded (i.e. \code{"threshold.crossed"}). If \code{method = "expanding"}, then each metric's evolution over time is returned in individual columns.}
#' \item{EWS$cor}{Dataframe of Kendall Tau correlations. Only returned if \code{method = "rolling"}.}
#' \item{plot}{Plot object. Only returned if \code{ggplotIt = "TRUE"}.}
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
#' #the bird abundance (no plotting).
#'
#' roll_ews <- univariate_EWS_wrapper(
#'  data = abundance_data[,1:2],
#'  metrics =  ews_metrics,
#'  ggplotIt = FALSE,
#'  method = "rolling",
#'  winsize = 50)
#'
#' #Expanding window early warning signal assessment of
#' #the bird abundance (with plotting).
#'
#' \dontrun{exp_ews <- univariate_EWS_wrapper(
#'  data = abundance_data,
#'  metrics = ews_metrics,
#'  method = "expanding",
#'  burn_in = 10,
#'  ggplotIt =TRUE,
#'  ylab = "Bird abundance")}
#'
#' #Expanding window early warning signal assessment of
#' #the bird abundance incorporating the trait
#' #information (with plotting).
#'
#' ews_metrics_trait <- c("SD","ar1","trait")
#' \dontrun{trait_exp_ews <- univariate_EWS_wrapper(
#'  data = abundance_data,
#'  metrics = ews_metrics_trait,
#'  method = "expanding",
#'  burn_in = 10,
#'  ggplotIt =TRUE,
#'  trait = abundance_data$trait,
#'  trait_lab = "Bill length (mm)",
#'  trait_scale = 10)}
#'
#' @importFrom stats quantile
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_alpha_manual
#' @importFrom ggplot2 guide_legend
#' @importFrom ggplot2 scale_colour_manual
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 margin
#' @importFrom ggplot2 unit
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 sec_axis
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 scale_linetype_manual
#' @importFrom dplyr across
#' @importFrom dplyr everything
#' @importFrom dplyr .data
#'
#' @export
univariate_EWS_wrapper <- function(data,metrics,method = c("expanding","rolling"),
                                   interpolate = FALSE, ggplotIt = TRUE, y_lab = "Generic indicator name",
                                   winsize = 50, threshold = 2,tail.direction = "one.tailed",
                                   burn_in = 5, trait = NULL,
                                   trait_lab = "Generic Trait Name",
                                  trait_scale = 1000){

  method <- match.arg(method,choices = c("expanding","rolling"))

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
      res[[i]]<-W_composite_ews(dat=data, indicators=sort(unlist(to.test[i])), weights=W, trait = trait, threshold=threshold, burn_in = burn_in, tail.direction = tail.direction,interpolate = interpolate)
    }

    bind.res<-data.table::rbindlist(res)
    bind.res$str<-(bind.res$metric.score-bind.res$rolling.mean)/bind.res$rolling.sd
    bind.res<-as.data.frame(bind.res)

    if(isTRUE(ggplotIt)){

      p<- ggplot(data = bind.res, aes(x=.data$time,y=.data$str,col=.data$metric.code)) +
        geom_hline(yintercept = threshold, linetype="solid", color = "grey", size=1)+
        geom_line()+
        geom_point(aes(x=.data$time, y = .data$str,alpha = as.factor(.data$threshold.crossed))) +
        scale_alpha_manual(values = c(0,1),
                           breaks = c("0","1"),labels = c("Undetected","Detected"), name = "EWS",
                           guide = guide_legend(order = 1, override.aes =
                                                  list(linetype = c(0),shape = c(16),col="black"))) +
        scale_colour_manual(values = scales::hue_pal()(length(to.test)),
                            guide = guide_legend(order = 2, override.aes =
                                                   list(linetype = rep(1,length(to.test)),shape= NA))) +
        ggthemes::theme_clean() + xlab("Time point") + ylab("Strength of EWS") +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        labs(color='EWS Indicator\nStrength') +
        theme(plot.margin = margin(c(10, 8, 0, 10)),
              legend.key.height = unit(0.3,"cm"),
              legend.key.width = unit(0.5,"cm"),
              legend.title = element_text(size = 10),
              legend.text = element_text(size=10))

      if(is.null(trait)==F){
        plot.dat<-data.frame("timeseries"=bind.res$time, "count.used"=bind.res$count.used) %>%
          dplyr::left_join(data.frame("timeseries" = c(burn_in:dim(data)[1]), "trait" =trait[burn_in:dim(data)[1]]),by= "timeseries")

        p2 <-ggplot(data = plot.dat, aes(x=.data$timeseries, y=.data$count.used)) +
          aes(group=NA)+
          geom_line(aes(y=.data$count.used, linetype = "Count")) +
          geom_line(aes(y=(.data$trait*trait_scale), linetype = "Trait"), size = 0.4, alpha = 0.4,col = "blue") +
          # geom_line(aes(y=.data$count.used),linetype=1) +
          # geom_line(aes(y=(.data$trait*trait_scale)),linetype=2, size = 0.4, alpha = 0.4,col = "blue") +
          geom_point(data =bind.res[bind.res$metric.code == bind.res$metric.code[length(bind.res$metric.code)],],
                     aes(x=.data$time, y = max(.data$count.used)*-0.1,col=.data$metric.code,alpha = as.factor(.data$threshold.crossed)),size = 3,pch= "|",col = "#53B400") +
          scale_alpha_manual(values = c(0,1),
                             breaks = c("0","1"),labels = c("Undetected","Detected"), name = "EWS",
                             guide = guide_legend(override.aes =
                                                    list(size = c(4),shape = c("|")))) +
          scale_linetype_manual(values = c(1,2),breaks = c("Count","Trait"), name = "Time series",
                               guide = guide_legend(override.aes =
                                                      list(col = c("black","blue"),alpha = c(1,0.4))))+
          scale_y_continuous(y_lab,sec.axis = sec_axis(~./trait_scale, name = trait_lab))+
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          xlab("Time point")+
          ylab(y_lab)+
          ggthemes::theme_clean()+
          annotate("label", size = 2, x = quantile(plot.dat$time,0.90), y =max(plot.dat$count.used)*0.95 , label = paste(c("EWS indicator:",bind.res$metric.code[length(bind.res$metric.code)]),collapse = " "))+
          guides(alpha = guide_legend(order = 1))+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm" ),
                legend.key.width = unit(0.5,"cm"),
                legend.title = element_text(size = 10),
                legend.text = element_text(size=10))

        final.p <- egg::ggarrange(p2,p,nrow = 2,heights = c(1, 1))
        print(final.p)

      }else if(is.null(trait)==T){
        plot.dat<-data.frame("time"=as.numeric(bind.res$time), "count.used"=bind.res$count.used)

        p4 <-ggplot(data = plot.dat, aes(x=.data$time, y=.data$count.used)) +
          aes(group=NA)+
          geom_line(col = "black")+
          geom_point(data =bind.res[bind.res$metric.code == bind.res$metric.code[length(bind.res$metric.code)],],
                     aes(x=.data$time, y = max(.data$count.used)*-0.1,col=.data$metric.code,alpha = as.factor(.data$threshold.crossed)),size = 3,pch= "|",col = "#53B400") +
          scale_alpha_manual(values = c(0,1),
                             breaks = c("0","1"),labels = c("Undetected","Detected"), name = "EWS",
                             guide = guide_legend(override.aes =
                                                    list(size = c(4),shape = c("|")))) +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          ylab(y_lab) +
          xlab("Time point")+
          ggthemes::theme_clean()+
          annotate("label", size = 2, x = quantile(plot.dat$time,0.90), y =max(plot.dat$count.used)*0.95 , label = paste(c("EWS indicator:",bind.res$metric.code[length(bind.res$metric.code)]),collapse = " "))+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm" ),
                legend.key.width = unit(0.5,"cm"),
                legend.title = element_text(size = 10),
                legend.text = element_text(size=10))

        final.p <- egg::ggarrange(p4,p,nrow = 2,heights = c(1, 1))
        print(final.p)
      }

    }
  }

  if(method == "rolling"){

    bind.res <- no.plot.ews(timeseries = data, winsize = winsize,interpolate = interpolate)

      bind.res$raw <- bind.res$raw[,c("timeindex",metrics)]
      bind.res$raw<-as.data.frame(bind.res$raw)
      bind.res$cor <- bind.res$cor[,metrics]

      if(isTRUE(ggplotIt)){

        plot.dat <- bind.res$raw %>%
          dplyr:: mutate(across(-c(.data$timeindex),~scale(.x)))%>%
          dplyr::left_join(data.frame("timeindex" = data[,1],"count.used" = data[,2]),by = "timeindex")%>%
          tidyr::pivot_longer(-c(.data$timeindex,.data$count.used), names_to = "metric.code", values_to = "str")

        cor.dat <- bind.res$cor %>%
          tidyr::pivot_longer(everything(),names_to = "metric.code", values_to = "cor") %>%
          dplyr::rowwise()%>%
          dplyr::mutate(cor = paste(c("Tau:", round(.data$cor, digits = 3)),collapse = " ")) %>%
          dplyr::mutate(timeindex = quantile(plot.dat$timeindex,0.8), str =  max(plot.dat$str)*0.8)

        p<- ggplot(data = plot.dat, aes(x=.data$timeindex,y=.data$str,group=.data$metric.code)) +
          geom_line(aes(col= .data$metric.code))+
          geom_text(data = cor.dat,aes(label = .data$cor),size = 3)+
          scale_colour_manual(values = scales::hue_pal()(length(metrics)),guide = guide_legend(override.aes = list(linetype = rep(1,7),shape=NA))) +
          ggthemes::theme_clean() + xlab("Time point") + ylab("Scaled metric value") +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          labs(color='EWS Indicator\nTrend') +
          facet_wrap(~.data$metric.code,nrow=4)+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm"),
                legend.key.width = unit(0.5,"cm"),
                legend.title = element_text(size = 10),
                legend.text = element_text(size=10))+
          guides(alpha = guide_legend(order = 1),
                 col = guide_legend(order = 2))

        p2 <-ggplot(data = plot.dat, aes(x=.data$timeindex, y=.data$count.used)) +
          aes(group=NA)+
          geom_line(col = "black")+
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          ylab(y_lab) +
          xlab("Time point")+
          ggthemes::theme_clean()+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm" ),
                legend.key.width = unit(0.5,"cm"),
                legend.title = element_text(size = 10),
                legend.text = element_text(size=10))

        final.p <- egg::ggarrange(p2,p,nrow = 2,heights = c(1, 2))
        print(final.p)
      }

  }

  if(isTRUE(ggplotIt) & exists("final.p")){
    return(list("EWS" = bind.res,"plot" = final.p))
  }else{
    return(bind.res)
  }

}
