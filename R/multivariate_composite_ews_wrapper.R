#' Multivariate Early Warning Signal Assessment
#'
#' A single function for performing early warning signal (EWS) assessment on multivariate systems where multiple time series have been measured. Both methods of EWS assessment can be performed (rolling or expanding windows) with the assessments returned as a dataframe with or without a standardised ggplot-based figure. The two methods of dimension reduction used to perform these assessments are Principal Component Analysis and Maximum/Minimum Autocorrelation Factors.
#'
#' @param data A dataframe where the first column is an equally spaced time vector and all other columns are individual time series. These could be different species, populations or measurements.
#' @param method Single string of either \code{"expanding"} or \code{"rolling"}. \code{"expanding"} calls composite, expanding window EWS assessment. \code{"rolling"} calls typical, rolling window EWS assessment.
#' @param ggplotIt Boolean. If \code{TRUE}, returns a ggplot plot of EWS strength trends AND the estimated dimension reduction.
#' @param winsize Numeric value. If \code{method = "rolling"}, defines the window size of the rolling window as a percentage of the time series' length.
#' @param burn_in Numeric value. If \code{method = "expanding"}, defines the number of data points to 'train' signals prior to EWS assessment.
#' @param threshold Numeric value of either \code{1} or \code{2}. Threshold*sigma is the value which, if the EWS strength exceeds it, constitutes a "signal".
#' @param tail.direction String of either \code{"one.tailed"} or \code{"two.tailed"}. \code{"one.tailed"} only indicates a warning if positive threshold sigma exceeded. \code{"two.tailed"} indicates a warning if positive OR negative threshold*sigma exceeded.
#'
#' @returns A list containing up to two objects: EWS outputs through time (\code{EWS}), and a plot object (\code{plot}) if \code{ggplotIt = TRUE}.
#' \item{EWS$raw}{Dataframe of EWS measurements through time. If \code{method = "expanding"}, then each metric has been rbound into a single dataframe and extra columns are provided indicating whether the threshold*sigma value has been exceeded (i.e. \code{"threshold.crossed"}). If \code{method = "expanding"}, then each metric's evolution over time is returned in individual columns.}
#' \item{EWS$dimred.ts}{Dataframe containing the dimension reduction time series}
#' \item{EWS$cor}{Dataframe of Kendall Tau correlations. Only returned if \code{method = "rolling"}.}
#' \item{plot}{Plot object. Only returned if \code{ggplotIt = "TRUE"}.}
#'
#' @examples
#' #Generate a random five species, non-transitioning
#' #ecosystem with 50 years of monitoring data.
#'
#' spp_data <- matrix(nrow = 50, ncol = 5)
#' spp_data <- sapply(1:dim(spp_data)[2], function(x){
#'  spp_data[,x] <- rnorm(50,mean=20,sd=5)})
#'  multi_spp_data <- as.data.frame(cbind("time" =
#'  seq(1:50), spp_data))
#'
#' #Rolling window early warning signal assessment of
#' #the ecosystem, without plotting.
#'
#' roll_ews <- multiEWS(
#'  data = multi_spp_data,
#'  method = "rolling",
#'  winsize = 50,
#'  ggplotIt =FALSE)
#'
#' #Expanding window early warning signal assessment of
#' #the ecosystem, with plotting.
#'
#' \dontrun{
#' exp_ews <- multiEWS(
#'  data = multi_spp_data,
#'  method = "expanding",
#'  burn_in = 10,
#'  ggplotIt = TRUE)
#'  }

#' @importFrom dplyr .data
#'
#' @export
multiEWS <- function(data, method = c("expanding","rolling"),
                                     ggplotIt = TRUE,winsize = 50,
                                     burn_in = 5, threshold = 2,
                                     tail.direction = "one.tailed"){

  method <- match.arg(method,choices = c("rolling","expanding"))
  pal <- c("#6886c4",
           "#bfbd3d",
           "#5d3099",
           "#69c756",
           "#e281fe",
           "#6ca181",
           "#76c3ef",
           "#d06329",
           "#90676f",
           "#ce5c6e",
           "#5d4216",
           "black")

  if(any(is.na(data))){
    stop('Data contains missing values. Interpolation of missing values is recommended')
  }

  if(method == "expanding"){

    bind.res <- wMAF(data = data,threshold = threshold, burn_in = burn_in,
                     tail.direction = tail.direction,
                      method = "expanding")

    bind.res$raw$str<-(bind.res$raw$metric.score-bind.res$raw$rolling.mean)/bind.res$raw$rolling.sd
    bind.res$raw<-as.data.frame(bind.res$raw)

    if(isTRUE(ggplotIt)){

      p<- ggplot(data = bind.res$raw, aes(x=.data$time,y=.data$str,col=.data$metric.code)) +
        geom_hline(yintercept = threshold, linetype="solid", color = "grey", size=1)+
        geom_line()+
        geom_point(aes(x=.data$time, y = .data$str,alpha = as.factor(.data$threshold.crossed))) +
        scale_alpha_manual(values = c(0,1),
                           breaks = c("0","1"),labels = c("Undetected","Detected"), name = "Multivariate EWS",
                           guide = guide_legend(order = 1, override.aes =
                                                  list(linetype = c(0),shape = c(16),col="black"))) +
        #scale_colour_manual(values = scales::hue_pal()(11),
        scale_colour_manual(values = pal,
                            guide = guide_legend(order = 2, override.aes =
                                                   list(linetype = rep(1,12),shape= NA))) +
        ggthemes::theme_clean() + xlab("Time point") + ylab("Strength of EWS") +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        labs(color='Multivariate EWS\nindicator strength') +
        theme(plot.margin = margin(c(10, 8, 5.5, 10)),
              legend.key.height = unit(0.3,"cm"),
              legend.key.width = unit(0.5,"cm"),
              legend.title = ggplot2::element_text(size = 10),
              legend.text = ggplot2::element_text(size=10),
              legend.background = ggplot2::element_blank(),
              legend.box.background = ggplot2::element_rect(colour = "black"))

        plot.dat<- bind.res$dimred.ts %>%
          dplyr::mutate(across(-.data$time,~scale(.))) %>%
          dplyr::filter(.data$time %in% bind.res$raw$time)%>%
          tidyr::pivot_longer(-.data$time,names_to = "dimred",values_to = "count.used")

        p2 <-ggplot(data = plot.dat, aes(x=.data$time, y=.data$count.used)) +
          geom_line(aes(col = .data$dimred))+
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          ylab("Scaled component value") +
          xlab("Time point")+
          ggthemes::theme_clean()+
          scale_colour_manual(values = c("#A1B4FE","#FFE7A1"),name = "Dimension\nreduction")+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm" ),
                legend.key.width = unit(0.5,"cm"),
                legend.title = ggplot2::element_text(size = 10),
                legend.text = ggplot2::element_text(size=10),
                legend.background = ggplot2::element_blank(),
                legend.box.background = ggplot2::element_rect(colour = "black"))

        final.p <- egg::ggarrange(p2,p,nrow = 2,heights = c(1, 1))
        print(final.p)

      }

    }

  if(method == "rolling"){

    bind.res <- wMAF(data = data,winsize = winsize,method = "rolling")


      if(isTRUE(ggplotIt)){

        plot.dat <- bind.res$raw %>%
          dplyr::mutate(time = as.numeric(.data$time))%>%
          dplyr::mutate(across(-c(.data$time),~scale(.x)))%>%
          tidyr::pivot_longer(-c(.data$time), names_to = "metric.code", values_to = "str")

        cor.dat <- bind.res$cor %>%
          tidyr::pivot_longer(everything(),names_to = "metric.code", values_to = "cor") %>%
          dplyr::rowwise()%>%
          dplyr::mutate(cor = paste(c("Tau:", round(.data$cor, digits = 3)),collapse = " ")) %>%
          dplyr::mutate(time = quantile(plot.dat$time,0.8), str =  max(plot.dat$str)*0.8)

        p<- ggplot(data = plot.dat, aes(x=.data$time,y=.data$str,group=.data$metric.code)) +
          geom_line(aes(col= .data$metric.code))+
          geom_text(data = cor.dat,aes(label = .data$cor),size = 3)+
          #scale_colour_manual(values = scales::hue_pal()(11),
          scale_colour_manual(values = pal,
                              guide = guide_legend(override.aes = list(linetype = rep(1,12),shape=NA))) +
          ggthemes::theme_clean() + xlab("Time point") + ylab("Scaled metric value") +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          labs(color='Multivariate EWS\nindicator trend') +
          facet_wrap(~metric.code,nrow=4)+
          theme(plot.margin = margin(c(10, 8, 5.5, 10)),
                legend.key.height = unit(0.3,"cm"),
                legend.key.width = unit(0.5,"cm"),
                legend.title = ggplot2::element_text(size = 10),
                legend.text = ggplot2::element_text(size=10),
                legend.background = ggplot2::element_blank(),
                legend.box.background = ggplot2::element_rect(colour = "black"))+
          guides(alpha = guide_legend(order = 1),
                 col = guide_legend(order = 2))

        p3 <-ggplot(data =  dplyr::filter(bind.res$dimred.ts,.data$time %in% plot.dat$time)%>%
                      tidyr::pivot_longer(-.data$time,names_to = "dimred",values_to = "count.used")
                    , aes(x=.data$time, y=.data$count.used)) +
          geom_line(aes(col = .data$dimred))+
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          ylab("Scaled component value") +
          xlab("Time point")+
          ggthemes::theme_clean()+
          scale_colour_manual(values = c("#A1B4FE","#FFE7A1"),name = "Dimension\nreduction")+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm" ),
                legend.key.width = unit(0.5,"cm"),
                legend.title = ggplot2::element_text(size = 10),
                legend.text = ggplot2::element_text(size=10),
                legend.background = ggplot2::element_blank(),
                legend.box.background = ggplot2::element_rect(colour = "black"))

        final.p <- egg::ggarrange(p3,p,nrow = 2,heights = c(1, 2))
        print(final.p)

    }

  }

  if(isTRUE(ggplotIt) & exists("final.p")){
    return(list("EWS" = bind.res,"plot" = final.p))
  }else{
    return(bind.res)
  }

}
