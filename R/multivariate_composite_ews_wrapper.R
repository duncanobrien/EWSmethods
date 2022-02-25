#' Perform Multivariate Early Warning Signal Assessment
#'
#' A single function for performing early warning signal (EWS) assessment on multivariate time series where multiple time series have been measureed Both methods of EWS assessment can be performed (rolling vs expanding windows) with the assessments returned as a dataframe and/or with a standardised ggplot-based figure.
#'
#' @param data A dataframe where first column is time (equally spaced) and all other columns are individual time series. These could be different species, populations or measurements.
#' @param method Single string of either \code{"expanding"} or \code{"rolling"}. \code{"expanding"} calls composite, expanding window EWS assessment. \code{"rolling"} calls typical, rolling window EWS assessment.
#' @param interpolate Boolean. If \code{TRUE}, linearly interpolates missing values found within the time series.
#' @param ggplotIt Boolean. If \code{TRUE}, returns a ggplot plot of EWS strength trends AND the estimated dimension reduction.
#' @param winsize Numeric value. If method = \code{"rolling"}, defines the window size of the rolling window as a percentage of the time series' length.
#' @param threshold Numeric value of either \code{1} or \code{2}. Threshold*sigma is the value which, if the EWS strength exceeds it, constitutes a "signal".
#' @param tail.direction String of either \code{"one.tailed"} or \code{"two.tailed"}. \code{"one.tailed"} only indicates a warning if positive threshold sigma exceeded. \code{"two.tailed"} indicates a warning if positive OR negative threshold*sigma exceeded.
#' @param burn_in Numeric value. The number of data points to 'train' signals prior to EWS assessment.
#'
#' @examples
#' #Generate random five species non-transitioning ecosystem with 50 years of monitoring data.
#' spp_data <- matrix(nrow = 50, ncol = 5)
#' spp_data <- sapply(1:dim(spp_data)[2], function(x){spp_data[,x] <- rnorm(50,mean=20,sd=5)})
#' multi_spp_data <- as.data.frame(cbind("time" = seq(1:50),spp_data))
#'
#' #Rolling window early warning signal assessment of the ecosystem, without plotting.
#' multivariate_EWS_wrapper(data = multi_spp_data, method = "rolling", winsize = 50, ggplotIt =FALSE)
#'
#' #Expanding window early warning signal assessment of the ecosystem, with plotting.
#' \dontrun{multivariate_EWS_wrapper(data = multi_spp_data, method = "expanding", burn_in = 10, ggplotIt =TRUE)}

#' @importFrom dplyr .data
#'
#' @export
multivariate_EWS_wrapper <- function(data, method = c("expanding","rolling"),
                                     interpolate = F, ggplotIt = T,
                                     winsize = 50, threshold = 2,
                                     tail.direction = "one.tailed", burn_in = 5){

  method <- match.arg(method,choices = c("rolling","expanding"))

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
        scale_colour_manual(values = scales::hue_pal()(11),
                            guide = guide_legend(order = 2, override.aes =
                                                   list(linetype = rep(1,11),shape= NA))) +
        ggthemes::theme_clean() + xlab("Date") + ylab("Strength of EWS") +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        labs(color='Multivariate EWS\nIndicator Strength') +
        theme(plot.margin = margin(c(10, 8, 0, 10)),
              legend.key.height = unit(0.3,"cm"),
              legend.key.width = unit(0.5,"cm"),
              legend.title = element_text(size = 10),
              legend.text = element_text(size=10))

        plot.dat<- bind.res$dimred.ts %>%
          dplyr::mutate(across(-.data$time,~scale(.))) %>%
          dplyr::filter(.data$time %in% bind.res$raw$time)%>%
          tidyr::pivot_longer(-.data$time,names_to = "dimred",values_to = "count.used")

        p2 <-ggplot(data = plot.dat, aes(x=.data$time, y=.data$count.used)) +
          geom_line(aes(col = .data$dimred))+
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          ylab("Scaled component value") +
          xlab("Date")+
          ggthemes::theme_clean()+
          scale_colour_manual(values = c("#A1B4FE","#FFE7A1"),name = "Dimension\nreduction")+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm" ),
                legend.key.width = unit(0.5,"cm"),
                legend.title = element_text(size = 10),
                legend.text = element_text(size=10))

        final.p <- egg::ggarrange(p2,p,nrow = 2,heights = c(1, 1))
        print(final.p)

      }

    }

  if(method == "rolling"){

    bind.res <- wMAF(data = data,winsize = winsize,method = "rolling")


      if(isTRUE(ggplotIt)){

        plot.dat <- bind.res$raw %>%
          dplyr::mutate(time = as.numeric(.data$time))%>%
          dplyr:: mutate(across(-c(.data$time),~scale(.x)))%>%
          tidyr::pivot_longer(-c(.data$time), names_to = "metric.code", values_to = "str")

        cor.dat <- bind.res$cor %>%
          tidyr::pivot_longer(everything(),names_to = "metric.code", values_to = "cor") %>%
          dplyr::rowwise()%>%
          dplyr::mutate(cor = paste(c("Tau:", round(.data$cor, digits = 3)),collapse = " ")) %>%
          dplyr::mutate(time = quantile(plot.dat$time,0.8), str =  max(plot.dat$str)*0.8)

        p<- ggplot(data = plot.dat, aes(x=.data$time,y=.data$str,group=.data$metric.code)) +
          geom_line(aes(col= .data$metric.code))+
          geom_text(data = cor.dat,aes(label = .data$cor),size = 3)+
          scale_colour_manual(values = scales::hue_pal()(11),
                              guide = guide_legend(override.aes = list(linetype = rep(1,11),shape=NA))) +
          ggthemes::theme_clean() + xlab("Date") + ylab("Scaled metric value") +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          labs(color='Multivariate EWS\nIndicator Trend') +
          facet_wrap(~metric.code,nrow=4)+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm"),
                legend.key.width = unit(0.5,"cm"),
                legend.title = element_text(size = 10),
                legend.text = element_text(size=10))+
          guides(alpha = guide_legend(order = 1),
                 col = guide_legend(order = 2))

        p3 <-ggplot(data =  dplyr::filter(bind.res$dimred.ts,.data$time %in% plot.dat$time)%>%
                      tidyr::pivot_longer(-.data$time,names_to = "dimred",values_to = "count.used")
                    , aes(x=.data$time, y=.data$count.used)) +
          geom_line(aes(col = .data$dimred))+
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          ylab("Scaled component value") +
          xlab("Date")+
          ggthemes::theme_clean()+
          scale_colour_manual(values = c("#A1B4FE","#FFE7A1"),name = "Dimension\nreduction")+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm" ),
                legend.key.width = unit(0.5,"cm"),
                legend.title = element_text(size = 10),
                legend.text = element_text(size=10))

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
