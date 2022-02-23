#' Perform Multivariate Early Warning Signal Assessment

#' @param data A dataframe where first column is time (equally spaced) and second column is abundance.
#' @param metrics String vector of early warning signal metrics. Options = c("cv", "acf", "ar1", "dr", "rr", "skew","kurt","mean.size","sd.size", "size.95","SD","trait").
#' @param trait A vector of trait values if desired.
#' @param threshold = "1" or "2". Threshold*sigma is the value which if EWS strength exceeds constitutes a "signal".
#' @param plotIt = base R plot of EWS strength trends.
#' @param ggplotIt = ggplot plot of EWS strength trends AND input abundance.
#' @param tail.direction = "one.tailed" or "two.tailed"."one.tailed" only indicates a warning if positive threshold sigma exceeded, "two.tailed"  indicates a warning if positive OR negative threshold*sigma exceeded.
#' @param burn_in = number of data points to 'train' signals prior to EWS assessment.
#' @param y_lab = if ggplotIt = TRUE, labels abundance y axis.
#' @param trait_lab = if ggplotIt = TRUE, & trait populated, & "trait" supplied in metrics, labels abundance second abundance y axis (represents trait values through time).
#' @param trait_scale = scales trait y axis relative to abundance y axis.
#' @param interpolate = TRUE interpolates missing values found within the abundance time series.
#' @param data_source = NULL. A string to categorise result. Useful if performing multiple assessments iteratively.
#' @param method = c("expanding","rolling")."expanding" calls composite, expanding window EWS assessment. "rolling" calls typical, rolling window EWS assessment.

#' @returns If ggplotIt = F, returns just EWS output.If ggplotIt = T, returns EWS output and plot object

#' @export
multivariate_EWS_wrapper <- function(data,metrics,threshold = 2,tail.direction = "one.tailed", burn_in = 5,
                                   ggplotIt = T,y_lab = "Generic Indicator Name",
                             interpolate = F,method = c("expanding","rolling"),
                             winsize = 50){

  method <- match.arg(method,choices = c("rolling","expanding"))

  if(method == "expanding"){

    bind.res <- wMAF(data = data,threshold = threshold, burn_in = burn_in,
                     tail.direction = tail.direction,
                      method = "expanding")

    bind.res$raw$str<-(bind.res$raw$metric.score-bind.res$raw$rolling.mean)/bind.res$raw$rolling.sd
    bind.res$raw<-as.data.frame(bind.res$raw)

    if(isTRUE(ggplotIt)){

      p<- ggplot(data = bind.res$raw, aes(x=time,y=str,col=metric.code)) +
        geom_hline(yintercept = threshold, linetype="solid", color = "grey", size=1)+
        geom_line()+
        geom_point(aes(x=time, y = str,alpha = as.factor(threshold.crossed))) +
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
          mutate(across(-time,~scale(.))) %>%
          dplyr::filter(time %in% bind.res$raw$time)%>%
          tidyr::pivot_longer(-time,names_to = "dimred",values_to = "count.used")

        p2 <-ggplot(data = plot.dat, aes(x=time, y=count.used)) +
          geom_line(aes(col = dimred))+
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
          mutate(time = as.numeric(time))%>%
          dplyr:: mutate(across(-c(time),~scale(.x)))%>%
          tidyr::pivot_longer(-c(time), names_to = "metric.code", values_to = "str")

        cor.dat <- bind.res$cor %>%
          tidyr::pivot_longer(everything(),names_to = "metric.code", values_to = "cor") %>%
          dplyr::rowwise()%>%
          dplyr::mutate(cor = paste(c("Tau:", round(cor, digits = 3)),collapse = " ")) %>%
          dplyr::mutate(time = quantile(plot.dat$time,0.8), str =  max(plot.dat$str)*0.8)

        p<- ggplot(data = plot.dat, aes(x=time,y=str,group=metric.code)) +
          geom_line(aes(col= metric.code))+
          geom_text(data = cor.dat,aes(label = cor),size = 3)+
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

        p3 <-ggplot(data =  filter(bind.res$dimred.ts,time %in% plot.dat$time)%>%
                      tidyr::pivot_longer(-time,names_to = "dimred",values_to = "count.used")
                    , aes(x=time, y=count.used)) +
          geom_line(aes(col = dimred))+
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
