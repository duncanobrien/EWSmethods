#' Perform Early Warning Signal Assessment
#'
#' @param data A dataframe where first column is time (equally spaced) and second column is abundance
#' @param metrics String vector of early warning signal metrics. Options = c("cv", "acf", "ar1", "dr", "rr", "skew","kurt","mean.size","sd.size", "size.95","SD","trait")
#' @param trait A vector of trait values if desired
#' @param threshold = "1" or "2". Threshold*sigma is the value which if EWS strength exceeds constitutes a "signal"
#' @param plotIt = base R plot of EWS strength trends
#' @param ggplotIt = ggplot plot of EWS strength trends AND input abundance
#' @param tail.direction = "one.tailed" or "two.tailed"."one.tailed" only indicates a warning if positive threshold sigma exceeded, "two.tailed"  indicates a warning if positive OR negative threshold*sigma exceeded,
#' @param burn_in = number of data points to 'train' signals prior to EWS assessment
#' @param y_lab = if ggplotIt = TRUE, labels abundance y axis
#' @param trait_lab = if ggplotIt = TRUE, & trait populated, & "trait" supplied in metrics, labels abundance second abundance y axis (represents trait values through time)
#' @param trait_scale = scales trait y axis relative to abundance y axis
#' @param interpolate = TRUE interpolates missing values found within the abundance time series
#' @param data_source = NULL. A string to categorise result. Useful if performing multiple assessments iteratively
#' @param method = c("expanding","rolling")."expanding" calls composite, expanding window EWS assessment. "rolling" calls typical, rolling window EWS assessment

#' @returns If ggplotIt = F, returns just EWS output.If ggplotIt = T, returns EWS output and plot object

#' @export
univariate_EWS_wrapper <- function(data,metrics,trait = NULL, threshold = 2,plotIt = F, tail.direction = "one.tailed", burn_in = 5, ggplotIt = T,
                             y_lab = "Generic Indicator Name", trait_lab = "Generic Trait Name",
                             trait_scale = 100000, interpolate = F,method = c("expanding","rolling"),
                             winsize = 50){

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
      res[[i]]<-W_composite_ews(dat=data, indicators=sort(unlist(to.test[i])), weights=W, trait = trait, threshold=threshold, burn_in = burn_in, tail.direction = tail.direction, plotIt=plotIt, interpolate = interpolate)
    }

    bind.res<-data.table::rbindlist(res)
    bind.res$str<-(bind.res$metric.score-bind.res$rolling.mean)/bind.res$rolling.sd
    bind.res<-as.data.frame(bind.res)

    if(isTRUE(ggplotIt)){

      p<- ggplot(data = bind.res, aes(x=time,y=str,col=metric.code)) +
        geom_hline(yintercept = threshold, linetype="solid", color = "grey", size=1)+
        geom_line()+
        geom_point(aes(x=time, y = str,alpha = as.factor(threshold.crossed))) +
        scale_alpha_manual(values = c(0,1),
                           breaks = c("0","1"),labels = c("Undetected","Detected"), name = "EWS",
                           guide = guide_legend(order = 1, override.aes =
                                                  list(linetype = c(0),shape = c(16),col="black"))) +
        scale_colour_manual(values = scales::hue_pal()(length(to.test)),
                            guide = guide_legend(order = 2, override.aes =
                                                   list(linetype = rep(1,length(to.test)),shape= NA))) +
        ggthemes::theme_clean() + xlab("Date") + ylab("Strength of EWS") +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        labs(color='EWS Indicator\nStrength') +
        theme(plot.margin = margin(c(10, 8, 0, 10)),
              legend.key.height = unit(0.3,"cm"),
              legend.key.width = unit(0.5,"cm"),
              legend.title = element_text(size = 10),
              legend.text = element_text(size=10))

      if(is.null(trait)==F){
        plot.dat<-data.frame("timeseries"=bind.res$time, "value"=bind.res$count.used,"trait"=trait[burn_in:(nrow(data)-1)])

        p2 <-ggplot(data = plot.dat, aes(x=timeseries, y=count.used)) +
          aes(group=NA)+
          geom_line(aes(y=value),linetype=1) +
          geom_line(aes(y=(trait*trait_scale)),linetype=2, size = 0.4, alpha = 0.4,col = "blue") +
          geom_point(data =bind.res[bind.res$metric.code == bind.res$metric.code[length(bind.res$metric.code)],],
                     aes(x=time, y = max(count.used)*-0.1,col=metric.code,alpha = as.factor(threshold.crossed)),size = 3,pch= "|",col = "#53B400") +
          scale_alpha_manual(values = c(0,1),
                             breaks = c("0","1"),labels = c("Undetected","Detected"), name = "EWS",
                             guide = guide_legend(override.aes =
                                                    list(size = c(4),shape = c("|")))) +
          scale_y_continuous(y_lab,sec.axis = sec_axis(~./trait_scale, name = trait_lab))+
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          xlab("Date")+
          ylab(y_lab)+
          ggthemes::theme_clean()+
          annotate("label", x = quantile(plot.dat$time,0.75), y =max(plot.dat$count.used)*0.8 , label = paste(c("EWS indicator:",bind.res$metric.code[length(bind.res$metric.code)]),collapse = " "))+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm" ),
                legend.key.width = unit(0.5,"cm"),
                legend.title = element_text(size = 10),
                legend.text = element_text(size=10))

        final.p <- egg::ggarrange(p2,p,nrow = 2,heights = c(1, 1))
        print(final.p)

      }else if(is.null(trait)==T){
        plot.dat<-data.frame("time"=as.numeric(bind.res$time), "count.used"=bind.res$count.used)

        p4 <-ggplot(data = plot.dat, aes(x=time, y=count.used)) +
          aes(group=NA)+
          geom_line(col = "black")+
          geom_point(data =bind.res[bind.res$metric.code == bind.res$metric.code[length(bind.res$metric.code)],],
                     aes(x=time, y = max(count.used)*-0.1,col=metric.code,alpha = as.factor(threshold.crossed)),size = 3,pch= "|",col = "#53B400") +
          scale_alpha_manual(values = c(0,1),
                             breaks = c("0","1"),labels = c("Undetected","Detected"), name = "EWS",
                             guide = guide_legend(override.aes =
                                                    list(size = c(4),shape = c("|")))) +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          ylab(y_lab) +
          xlab("Date")+
          ggthemes::theme_clean()+
          annotate("label", x = quantile(plot.dat$time,0.75), y =max(plot.dat$count.used)*0.8 , label = paste(c("EWS indicator:",bind.res$metric.code[length(bind.res$metric.code)]),collapse = " "))+
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
      bind.res$raw$data_source <- data_source
      bind.res$raw<-as.data.frame(bind.res$raw)
      bind.res$cor <- bind.res$cor[,metrics]

      if(isTRUE(ggplotIt)){

        plot.dat <- bind.res$raw %>%
          dplyr:: mutate(across(-c(timeindex),~scale(.x)))%>%
          dplyr::left_join(data.frame("timeindex" = data[,1],"count.used" = data[,2]),by = "timeindex")%>%
          tidyr::pivot_longer(-c(timeindex,count.used), names_to = "metric.code", values_to = "str")

        cor.dat <- bind.res$cor %>%
          tidyr::pivot_longer(everything(),names_to = "metric.code", values_to = "cor") %>%
          dplyr::rowwise()%>%
          dplyr::mutate(cor = paste(c("Tau:", round(cor, digits = 3)),collapse = " ")) %>%
          dplyr::mutate(timeindex = quantile(plot.dat$timeindex,0.8), str =  max(plot.dat$str)*0.8)

        p<- ggplot(data = plot.dat, aes(x=timeindex,y=str,group=metric.code)) +
          geom_line(aes(col= metric.code))+
          geom_text(data = cor.dat,aes(label = cor),size = 3)+
          scale_colour_manual(values = scales::hue_pal()(length(metrics)),guide = guide_legend(override.aes = list(linetype = rep(1,7),shape=NA))) +
          ggthemes::theme_clean() + xlab("Date") + ylab("Scaled metric value") +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          labs(color='EWS Indicator\nTrend') +
          facet_wrap(~metric.code,nrow=4)+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm"),
                legend.key.width = unit(0.5,"cm"),
                legend.title = element_text(size = 10),
                legend.text = element_text(size=10))+
          guides(alpha = guide_legend(order = 1),
                 col = guide_legend(order = 2))

        p2 <-ggplot(data = plot.dat, aes(x=timeindex, y=count.used)) +
          aes(group=NA)+
          geom_line(col = "black")+
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          ylab(y_lab) +
          xlab("Date")+
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
