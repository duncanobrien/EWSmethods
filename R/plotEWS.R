#' Plot an EWSmethods object
#'
#' A function for visualising the output of \code{uniEWS} or \code{multiEWS} using ggplot2 inspired figures.
#'
#' @param x An EWSmethods object created by \code{uniEWS} or \code{multiEWS}
#' @param ... Additional arguments to pass to the plotting functions. Options shared below.
#' * \code{y_lab} String label. Labels the abundance y axis.
#' * \code{trait_lab} String label. If \code{trait} argument populated in \code{uniEWS} or \code{multiEWS}, & \code{"trait"} supplied in metrics, labels the right side y axis which represents trait values through time.
#' * \code{trait_scale} Numeric value. Scales trait y axis relative to abundance y axis.
#'
#' @returns A ggplot2 object.
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
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr .data
#'
#' @examples
#' data(simTransComms)
#'
#' #Subset the third community prior to the transition
#'
#' pre_simTransComms <- subset(simTransComms$community3,time < inflection_pt)
#'
#' #Perform multivariate EWS assessments
#' roll_ews <- multiEWS(
#'  data = pre_simTransComms[,2:7],
#'  method = "rolling",
#'  winsize = 50)
#'
#'  #Plot outcome
#' \dontrun{
#'  plot(roll_ews)
#' }
#'
#' #Perform univariate EWS assessments on
#' #simulated data with traits
#'
#' abundance_data <- data.frame(time = seq(1:50),
#'  abundance = rnorm(50,mean = 20),
#'  trait = rnorm(50,mean=1,sd=0.5))
#'
#' trait_ews <- uniEWS(
#'  data = abundance_data[,1:2],
#'  metrics = c("ar1","SD","trait"),
#'  method = "expanding",
#'  trait = abundance_data[,3],
#'  burn_in = 10)
#'
#' #Plot outcome
#' \dontrun{
#'  plot(trait_ews, y_lab = "Abundance",
#'  trait_lab = "Trait value",
#'  trait_scale = 10)
#' }
#'
#' @export

plot.EWSmethods <- function(x,...){

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
           "black",
           "grey")

  plot_labels <- prepare_plot_labs(...)

  if("uniEWS" %in% class(x)){
    if("rollEWS" %in% class(x)){
      metrics <- colnames(x$EWS$raw)[!(colnames(x$EWS$raw) %in% c("time","count.used"))]

      plot.dat <- x$EWS$raw %>%
        dplyr::mutate(across(-c(.data$time,.data$count.used),~scale(.x)))%>%
        tidyr::pivot_longer(-c(.data$time,.data$count.used), names_to = "metric.code", values_to = "str")

      if(length(metrics) == 1){
        cor.dat <- data.frame("metric.code" = metrics,
                              "cor" = paste(c("Tau:",round(x$EWS$cor, digits = 3)),collapse = " ")) %>%
          dplyr::mutate(time = quantile(plot.dat$time,0.85), str =  max(plot.dat$str)*0.8)

      }else{
        cor.dat <- x$EWS$cor %>%
          tidyr::pivot_longer(everything(),names_to = "metric.code", values_to = "cor") %>%
          dplyr::rowwise()%>%
          dplyr::mutate(cor = paste(c("Tau:", round(.data$cor, digits = 3)),collapse = " ")) %>%
          dplyr::mutate(time = quantile(plot.dat$time,0.8), str =  max(plot.dat$str)*0.8)
      }

      p <- ggplot(data = plot.dat, aes(x=.data$time,y=.data$str,group=.data$metric.code)) +
        geom_line(aes(col= .data$metric.code))+
        geom_text(data = cor.dat,aes(label = .data$cor),size = 3, hjust = 0.75)+
        scale_colour_manual(values = pal[1:length(metrics)],guide = guide_legend(override.aes = list(linetype = rep(1,7),shape=NA))) +
        ggthemes::theme_clean() + xlab("Time point") + ylab("Scaled metric value") +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        labs(color='EWS indicator\ntrend') +
        facet_wrap(~.data$metric.code,nrow=4)+
        theme(plot.margin = margin(c(10, 8, 5.5, 10)),
              legend.key.height = unit(0.3,"cm"),
              legend.key.width = unit(0.5,"cm"),
              legend.title = ggplot2::element_text(size = 10),
              legend.text = ggplot2::element_text(size=10),
              legend.background = ggplot2::element_blank(),
              legend.box.background = ggplot2::element_rect(colour = "black"))+
        guides(alpha = guide_legend(order = 1),
               col = guide_legend(order = 2))

      p2 <- ggplot(data = plot.dat, aes(x=.data$time, y=.data$count.used)) +
        aes(group=NA)+
        geom_line(col = "black")+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        ylab(plot_labels$y_lab) +
        xlab("Time point")+
        ggthemes::theme_clean()+
        theme(plot.margin = margin(c(10, 8, 0, 10)),
              legend.key.height = unit(0.3,"cm" ),
              legend.key.width = unit(0.5,"cm"),
              legend.title = ggplot2::element_text(size = 10),
              legend.text = ggplot2::element_text(size=10),
              legend.background = ggplot2::element_blank(),
              legend.box.background = ggplot2::element_rect(colour = "black"))

      final.p <- egg::ggarrange(p2,p,nrow = 2,heights = c(1, 2), draw = F)

    }else if("expEWS" %in% class(x)){

      metrics <- unique(x$EWS$metric.code)
      tail.direction <- x$tail.direction

      p <- ggplot(data = x$EWS, aes(x=.data$time,y=.data$str,col=.data$metric.code)) +
        geom_hline(yintercept = unique(x$threshold), linetype="solid", color = "grey", linewidth=1)+
        geom_line()+
        geom_point(aes(x=.data$time, y = .data$str,alpha = as.factor(.data$threshold.crossed))) +
        geom_line(aes(alpha = "0"))+
        scale_alpha_manual(values = c(0,1),
                           breaks = c("0","1"),labels = c("Undetected","Detected"), name = "EWS",
                           guide = guide_legend(order = 1, override.aes =
                                                  list(linetype = c(1,0),shape = c(NA,16),alpha = c(1,1),col="black"))) +
        scale_colour_manual(values = pal[1:length(metrics)],
                            guide = guide_legend(order = 2, override.aes =
                                                   list(linetype = rep(1,length(metrics)),shape= NA))) +
        ggthemes::theme_clean() + xlab("Time point") + ylab("Strength of EWS") +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        labs(color='EWS indicator\nstrength') +
        theme(plot.margin = margin(c(10, 8, 5.5, 10)),
              legend.key.height = unit(0.3,"cm"),
              legend.key.width = unit(0.5,"cm"),
              legend.title = ggplot2::element_text(size = 10),
              legend.text = ggplot2::element_text(size=10),
              legend.background = ggplot2::element_blank(),
              legend.box.background = ggplot2::element_rect(colour = "black"))

      if(tail.direction == "two.tailed"){
        p <- p + geom_hline(yintercept = -x$threshold, linetype="solid", color = "grey", size=1)
      }

      if("trait" %in% metrics){
        plot.dat <- data.frame("time"=x$EWS$time, "count.used"=x$EWS$count.used, "trait" = x$EWS$trait)

        ews.data <- x$EWS %>%
          dplyr::mutate(min = min(.data$count.used)*0.75) %>%
          dplyr::filter(.data$metric.code == .data$metric.code[length(.data$metric.code)] & .data$threshold.crossed == 1)

        p2 <- ggplot(data = plot.dat, aes(x=.data$time, y=.data$count.used)) +
          aes(group=NA)+
          geom_line(aes(y=.data$count.used, linetype = "Count")) +
          geom_line(aes(y=(.data$trait*plot_labels$trait_scale), linetype = "Trait"), size = 0.4, alpha = 0.4,col = "blue") +
          geom_point(data = ews.data,
                     aes(x=.data$time, y = .data$min,col=.data$metric.code,alpha = as.factor(.data$threshold.crossed)),size = 3,pch= "|",col = "#5d3099") +
          scale_alpha_manual(values = c(1),
                             breaks = c(0,"1"),labels = c(NA,"Detected"), name = "EWS",
                             guide = guide_legend(override.aes =
                                                    list(size = c(4),shape = c("|")))) +
          scale_linetype_manual(values = c(1,2),breaks = c("Count","Trait"), name = "Time series",
                                guide = guide_legend(override.aes =
                                                       list(col = c("black","blue"),alpha = c(1,0.4))))+
          scale_y_continuous(plot_labels$y_lab,sec.axis = sec_axis(~./plot_labels$trait_scale, name = plot_labels$trait_lab))+
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          xlab("Time point")+
          ylab(plot_labels$y_lab)+
          ggthemes::theme_clean()+
          annotate("label", size = 2, x = quantile(plot.dat$time,0.90), y =max(plot.dat$count.used)*0.95 , label = paste(c("EWS indicator:",x$EWS$metric.code[length(x$EWS$metric.code)]),collapse = " "),hjust = 0.75)+
          guides(alpha = guide_legend(order = 1))+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm" ),
                legend.key.width = unit(0.5,"cm"),
                legend.title = ggplot2::element_text(size = 10),
                legend.text = ggplot2::element_text(size=10),
                legend.background = ggplot2::element_blank(),
                legend.box.background = ggplot2::element_rect(colour = "black"))

        final.p <- egg::ggarrange(p2,p,nrow = 2,heights = c(1, 1), draw = F)

      }else if(!("trait" %in% metrics)){
        plot.dat<-data.frame("time"=as.numeric(x$EWS$time), "count.used"=x$EWS$count.used)

        ews.data <- x$EWS %>%
          dplyr::mutate(min = min(.data$count.used)*0.75) %>%
          dplyr::filter(.data$metric.code == .data$metric.code[length(.data$metric.code)] & .data$threshold.crossed == 1)

        p4 <- ggplot(data = plot.dat, aes(x=.data$time, y=.data$count.used)) +
          aes(group=NA)+
          geom_line(col = "black")+
          geom_point(data = ews.data,
                     aes(x=.data$time, y = .data$min,col=.data$metric.code,alpha = as.factor(.data$threshold.crossed)),size = 3,pch= "|",col = "#5d3099") +
          scale_alpha_manual(values = c(1),
                             breaks = c(0,"1"),labels = c(NA,"Detected"), name = "EWS",
                             guide = guide_legend(override.aes =
                                                    list(size = c(4),shape = c("|")))) +
          scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
          ylab(plot_labels$y_lab) +
          xlab("Time point")+
          ggthemes::theme_clean()+
          annotate("label", size = 2, x = quantile(plot.dat$time,0.90), y =max(plot.dat$count.used)*0.95 , label = paste(c("EWS indicator:",x$EWS$metric.code[length(x$EWS$metric.code)]),collapse = " "),hjust=0.75)+
          theme(plot.margin = margin(c(10, 8, 0, 10)),
                legend.key.height = unit(0.3,"cm" ),
                legend.key.width = unit(0.5,"cm"),
                legend.title = ggplot2::element_text(size = 10),
                legend.text = ggplot2::element_text(size=10),
                legend.background = ggplot2::element_blank(),
                legend.box.background = ggplot2::element_rect(colour = "black"))

        final.p <- egg::ggarrange(p4,p,nrow = 2,heights = c(1, 1), draw = F)
      }
    }
  }

  if("multiEWS" %in% class(x)){
    if("rollEWS" %in% class(x)){

      metrics <- colnames(x$EWS$raw)[!(colnames(x$EWS$raw) %in% c("time"))]

      plot.dat <- x$EWS$raw %>%
        dplyr::mutate(time = as.numeric(.data$time))%>%
        dplyr::mutate(across(-c(.data$time),~scale(.x)))%>%
        tidyr::pivot_longer(-c(.data$time), names_to = "metric.code", values_to = "str")

      cor.dat <- x$EWS$cor %>%
        tidyr::pivot_longer(everything(),names_to = "metric.code", values_to = "cor") %>%
        dplyr::rowwise()%>%
        dplyr::mutate(cor = paste(c("Tau:", round(.data$cor, digits = 3)),collapse = " ")) %>%
        dplyr::mutate(time = quantile(plot.dat$time,0.85), str =  max(plot.dat$str)*0.8)

      p <- ggplot(data = plot.dat, aes(x=.data$time,y=.data$str,group=.data$metric.code)) +
        geom_line(aes(col= .data$metric.code))+
        geom_text(data = cor.dat,aes(label = .data$cor),size = 3,hjust=0.75)+
        scale_colour_manual(values = pal[1:length(metrics)],
                            guide = guide_legend(override.aes = list(linetype = rep(1,length(metrics)),shape=NA))) +
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

      p3 <- ggplot(data =  dplyr::filter(x$EWS$dimred.ts,.data$time %in% plot.dat$time)%>%
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

      final.p <- egg::ggarrange(p3,p,nrow = 2,heights = c(1, 2), draw = F)

    }else if("expEWS" %in% class(x)){

      metrics <- unique(x$EWS$raw$metric.code)

      p<- ggplot(data = x$EWS$raw, aes(x=.data$time,y=.data$str,col=.data$metric.code)) +
        geom_hline(yintercept = x$threshold, linetype="solid", color = "grey", size=1)+
        geom_line()+
        geom_point(aes(x=.data$time, y = .data$str,alpha = as.factor(.data$threshold.crossed))) +
        geom_line(aes(alpha = "0"))+
        scale_alpha_manual(values = c(0,1),
                           breaks = c("0","1"),labels = c("Undetected","Detected"), name = "EWS",
                           guide = guide_legend(order = 1, override.aes =
                                                  list(linetype = c(1,0),shape = c(NA,16),alpha = c(1,1),col="black"))) +
        scale_colour_manual(values = pal[1:length(metrics)],
                            guide = guide_legend(order = 2, override.aes =
                                                   list(linetype = rep(1,length(metrics)),shape= NA))) +
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

      plot.dat <- x$EWS$dimred.ts %>%
        dplyr::mutate(across(-.data$time,~scale(.))) %>%
        dplyr::filter(.data$time %in% x$EWS$raw$time)%>%
        tidyr::pivot_longer(-.data$time,names_to = "dimred",values_to = "count.used")

      p2 <- ggplot(data = plot.dat, aes(x=.data$time, y=.data$count.used)) +
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

      final.p <- egg::ggarrange(p2,p,nrow = 2,heights = c(1, 1),draw = F)
    }
  }
  final.p
}


#' Prepare Generic Labels
#'
#' An internal function for preparing plot labels readable by \code{...} in \code{plot.EWSmethods}.
#'
#' @param y_lab String to label raw data
#' @param trait_lab String to label trait data
#' @param trait_scale Numeric scalar of trait data relative to raw data
#' @keywords internal
#' @noRd

prepare_plot_labs <- function(y_lab = "Generic indicator name", trait_lab = "Generic trait name", trait_scale = 1000){

  return(list("y_lab" = y_lab, "trait_lab" = trait_lab, "trait_scale" = trait_scale))
}
