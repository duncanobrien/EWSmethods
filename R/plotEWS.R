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
        dplyr::mutate(across(-c("time","count.used"),~scale(.x)))%>%
        tidyr::pivot_longer(-c("time","count.used"), names_to = "metric.code", values_to = "str")

      if(length(metrics) == 1){
        cor.dat <- data.frame("metric.code" = metrics,
                              "cor" = paste(c("tau:",round(x$EWS$cor, digits = 3)),collapse = " ")) %>%
          dplyr::mutate(time = quantile(plot.dat$time,0.85), str =  max(plot.dat$str)*0.8)

      }else{
        cor.dat <- x$EWS$cor[1,] %>%
          tidyr::pivot_longer(everything(),names_to = "metric.code", values_to = "cor") %>%
          dplyr::rowwise()%>%
          dplyr::mutate(cor = paste(c("tau:", round(.data$cor, digits = 3)),collapse = " ")) %>%
          dplyr::mutate(time = quantile(plot.dat$time,0.8), str =  max(plot.dat$str)*0.8)
      }

      p <- ggplot(data = plot.dat, aes(x=.data$time,y=.data$str,group=.data$metric.code)) +
        geom_line(aes(col= .data$metric.code))+
        geom_text(data = cor.dat,aes(label = .data$cor),size = 3, hjust = 0.75)+
        scale_colour_manual(values = pal[1:length(metrics)],guide = guide_legend(override.aes = list(linetype = rep(1,7),shape=NA))) +
        xlab("Time point") + ylab("Scaled metric value") +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        labs(color='EWS indicator\ntrend') +
        facet_wrap(~.data$metric.code,nrow=4)+
        theme_clean()+
        theme(plot.margin = margin(c(10, 8, 5.5, 10)))+
        guides(alpha = guide_legend(order = 1),
               col = guide_legend(order = 2))

      p2 <- ggplot(data = plot.dat, aes(x=.data$time, y=.data$count.used)) +
        aes(group=NA)+
        geom_line(col = "black")+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        ylab(plot_labels$y_lab) +
        xlab("Time point")+
        theme_clean() +
        theme(plot.margin = margin(c(10, 8, 0, 10)))

      final.p <- egg::ggarrange(p2,p,nrow = 2,heights = c(1, 2), draw = F)

    }else if("expEWS" %in% class(x)){

      metrics <- unique(x$EWS$metric.code)
      tail.direction <- x$tail.direction

      p <- ggplot(data = tidyr::drop_na(x$EWS,"str"), aes(x=.data$time,y=.data$str,col=.data$metric.code)) +
        geom_hline(yintercept = unique(x$threshold), linetype="solid", color = "grey", linewidth=1)+
        geom_line()+
        geom_point(aes(x=.data$time, y = .data$str,alpha = as.factor(.data$threshold.crossed))) +
        geom_point(aes(x=.data$time, y = .data$str,alpha = factor(.data$threshold.crossed,levels = c(0,1)))) +
        geom_line(aes(alpha = "0"))+
        scale_alpha_manual(values = c(0,1),
                           breaks = c("0","1"),labels = c("Undetected","Detected"), name = "EWS",
                           guide = guide_legend(order = 1, override.aes =
                                                  list(linetype = c(1,0),shape = c(NA,16),alpha = c(1,1),col="black")),
                           drop=FALSE) +
        scale_colour_manual(values = pal[1:length(metrics)],
                            guide = guide_legend(order = 2, override.aes =
                                                   list(linetype = rep(1,length(metrics)),shape= NA))) +
        xlab("Time point") + ylab("Strength of EWS") +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        labs(color='EWS indicator\nstrength') +
        theme_clean() +
        theme(plot.margin = margin(c(10, 8, 5.5, 10)))

      if(tail.direction == "two.tailed"){
        p <- p + geom_hline(yintercept = -x$threshold, linetype="solid", color = "grey", linewidth=1)
      }

      if("trait" %in% metrics){
        plot.dat <- tidyr::drop_na(x$EWS,"str") %>%
          dplyr::select("time", "count.used","trait")

        ews.data <- x$EWS %>%
          dplyr::mutate(min = min(.data$count.used)*0.75) %>%
          dplyr::filter(.data$metric.code == .data$metric.code[length(.data$metric.code)] & .data$threshold.crossed == 1)

        p2 <- ggplot(data = plot.dat, aes(x=.data$time, y=.data$count.used)) +
          aes(group=NA)+
          geom_line(aes(y=.data$count.used, linetype = "Count")) +
          geom_line(aes(y=(.data$trait*plot_labels$trait_scale), linetype = "Trait"), linewidth = 0.4, alpha = 0.4,col = "blue") +
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
          annotate("label", size = 2, x = quantile(plot.dat$time,0.90), y =max(plot.dat$count.used)*0.95 , label = paste(c("EWS indicator:",x$EWS$metric.code[length(x$EWS$metric.code)]),collapse = " "),hjust = 0.75)+
          guides(alpha = guide_legend(order = 1))+
          theme_clean()+
          theme(plot.margin = margin(c(10, 8, 0, 10)))

        final.p <- egg::ggarrange(p2,p,nrow = 2,heights = c(1, 1), draw = F)

      }else if(!("trait" %in% metrics)){
        plot.dat <- tidyr::drop_na(x$EWS,"str") %>%
          dplyr::select("time", "count.used")

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
          annotate("label", size = 2, x = quantile(plot.dat$time,0.90), y =max(plot.dat$count.used)*0.95 , label = paste(c("EWS indicator:",x$EWS$metric.code[length(x$EWS$metric.code)]),collapse = " "),hjust=0.75)+
          theme_clean()+
          theme(plot.margin = margin(c(10, 8, 0, 10)))

        final.p <- egg::ggarrange(p4,p,nrow = 2,heights = c(1, 1), draw = F)
      }
    }
  }

  if("multiEWS" %in% class(x)){
    if("rollEWS" %in% class(x)){

      metrics <- colnames(x$EWS$raw)[!(colnames(x$EWS$raw) %in% c("time"))]

      plot.dat <- x$EWS$raw %>%
        dplyr::mutate(time = as.numeric(.data$time))%>%
        dplyr::mutate(across(-c("time"),~scale(.x)))%>%
        tidyr::pivot_longer(-c("time"), names_to = "metric.code", values_to = "str")

      if(length(metrics) == 1){
        cor.dat <- data.frame("metric.code" = metrics,
                              "cor" = paste(c("tau:",round(x$EWS$cor[1,], digits = 3)),collapse = " ")) %>%
          dplyr::mutate(time = quantile(plot.dat$time,0.85), str =  max(plot.dat$str)*0.8)

      }else{
        cor.dat <- x$EWS$cor[1,] %>%
          tidyr::pivot_longer(everything(),names_to = "metric.code", values_to = "cor") %>%
          dplyr::rowwise()%>%
          dplyr::mutate(cor = paste(c("tau:", round(.data$cor, digits = 3)),collapse = " ")) %>%
          dplyr::mutate(time = quantile(plot.dat$time,0.85), str =  max(plot.dat$str)*0.8)
      }

      p <- ggplot(data = plot.dat, aes(x=.data$time,y=.data$str,group=.data$metric.code)) +
        geom_line(aes(col= .data$metric.code))+
        geom_text(data = cor.dat,aes(label = .data$cor),size = 3,hjust=0.75)+
        scale_colour_manual(values = pal[1:length(metrics)],
                            guide = guide_legend(override.aes = list(linetype = rep(1,length(metrics)),shape=NA))) +
        xlab("Time point") + ylab("Scaled metric value") +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        labs(color='Multivariate EWS\nindicator trend') +
        facet_wrap(~metric.code,nrow=4)+
        theme_clean()+
        theme(plot.margin = margin(c(10, 8, 5.5, 10)))+
        guides(alpha = guide_legend(order = 1),
               col = guide_legend(order = 2))

      p3 <- ggplot(data =  dplyr::filter(x$EWS$dimred.ts,.data$time %in% plot.dat$time)%>%
                    tidyr::pivot_longer(-.data$time,names_to = "dimred",values_to = "count.used")
                  , aes(x=.data$time, y=.data$count.used)) +
        geom_line(aes(col = .data$dimred))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        ylab("Scaled component value") +
        xlab("Time point")+
        scale_colour_manual(values = c("#A1B4FE","#FFE7A1"),name = "Dimension\nreduction")+
        theme_clean()+
        theme(plot.margin = margin(c(10, 8, 0, 10)))

      final.p <- egg::ggarrange(p3,p,nrow = 2,heights = c(1, 2), draw = F)

    }else if("expEWS" %in% class(x)){

      metrics <- unique(x$EWS$raw$metric.code)

      p<- ggplot(data = tidyr::drop_na(x$EWS$raw,"str"), aes(x=.data$time,y=.data$str,col=.data$metric.code)) +
        geom_hline(yintercept = x$threshold, linetype="solid", color = "grey", linewidth=1)+
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
        xlab("Time point") + ylab("Strength of EWS") +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        labs(color='Multivariate EWS\nindicator strength') +
        theme_clean()+
        theme(plot.margin = margin(c(10, 8, 5.5, 10)))

      plot.dat <- x$EWS$dimred.ts %>%
        dplyr::mutate(across(-"time",~scale(.))) %>%
        dplyr::filter(.data$time %in% tidyr::drop_na(x$EWS$raw,"str")$time)%>%
        tidyr::pivot_longer(-"time",names_to = "dimred",values_to = "count.used")

      p2 <- ggplot(data = plot.dat, aes(x=.data$time, y=.data$count.used)) +
        geom_line(aes(col = .data$dimred))+
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        ylab("Scaled component value") +
        xlab("Time point")+
        scale_colour_manual(values = c("#A1B4FE","#FFE7A1"),name = "Dimension\nreduction")+
        theme_clean()+
        theme(plot.margin = margin(c(10, 8, 0, 10)))

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

#' Theme Clean for ggplot2
#'
#' Custom plot theme
#'
#' @param ... Additional arguments to pass to ggplot2 theme function.
#' @param base_size Base size of plot text.
#' @keywords internal
#' @noRd

theme_clean <- function(..., base_size = 12){
  ggplot2::theme(
    axis.line.x = ggplot2::element_line(
      colour = "black",
      linewidth = 0.5,
      linetype = "solid"),
    axis.line.y = ggplot2::element_line(
      colour = "black",
      linewidth = 0.5,
      linetype = "solid"),
    axis.text = ggplot2::element_text(size = ceiling(base_size * 0.7), colour = "black"),
    axis.title = ggplot2::element_text(size = ceiling(base_size * 0.8)),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(colour = "gray", linetype = "dotted"),
    panel.grid.major.x = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    panel.border = ggplot2::element_blank(),
   # strip.background = ggplot2::element_rect(linetype = 0),
    strip.background =  ggplot2::element_blank(),
    strip.text = ggplot2::element_text(),
    strip.text.x = ggplot2::element_text(vjust = 0.5),
    strip.text.y = ggplot2::element_text(angle = -90),
    plot.background = ggplot2::element_rect(colour = "black"),
    plot.title = ggplot2::element_text(size = ceiling(base_size * 1.1), face = "bold"),
    plot.subtitle = ggplot2::element_text(size = ceiling(base_size * 1.05)),
    legend.position = "right",
    legend.key = ggplot2::element_rect(fill = "white", colour = NA),
    legend.key.height = ggplot2::unit(0.3,"cm"),
    legend.key.width = ggplot2::unit(0.5,"cm"),
    legend.title = ggplot2::element_text(size = 10,face = "bold",family = "sans"),
    legend.text = ggplot2::element_text(size=10, family = "sans"),
    legend.background = ggplot2::element_blank(),
    legend.box.background = ggplot2::element_rect(colour = "black"))
}
