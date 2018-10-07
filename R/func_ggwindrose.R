#' Wind Rose (ggplot2)
#' 
#' A ggplot2 windrose
#' 
#' @param d data.frame containing winddata
#' @param ws column name for windspeed as character, default ws
#' @param wd column name for winddirection as character, default wd
#' @param breaks number of wedges to break rose into default 16 (22.5 degree bins)
#' @param nbin number of bins to separate wind speed into, default 5
#' @param col colours for bins to pass to scale_fill_manual. must be of length nbin
#' @param group parameter to pass to facet wrap. Use non-standard evaluation (unquoted) to pass to \code{facet_wrap()}
#' 
#' @export
#' 
#' @author W. S. Drysdale

ggwindRose = function(d,ws = "ws",wd = "wd",breaks = 16,nbin = 5,col = NULL,group = NULL){
  group_call = substitute(group)
  
  if(!is.null(col)){
    if(nbin > length(col))
      stop("length of col should be at least equal to nbin")
  }

    
  bin = function(d,n){
    n = n+1
    r = range(d,na.rm = T)
    breaks = seq(0,r[2],length.out = n)
    bs = breaks[1:(n-1)]
    be = breaks[2:n]
    data.frame(bin = rev(1:(n-1)),start = bs[1:(n-1)],end = be)
  }
  
  wbin = bin(d[,ws],nbin)
  
  choose_bin = function(d,bins){
    if(is.na(d))
      NA
    else
      bins$bin[d >= bins$start & d <= bins$end][1]
  }
  
  d$ws_bin = plyr::aaply(d[,ws] %>% as.array,1,function(x) choose_bin(x,bins = wbin)) %>% as.factor()
  
  if(is.null(col))
    col = viridis(nbin) %>% rev
  plt = ggplot(d)+
    geom_histogram(aes_string(wd,fill = "ws_bin"),binwidth = 360/breaks)+
    scale_fill_manual(values = col,name = "Wind Speed",
                      labels = rev(paste(round(wbin$start,2),round(wbin$end,2),sep = " - ")))+
    scale_x_continuous(breaks = c(90,180,270,360),
                       limits = c(0+(360/(2*breaks)),360+(360/(2*breaks))),
                       labels = c("    E","\nS","W    ","N\n"),name = "")+
    coord_polar(start = deg2rad(360/(2*breaks)))+
    scale_y_continuous(name = "Counts")+
    theme(plot.background = element_rect(fill = "White"),
          panel.background = element_rect(fill = "White"),
          panel.grid.major = element_line(colour = "black"),
          panel.grid.minor = element_line(colour = "black"),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(colour = "black",size = rel(2)),
          plot.margin=unit(c(0.1,0,0,0),"cm")
    )
  
  if(!is.null(group))
    plt = plt + facet_wrap(~eval(group_call))
  #return
  plt
}

