#' ggplot polarPlot
#' 
#' Take the data output of \code{openair::polarPlot} and plot using ggplot
#' 
#' @param polarPlotData data from \code{openair::polarPlot} output
#' @param legendName name for legend
#' @param nudge_nsew nudges the position of the N,S,E,W labels by multipling the radius of the plot. Default 1.1
#' @param label Add a label in the upper right corner - useful when constructing multipanel figures. Default NULL
#' 
#' @author W. S. Drysdale
#' 
#' @export


ggplot_polarPlot = function(polarPlotData, legendName = "z", nudge_nsew = 1.1, label = NULL){
  
  require(ggforce)
  require(shadowtext)
  
  deg2rad = function(deg) {(deg * pi) / (180)}
  
  u_breaks = pretty(Mod(polarPlotData$u))
  u_breaks = u_breaks[!u_breaks %in% 0]
  
  circle_dat = data.frame(x0 = rep(0,length(u_breaks)),
                          y0 = rep(0,length(u_breaks)),
                          r = u_breaks,
                          size = c(rep(0.8,(length(u_breaks)-1)),1.1))
  
  lab_dat = data.frame(x = -u_breaks*sin(deg2rad(45)),
                       y = u_breaks*cos(deg2rad(45)),
                       labels = u_breaks)
  
  se = range(polarPlotData$u,na.rm = T)
  
  nsew_dat = data.frame(u = c(0,0,se[2]*nudge_nsew,se[2]*-nudge_nsew),
                        v = c(se[2]*nudge_nsew,se[2]*-nudge_nsew,0,0),
                        label = c("N","S","E","W"))
  
  g = ggplot(polarPlotData)+
    geom_raster(aes(u,v,fill = z))+
    ggforce::geom_circle(data = circle_dat,
                         aes(x0 = x0,
                             y0 = y0,
                             r = r,
                             size = size))+
    shadowtext::geom_shadowtext(data = lab_dat,
                                aes(x = x,
                                    y = y,
                                    label = labels),
                                size = 5)+
    geom_text(data = nsew_dat,
              aes(x = u,
                  y = v,
                  label = label),
              size = 8,
              fontface = "bold")+
    scale_fill_gradientn(colours = viridis(200),na.value = NA,name = legendName)+
    scale_size_continuous(range = c(0.8,1.1))+
    guides(size = "none")+
    theme(aspect.ratio = 1,
          panel.background = element_rect("white"),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 15,face = "bold"))
  
  if(!is.null(label))
    
    g = g+shadowtext::geom_shadowtext(data = data.frame(x = se[2], y = se[2], label = label),
                                      aes(x = x,y = y, label = label),size = 12)
  
  # Return
  g
  
}



