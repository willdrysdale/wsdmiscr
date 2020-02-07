#' ggplot Annulus
#' 
#' Take the data output of \code{openair::polarAnnulus} and plot using ggplot
#' 
#' @param polarAnnulusData data from \code{openair::polarAnnulus} output
#' @param legendName name for legend
#' @param tickScale Tick width is calculated from the maximum value in the u direction times this factor. Default 0.05
#' 
#' @author W. S. Drysdale
#' 
#' @export


ggplot_Annulus = function(polarAnnulusData, legendName = "z", tickScale = 0.05){
  require(ggforce)
  require(shadowtext)
  
  circle_dat = data.frame(x0 = c(0,0),
                          y0 = c(0,0),
                          r = range(Mod(polarAnnulusData$u[!is.na(polarAnnulusData$z) & polarAnnulusData$v == 0]),
                                    na.rm = T))
  
  se = c(circle_dat$r[1],circle_dat$r[2])
  
  line_dat = data.frame(u = c(0,0,0,0,se,-se),
                        v = c(se,-se,0,0,0,0),
                        group = c("N","N","S","S","E","E","W","W"))
  
  tick_pos = seq(se[1],se[2],length.out = 7)
  
  tick_labs = c(0,rep(NA,11),23,NA)
  
  tick_width = max(polarAnnulusData$u,na.rm = T)*tickScale
  
  tick_dat = data.frame(u = c(rep(c(0-tick_width,0+tick_width),7),
                              rep(c(0-tick_width,0+tick_width),7),
                              rep(tick_pos,each = 2),
                              -rep(tick_pos,each = 2)),
                        v = c(rep(tick_pos,each = 2),
                              -rep(tick_pos,each = 2),
                              rep(c(0-tick_width,0+tick_width),7),
                              rep(c(0-tick_width,0+tick_width),7)),
                        label = c(tick_labs,tick_labs,rep(NA,28)),
                        group = c(rep(1:28,each = 2))
  )
  
  nsew_dat = data.frame(u = c(0,0,se[2]*1.1,se[2]*-1.1),
                        v = c(se[2]*1.1,se[2]*-1.1,0,0),
                        label = c("N","S","E","W"))
  
  ggplot(polarAnnulusData)+
    geom_raster(aes(u,v,fill = z))+
    geom_path(data = line_dat,
              aes(u,v,group = group),
              col = "black",
              size = 1.1)+
    ggforce::geom_circle(data = circle_dat,
                         aes(x0 = x0,y0 = x0,r = r),
                         col = "black",
                         size = 1.1)+
    geom_path(data = tick_dat,
              aes(u,v,group = group),
              size = 1.1,
              col = "black")+
    shadowtext::geom_shadowtext(data = tick_dat,
                                aes(u,v,label = label),
                                size = 5)+
    geom_text(data = nsew_dat,
              aes(x = u,
                  y = v,
                  label = label),
              size = 8,
              fontface = "bold")+
    scale_fill_gradientn(colours = viridis(200),
                         na.value = NA,
                         name = legendName)+
    theme(aspect.ratio = 1,
          panel.background = element_rect("white"),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.text = element_text(size = 15),
          legend.title = element_text(size = 15,face = "bold"))
}


