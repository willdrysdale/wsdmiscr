#' Create Wind Barbs
#' 
#' Creates a wind bard based on windspeed in meters per second \cr
#' taken from StackOverflow https://stackoverflow.com/questions/32705013/plot-wind-barb-in-r and edited for ms-1 
#' 
#' @param x windspeed in knots
#' @param mlength Controls stem length
#' @param wblength Controls Spar/Flag length
#' 
#' @export


wind_barb <- function(x, mlength=0.1, wblength=0.025, col = "black",lwd = 1) {
  library("grid")
  # Calculate which / how many barbs
  # any triangles (25)
  fif <- floor(x /25)
  # and then look for longer lines for remaining speed (5)
  tn <- floor( (x - fif* 25)/5)
  # and then look for shorter lines for remaining speed (2.5)
  fv <- floor( (x - fif* 25 - tn* 5)/2.5)
  
  # Spacing & barb length
  yadj <- 0.5+mlength
  dist <- (yadj-0.5) / 10    
  xadj <- 0.5+wblength
  xfadj <- 0.5+wblength/2        
  
  # Create grobs
  main_grob <- linesGrob(0.5, c(0.5, yadj), gp = gpar(col = col,lwd = lwd))
  
  # 50 windspeed
  if(fif != 0) {
    fify <- c(yadj, yadj-dist*seq_len(2* fif) )
    fifx <- c(0.5, xadj)[rep(1:2, length=length(fify))]
    fif_grob <- pathGrob(fifx, fify, gp=gpar(col=col,lwd = lwd))
  } else {
    fif_grob <- NULL
    fify <- yadj+dist
  }
  
  # Ten windspeed
  if(tn != 0) {
    tny <- lapply(seq_len(tn) , function(x) min(fify) - dist*c(x, x-1))  
    tn_grob <- do.call(gList, 
                       mapply(function(x,y) 
                         linesGrob(x=x, y=y, gp=gpar(col= col,lwd = lwd)),
                         x=list(c(0.5, xadj)), y=tny, SIMPLIFY=FALSE))
  } else {
    tn_grob <- NULL
    tny <- fify
  }
  
  # Five windspeed
  if(fv != 0) {
    fvy <- lapply(seq_len(fv) , function(x) min(unlist(tny)) -dist* c(x, x-0.5))
    fv_grob <- do.call(gList, 
                       mapply(function(x,y) 
                         linesGrob(x=x, y=y, gp=gpar(col=col,lwd = lwd)),
                         x=list(c(0.5, xfadj)), y=fvy, SIMPLIFY=FALSE))
  } else {
    fv_grob <- NULL
  }    
  
  # Draw    
  #grid.newpage()
  grid.draw(gList(main_grob, fif_grob, tn_grob, fv_grob))
}