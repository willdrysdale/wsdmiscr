#' Footprint Inventory Extract
#' 
#' Extracts the values of an inventory, weighted by their flux footprint
#' 
#' @param fp footprint matrix
#' @param inv inventory as a projected raster object
#' @param loc c(lat,long) coordinates for the center of the footprint matrix
#' @param rescale default false, rescale total footprint sum to 1
#' 
#' @return The sum of the inventory weighted by the footprint
#' 
#' @export
#' 
#' @author W S Drysdale

footprint_inventory_extract = function(fp,inv,loc,full_output = F,rescale = F){
  rotate = function(x) t(apply(x, 2, rev))
  fp = fp %>% rotate %>%  melt()
  grid_size = mean(c(max(fp$X1),max(fp$X2)))
  fp$X1 = fp$X1-(max(fp$X1)/2)
  fp$X2 = fp$X2-(max(fp$X2)/2)
  if(rescale){
    fct = 1/sum(fp$value,na.rm = T)
    fp$value = fp$value*fct
  }
  
  fp$lat = ((1/length_of_lat(loc[1]))*grid_size*fp$X2)+loc[1]
  fp$lon = ((1/length_of_long(loc[1]))*grid_size*fp$X1)+loc[2]
  
  fp_spdf = SpatialPointsDataFrame(coords = fp[,c("lon","lat")],data = fp[,c("lat","lon","value")],
                                   proj4string = CRS("+proj=longlat +ellps=airy +datum=OSGB36 +no_defs"))
  
  fp_spdf = sp::spTransform(fp_spdf,crs(inv))
  
  fp_spdf$inv_vals = raster::extract(inv,fp_spdf)
  fp_spdf$inv_weight = fp_spdf$value*fp_spdf$inv_vals
  
  #Return
  fp_spdf$inv_weight %>% sum(na.rm = T)
}
