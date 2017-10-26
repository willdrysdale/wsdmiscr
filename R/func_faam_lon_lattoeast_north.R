#' FAAM lon lat to easting northing
#' 
#' Takes the lat lon information from a faam merge file and adds easting and northing columsn
#' 
#' @param df faam merge csv with date column
#' 
#' @param missing_data missing dat flag for this flight
#' 
#' @return original datafram with easting and northing columns
#' 
#' @author Will S. Drysdale
#' @author Beth Nelson
#' 
#' @export


faam_lon_lat.to.east_north = function(df,missing_data){
  
  ukgrid = "+init=epsg:27700"
  latlong = "+init=epsg:4326"
  
  df[df == missing_data] = NA
  
  d = data.frame(lon_gin = df$lon_gin,lat_gin = df$lat_gin,date = df$date)
  d = d[complete.cases(d),]
  d[d$lat_gin == 0,] = NA
  d = d[complete.cases(d),]
  d$ID = seq(1:nrow(d))
  coords_ll = cbind(d$lon_gin,d$lat_gin)
  
  coords_ll = sp::SpatialPointsDataFrame(coords_ll,data = data.frame(date = d$date),proj4string = CRS(latlong))
  coords_en = sp::spTransform(coords_ll,CRS(ukgrid))
  
  coords_en = data.frame(coords_en@coords)
  coords_en = cbind(d$date,coords_en)
  names(coords_en) = c("date","easting","northing")
  df = left_join(df,coords_en,"date")
  #return
  df
}