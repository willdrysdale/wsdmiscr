#' latlon_to_bbox
#' 
#' Converts a lat lon pair of coordinates to a roughly square bounding box
#' 
#' 
#' @param latlon lat lon pair as a vector
#' @param res_in_km rough resolution in kilometers
#' 
#' @return bounding box of  c(lowerleftlon,lowerleftlat,upperrightlon,upperrightlat), which can be passed directly to \code{ggmap::get_stamenmap}
#' 
#' @author W. S. Drysdale
#' 
#' @export



latlon_to_bbox = function(latlon,res_in_km = 100){
  
  lat_length = wsdmiscr::length_of_lat(latlon[1])/1000
  lon_length = wsdmiscr::length_of_long(latlon[1])/1000
  ratio = lat_length/lon_length
  lat_length = (1/lat_length)*res_in_km
  lon_length = lat_length/ratio
  
  c(latlon[2]-(lat_length/2),
    latlon[1]-(lon_length/2),
    latlon[2]+(lat_length/2),
    latlon[1]+(lon_length/2))
}
