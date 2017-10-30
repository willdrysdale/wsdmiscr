#' faam_leg_hysplit_intersect
#' 
#' Calculates the interesct between a flight leg from flight_range_subset and a hysplit trajectory from coords_from_kml
#' 
#' @param flight_leg flight_range_subset data.frame
#' @param traj coords_from_kml data.frame
#' 
#' @return dataframe[lon,lat] 
#' 
#' @author Will S Drysdale
#' 
#' @export

faam_leg_hysplit_intersect = function(flight_leg,traj){
  
  #Function to transform coord data to sp objects
  
  
  flight_leg = wsdmiscr::points_to_line(flight_leg,"lon_gin","lat_gin")
  
  traj = points_to_line(traj,"lon","lat")
  
  #find intersect
  inter = rgeos::gIntersection(flight_leg,traj)
  
  #create data.frame
  inter = data.frame(lon = inter@coords[1],lat = inter@coords[2])
  
  #return
  inter
  
}