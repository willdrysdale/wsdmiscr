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
  points_to_line <- function(data, long, lat, id_field = NULL, sort_field = NULL) {
    
    # Convert to SpatialPointsDataFrame
    coordinates(data) <- c(long, lat)
    
    # If there is a sort field...
    if (!is.null(sort_field)) {
      if (!is.null(id_field)) {
        data <- data[order(data[[id_field]], data[[sort_field]]), ]
      } else {
        data <- data[order(data[[sort_field]]), ]
      }
    }
    
    # If there is only one path...
    if (is.null(id_field)) {
      
      lines <- SpatialLines(list(Lines(list(Line(data)), "id")))
      
      return(lines)
      
      # Now, if we have multiple lines...
    } else if (!is.null(id_field)) {  
      
      # Split into a list by ID field
      paths <- sp::split(data, data[[id_field]])
      
      sp_lines <- SpatialLines(list(Lines(list(Line(paths[[1]])), "line1")))
      
      for (p in 2:length(paths)) {
        id <- paste0("line", as.character(p))
        l <- SpatialLines(list(Lines(list(Line(paths[[p]])), id)))
        sp_lines <- spRbind(sp_lines, l)
      }
      
      return(sp_lines)
    }
  }
  
  flight_leg = points_to_line(flight_leg,"lon_gin","lat_gin")
  
  traj = points_to_line(traj,"lon","lat")
  
  #find intersect
  inter = rgeos::gIntersection(flight_leg,traj)
  
  #create data.frame
  inter = data.frame(lon = inter@coords[1],lat = inter@coords[2])
  
  #return
  inter
  
}