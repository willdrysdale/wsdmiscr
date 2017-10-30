#' Create Area Polygon
#' 
#' USing XY coordinate files with lat and lon as the first two columns creates a polygon 
#' 
#' @param top_line SpatialLines object describing the top edge of the polygon
#' @param bot_line SpatialLines object describing the bot edge of the polygon
#' @param reverse Does the bot_line need to be orderd in reverse to describe the polygon correctly, default FALSE
#' @param crs_args supply the CRS arguments to describe the polygon, default "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
#' 
#' @export
#' 
#' @author Will S. Drysdale

create_area_polygon = function(top_line,
                               bot_line,
                               reverse = F,
                               crs_args = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") {
  #reverse bot line if required to make polygon
  if (reverse == T)
    bot_line = bot_line[dim(bot_line)[1]:1, ]
  
  #take datframe and convert to SpatialPolygons
  outline = rbind(top_line, bot_line)
  outline = outline[, 1:2]
  outline = sp::Polygon(outline)
  outline = sp::Polygons(list(outline), 1)
  outline = sp::SpatialPolygons(list(outline))
  proj4string(outline) = CRS(crs_args)
  
  #Return
  outline
}