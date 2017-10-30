#' Subset Grid
#' 
#' Select an area of a grid file using a SpatialPolygons object
#' 
#' @param grid grid file e.g NAEI as SpatialGridDataFrame
#' @param sp_poly polygon defining area to subset
#' 
#' @export
#' 
#' @author Will S. Drysdale


subset_grid = function(grid,sp_poly){
  grid_p4s = as.character(grid@proj4string)
  sp_poly_p4s = as.character(sp_poly@proj4string)
  
  if(grid_p4s != sp_poly_p4s)
    grid = plotKML::reproject(grid, sp_poly@proj4string)
  
  grid = grid[sp_poly,]
  grid = grid[!is.na(grid@data),]
  grid@bbox = sp_poly@bbox
  
  #Return
  grid
}
