convert_footprint_df_to_SpatialPointsDataFrame = function(df,dimpath){
  d = df
  #load dimentions  
  dim = read.table(dimpath,nrows=5, row.names=1)
  
  #calculate coordinates from dimentions
  x_coords = (seq(1,dim["ncols",])*dim["cellsize",])+dim["xllcorner",]
  yulcorner = (dim["yllcorner",]+(dim["nrows",]*dim["cellsize",]))-dim["cellsize",]
  y_coords = seq(yulcorner,dim["yllcorner",],-dim["cellsize",])
  
  #reshape footprint to long
  names(d) = x_coords
  d$north = y_coords
  d_melt = melt(d,"north",variable_name = "east")
  d_melt$east  = as.numeric(as.character(d_melt$east))
  
  #format data and coordiantes from SpatialPointsDataFrame()
  d_coords = d_melt[,1:2]
  d_data = data.frame(d_melt[,3])
  
  #convert to SpatialPointsDataFrame
  d_spdf = SpatialPointsDataFrame(coords = d_coords, data = d_data,
                                  proj4string = CRS("+proj=utm +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  #return
  d_spdf
}
