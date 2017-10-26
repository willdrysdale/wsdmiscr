#' coords_from_kml
#' 
#' Reads coordinates into a dataframe from a hysplit kml file
#' 
#' @param kml_path location of kml file

coords_from_kml = function(kml_path){
  #Read KML coordinates
  kml = maptools::getKMLcoordinates(kml_path)
  trajs = list()
  
  #Select entries with width of three
  for (i in 1:length(kml)){
    
    if (i == 1){
      if (dim(kml[[i]])[1] > 1){
        df = data.frame(kml[[i]])
        names(df) = c("lon","lat","alt")
        trajs[[1]] = df
      }
      
    }else{
      if (dim(kml[[i]])[1] > 1){
        df = data.frame(kml[[i]])
        names(df) = c("lon","lat","alt")
        trajs[[length(trajs)+1]] = df
      }
    }
  }
  
  #return
  trajs
}