#' Load NAEI Layers
#' 
#' Read the unzipped NAEI sector layers into a raster stack
#' 
#' @param dir directory containing".asc" files
#' 
#' @author W. S. Drysdale
#' 
#' @export

load_naei_layers = function(dir = "C:/Users/Will/Google Drive/PhD/site_BT Tower/NAEI Data/NAEI NO2 2014/"){
  require(raster)
  require(stringr)
  require(rgdal)
  
  files = list.files(dir)
  files = files[str_detect(files,".asc")]
  file_total = files[str_detect(files,"total")]
  file_reac = files[str_detect(files,"rea")]
  files = files[!str_detect(files,"tota")]
  
  tot = rgdal::readGDAL(paste0(dir,file_total)) %>% raster()
  rea = rgdal::readGDAL(paste0(dir,file_reac)) %>% raster()
  
  point = tot-rea
  
  for(i in 1:length(files)){
    if(i == 1){
      naei = rgdal::readGDAL(paste0(dir,files[i])) %>% raster()
    }else{
      temp = rgdal::readGDAL(paste0(dir,files[i])) %>% raster()
      naei = raster::stack(naei,temp)
    }
  }
  naei = raster::stack(naei,point)
  names(naei) = str_remove(c(files,str_replace(file_total,"total","point")),".asc")
  
  #return
  naei
}
