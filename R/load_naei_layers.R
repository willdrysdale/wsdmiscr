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
  
  if(length(files) == 0){
    files = list.files(dir)
    files = files[str_detect(files,".txt")]
  }
  
  file_total = ifelse(str_detect(paste0(files,collapse = ""),"total"),files[str_detect(files,"total")],NULL)
  file_reac = ifelse(str_detect(paste0(files,collapse = ""),"rea"),files[str_detect(files,"rea")],NULL)
  
  if(!is.null(file_total)){
    files = files[!str_detect(files,"tota")]
    tot = rgdal::readGDAL(paste0(dir,file_total)) %>% raster()
  }
  
  if(!is.null(file_reac)){
    rea = rgdal::readGDAL(paste0(dir,file_reac)) %>% raster()
  }
  
  if(all(!is.null(file_total),!is.null(file_reac))){
    point = tot-rea
  }
  
  for(i in 1:length(files)){
    if(i == 1){
      naei = rgdal::readGDAL(paste0(dir,files[i])) %>% raster()
    }else{
      temp = rgdal::readGDAL(paste0(dir,files[i])) %>% raster()
      naei = raster::stack(naei,temp)
    }
  }
  
  if(!is.null(file_reac)){
    naei = raster::stack(naei,point)
    names(naei) = str_remove(c(files,str_replace(file_total,"total","point")),".asc")
  }else{
    names(naei) = str_remove(files[-which(str_detect(files,"total"))],".asc")
  }

  
  #return
  naei
}
