#' zip_map
#' 
#' map a function across the contents of a zip file. Default reads each element using \code{read.csv} into a list created by \code{purrr::map}
#' 
#' @param path path to zip file, to pass to 
#' @param .f function to map, default \code{utils::read.csv}
#' @param map_f describes how \code{.f} will be mapped. 
#' @param zipMeta automatically obtained via \code{zipMeta = unzip(path,list = T)} unless an alternative is specified. 
#'                Files to be read must be stored in \code{zipMeta$Name}
#'                
#' @author W. S. Drysdale
#' 
#' @export


zip_map = function(path,
                   .f = utils::read.csv,
                   map_f = purrr::map,
                   zipMeta = NULL){
  require(purrr)
  
  if(is.null(zipMeta)){
    zipMeta = unzip(path,list = T)
  }
    
  
  dat = map_f(zipMeta$Name,
              ~.f(unz(path,.x)) #TODO pass custom .f arguments via ...
  )
  
  dat
}