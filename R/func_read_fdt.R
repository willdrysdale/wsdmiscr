#' Read Float Data Format
#' 
#' Read the .fdt output of PTRwid doi: 10.5194/amt-8-3903-2015
#' 
#' @param filename path to .fdt file
#' @param headers character vector of header names, or data.frame where names(headers) returns a character vector of header names
#' 
#' @return data.frame
#' 
#' @export

read_fdt = function(filename, headers = NULL){
  # get number of dimentions
  ndim = readLines(filename,n = 1) %>% as.integer()
  
  # get length of dimentions
  dimen = vector("integer",ndim)
  for(i in seq_along(1:ndim)){
    dimen[i] = readr::read_lines(filename,skip = i,n_max = 1) %>% 
      as.integer()
  }
  
  # load main data
  long = read.table(filename,skip = ndim+1)
  
  # reshape into data.frame
  wide = long$V1 %>% 
    array(dim = dimen) %>% 
    t() %>% 
    data.frame()
  
  # format headers if supplied
  if(is.null(headers))
    return(wide)
  else{
    if(class(headers) == "character"){
      names(wide) = headers
      return(wide)
    }
    if(class(headers) == "data.frame"){
      names(wide) = names(headers)
      return(wide)
    }
    
    warning("headers must be of class character or data.frame, returning file with default names")
    wide
  }
}
