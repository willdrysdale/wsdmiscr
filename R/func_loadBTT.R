#' Load BT Tower Data
#' 
#' Loads in BT tower data based on parameters in the btconfig.R file.
#' Voncentration should be in the subdirectory "conc" and meterological data
#' in the sub directory "met"
#' Defaults to C:/Users/username/Documents/R/win-library/3.3/wsdmiscr/extdata/btconfig.R
#' 
#' @export


load_BTT = function(){
  #load filepath
  source(system.file("extdata/btconfig.R",package = "wsdmiscr"))
  #select conc file
  flist = dir(paste(btfilepath,"conc/",sep = ""))
  if (length(flist) > 1){
    print(flist)
    j = as.numeric(choosefileprompt(flist))
  } else 
    j = 1
  
  x = read.csv(paste(btfilepath,"conc/",flist[j],sep = ""))
  
  #select met file
  flist = dir(paste(btfilepath,"met/",sep = ""))
  if (length(flist) > 1){
    print(flist)
    j = as.numeric(choosefileprompt(flist))
  } else 
    j = 1
  
  y = read.csv(paste(btfilepath,"met/",flist[j],sep = ""))
  
  #merge
  z = merge(x,y,by = "date", all = T)
  #format date
  z$date = ymd_hms(z$date)
  #load into global enviroment
  assign("bt", z, envir=globalenv())
}