#' NCAS Filename
#' 
#' Formats the filename for btnox_adddata() into the NCAS data archive format:
#' <instrument_name>_<platform_name>_<Date-Time>_<data_product>_<version>
#' 
#' @param d dataframe of data being named
#' @param instrument string name of instument
#' @param location string name of location
#' @param version integer or double version number
#' 
#' 
#' @return string filename
#' 
#' @export

ncas_filename = function(d,instrument,location,version){
  m = c("00","01","02","03","04","05","06","07","08","09")
  
  if (month(d$date[1]) < 10)
    mos = m[as.numeric(month(d$date[1])+1)]
  else 
    mos = month(d$date[1])
  
  if (day(d$date[1]) < 10)
    ds = m[as.numeric(day(d$date[1])+1)]
  else 
    ds = day(d$date[1])            
  
  if (hour(d$date[1]) < 10)
    hs = m[as.numeric(hour(d$date[1])+1)]
  else 
    hs = hour(d$date[1])
  
  if (minute(d$date[1]) < 10)
    ms = m[as.numeric(minute(d$date[1])+1)]
  else 
    ms = minute(d$date[1])
  
  j = nrow(d)
  
  if (month(d$date[j]) < 10)
    moe= m[as.numeric(month(d$date[j])+1)]
  else 
    moe = month(d$date[j])
  
  if (day(d$date[j]) < 10)
    de = m[as.numeric(day(d$date[j])+1)]
  else 
    de = day(d$date[j])        
  
  if (hour(d$date[j]) < 10)
    he = m[as.numeric(hour(d$date[j])+1)]
  else
    he = hour(d$date[j])
  
  if (minute(d$date[j]) < 10)
    me = m[as.numeric(minute(d$date[j])+1)]
  else
    me = minute(d$date[j])
  
  filename = paste(instrument,"_",location,"_",year(d$date[1]),mos,ds,"-",hs,ms,"_",year(d$date[j]),moe,de,"-",he,me,"_",
                   "v",version,sep = "")
#Return
  filename
}




