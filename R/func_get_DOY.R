#' Get DOY
#' 
#' from a POSIXct Date, return DOY.decimal_day
#' 
#' @param d a POSIXct Date
#' 
#' @return DOY.decimal_day
#' 
#' @export
#' 
#' @author Will S. Drysdale

get_DOY = function(d){
  
  DOY = as.numeric(lubridate::yday(d$date))
  
  h = as.numeric(lubridate::hour(d$date)*3600)
  m = as.numeric(lubridate::minute(d$date)*60)
  s = as.numeric(lubridate::second(d$date))
  
  all_seconds = h+m+s
  
  decimal_day = all_seconds/86400
  
  DOY_dd = DOY+decimal_day
  
  #Return
  DOY_dd
}