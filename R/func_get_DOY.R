#' Get DOY
#' 
#' from a POSIXct Date, return DOY.decimal_day
#' 
#' @param d a POSIXct Date or data.frame containing a column named date
#' 
#' @return DOY.decimal_day
#' 
#' @export
#' 
#' @author Will S. Drysdale

get_DOY = function(d){
  
  if("date.frame" %in% class(d))
    x = d$date
  else
    x = d
  
  DOY = as.numeric(lubridate::yday(x))
  
  h = as.numeric(lubridate::hour(x)*3600)
  m = as.numeric(lubridate::minute(x)*60)
  s = as.numeric(lubridate::second(x))
  
  all_seconds = h+m+s
  
  decimal_day = all_seconds/86400
  
  DOY_dd = DOY+decimal_day
  
  #Return
  DOY_dd
}
