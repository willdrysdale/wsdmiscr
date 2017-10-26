#' date_from_decimal_day
#' 
#' converts DOY.decimalday into a POSIXct object.
#' 
#' @param decimal_day date stored as DOY.decimalday
#' @param date_origin origin of decimal_day as YYYY/MM/DD
#' 
#' @return decimal_day as POSIXct
#' 
#' @export

date_from_decimal_day= function(decimal_day,date_origin){
  DOY = floor(decimal_day)
  decimal = decimal_day - DOY
  
  day = as.Date(DOY, origin = date_origin )
  time = chron::times(decimal)
  date = ymd_hms(paste(day,time))
  date = date + 86400 #correct for date being one day early
  #return
  date
}