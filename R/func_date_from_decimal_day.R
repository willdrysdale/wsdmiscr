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

date_from_decimal_day = function(decimal_day,date_origin,tz = "UTC"){
  DOY = floor(decimal_day)
  decimal = decimal_day - DOY
  DOY = DOY-1 # as.Date uses zero indexing, therfore Jan 1st is day 0
  
  day = as.Date(DOY, origin = date_origin )
  time = chron::times(decimal)
  date = as.POSIXct(paste(day,time),format = "%Y-%m-%d %H:%M:%OS",tz = tz)
  #return
  date
}