#' Pad Time Series
#' 
#' Pad a time series by joining it to a regular timestamp
#' 
#' @param d data frame with a "date" of class POSIXct POSIXt
#' @param period regular time series period in seconds
#' 
#' @author W S Drysdale
#' 
#' @export


pad_time_series = function(d,period){
  if(!"date" %in% names(d))
    stop("Column `date` not found")
  if(!paste0(class(d$date),collapse = "") == "POSIXctPOSIXt")
    stop("date must be of class POSIXct POSIXt")
  
  ts = data.frame(date = seq(min(d$date,na.rm = T),max(d$date,na.rm = T),period))
  
  left_join(ts,d,"date")
  
}
