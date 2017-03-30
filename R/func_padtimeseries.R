#' Pad Time Series
#' 
#' Merges a data set into a new continuous time series
#' 
#' @param d dataframe of current time series - Date should be of POSIXct format in coloumn with header "date"
#' @param start String of start date for new timeseries in format "YYYY/MM/DD"
#' @param end String of end date for new timeseries in format "YYYY/MM/DD"
#' @param period time intervals for new time series - These should create time that match current time series
#' 
#' @return Dataframe of new timeseries
#' 
#' @export

padtimeseries = function(d,start,end,period){
  #Format dates
  s = ymd_hms(start)
  e = ymd_hms(end)
  #create new time series
  t = data.frame(seq(s,e,by = period))
  names(t) = "date"
  #Merge Time Series
  t = merge(t,d,by = "date",all = T)
  #Return
  return(t)
}