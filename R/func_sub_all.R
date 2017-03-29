#' Subset All
#' 
#' Takes a time series and divides it into lists of various paramaeters: Year, Month, Season, Weekday/end,Night/day
#' 
#' @param d Dataframe of time series, date time information must be in column labled "date". Date information will be processed using lubridate's ymd_hms, make sure the format is compatible
#' 
#' @return Large list object with sublists ultimatley containing data frames with mixtures of the subsetting parameters applied
#' 
#' @examples sub_all(mydata)
#' 
#' @export

########## Subset Everything ###############
sub_all = function(d){
  d$date = ymd_hms(d$date) 
  d$year = year(d$date)
  d$month = month(d$date)  
  d$weekday = wday(d$date) 
  d = cutData(d, type = "daylight") 
  
  list1 = list(
    sub_lowlevel(d),
    sub_month(d),
    sub_seasonlevel(d),
    sub_yearlevel(d)
  )
  n = c("All","Month","Season","Year")
  names(list1) = n
  #Return
  list1
}