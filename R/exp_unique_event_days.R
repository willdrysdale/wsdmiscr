#' Unique Event Experiment Days
#' 
#' Variation on Unique Event Experiment
#' Produces a diurnal of a given day, and provides a comparison to the same type of day for given days in the past and/or future
#' e.g. Sunday 6th November 2016 is compared to an average of the preceeding 4 and following 4 days
#' 
#' @param d dataframe containing the time series to apply the experiment to
#' @param unique_day the date of the unique event in the format "2016/11/06"
#' @param day_before numerical number of days preeceeding to include average days from
#' @param day_after numerical number of days following to include average days from
#' @param pol pollutant to perform experiment on - only handles one pollutant at a time
#' 
#' @return dataframe ready for timeVariation
#' 
#' @export

exp_unique_event_days = function(d,unique_day,day_before,day_after,pol,period){
  unique_day = ymd(unique_day)
  days_before = data.frame(NULL)
  days_after = data.frame(NULL)
  df_unique_day = selectByDate(d,start = unique_day, end = unique_day)
  if (day_before > 0){
    
    days_before = selectByDate(d,start = unique_day - days(1), end = unique_day - days(1))
    for (i in 2:day_before){
      days_before = rbind(days_before,selectByDate(d,start = unique_day - days(i), end = unique_day - days(i)))
    }
  }
  if (day_after > 0){
    days_after = selectByDate(d,start = unique_day + days(1), end = unique_day + days(1))
    for (i in 2:day_after){
      days_after = rbind(days_after,selectByDate(d,start = unique_day + days(i), end = unique_day + days(i)))
    }
  }
  average_days = rbind(days_before,days_after)
  average_days = padtimeseries(d = average_days,start = min(average_days$date),end = max(average_days$date),period)
  df_unique_day = padtimeseries(d = df_unique_day,start = min(average_days$date),end = max(average_days$date),period)
  average_days = average_days[,c("date",pol)]
  df_unique_day = df_unique_day[,c("date",pol)]
  all_days = merge(average_days,df_unique_day,by = "date",all = T)
  #timeVariation(all_days,pol = c(names(all_days)[2],names(all_days)[3]))
  #Return
  return(all_days)
}