#' Unique Event Experiment
#' 
#' Produces a diurnal of a give day, and provides a comparison to the same type of day for given weeks in the past and/or future
#' e.g. Sunday 6th November 2016 is compared to an average of the preceeding 4 and following 4 sundays
#' 
#' @param d dataframe containing the time series to apply the experiment to
#' @param unique_day the date of the unique event in the format "2016/11/06"
#' @param weeks_before numerical number of weeks preeceeding to include average days from
#' @param weeks_after numerical number of weeks following to include average days from
#' @param pol pollutant to perform experiment on - only handles one pollutant at a time
#' 
#' @return Runs a time variation on the pollutant selected and returns a dataframe of the timeseries beofre being run
#' 
#' @export

exp_unique_event = function(d,unique_day,weeks_before,weeks_after,pol,period){
  unique_day = ymd(unique_day)
  days_before = data.frame(NULL)
  days_after = data.frame(NULL)
  df_unique_day = selectByDate(d,start = unique_day, end = unique_day)
  if (weeks_before > 0){
    
    days_before = selectByDate(d,start = unique_day - weeks(1), end = unique_day - weeks(1))
    for (i in 2:weeks_before){
      days_before = rbind(days_before,selectByDate(d,start = unique_day - weeks(i), end = unique_day - weeks(i)))
    }
  }
  if (weeks_after > 0){
    days_after = selectByDate(d,start = unique_day + weeks(1), end = unique_day + weeks(1))
    for (i in 2:weeks_after){
      days_after = rbind(days_after,selectByDate(d,start = unique_day + weeks(i), end = unique_day + weeks(i)))
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