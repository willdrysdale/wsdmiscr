#' Choose days
#' 
#' When chooseday() is asked for multiple days, they are selected and combined into a new data frame for further processing
#' 
#' @param d dataframe that day is to be selected from. date time information must be POSIXct in column labled "date"
#' @param mymonth days as a vector of integers, months < 10 must be given as single digit
#' 
#' @return dataframe containing all data from corresponding days

choosedays = function(d,myday){
  lday = list()
  for (i in 1:length(myday)){
    lday[[i]] = with(d,d[day(date) == myday[i],])}
  f = lday[[1]]
  for (i in 2:length(lday)){
    f = rbind(f,lday[[i]])}
  #return
  f
}