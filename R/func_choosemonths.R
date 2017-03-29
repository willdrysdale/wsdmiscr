#' Choose months
#' 
#' When chooseday() is asked for multiple months, they are selected and combined into a new data frame for further processing
#' 
#' @param d dataframe that day is to be selected from. date time information must be POSIXct in column labled "date"
#' @param mymonth month as a vector of integers, months < 10 must be given as single digit
#' 
#' @return dataframe containing all data from corresponding months

choosemonths = function(d,mymonth){
  lmonth = list()
  for (i in 1:length(mymonth)){
    lmonth[[i]] = with(d,d[month(date) == mymonth[i],])}
  f = lmonth[[1]]
  for (i in 2:length(lmonth)){
    f = rbind(f,lmonth[[i]])}
  #return
  f
}