#' Choose years
#' 
#' When chooseday() is asked for multiple years, they are selected and combined into a new data frame for further processing
#' 
#' @param d dataframe that day is to be selected from. date time information must be POSIXct in column labled "date"
#' @param myyear 4 digit year as vector of integers
#' 
#' @return dataframe containing all data from corresponding years

chooseyears = function(d,myyear){
  lyear = list()
  for (i in 1:length(myyear)){
    lyear[[i]] = with(d,d[year(date) == myyear[i],])}
  f = lyear[[1]]
  for (i in 2:length(lyear)){
    f = rbind(f,lyear[[i]])}
  #return
  f
  }