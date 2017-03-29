#' Choose Day
#' 
#' Given a dataframe, allows either a specific day, range or selction of days to be selected
#' 
#' @param d dataframe that day is to be selected from. date time information must be POSIXct in column labled "date"
#' @param myyear 4 digit year as integer 
#' @param mymonth month as integer, months < 10 must be given as single digit
#' @param myday day as integer, days < 10 must be given as single digit
#' 
#' @return dataframe containing all data from corresponding day
#' 
#' 
#' @export


chooseday = function(d,myyear,mymonth,myday){
  if (missing(myyear))  
    stop("Please Specifiy a Year")
  
  if (missing(mymonth))
    stop("Please Specifiy a Month")
  
  if (missing(myday))
    stop("Please Specifiy a Day")
  
  yearmany = F
  monthmany = F
  daymany = F

  if (length(myyear) > 1)
    yearmany = T
  
  if (length(mymonth) > 1)
    monthmany = T
  
  if (length(myday) > 1)
    daymany = T

  many = c(yearmany,monthmany,daymany)
  
  if (many[1] == T)
    d = chooseyears(d,myyear)
  else
    d = with(d,d[year(date) == myyear,])
  
  if (many[2] == T)
    d = choosemonths(d,mymonth)
  else
    d = with(d,d[month(date) == mymonth,])
    
  if (many[3] == T)
    d = choosedays(d,myday)
  else
    d = with(d,d[day(date) == myday,])

  #Return
  d
}