#' Subset Weekdays individually
#' 
#' Forms subsets of weekdays separatley, with lower level subsetting avaliable
#' 
#' @param d dataframe of time series passed by previous subsetting function
#' @param dn boolean value to determine whether the function should apply the Night/day subsetting
#' 
#' @return either Data Frames for weekdays separately or lists for each day with Night/Day subsetting applied

########## Weekdays ########################
sub_weekdays = function(d,dn){
  
  #Subset/Split
  mon = subset(d, d$weekday == 2)
  tue = subset(d, d$weekday == 3)
  wed = subset(d, d$weekday == 4)
  thu = subset(d, d$weekday == 5) 
  fri = subset(d, d$weekday == 6)
  
  #List Formation
  if (dn == F){
    wdays = list(
      mon,
      tue,
      wed,
      thu,
      fri
    )
  } else {
    wdays = list(
      sub_nightday(mon),
      sub_nightday(tue),
      sub_nightday(wed),
      sub_nightday(thu),
      sub_nightday(fri)
    )  
  }
  n = c("mon","tues","wed","thurs","fri")
  names(wdays) = n
  #Return
  wdays
}