#' Subset weekends individually
#' 
#' Forms subsets of Saturdays and Sundays separatley, with lower level subsetting avaliable
#' 
#' @param d dataframe of time series passed by previous subsetting function
#' @param dn boolean value to determine whether the function should apply the Night/day subsetting
#' 
#' @return either Data Frames for Saturday and Sunday separately or lists for each day with Night/Day subsetting applied

########## Weekdays ########################
sub_weekends = function(d,dn){
  
  #Subset/Split
  sat = subset(d, d$weekday == 7)
  sun = subset(d, d$weekday == 1)
  
  #List Formation
  if (dn == F){
    wends = list(
      sat,
      sun
    )
  }else{
    wends = list(
      sub_nightday(sat),
      sub_nightday(sun)
    )
  }
  n = c("sat","sun")
  names(wends) = n
  #Return
  wends
}