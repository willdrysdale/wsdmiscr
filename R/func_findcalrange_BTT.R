#' BTT Find Cal range
#' 
#' Examines when the cal valve is on and off and returns the range of times that the cals occured in
#' used within BTT Calibration profile function
#' 
#' @param d  Bound Crit files
#' 
#' @return data frame containg start and end times and rows for calibrations 

BTT_find_cal_ranges = function(d){
  startrow = NULL
  endrow = NULL
  #Skip NA
  for (i in 2:nrow(d)){
    if(is.na(d$NO_cal[i])){
      d$NO_cal[i] = 0
    }
    if(is.na(d$NO_cal[i-1])){
      d$NO_cal[i] = 0
    }
    #No change to Cal Valve
    if (d$NO_cal[i] == d$NO_cal[i-1])
      next
    #cal valuve opens
    if (d$NO_cal[i] > d$NO_cal[i-1]){
      startrow = c(startrow,i)
      next
    }
    #Cal valve closes
    if (d$NO_cal[i] < d$NO_cal[i-1]){
      endrow = c(endrow,i-1)
      next
    }
  }
    start = d$UNIX_TS_min[startrow]
    end = d$UNIX_TS_min[endrow]
    
  d2 = data.frame(parse_unix_time(start),startrow,parse_unix_time(end),endrow)
  names(d2) = c("start","startrow","end","endrow")
  return(d2)
  
}