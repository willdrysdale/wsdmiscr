#' Frind Cal range
#' 
#' Examines when the cal valve is on and off and returns the range of times that the cals occured in
#' 
#' @param Raw AQD NOx output
#' 
#' @return data frame containg start and end times and rows for calibrations 

find_cal_ranges = function(d){
  start = NULL
  end = NULL
  startrow = NULL
  endrow = NULL
  #Skip NA
  for (i in 2:nrow(d)){
    if(is.na(d$NOx_cal_valve[i])){
      d$NOx_cal_valve[i] = 0
    }
    if(is.na(d$NOx_cal_valve[i-1])){
      d$NOx_cal_valve[i] = 0
    }
    #No change to Cal Valve
    if (d$NOx_cal_valve[i] == d$NOx_cal_valve[i-1])
     next
    #cal valuve opens
    if (d$NOx_cal_valve[i] > d$NOx_cal_valve[i-1]){
      start = c(start,d$UNIX_TS[i])
      startrow = c(startrow,i)
      next
    }
    #Cal valve closes
    if (d$NOx_cal_valve[i] < d$NOx_cal_valve[i-1]){
      end = c(end,d$UNIX_TS[i-1])
      endrow = c(endrow,i-1)
      next
    }
  }
  
  d2 = data.frame(parse_unix_time(start),startrow,parse_unix_time(end),endrow)
  names(d2) = c("start","startrow","end","endrow")
  return(d2)

}