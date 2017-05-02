#' Find Zero range
#' 
#' Examines when the zero valve is on and off and returns the range of times that the zero occured in
#' 
#' @param d Raw AQD NOx output
#' 
#' @return data frame containg start and end times and rows for zeros 

find_zero_ranges = function(d){
  startrow = NULL
  endrow = NULL
  #Skip NA
  for (i in 2:nrow(d)){
    if(is.na(d$zero_valve_1[i])){
      d$zero_valve_1[i] = 0
    }
    if(is.na(d$zero_valve_1[i-1])){
      d$zero_valve_1[i] = 0
    }
    #No change to Cal Valve
    if (d$zero_valve_1[i] == d$zero_valve_1[i-1])
      next
    #cal valuve opens
    if (d$zero_valve_1[i] > d$zero_valve_1[i-1]){
      startrow = c(startrow,i)
      next
    }
    #Cal valve closes
    if (d$zero_valve_1[i] < d$zero_valve_1[i-1]){
      endrow = c(endrow,i-1)
      next
    }
  }
  
  if (length(endrow) > length(startrow))
    startrow = c(1,startrow)
  if (length(endrow) < length(startrow))
    endrow = c(endrow,nrow(d))
  
  if ("UNIX_TS" %in% names(d)){
    start = d$UNIX_TS[startrow]
    end = d$UNIX_TS[endrow]
  }
  if ("UNIX.TS" %in% names(d)){
    start = d$UNIX.TS[startrow]
    end = d$UNIX.TS[endrow]
  }
  
  
  d2 = data.frame(parse_unix_time(start),startrow,parse_unix_time(end),endrow)
  names(d2) = c("start","startrow","end","endrow")
  return(d2)
  
}