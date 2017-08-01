#' Find Range
#' 
#' Used to return ranges of rows where a valve has switched from 0 to 1 or vice versa. Improves upon find_cal_range and find_zero_range
#' 
#' @param d dataframe with data and time stamp stored as UNIX_TS or UNIX.TS
#' @param col numerical value for column containg valve values
#' 
#' @return dataframe containing "start","startrow","end","endrow" 


find_ranges = function(d,col){
  switch = d[,col]
  switch[is.na(switch)] = 0
  
  ranges = rle(switch)
  rows = cumsum(ranges$lengths)
  if (ranges$values[1] == 0){
    startrow = rows[seq(1,length(rows),2)]
    
    endrow = rows[seq(2,length(rows),2)]
  }else{
    endrow = rows[seq(1,length(rows),2)]
    startrow = rows[seq(2,length(rows),2)]
    startrow = c(1,startrow)
  }
  if(length(startrow) != length(endrow))
    startrow = startrow[1:length(startrow)-1]
  
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