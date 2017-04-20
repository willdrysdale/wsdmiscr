#' Frind Cal range
#' 
#' Examines when the cal valve is on and off and returns the range of times that the cals occured in
#' 
#' @param Raw AQD NOx output

find_cal_ranges = function(d){
  start = NULL
  end = NULL
  startrow = NULL
  endrow = NULL
  for (i in 2:nrow(d)){
    if(is.na(d$NOx_cal_valve[i])){
      d$NOx_cal_valve[i] = 0
      next
    }
    
    if(is.na(d$NOx_cal_valve[i-1])){
      d$NOx_cal_valve[i] = 0
      next
    }
    
    if (d$NOx_cal_valve[i] == d$NOx_cal_valve[i-1])
     next
    
    if (d$NOx_cal_valve[i] > d$NOx_cal_valve[i-1]){
      start = c(start,d$TheTime_edit[i])
      startrow = c(startrow,i)
      next
    }
    
    if (d$NOx_cal_valve[i] < d$NOx_cal_valve[i-1]){
      end = c(end,d$TheTime_edit[i-1])
      endrow = c(endrow,i-1)
      next
    }
  }
  
  d2 = data.frame(parse_excel_date(start),startrow,parse_excel_date(end),endrow)
  names(d2) = c("start","startrow","end","endrow")
  return(d2)

}