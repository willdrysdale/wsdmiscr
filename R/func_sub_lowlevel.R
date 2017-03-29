#' Low Level Subsetting
#' 
#' Applies all subsetting below month level to a given dataframe: Weekday/end and Night/day
#' 
#' @param d dataframe of time series passed by previous subsetting function
#' 
#' @return List contaning dataframes and lists of all the low level subsetting combinations

########## Low Level Subsetting ############
sub_lowlevel = function(d){
  lowlevellist = list(
    nightday = sub_nightday(d),
    wday = list(
      weekdaygrp = list(
        wdaygrp = sub_weekdaygrp(d),
        wdgrp_dn = sub_nightday(sub_weekdaygrp(d))
      ),
      weekdays = list(
        wdays = sub_weekdays(d,F),
        wdays_dn = sub_weekdays(d,T)
      )
    ),
    wend = list(
      weekendgrp = list(
        wegrp = sub_weekendgrp(d),
        wegrp_dn = sub_nightday(sub_weekendgrp(d))
      ),  
      weekends = list(
        wends = sub_weekends(d,F),
        wends_dn = sub_weekends(d,T)
      )  
    ) 
  )
  #return
  lowlevellist
}