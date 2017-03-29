#' Subset Weekdays together
#' 
#' Forms subsets of weekdays combined
#' 
#' @param d dataframe of time series passed by previous subsetting function
#' 
#' @return dataframe with weekdays only


########## Weekday Group ###################
sub_weekdaygrp = function(d){
  
  #Subset/Split
  weekday = subset(d, d$weekday %in% c(2:6))
  
  #Return
  weekday
}