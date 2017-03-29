#' Subset Weekends together
#' 
#' Forms subset containing data from both Saturdays and Sundays
#' 
#' @param d dataframe of time series passed by previous subsetting function
#' 
#' @return dataframe containing data from Saturday and Sundays

########## Weekend Group ###################
sub_weekendgrp = function(d){
  
  #Subset/Split
  weekend = subset(d, d$weekday %in% c(1,7))
  
  #Return
  weekend
}