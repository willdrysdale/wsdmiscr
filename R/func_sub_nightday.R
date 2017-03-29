#' Subset Night/Day
#' 
#' Subsets data based on whether the data was recorded during the day or night
#' 
#' @param d dataframe of time series passed by previous subsetting function
#' 
#' @return list of two dataframes containg subsetting by day or night time


########## Night|Day #######################
sub_nightday = function(d){
  
  #Subset/Split
  daylight = subset(d, d$daylight == "daylight") 
  nighttime = subset(d, d$daylight == "nighttime")
  
  #List Formation
  nightday = list(
    daylight,
    nighttime
  )
  n = c("day","night")
  names(nightday) = n
  #Return
  nightday
}