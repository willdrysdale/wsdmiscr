#' subset Season
#' 
#' Subsets into seasons spring: MAM, summer: JJA, autumn: SON, winter: DJF
#' 
#' @param d dataframe of time series passed by previous subsetting function
#' @param season character string "spring","summer,"winter","autumn" determining which season is returned
#' 
#' @return dataframe of requested season


########## Season ##########################
sub_season = function(d,season){
  #Subset/Split
  if (season == "spring"){
    s = subset(d, d$month %in% 3:5)
  }
  if (season == "summer"){
    s = subset(d, d$month %in% 6:8)
  }
  if (season == "autumn"){
    s = subset(d, d$month %in% 9:11)
  }
  if (season == "winter"){
    s = subset(d, d$month %in% c(1,2,12))
  }
  #Return
  s
}