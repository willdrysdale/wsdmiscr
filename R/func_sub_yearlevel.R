#' Year Level Subsetting
#' 
#' Subsets the data into years, and then applies lower level subsets additionally
#' 
#' @param d dataframe of time series passed by previous subsetting function
#' 
#' @return Large list containing dataframes of years and lists with lower level subsetting applied


########## Year Level Subsetting ###########
sub_yearlevel = function(d){
  yearlist = lets_split(d,"year")
  year_sub = lapply(yearlist,sub_lowlevel)
  year_month_sub = lapply(yearlist,sub_month)
  year_season_sub = lapply(yearlist,sub_seasonlevel)
  names(year_sub) =  names(yearlist)
  yearl = list(
    yearlist,
    year_sub,
    year_month_sub,
    year_season_sub
  )
  n = c("yearlist","year_sub","year_month_sub","year_season_sub")
  names(yearl) = n
  #Return
  yearl
}