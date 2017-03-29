#' Season Level Subsetting
#' 
#' Applies sub_season() for each season, and then applies lower level subsets additionally
#' 
#' @param d dataframe of time series passed by previous subsetting function
#' 
#' @return Large list containing data frames of subsetted seasons and lists with lower level subsetting applied

########## Season Level Subsetting #########
sub_seasonlevel = function(d) {
  seasons = c("spring","summer","autumn","winter")
  seasonl = list(
    spring = sub_season(d,seasons[1]),
    summer = sub_season(d,seasons[2]),
    autumn = sub_season(d,seasons[3]),
    winter = sub_season(d,seasons[4]),
    spring_sub = sub_lowlevel(sub_season(d,seasons[1])),
    summer_sub = sub_lowlevel(sub_season(d,seasons[2])),
    autumn_sub = sub_lowlevel(sub_season(d,seasons[3])),
    winter_sub = sub_lowlevel(sub_season(d,seasons[4]))
  )
}