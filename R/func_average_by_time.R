#' Average By Time
#' 
#' Function used to average data to new time resolution using the dplyr grouping. It is an attempt to be more efficent than
#' openair::timeAverage to be used when dealing with unusually large datasets. It is certainly nowhere near as robust. \cr
#' Use at your own risk
#' 
#' @param df data.frame
#' @param time_stamp "floor" "round" or "ceiling". How is the time_stamp rounded to the resolution? 
#' conside the implication on the resulting timestamp. Floor is default, giving a timestamp that referes to the begining
#' of the averaging period
#' @param res time resolution to round to. Use lubridate "unit" format. Default "1 min"
#' @param .f function to use when using summarise_all. default mean
#' @param time_col what column contains the time information, default date
#' @param ... extra arguments to pass to .f
#' 
#' @author W. S. Drysdale
#' 
#' @export

average_by_time = function(df,
                           time_stamp = c("floor","round","ceiling")[1],
                           res = "1 min",
                           .f = mean,
                           time_col = "date",
                           ...){
  if(time_stamp == "floor")
    round_func = lubridate::floor_date
  
  if(time_stamp == "ceiling")
    round_func = lubridate::ceiling_date
  
  if(time_stamp == "round")
     round_func = lubridate::round_date
  
  df[,time_col] = round_func(df[,time_col],unit = res)
  
  df_grp = df %>%
    dplyr::group_by_(time_col) %>% 
    dplyr::summarise_all(.f,...)
  
  #return
  as.data.frame(df_grp)
  
}
