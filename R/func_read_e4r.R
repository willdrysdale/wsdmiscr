#' Read eddy4R
#' 
#' Reads the eddy4r output of Will's branch as of 18/11/20
#' 
#' @param file_path_to_EC folder containing eddy4r output
#' @param grp name this group of files e.g run_id
#' @param dfdd date as "yyyy-mm-dd" for date_from_decimal_day
#' @param dur flux aggregation period in seconds
#' @param round sensible value to round the flux aggregation period to
#' 
#' @return A data.frame containing the bound mean, error, stationarity and standard deviation files. With the timestamp adjusted to the begining of the period
#' 
#' @export 

read.e4r = function(file_path_to_EC,grp,dfdd = "2017-01-01",dur = 3600,round = "hour"){
  mn = read.csv(paste0(file_path_to_EC,"_mn.csv"))
  err = read.csv(paste0(file_path_to_EC,"_erro.csv"))
  st = read.csv(paste0(file_path_to_EC,"_stat.csv"))
  sd = read.csv(paste0(file_path_to_EC,"_sd.csv"))
  
  names(sd) %<>% paste0("_sd")
  
  c = cbind(mn,err,st,sd)
  names(c) %<>% paste0("_",grp)
  c$date = wsdmiscr::date_from_decimal_day(c[,c(paste0("DOY_",grp))],dfdd) %>% -(dur/2) %>% round_date(round)
  
  c2 = c
  c2 %<>% wsdmiscr::pad_time_series(dur)
  c2$grp = grp
  names(c2) = c("date",names(mn),names(err),names(st),names(sd),"grp")
  
  #Return
  list(c = c,c2 = c2)
  
}