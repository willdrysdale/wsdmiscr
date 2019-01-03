#' Read eddy4R
#' 
#' Reads the eddy4r output of Will's branch as of 2019/01/03
#' 
#' @param file_path_to_EC folder containing eddy4r output
#' @param grp name this group of files e.g run_id
#' @param dfdd date as "yyyy-mm-dd" for date_from_decimal_day
#' @param dur flux aggregation period in seconds
#' @param round sensible value to round the flux aggregation period to
#' @param output type of output required. c has the group determined in the column header, c2 has a separate group column, list is a list of both
#' @param suffix which e4r file suffixes are to be read in, default as \code{c("erro","sd","st")}. "mn" is always read
#' 
#' @return A list or data.frame, depending on \code{output} containing the mean, bound with any other files defined in \code{suffix}. With the timestamp adjusted to the begining of the period
#' 
#' @author W. S. Drysdale
#' 
#' @export 

read.e4r = function(file_path_to_EC,
                    grp,
                    dfdd = "2017-01-01",
                    dur = 3600,
                    round = "hour",
                    tz = "UTC",
                    output = c("list","c","c2")[1],
                    suffix = c("erro","stat","sd")){
  
  seg = list()
  
  seg$mn = tryCatch({
    suppressWarnings(read.csv(paste0(file_path_to_EC,"_mn.csv")))
  },error = function(e){
    message("mn file could not be read, e4r output not loaded")
    "mn_failed"
  }
  )
  
  if(class(seg$mn) == "character")
    stop()
  
  for(i in 1:length(suffix)){
    
    seg[[suffix[i]]] = tryCatch({
      read.csv(paste0(file_path_to_EC,"_",suffix[i],".csv"))
    },error = function(e){
      message(paste0(suffix[i]," file could not be read, skipping"))
    }
      )
  }
  if("sd" %in% suffix)
    names(seg$sd) %<>% paste0("_sd")
  
  #c = cbind(mn,err,st,sd)
  
  c = seg[[1]]
  for(i in 2:length(seg))
    c = cbind(c,seg[[i]])
  c2_names = names(c)
  names(c) %<>% paste0("_",grp)
  c$date = wsdmiscr::date_from_decimal_day(c[,c(paste0("DOY_",grp))],dfdd,tz = tz) %>% -(dur/2) %>% round_date(round)
  
  c2 = c
  c2 %<>% wsdmiscr::pad_time_series(dur)
  c2$grp = grp
  
  c2_names = c2_names[c2_names != "date"]
  
  names(c2) = c("date",c2_names,"grp")
  
  #Return
  if(output == "list")
    list(c = c,c2 = c2)
  if(output == "c")
    c
  if(output == "c2")
    c2
  
}
