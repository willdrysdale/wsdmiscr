#' read.faam_flight_sum
#' 
#' Read a FAAM flight summary file into a dataframe
#' 
#' @param flight_sum_path file location of flight summary 
#' 
#' @author Will S. Drysdale


read.faam_flight_sum = function(flight_sum_path){
  #read in the fixed width flight summary
  flight_sum = read.fwf(flight_sum_path,
                        widths = c(8,9,20,18,4,30),
                        skip = 9,colClasses = "character"
                        )
  #name columns
  names(flight_sum) = c("start_time","end_time","event","height","hdg","comments")
  
  #strip flight date
  date = readLines(flight_sum_path,n = 3)[3]
  date = str_split(date,"Date: ")
  date = date[[1]][2]
  
  #strip flight number
  flight_no = readLines(flight_sum_path,n = 2)[2]
  flight_no = str_split(flight_no," ")
  flight_no = flight_no[[1]][3]
  flight_sum$flight_no = flight_no
  
  #format date time
  flight_sum$start_time[!is.na(flight_sum$start_time)] = paste(date,flight_sum$start_time[!is.na(flight_sum$start_time)],sep = " ")
  start_time = dmy_hms(flight_sum$start_time)
  flight_sum$start_time = start_time
  
  flight_sum$end_time[!is.na(flight_sum$end_time)] = paste(date,flight_sum$end_time[!is.na(flight_sum$end_time)],sep = " ")
  end_time = dmy_hms(flight_sum$end_time)
  flight_sum$end_time = end_time
  
  #create flag for events with only a timestamp (point) or duration (range)
  flight_sum$point_event = 0
  flight_sum$range_event = 0
  flight_sum$point_event[is.na(flight_sum$end_time)] = 1
  flight_sum$range_event[!is.na(flight_sum$end_time)] = 1
  
  #return
  flight_sum
  
}
