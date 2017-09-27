#' Flight range subset
#' 
#' creates list of range events in a flight
#' 
#' @param flight_data faam merge file with date field created
#' @param flight_sum faam flight summary read through read.faaa_flight_sum
#' 
#' @author W S Drysdale
#' 
#' @export


flight_range_subset = function(flight_data,flight_sum){
  range_rows = which(flight_sum$range_event == 1)
  range_names = trimws(flight_sum$event[range_rows],"right")
  range_flight_list = list()
  for (j in 1:length(range_rows)){
    range_flight_list[[j]] = flight_data[(flight_data$date > flight_sum$start_time[range_rows[j]]) &
                                      (flight_data$date < flight_sum$end_time[range_rows[j]]),]
  }
  names(range_flight_list) = range_names
  #return
  range_flight_list
}
