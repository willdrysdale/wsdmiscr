#' Convert Met Data Worker
#'
#' Passes each line of the data frame first to have the wind vectors, then temperature converted
#'
#' @param d dataframe row by row via adply

convert_met_data_wrk = function(d){
  d = convert_wind_vectors(d)
  d = convert_temp_sos_c(d)
  return(d)
}