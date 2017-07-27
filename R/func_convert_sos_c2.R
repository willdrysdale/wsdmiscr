#' Convert Temperature 2
#' 
#' Converts temperature from speed of sound to Celcius
#' 
#' @param d column of data
#' 
#' @export

convert_temp_sos_c_2 = function(d){
  #mask NA
  d[is.na(d)] = -5667.9
  #Convert Speed of sound to degrees celcius
  d = (d - 331.5)/0.6
  #d = (d - 323.44)/0.5564
  
  #Unmask  NA
  d[d == -9999] = NA
  
  return(d)
  
}