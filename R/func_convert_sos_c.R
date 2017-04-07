#' Convert Temperature
#' 
#' Converts temperature from speed of sound to Celcius
#' 
#' @param dataframe row by row via adply
#' 
#' @export

convert_temp_sos_c = function(d){
  #Parse NA
  if(is.na(d$sonic_temp)){
    d$temp = NA
    return(d)
  }
  #Convert Speed of sound to degrees celcius
  d$temp = (d$sonic_temp - 331.5)/0.6
  
  return(d)
  
}